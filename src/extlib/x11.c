
/*    
 * libneuro, a light weight abstraction of high or lower libraries 
 * and toolkit for applications.
 * Copyright (C) 2005-2006  Nicholas Niro, Robert Lemay
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/* x11.c
 * X11 driver(abstraction layer) source.
 */

#include <global.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>

#include <events.h> /* to send input trigger events */
#include <extlib.h>
#include <ebuf.h>
#include <other.h>
#include <graphics.h>
#include "../video/video.h" /* Graphics_RedrawScreen */
#include <errno.h> /* errno */

#define buffer_old_method 0
#define USE_ALPHA 1

NEURO_MODULE_CHANNEL("x11");

/* hardcoded generated alpha results */
#include "alpha.inc"

typedef struct V_OBJECT
{
	Display *display;
	i32 screen;
	GC GC;

	Window *cwin; /* pointer to the current window in use or pixmap */
	Window rwin; /* the root window */
	Window win;

	XGCValues wValue;
	
	XImage *raw_data;
	Pixmap data; /*The set of pixels for the image*/
	Pixmap shapemask; /*The bitmask for transparency*/
	u8 pixel_data_changed; /* if this is set to 1, next blit will do a XPutImage 
	to flush the pixels with the actual image on the server*/

	u8 alpha; /* the alpha to be applied to the image */
}V_OBJECT;

static EBUF *vobjs;

static V_OBJECT *dmain;
static V_OBJECT *scldmain; /* buffer (double) */

#if temp
static Pixmap pixel; /*a 1x1 pixel buffer fo pixels Input Output*/
#endif /* temp */

static i32 swidth = 800, sheight = 600; /* externally settable screen size */

static u8 Toggle_Exposed = 1;
static u8 mouse_wheel = 0; /* mouse wheel variable */
static char current_keymap[32]; /* present map of keys pressed */

static XColor tpixel;

static void DirectDrawAlphaRect(Rectan *rectangle, u32 color, u32 alpha) __attribute__((__unused__));

static void
clean_Vobjects(void *src)
{
	V_OBJECT *buf;

	buf = (V_OBJECT*)src;

	/* if (!buf)
		return; */

	TRACE(Neuro_s("cleaning process ... elem 0x%x", buf));

	/*if (dmain->display == buf->display)
		Debug_Val(10, "This element is the main display --\n");*/
	
	if (buf->data)
	{
		TRACE("Freeing pixmap data");
		XFreePixmap(dmain->display, buf->data);
	}

	if (buf->shapemask)
	{
		TRACE("Freeing pixmap mask");
		XFreePixmap(dmain->display, buf->shapemask);
	}

	if (buf->raw_data)
	{
		TRACE("destroying XImage raw data");
		(buf->raw_data->f.destroy_image)(buf->raw_data);
	}
	
	if (buf->GC)
	{
		TRACE("Freeing Graphic Context");
		XFreeGC(buf->display, buf->GC);
	}
	
	if (buf->win)
	{
		TRACE("Destroying Window");
		XDestroyWindow(buf->display, buf->win);
	}
	
	if (buf->display)
	{
		TRACE(Neuro_s("Closing display elem 0x%x display 0x%x", buf, buf->display));
		XCloseDisplay(buf->display);
	}
	/* Debug_Val(10, "cleaning process done ...\n"); */
}

static char
keycode_value(char num, u8 *anchor)
{
	u8 values[8] = {
		1, 2, 4, 8, 16, 32, 
		64, 128
	};
	u8 spcl_used = 0;
	u8 i = 8;

	/* the anchor is used in case a num contains 
	 * more than one key at a time. It is 
	 * used to skip the last key we sent
	 * the status of and be able to send the
	 * second. This method only works when 2
	 * keys are pressed at once, not 3 or more...
	 * ( I really doubt that would be needed)
	 */
	
	while (i-- > 0)
	{
		if ((num & values[i]) == values[i])
		{
			if (*anchor == 1 && spcl_used == 0)
			{
				spcl_used = 1;
				continue;
			}
			else
			{
				if (*anchor == 1)
				{
					*anchor = 0;
				}
				else
				{
					if (num - (num & values[i]) > 0)
						*anchor = 1;	
				}
			}
			
			return i;
		}
	}

	/* avoid death loops */
	if (*anchor == 1)
		*anchor = 0;
	return -1;
}

/* our own abstraction of the function XPutImage to support 
 * the shared memory version transparently
 */
static int ab_XPutImage(Display *display, Drawable d, GC gc, XImage *image,
		int src_x, int src_y, int dest_x, int dest_y, unsigned int width,
		unsigned int height)
{
	return XPutImage(display, d, gc, image, src_x, src_y, dest_x, dest_y, width, height);
}

/* our own abstraction of the function XGetImage to support 
 * the shared memory version transparently
 */
static XImage *ab_XGetImage(Display *display, Drawable d, int x, int y, unsigned int width,
		unsigned int height, unsigned long plane_mask, int format)
{
	return XGetImage(display, d, x, y, width, height, plane_mask, format);
}

static void 
sync_pixels(V_OBJECT *src)
{
	i32 h, w;
	
	if (src->pixel_data_changed == 1)
	{
		Neuro_GiveImageSize(src, &w, &h);

		if (src->raw_data)
			(src->raw_data->f.destroy_image)(src->raw_data);
		/* Debug_Val(0, "cwin %d size %dx%d\n", *src->cwin, w, h); */
		src->raw_data = ab_XGetImage(dmain->display, *src->cwin, 
			0, 0, w, h, 
			AllPlanes, ZPixmap);
		src->pixel_data_changed = 0;
	}	
}

/*  */
static void
CreatePixmap(XImage *image, Pixmap master, Pixmap *pix)
{
	GC gc = NULL;
	XGCValues values;
	int _err = 0;

	*pix = 0;

	if (!image)
		return;

	/* Debug_Val(0, "%dx%d bpp %d\n", image->width, image->height, image->depth); */
	
	*pix = XCreatePixmap(dmain->display, master, image->width, image->height, image->depth);

	/* in case we have a XYBitmap */
	values.foreground = 1;
	values.background = 0;

	gc = XCreateGC(dmain->display, *pix, GCForeground | GCBackground, &values);
	
	_err = ab_XPutImage(dmain->display, *pix, gc, image, 0, 0, 
		 0, 0, image->width, image->height);
	
	
	if (_err != 0)
	{
		ERROR(Neuro_s("error number %d couldn't put pixels in the shapemask pixmap.\n", _err));

		if (*pix)
			XFreePixmap(dmain->display, *pix);
	}

	if (gc)
		XFreeGC(dmain->display, gc);
}

/* creates a mask */
static XImage *
CreateMask(v_object *vobj, i32 width, i32 height)
{
	XImage *mask = NULL;
	V_OBJECT *obj;

	obj = (V_OBJECT*)vobj;
	
	if (!obj)
		return NULL;
	
	mask = XCreateImage(dmain->display, XDefaultVisual(dmain->display, dmain->screen), 
			1, XYBitmap, 0, NULL, width, height, 8, 0);

	if (!mask)
		return NULL;

	if (mask->height != height)
	{
		ERROR(Neuro_s("Incorrect mask height %d need to be %d", mask->height, height));
		
		return NULL;
	}
	
	/* Debug_Val(0, "BYTES PER LINE %d\n", mask->bytes_per_line); */
	
	mask->data = malloc(mask->bytes_per_line * height);
	
	return mask;
}

void
Lib_SyncPixels(v_object *src)
{
	V_OBJECT *tmp;
	i32 h, w;

	tmp = (V_OBJECT*)src;

	if (!tmp)
		return;
	
	Neuro_GiveImageSize(tmp, &w, &h);
	if (tmp->raw_data)
		(tmp->raw_data->f.destroy_image)(tmp->raw_data);	

	tmp->raw_data = ab_XGetImage(dmain->display, *tmp->cwin, 
		0, 0, w, h, 
		AllPlanes, ZPixmap);
}

void
Lib_SetScreenSize(i32 width, i32 height)
{
	swidth = width;
	sheight = height;
}

void
Lib_GetScreenSize(i32 *width, i32 *height)
{
	*width = swidth;
	*height = sheight;
}

/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{	
	V_OBJECT *tmp;
	XSetWindowAttributes wattrib;
	/* XGCValues wValue; */

	
	Neuro_CreateEBuf(&vobjs);
	Neuro_SetcallbEBuf(vobjs, clean_Vobjects);
	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
	
	tmp = Neuro_GiveCurEBuf(vobjs);
	
	tmp->display = XOpenDisplay(NULL);
	tmp->screen = XDefaultScreen(tmp->display);

	tmp->rwin = XRootWindow(tmp->display, tmp->screen);

	/* uncomment this if u want it all to be on the root window 
	 * and comment this if u want a window.
	 */
	/* tmp->cwin = &tmp->rwin; */
	
	if (tmp->cwin == NULL)
	{
		wattrib.backing_store = WhenMapped;
		wattrib.background_pixel = BlackPixel(tmp->display, tmp->screen);
		wattrib.event_mask = KeyPressMask | KeyReleaseMask | ButtonPressMask | 
					ButtonReleaseMask | EnterWindowMask |
					LeaveWindowMask | PointerMotionMask |
					Button1MotionMask |
					Button2MotionMask | Button3MotionMask |
					Button4MotionMask | Button5MotionMask |
					ExposureMask | VisibilityChangeMask |
					StructureNotifyMask |
					SubstructureNotifyMask | SubstructureRedirectMask |
					FocusChangeMask | PropertyChangeMask |
					ColormapChangeMask | OwnerGrabButtonMask;
	
		tmp->win = XCreateWindow(tmp->display, tmp->rwin,
			200, 200, swidth, sheight, 1, CopyFromParent,
			CopyFromParent, NULL, CWBackingStore | CWBackPixel | CWEventMask, &wattrib);
		
		tmp->cwin = &tmp->win;
	
		XMapWindow(tmp->display, tmp->win);
	}

	/* XSelectInput(tmp->display, *tmp->cwin, ExposureMask | KeyPressMask | KeyReleaseMask | ButtonPressMask |
		       ButtonReleaseMask | FocusChangeMask | PointerMotionMask); */

	XFlush(tmp->display);

	/* Debug_Val(0, "graphics exposures %d\n", tmp->wValue.graphics_exposures); */
	tmp->wValue.graphics_exposures = 0;
	tmp->wValue.foreground = 1;
	tmp->wValue.background = 0;
	tmp->GC = XCreateGC(tmp->display, *tmp->cwin, GCGraphicsExposures |
			GCForeground | GCBackground, &tmp->wValue);
	
	dmain = tmp;
	*screen = tmp;

	
	if (screen_buf)
	{
		V_OBJECT *tmp2;

		tmp2 = (V_OBJECT*)Lib_CreateVObject(0, swidth, sheight, 
				DefaultDepth(dmain->display, dmain->screen), 0, 0, 0, 0);

		if (tmp2 == NULL)
		{
			ERROR("loading the default surface failed");
			return 1;
		}

		scldmain = tmp2;
		*screen_buf = tmp2;
	}
	else
	{
		scldmain = tmp;
	}

	tpixel.red = 0xffff;
	tpixel.green = 0;
	tpixel.blue = 0;
	tpixel.flags = DoRed | DoGreen | DoBlue;
	XAllocColor(dmain->display, DefaultColormap(dmain->display, dmain->screen), &tpixel);

	/* Debug_Val(0, "Screen addr %d buffer addr %d\n", dmain, scldmain); */
	return 0;
}

u32 
Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b)
{
#if ask_X11_for_color
	XColor color;
	
	if (IsLittleEndian)
	{
		color.red = (u16)(r << 8) | r;
		color.green = (u16)(g << 8) | g;
		color.blue = (u16)(b << 8) | b;
	}
	else
	{
		color.red = (u16)(r >> 8) | r;
		color.green = (u16)(g >> 8) | g;
		color.blue = (u16)(b >> 8) | b;
	}
	color.flags = DoRed | DoGreen | DoBlue;
	XAllocColor(dmain->display, DefaultColormap(dmain->display, dmain->screen), &color);

	return color.pixel;
#endif /* ask_X11_for_color */

	u32 output;
	
	switch (DefaultDepth(dmain->display, dmain->screen))
	{
		case 32:
		{
			output = Neuro_GiveRGB32(r, g, b);
		}
		break;

		case 24:
		{
			output = Neuro_GiveRGB24(r, g, b);
		}
		break;

		case 16:
		{
			output = Neuro_GiveRGB16(r, g, b);
		}
		break;

		case 8:
		{
			output = Neuro_GiveRGB8(r, g, b);
		}
		break;

		default:
		{
			ERROR("INVALID Depth! we only support 32, 24, 16 or 8 bits screen depth");
			return 0;
		}
		break;
	}

	return output;
}

void
Lib_SetColorKey(v_object *vobj, u32 key)
{
	i32 width;
	i32 height;
	u32 x;
	u32 y;
	V_OBJECT *buf;
	XImage *mask_data;
	
	buf = (V_OBJECT*)vobj;

	if (!buf)
		return;

	Neuro_GiveImageSize(vobj, &width, &height);

	if (buf->shapemask)
	{
		/* we fetch the pixel data of the existing mask */
		mask_data = ab_XGetImage(dmain->display, buf->shapemask, 0, 0, width, height, AllPlanes, ZPixmap);	
	}
	else
	{
		/* we create a new mask pixel data for the image */
		mask_data = CreateMask(vobj, width, height);
	}

	if (!mask_data)
	{
		ERROR("the variable mask_data is empty");
		return;
	}

	/* consistency check */
	if (mask_data->width != width)
	{
		ERROR(Neuro_s("mask_data has a different width than the image! mask_data width %d image width %d", mask_data->width, width));

		(mask_data->f.destroy_image)(mask_data);

		return;
	}

	if (mask_data->height != height)
	{
		ERROR(Neuro_s("mask_data has a different height than the image! mask_data height %d image height %d", mask_data->height, height));

		(mask_data->f.destroy_image)(mask_data);

		return;
	}

	/* we populate the mask with transparency info */

	y = height;
	while (y-- > 0)
	{
		x = width;
		while (x-- > 0)
		{
			if(Lib_GetPixel(vobj, x, y) == key)
			{
				(mask_data->f.put_pixel)(mask_data, x, y, 0);
			}
			else
			{
				(mask_data->f.put_pixel)(mask_data, x, y, 1);
			}
		}
	}
	
	
	if(!buf->shapemask)
	{
		CreatePixmap(mask_data, *buf->cwin, &buf->shapemask);
	}
	else
	{ /*If the mask already exists*/
		GC gc = NULL;
		XGCValues values;
		int _err = 0;

		/* in case we have a XYBitmap */
		values.foreground = 1;
		values.background = 0;

		gc = XCreateGC(dmain->display, buf->shapemask, GCForeground | GCBackground, &values);
	
		_err = ab_XPutImage(dmain->display, buf->shapemask, gc, mask_data, 0, 0, 
			 0, 0, mask_data->width, mask_data->height);

	
		if (_err != 0)
		{
			ERROR(Neuro_s("error number %d couldn't put pixels in the shapemask pixmap.\n", _err));
		}

		if (gc)
			XFreeGC(dmain->display, gc);
	}

	if (mask_data)
		(mask_data->f.destroy_image)(mask_data);
}

void
Lib_SetAlpha(v_object *vobj, u8 alpha)
{
	V_OBJECT *tmp;
	
	tmp = (V_OBJECT*)vobj;

	if (tmp == NULL)
		return;
	
	tmp->alpha = alpha;	
}

void 
Lib_PutPixel(v_object *srf, int x, int y, u32 pixel)
{
	V_OBJECT *tmp;
	
	tmp = (V_OBJECT*)srf;

	if (tmp == NULL)
		return;	

	
	XSetForeground(dmain->display, dmain->GC, pixel);
	XDrawPoint(dmain->display, *tmp->cwin, dmain->GC, x, y);
	

	/*
	if (tmp->raw_data == NULL)
	{
		NEURO_ERROR("the XImage raw_data is empty", NULL);
		return;
	}

	(tmp->raw_data->f.put_pixel)(tmp->raw_data, x, y, pixel);*/



	/* tmp->pixel_data_changed = 1; */
}

u32 
Lib_GetPixel(v_object *srf, int x, int y)
{
	V_OBJECT *tmp;

	tmp = (V_OBJECT*)srf;

	if (tmp == NULL)
		return 1;

	/* way too slow */
	/*if (tmp->pixel_data_changed == 1)
		sync_pixels(tmp);*/

	if (tmp->raw_data == NULL)
	{
		ERROR("the XImage raw_data is empty");
		return 1;
	}
	
	return (tmp->raw_data->f.get_pixel)(tmp->raw_data, x, y);
}

/* TODO optimization is HIGHLY needed... 
 * we will buffer all possible matches with
 * i* a* with alpha (example of i* is iR... same for a*)
 * when thats done, put the argument alpha back to an unsigned
 * integer instead of a double
 */
static u32
AlphaPixels(u32 alpha_color, u32 indep_color, u32 alpha)
{
	u8 aR, aG, aB; /* depend color (alpha on this one) */
	u8 iR, iG, iB; /* indep color */
	register u8 rR, rG, rB; /* result color */

	Neuro_GiveConvertRGB(alpha_color, &aR, &aG, &aB);
	Neuro_GiveConvertRGB(indep_color, &iR, &iG, &iB);

	/*
	rR = (iR * (1 - (alpha / 255))) + aR * (alpha / 255);
	rG = (iG * (1 - (alpha / 255))) + aG * (alpha / 255);
	rB = (iB * (1 - (alpha / 255))) + aB * (alpha / 255);
	*/
	
	rR = HCD_IAlpha[iR][alpha] + HCD_Alpha[aR][alpha];
	rG = HCD_IAlpha[iG][alpha] + HCD_Alpha[aG][alpha];
	rB = HCD_IAlpha[iB][alpha] + HCD_Alpha[aB][alpha];
	

		
	return Neuro_MapRGB(rR, rG, rB);
}

static void
DirectDrawAlphaRect(Rectan *rectangle, u32 color, u32 alpha)
{
	u32 screen_color;
	Rectan tmp;
	v_object *screen;
	u32 conv_color;
	
	if (!rectangle)
		return;

	if (alpha > 255)
		alpha = 255;

	/* memcpy(&tmp, rectangle, sizeof(Rectan)); */
	tmp.h = rectangle->h;
	
	screen = Neuro_GetScreenBuffer();
	
	/* sync the screen pixel map */
	Lib_SyncPixels(screen);
		
	Lib_LockVObject(screen);

	while (tmp.h-- > 0)
	{
		tmp.w = rectangle->w;
		while (tmp.w-- > 0)
		{
			screen_color = Lib_GetPixel(screen, rectangle->x + tmp.w, rectangle->y + tmp.h);

			
			conv_color = AlphaPixels(screen_color, color, alpha);

			Lib_PutPixel(screen, rectangle->x + tmp.w, rectangle->y + tmp.h, conv_color);
			
		}
	}

	
	Lib_UnlockVObject(screen);
}

static void
DirectDrawAlphaImage(Rectan *src, Rectan *dst, u32 alpha, void *image, void *destination)
{
	Rectan rsrc, rdst;
	u32 screen_color;
	u32 image_color;
	u32 color;
	
	if (!src || !dst || !image)
		return;

	memcpy(&rsrc, src, sizeof(Rectan));
	rdst.x = dst->x;
	rdst.y = dst->y;

	/*Debug_Val(0, "rsrc coord (%d,%d) size (%dx%d)\n", 
			rsrc.x, rsrc.y, 
			rsrc.w, rsrc.h);*/
	
	/* sync the screen pixel map */
	Lib_SyncPixels(destination);
	
	
	Lib_LockVObject(destination);
	
	while (rsrc.h-- > rsrc.y)
	{
		rsrc.w = src->w;

		while (rsrc.w-- > rsrc.x)
		{
			image_color = Lib_GetPixel(image, rsrc.x + rsrc.w, rsrc.y + rsrc.h);
			
			if (image_color == 0)
				continue;

			screen_color = Lib_GetPixel(destination, rdst.x + rsrc.w, rdst.y + rsrc.h);
			
			/* color = AlphaPixels(screen_color, image_color, alpha); */
			color = AlphaPixels(image_color, screen_color, alpha);


			Lib_PutPixel(destination, rdst.x + rsrc.w, rdst.y + rsrc.h, color);
			
		}
	}
	
	Lib_UnlockVObject(destination);
}


void
Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst)
{
	V_OBJECT *vsrc, *vdst;
	Rectan Rsrc, Rdst;
	i32 h, w;
	int _err = 0;
	int ClipX, ClipY;
	
	vsrc = (V_OBJECT*)source;
	vdst = (V_OBJECT*)destination;

	/* if there was pixel manipulations 
	 * we need to sync client server pixel data.
	 */
	sync_pixels(vsrc);
	sync_pixels(vdst);


	if (src == NULL)
	{
		Neuro_GiveImageSize(source, &w, &h);
		Rsrc.x = 0;
		Rsrc.y = 0;
		Rsrc.h = (i16)h;
		Rsrc.w = (i16)w;
	}
	else
	{
		Rsrc.x = src->x;
		Rsrc.y = src->y;
		Rsrc.h = src->h;
		Rsrc.w = src->w;
	}

	if (dst == NULL)
	{
		Rdst.x = 0;
		Rdst.y = 0;
		Rdst.h = 0;
		Rdst.w = 0;
	}
	else
	{
		Rdst.x = dst->x;
		Rdst.y = dst->y;
		Rdst.h = dst->h;
		Rdst.w = dst->w;
	}
	
	ClipX = Rdst.x;
	ClipY = Rdst.y;

	if (Rsrc.x > 0 || Rsrc.y > 0)
	{
		ClipX -= Rsrc.x;
		ClipY -= Rsrc.y;
	}

	/* Debug_Val(0, "Will draw object %d alpha %d on %d\n", *vsrc->cwin, vsrc->alpha, *vdst->cwin); */

#if USE_ALPHA
	if (vsrc->alpha == 255 || vsrc == scldmain) /* opaque */
#endif /* USE_ALPHA */
	{

		if (vsrc->shapemask)
		{
			XSetClipMask(dmain->display, dmain->GC, vsrc->shapemask);
		
			XSetClipOrigin(dmain->display, dmain->GC, ClipX, ClipY);

			XSetForeground(dmain->display, dmain->GC, tpixel.pixel);
		}
		
		_err = XCopyArea(dmain->display, *vsrc->cwin, *vdst->cwin, dmain->GC, 
				Rsrc.x, Rsrc.y, Rsrc.w, Rsrc.h,
				Rdst.x, Rdst.y);

		XSetClipMask(dmain->display, dmain->GC, None);	
	}
#if USE_ALPHA
	else
	{
		if (vsrc->alpha > 0 )
		{
			/* we apply transparency based on the alpha value */
			DirectDrawAlphaImage(&Rsrc, &Rdst, vsrc->alpha, source, destination);
			/* Debug_Val(0, "Image drawn transparent\n"); */
		}
		else
		{
			/* an image with alpha 0 will not be drawn at all */
			/* Debug_Val(0, "Image not drawn\n"); */
		}
	}
#endif /* USE_ALPHA */
	
}

void
Lib_LoadBMP(const char *path, v_object **img)
{
	t_tick chrono;


	chrono = Neuro_GetTickCount();
	*img = Bitmap_LoadBMP(path);

	TRACE(Neuro_s("Loading a bitmap took %d\n", Neuro_GetTickCount() - chrono));
	
	return; /* int needed */
}

void
Lib_LoadBMPBuffer(void *data, v_object **img)
{
#if temp
	EBUF *temp;
	V_OBJECT *tmp;
	char **buffer;
	char **initbuf;
	/* int i = 0; */
	int _err = 0;
	t_tick chrono;

	/* Debug_Val(0, "V_OBJECT size %d\n", sizeof(V_OBJECT)); */
	if (Neuro_EBufIsEmpty(vobjs))
	{
		*img = NULL;
		return;
	}
	
	chrono = Neuro_GetTickCount();

	
	setBitmapColorKey(color_key);
	readBitmapFileToPixmap(data, &temp);
	
	TRACE(Neuro_s("Converting Bitmap to pixmap %d\n", Neuro_GetTickCount() - chrono));
	
	if (!temp)
	{
		Debug_Val(0, "Error loading the buffer file, it might not exist or its not a bitmap\n");
		return;
	}

	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));

	tmp = Neuro_GiveCurEBuf(vobjs);

	buffer = (char**)Neuro_GiveEBufCore(temp);	
	
	initbuf = buffer;
	
	chrono = Neuro_GetTickCount();
	/*_err = XpmCreatePixmapFromData(dmain->display, *dmain->cwin, initbuf, 
			&tmp->data, &tmp->shapemask, &tmp->attrib);*/
	
	_err = XpmCreatePixmapFromData(dmain->display, *dmain->cwin, initbuf, 
		&tmp->data, &tmp->shapemask, NULL);

	Debug_Val(0, "Converting pixmap to Ximage %d\n", Neuro_GetTickCount() - chrono);
	
	tmp->cwin = &tmp->data;
	/* tmp->cwin = &tmp->shapemask; */

	/* by default the image is fully opaque */
	tmp->alpha = 255;
	
	if (_err == 0)
	{
		i32 h, w;

		*img = tmp;

		
		Neuro_GiveImageSize(tmp, &w, &h);
		
		tmp->raw_data = ab_XGetImage(dmain->display, *tmp->cwin, 
			0, 0, w, h, 
			AllPlanes, ZPixmap);
		
		Debug_Val(0, "Successfully loaded the buffer file %s\n");

		/* Debug_Val(0, "At Address %d alpha %d\n", *tmp->cwin, tmp->alpha); */
	}
	else
		Debug_Val(0, "Error loading the buffer file with error %d\n", _err);
	
	Neuro_CleanEBuf(&temp);
	
	return; /* int needed */
#endif /* temp */
}

v_object *
Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask,
		u32 Bmask, u32 Amask)
{
	V_OBJECT *tmp2;
		
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
		
	tmp2 = Neuro_GiveCurEBuf(vobjs);

	if (!dmain)
	{
		ERROR("Main screen buffer dmain is NULL");
		return NULL;
	}
	/* Debug_Val(0, "width %d height %d asked depth %d  default_depth %d\n", 
			width, height, depth,
			DefaultDepth(dmain->display, dmain->screen));*/

	tmp2->raw_data = XCreateImage(dmain->display, 
			XDefaultVisual(dmain->display, dmain->screen), 
			DefaultDepth(dmain->display, dmain->screen), 
			ZPixmap, 0, NULL, width, height, 16, 0);

	if (tmp2->raw_data == NULL)
	{
		ERROR(Neuro_s("XCreateImage -- input height %d", errno));


		return NULL;
	}

	tmp2->raw_data->data = calloc(1, tmp2->raw_data->bytes_per_line * height);

	tmp2->data = XCreatePixmap(dmain->display, *dmain->cwin, width, height, 
			DefaultDepth(dmain->display, dmain->screen));

		
	ab_XPutImage(dmain->display, tmp2->data, dmain->GC, tmp2->raw_data, 0, 0, 
			0, 0, width, height);
	
	tmp2->alpha = 255;

	tmp2->display = NULL;

	tmp2->cwin = &tmp2->data;
	
	
	return (v_object*)tmp2;
}

void
Lib_UpdateRect(v_object *source, Rectan *src)
{
	XFlush(dmain->display);
}

void
Lib_FillRect(v_object *source, Rectan *src, u32 color)
{
	V_OBJECT *tmp;
	Rectan Vsrc;
	i32 h, w;

	tmp = (V_OBJECT*)source;

	if (src == NULL)
	{
		Neuro_GiveImageSize(source, &w, &h);
		Vsrc.x = 0;
		Vsrc.y = 0;
		Vsrc.w = w;
		Vsrc.h = h;
	}
	else
	{
		Vsrc.x = src->x;
		Vsrc.y = src->y;
		Vsrc.w = src->w;
		Vsrc.h = src->h;
	}

	XSetForeground(dmain->display, dmain->GC, color);
	
	XFillRectangle(dmain->display, *tmp->cwin, dmain->GC, 
			Vsrc.x, Vsrc.y, Vsrc.w, Vsrc.h);

	XSetForeground(dmain->display, dmain->GC, BlackPixel(dmain->display, dmain->screen));

	/* tmp->pixel_data_changed = 1; */
}

void
Lib_Flip(v_object *source)
{

}

void
Lib_FreeVobject(v_object *source)
{
	if (!source)
		return;

	/* forbid the cleaning of the main screen and its buffer 
	 * (those will be cleaned when the module quits).
	 */

	if (source == dmain || source == scldmain)
		return;
	
	Neuro_SCleanEBuf(vobjs, source);
	/* clean_Vobjects(source); */
}

void 
Lib_LockVObject(v_object *vobj)
{
	
}

void 
Lib_UnlockVObject(v_object *vobj)
{
	V_OBJECT *tmp;

	
	tmp = (V_OBJECT*)vobj;

	if (tmp == NULL)
		return;
	

	tmp->pixel_data_changed = 1;
	
	sync_pixels(tmp);
}

void
Lib_GiveVobjectProp(v_object *source, Rectan *output)
{

}

u32
Lib_GetDefaultDepth(void)
{
	return DefaultDepth(dmain->display, dmain->screen);
}

void
Lib_GetVObjectData(v_object *vobj, u32 *flags, i32 *h, i32 *w, u32 *pitch, 
		void **pixels, Rectan **clip_rect, u8 *bpp, 
		u32 *Rmask, u32 *Gmask, u32 *Bmask, u32 *Amask)
{
	V_OBJECT *buf;
	Window wroot;
	int wx, wy;
	u32 wwidth, wheight;
	u32 wborder;
	u32 wdepth;
	
	buf = (V_OBJECT*)vobj;

	if (!buf)
		return;
	
	/* Debug_Val(0, "Fetching from cwin id %d shapemask id %d\n", *buf->cwin, buf->shapemask); */
	XGetGeometry(dmain->display, *buf->cwin, &wroot, &wx, &wy, 
			&wwidth, &wheight, &wborder, &wdepth);
	
	/* Debug_Val(0, "XGetGeometry root %d x,y (%d,%d) size (%dx%d) border %d %d bpp\n",
			wroot, wx, wy, wwidth, wheight, wborder, wdepth);*/
	
	if (h)
	{
		/*
		if (buf == dmain || buf == scldmain)
		{
			*h = height;
		}
		else
		{
			*h = buf->attrib.height;
		}
		
		if (*h != wwdith)
			Error_Print("doesn't match width");
		*/
		*h = wheight;
	}
	
	if (w)
	{
		/*
		if (buf == dmain || buf == scldmain)
		{
			*w = width;
		}
		else
		{
			*w = buf->attrib.width;
		}
		*/

		*w = wwidth;
	}

	if (bpp)
	{
		*bpp = wdepth;
	}
}

void
Lib_VideoExit(void)
{
	/* clean the tiny pixel buffer for pixels I/O */
#if temp
	if (pixel)
		XFreePixmap(dmain->display, pixel);
#endif /* temp */
	
	/* Debug_Val(0, "there are %d elements in the main vobjects buffer\n", 
			Neuro_GiveEBufCount(vobjs));*/
	
	Neuro_CleanEBuf(&vobjs);
}

/*----------------- Input Events -----------------*/

u8
Lib_CheckKeyStatus(u32 key)
{
	/* char keyd[32]; */
	u32 i = 32;
	KeyCode keytocheck;
	u8 anchor;
	u8 temp;
	
	if (Neuro_EBufIsEmpty(vobjs) || Toggle_Exposed == 0)
	{
		return 0;
	}

	/* XQueryKeymap(dmain->display, keyd); */

	keytocheck = XKeysymToKeycode(dmain->display, key);
	
	anchor = 0;
	
	while (i-- > 0)
	{
		if (current_keymap[i])
		{
			temp = keycode_value(current_keymap[i], &anchor);
			/*Debug_Val(0, "Keycode[%d][%d] temp %d 8* %d n %d -- %d\n", i, anchor,
						temp,
						(8 * i) + temp,
						current_keymap[i], keytocheck);*/
			if (anchor == 1)
			{
				while (anchor == 1)
				{
					if (((8 * i) + temp) == keytocheck)
						return 1;
				
					temp = keycode_value(current_keymap[i], &anchor);
					
					if (((8 * i) + temp) == keytocheck)
						return 1;

				}
			}
			else
			{
				if (((8 * i) + temp) == keytocheck)
					return 1;
			}
		}
	}
	
	return 0;
}

u8
Lib_GetMouseState(i32 *x, i32 *y)
{
	Window croot;
	Window cchild;
	i32 rx, ry;
	u32 mask;
	u8 value = 0;
	
	if (Neuro_EBufIsEmpty(vobjs) || Toggle_Exposed == 0)
	{
		*x = 0;
		*y = 0;
		return 0;
	}
	
	if (XQueryPointer(dmain->display, *dmain->cwin, &croot, &cchild, &rx, &ry, x, y, &mask) == 0)
	{
		return 0;
	}

	/* Debug_Val(0, "lib mouse state %d\n", mask); */

	if (mask & Button5Mask)
		value = 5;
	
	if (mask & Button4Mask)
		value = 4;

	if (mask & Button3Mask)
		value = 3;
	
	if (mask & Button2Mask)
		value = 2;
	
	if (mask & Button1Mask)
		value = 1;

	if (mouse_wheel && value == 0)
	{
		value = mouse_wheel;
		mouse_wheel = 0;
	}
		
	
	/* Debug_Val(0, "mouse status raw %x filtered %d\n", mask, value); */
	return value;
}

void
Lib_EventPoll(void)
{

}

i32
Lib_PollEvent(void *event_input)
{	
	XEvent event;

	if (Neuro_EBufIsEmpty(vobjs))
	{
		return 1;
	}

	/* FIXME using callbacks or event handling, 
	 * this poll shouldn't be needed anymore.
	 *
	 * Using events instead of a loop will
	 * definitely make the project MUCH faster.
	 */
	/* Events_Poll(); */

	/*
	XNextEvent(dmain->display, &event);

	switch (event.type)
	{
		case Expose:
		{
			Neuro_RedrawScreen();
		}
		break;

		default:
		{
			
		}
		break;
	}
	*/

	/* the expose event... ie when other windows or stuff goes above our
	 * own window and we need to redraw.
	 */
	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, Expose, &event) == True)  

	{
		Toggle_Exposed = 1;
		/* Debug_Print("Redrawing from expose"); */
		Graphics_RedrawScreen();
	}

	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, FocusOut, &event) == True)  
	{
		/* Debug_Print("Lost Focus"); */
		Toggle_Exposed = 0;	
	}

	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, FocusIn, &event) == True)  
	{
		/* Debug_Print("Got Focus"); */
		Toggle_Exposed = 1;
	}
	
	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, ButtonPress, &event) == True)  
	{
		/* Debug_Val(0, "Button event %d\n", event.xbutton.button); */
		mouse_wheel = event.xbutton.button;

		TRACE("got an X button event press");
		Events_TriggerButton(event.xbutton.button, event.xbutton.x, event.xbutton.y, 0);
	}

	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, ButtonRelease, &event) == True)  
	{
		/* Debug_Val(0, "Button event %d\n", event.xbutton.button); */
		mouse_wheel = event.xbutton.button;

		TRACE("got an X button event release");
		Events_TriggerButton(event.xbutton.button, event.xbutton.x, event.xbutton.y, 1);
	}

	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, MotionNotify, &event) == True)  
	{
		/* NEURO_TRACE("Mouse motion XEvent", NULL);*/
		Events_TriggerMotion(event.xmotion.x, event.xmotion.y);
	}

	if (XCheckTypedWindowEvent(dmain->display, *dmain->cwin, KeyRelease, &event) == True)
	{
		Events_TriggerKey(XKeycodeToKeysym(dmain->display, event.xkey.keycode, 0), 0);
		/* XQueryKeymap(dmain->display, current_keymap); */
		TRACE("got an X key event release");
	}

	if (XCheckTypedWindowEvent(dmain->display, *dmain->cwin, KeyPress, &event) == True)
	{
		/* XQueryKeymap(dmain->display, current_keymap); */
		Events_TriggerKey(XKeycodeToKeysym(dmain->display, event.xkey.keycode, 0), 1);
		TRACE("got an X key event press");
	}	

	return 0;
}

int
Lib_EventInit(void)
{
	return 0;
}

void
Lib_EventExit(void)
{
	
}
