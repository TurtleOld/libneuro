
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

#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/xpm.h>

#include <extlib.h>
#include <ebuf.h>
#include <other.h>
#include <graphics.h>

/* freetype includes */
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#define buffer_old_method 0
#define USE_ALPHA 1

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
	Pixmap data;
	Pixmap shapemask;
	XpmAttributes attrib;
	u8 pixel_data_changed; /* if this is set to 1, next blit will do a XPutImage 
	to flush the pixels with the actual image on the server*/

	u8 alpha; /* the alpha to be applied to the image */
}V_OBJECT;

static EBUF *vobjs;

static V_OBJECT *dmain;
static V_OBJECT *scldmain; /* buffer (double) */

static u32 color_key = 0; /* a variable to set the transparent color when loading bitmaps */

#if temp
static Pixmap pixel; /*a 1x1 pixel buffer fo pixels Input Output*/
#endif /* temp */

static u32 swidth = 800, sheight = 600; /* externally settable screen size */

static u8 Toggle_Exposed = 1;
static u8 mouse_wheel = 0; /* mouse wheel variable */

static FT_Library font_lib;

static XColor tpixel;

static void DirectDrawAlphaRect(Rectan *rectangle, u32 color, u32 alpha) __attribute__((__unused__));

static void
clean_Vobjects(void *src)
{
	V_OBJECT *buf;

	buf = (V_OBJECT*)src;	

	/* Debug_Val(10, "cleaning process ...\n"); */

	/*if (dmain->display == buf->display)
		Debug_Val(10, "This element is the main display --\n");*/
	
	if (buf->data)
	{
		/* Debug_Val(10, "Freeing pixmap data\n"); */
		XFreePixmap(dmain->display, buf->data);
	}

	if (buf->shapemask)
	{
		/* Debug_Val(10, "Freeing pixmap mask\n"); */
		XFreePixmap(dmain->display, buf->shapemask);
	}

	if (buf->raw_data)
	{
		/* Debug_Val(10, "destroying XImage raw data\n"); */
		XDestroyImage(buf->raw_data);
	}
	
	if (buf->GC)
	{
		/* Debug_Val(10, "Freeing Graphic Context\n"); */
		XFreeGC(buf->display, buf->GC);
	}
	
	if (buf->win)
	{
		/* Debug_Val(10, "Destroying Window\n"); */
		XDestroyWindow(buf->display, buf->win);
	}
	
	if (buf->display)
	{
		/* Debug_Val(10, "Closing display\n"); */
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

static void 
sync_pixels(V_OBJECT *src)
{
	i32 h, w;
	
	if (src->pixel_data_changed == 1)
	{
		/* Neuro_GiveImageSize(src, &w, &h); */
		h = src->raw_data->height;
		w = src->raw_data->width;
#if old
		XPutImage(dmain->display, *src->cwin, dmain->GC, src->raw_data, 
				0, 0, 0, 0, w, h);
#endif /* old */
		if (src->raw_data)
			XDestroyImage(src->raw_data);
		/* Debug_Val(0, "cwin %d size %dx%d\n", *src->cwin, w, h); */
		src->raw_data = XGetImage(dmain->display, *src->cwin, 
			0, 0, w, h, 
			AllPlanes, ZPixmap);
		src->pixel_data_changed = 0;
	}	
}

/*  */
static void
CreatePixmap(XImage *image, Pixmap master, Pixmap *pix)
{
	GC gc;
	XGCValues values;
	int _err = 0;

	if (!image)
	{
		*pix = 0;
		return;
	}
	
	*pix = XCreatePixmap(dmain->display, master, image->width, image->height, image->depth);

	values.foreground = 1;
	values.background = 0;

	gc = XCreateGC(dmain->display, *pix, GCForeground | GCBackground, &values);
	
	_err = XPutImage(dmain->display, *pix, gc, image, 0, 0, 
		 0, 0, image->width, image->height);
	
	if (_err != 0)
	{
		Debug_Val(0, "error number %d couldn't put pixels in the shapemask pixmap.\n", _err);
		XFreePixmap(dmain->display, *pix);

		*pix = 0;
	}

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
		Debug_Val(0, "Incorrect mask height %d need to be %d\n", mask->height, height);
		
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
		XDestroyImage(tmp->raw_data);	

	tmp->raw_data = XGetImage(dmain->display, *tmp->cwin, 
		0, 0, w, h, 
		AllPlanes, ZPixmap);
}

void
Lib_SetScreenSize(u32 width, u32 height)
{
	swidth = width;
	sheight = height;
}

void
Lib_GetScreenSize(u32 *width, u32 *height)
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
	
		tmp->win = XCreateWindow(tmp->display, tmp->rwin,
			200, 200, swidth, sheight, 1, DefaultDepth(tmp->display, tmp->screen),
			CopyFromParent, CopyFromParent, CWBackingStore | CWBackPixel, &wattrib);
			
		tmp->cwin = &tmp->win;
	
		XMapWindow(tmp->display, tmp->win);
	}

	XSelectInput(tmp->display, *tmp->cwin, ExposureMask | KeyPressMask | ButtonPressMask |
		       ButtonReleaseMask | FocusChangeMask);

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
		
		Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
		
		tmp2 = Neuro_GiveCurEBuf(vobjs);
		
#if buffer_old_method
		/* Debug_Print("Beacon 3"); */
		tmp2->data = XCreatePixmap(tmp->display, *tmp->cwin, swidth, sheight, 
				DefaultDepth(tmp->display, tmp->screen));

		/* Debug_Print("Beacon 4"); */
		tmp2->cwin = &tmp2->data;
		
		tmp2->raw_data = XGetImage(tmp->display, *tmp2->cwin, 0, 0, swidth, sheight, DefaultDepth(tmp->display, tmp->screen), ZPixmap);
		/* XInitImage(tmp2->raw_data); */
		/* Debug_Print("Beacon 5"); */
#else /* NOT buffer_old_method */
		
		/* Debug_Print("Beacon 3"); */
		tmp2->raw_data = XCreateImage(tmp->display, 
				XDefaultVisual(tmp->display, tmp->screen), 
				DefaultDepth(tmp->display, tmp->screen), 
				ZPixmap, 0, NULL, swidth, sheight, 32, 0);

		/* Debug_Print("Beacon 4"); */

		tmp2->raw_data->data = calloc(1, tmp2->raw_data->bytes_per_line * sheight);

		tmp2->data = XCreatePixmap(tmp->display, *tmp->cwin, swidth, sheight, 
				DefaultDepth(tmp->display, tmp->screen));

		/* Debug_Print("Beacon 5"); */
		
		XPutImage(tmp->display, tmp2->data, tmp->GC, tmp2->raw_data, 0, 0, 
				0, 0, swidth, sheight);
		
		tmp2->cwin = &tmp2->data;

		/* Debug_Print("Beacon 6"); */
#endif /* NOT buffer_old_method */
		
		
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
			Error_Print("INVALID Depth! we only support 32, 24, 16 or 8 bits screen depth");
			return 0;
		}
		break;
	}

	return output;
}

void
Lib_SetColorKey(v_object *vobj, u32 key)
{
	color_key = key;
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
	
	tmp->pixel_data_changed = 1;
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
		Error_Print("the XImage raw_data is empty");
		return 1;
	}
	
	return XGetPixel(tmp->raw_data, x, y);
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


	/* rR = (iR * (1 - (alpha / 255))) + aR * (alpha / 255);
	rG = (iG * (1 - (alpha / 255))) + aG * (alpha / 255);
	rB = (iB * (1 - (alpha / 255))) + aB * (alpha / 255);*/

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
	u32 new_color;
	
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

			
			new_color = AlphaPixels(screen_color, color, alpha);

			Lib_PutPixel(screen, rectangle->x + tmp.w, rectangle->y + tmp.h, new_color);
			
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
	u32 new_color;
	
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
			
			/* new_color = AlphaPixels(screen_color, image_color, alpha); */
			new_color = AlphaPixels(image_color, screen_color, alpha);


			Lib_PutPixel(destination, rdst.x + rsrc.w, rdst.y + rsrc.h, new_color);
			
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
	readBitmapFileToPixmap(path, &temp);
	
	Debug_Val(0, "Converting Bitmap to pixmap %d\n", Neuro_GetTickCount() - chrono);
	
	if (!temp)
	{
		Debug_Val(0, "Error loading the file %s, it might not exist or its not a bitmap\n", path);
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
		
		tmp->raw_data = XGetImage(dmain->display, *tmp->cwin, 
			0, 0, w, h, 
			AllPlanes, ZPixmap);
		
		Debug_Val(0, "Successfully loaded the file %s\n", path);

		/* Debug_Val(0, "At Address %d alpha %d\n", *tmp->cwin, tmp->alpha); */
	}
	else
		Debug_Val(0, "Error loading the file %s with error %d\n", path, _err);
	
	Neuro_CleanEBuf(&temp);
	
	return; /* int needed */
}

void
Lib_LoadBMPBuffer(void *data, v_object **img)
{
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
	
	Debug_Val(0, "Converting Bitmap to pixmap %d\n", Neuro_GetTickCount() - chrono);
	
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
		
		tmp->raw_data = XGetImage(dmain->display, *tmp->cwin, 
			0, 0, w, h, 
			AllPlanes, ZPixmap);
		
		Debug_Val(0, "Successfully loaded the buffer file %s\n");

		/* Debug_Val(0, "At Address %d alpha %d\n", *tmp->cwin, tmp->alpha); */
	}
	else
		Debug_Val(0, "Error loading the buffer file with error %d\n", _err);
	
	Neuro_CleanEBuf(&temp);
	
	return; /* int needed */
}

v_object *
Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask,
		u32 Bmask, u32 Amask)
{
	V_OBJECT *tmp2;
		
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
		
	tmp2 = Neuro_GiveCurEBuf(vobjs);
		
	tmp2->raw_data = XCreateImage(dmain->display, 
			XDefaultVisual(dmain->display, dmain->screen), 
			DefaultDepth(dmain->display, dmain->screen), 
			ZPixmap, 0, NULL, width, height, depth, 0);

	tmp2->raw_data->data = calloc(1, tmp2->raw_data->bytes_per_line * height);

	tmp2->data = XCreatePixmap(dmain->display, *dmain->cwin, width, height, 
			DefaultDepth(dmain->display, dmain->screen));

		
	XPutImage(dmain->display, tmp2->data, dmain->GC, tmp2->raw_data, 0, 0, 
			0, 0, width, height);
	
	tmp2->alpha = 255;

	tmp2->cwin = &tmp2->data;
	
	
	return (v_object*)tmp2;
}

/* unicode renderer */
v_object *
Lib_RenderUnicode(font_object *ttf, u32 size, u32 character, i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst)
{
	int _err = 0;
	static u32 expect = 0;
	static u32 codepoint = 0;
	v_object *output = NULL;
	u8 space_char = 0;
	FT_Face face;
	XImage *mask_data = NULL;

	face = (FT_Face)ttf;

	if (!face)
		return NULL;

	if (character == ' ')
	{
		space_char = 1;
	}
	else
	{
		if (!src || !dst)
			return NULL;
	}

	_err = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
	if (_err)
	{
		Error_Print("Couldn't select the encoding unicode");
		return NULL;
	}
	
	_err = FT_Set_Char_Size(face, size * 64, size * 64, 72, 72);
	if (_err)
	{
		Error_Print("Couldn't set face character size");
		return NULL;
	}

	/* support for multi bytes characters */	
	if (character >= 0xC0)
	{
		if (character < 0xE0)
		{
			codepoint = character & 0x1F;
			expect = 1;
		}
		else if (character < 0xF0)
		{
			codepoint = character & 0x0F;
			expect = 2;
		}
		else if (character < 0xF8)
		{
			codepoint = character & 0x07;
			expect = 3;
		}
		return NULL;
	}
	else if (character >= 0x80)
	{
		--expect;

		if (expect >= 0)
		{
			codepoint <<= 6;
			codepoint += character & 0x3F;
		}

		if (expect > 0)
			return NULL;

		expect = 0;
	}
	else
		codepoint = character;

	_err = FT_Load_Char(face, codepoint, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
	/* _err = FT_Load_Char(face, character, FT_LOAD_MONOCHROME); */

	if (_err)
	{
		Error_Print("Couldn't load character");
		return NULL;
	}

	{
		v_object *screen;
		u32 pixel, row;
		FT_BitmapGlyph bitmap;
		u8 R, G, B;

		Neuro_GiveConvertRGB(color, &R, &G, &B);
		
		if (face->glyph->format != FT_GLYPH_FORMAT_BITMAP)
		{
			Debug_Print("Unknown non bitmap format");
			return NULL;
		}
		/*if (face->glyph->format == FT_GLYPH_FORMAT_BITMAP)
		{
			Debug_Print("Bitmap format");
		}
		else
		{
			Debug_Print("Unknown non bitmap format");
		}
		*/
		
		/* Debug_Val(0, "font size %dx%d\n", face->glyph->bitmap.width,
				face->glyph->bitmap.rows); */
		if (!space_char)
		{
			/* allocate the surface */
			output = Lib_CreateVObject(0, face->glyph->bitmap.width, 
					face->glyph->bitmap.rows, 16, 0, 0, 0, 0);

			/* create the mask */
			/* mask_data = CreateMask(output, face->glyph->bitmap.width, face->glyph->bitmap.rows); */
		}
		
		bitmap = (FT_BitmapGlyph)face->glyph;
		pixel = 0;
		row = 0;


		screen = Neuro_GetScreenBuffer();

		Lib_SyncPixels(output);

		Lib_LockVObject(output);

		if (!space_char)
		{
			/* create the mask */
			mask_data = CreateMask(output, face->glyph->bitmap.width, face->glyph->bitmap.rows);
		}
		
		if (face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY && !space_char)
		{
			while (row < face->glyph->bitmap.rows)
			{	
				pixel = 0;	
				
				while (pixel < face->glyph->bitmap.width)
				{
					u32 gray;
					
					if ((gray = face->glyph->bitmap.buffer[(row * face->glyph->bitmap.pitch) + pixel]))
					{
						u32 tcolor, rcolor;
						i16 tx, ty;
						
						tx = (*x + pixel) + face->glyph->metrics.horiBearingX / 64;
						ty = (*y + row) - face->glyph->metrics.horiBearingY / 64;

						/*Debug_Val(0, "tcolor %d rcolor %d alpha %d\n", tcolor, 
							rcolor, gray);*/
						tcolor = Lib_GetPixel(screen, tx, ty);
						rcolor = AlphaPixels(color, tcolor, (double)(256 - gray));
						Lib_PutPixel(screen, tx, ty, rcolor);
						
					}
					else
					{
						/* if (character == 'X')
							Debug_Val(0, "0");*/
					}

					pixel++;
				}
				
				if (character == 'X')
					Debug_Val(0, "\n");
				row++;
			}
		}

		if (face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO && !space_char)
		{
			u16 cbits[8];
			u16 i = 8;
			i16 tx, ty;
			u8 gray;
			u8 *bm, *rstart;

			bm = face->glyph->bitmap.buffer;
			rstart = bm;

			if (IsLittleEndian())
			{
				cbits[7] = 0x80;
				cbits[6] = 0x40;
				cbits[5] = 0x20;
				cbits[4] = 0x10;
				cbits[3] = 0x08;
				cbits[2] = 0x04;
				cbits[1] = 0x02;
				cbits[0] = 0x01;
			}
			else
			{
				cbits[7] = 0x08;
				cbits[6] = 0x04;
				cbits[5] = 0x02;
				cbits[4] = 0x01;
				cbits[3] = 0x80;
				cbits[2] = 0x40;
				cbits[1] = 0x20;
				cbits[0] = 0x10;
			}
			
			pixel = 0;
			while (row < face->glyph->bitmap.rows)
			{
				gray = *bm;
				i = 8;
				while (i-- > 0)
				{			
					/* tx = (*x + pixel) + face->glyph->metrics.horiBearingX / 64;
					ty = (*y + row) - face->glyph->metrics.horiBearingY / 64; */
					/* tx = pixel + face->glyph->metrics.horiBearingX / 64;
					ty = row - face->glyph->metrics.horiBearingY / 64;*/
					tx = pixel;
					ty = row;
						
					if (gray & cbits[i])
					{
						/* Debug_Val(0, "dot at (%d,%d)\n", tx, ty); */
						Lib_PutPixel(output, tx, ty, color);
						
						if (mask_data)
							XPutPixel(mask_data, tx, ty, 1);
					}
					else
					{
						if (mask_data)
							XPutPixel(mask_data, tx, ty, 0);
					}

					if (pixel >= face->glyph->bitmap.width - 1)
					{
						row++;

						pixel = 0;
						rstart += face->glyph->bitmap.pitch;
						bm = rstart;
						bm--;
						
						break;
					}
					pixel++;
				}

				bm++;
			}
		}	


		if (!space_char)
		{
			V_OBJECT *obj;
			src->x = 0;
			src->y = 0;
			src->w = face->glyph->bitmap.width;
			src->h = face->glyph->bitmap.rows;

			dst->x = *x + face->glyph->metrics.horiBearingX / 64;
			dst->y = *y - face->glyph->metrics.horiBearingY / 64;
			dst->w = 0;
			dst->h = 0;
			
			obj = (V_OBJECT*)output;
			
			CreatePixmap(mask_data, *obj->cwin, &obj->shapemask);
			XDestroyImage(mask_data);
		}

		*x = *x + face->glyph->metrics.horiAdvance / 64;
		/* *y = *y + face->glyph->metrics.vertAdvance / 64; */

		Lib_UnlockVObject(output);
	}


	return output;
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
Lib_GetDefaultDepth()
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
	
	if (!vobj)
		return;
	
	buf = (V_OBJECT*)vobj;
	
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

font_object *
Lib_LoadFontFile(char *fonts_file_path)
{
	FT_Face face;
	int _err = 0;
	
	_err = FT_New_Face(font_lib, fonts_file_path, 0, &face);

	if (_err != 0)
	{
		return NULL;
	}

	return face;
}

int
Lib_FontsInit()
{
	return FT_Init_FreeType(&font_lib);
}

void
Lib_FontsExit()
{
	FT_Done_FreeType(font_lib);
}

void
Lib_VideoExit()
{
	/* clean the tiny pixel buffer for pixels I/O */
#if temp
	if (pixel)
		XFreePixmap(dmain->display, pixel);
#endif /* temp */
	Neuro_CleanEBuf(&vobjs);
}

/*----------------- Input Events -----------------*/

u8 *
Lib_GetKeyState(i32 *numkeys)
{
	return NULL;
}

u8
Lib_CheckKeyStatus(u32 key)
{
	char keyd[32];
	u32 i = 32;
	KeyCode keytocheck;
	u8 anchor;
	u8 temp;
	
	if (Neuro_EBufIsEmpty(vobjs) || Toggle_Exposed == 0)
	{
		return 0;
	}

	XQueryKeymap(dmain->display, keyd);

	keytocheck = XKeysymToKeycode(dmain->display, key);
	
	anchor = 0;
	
	while (i-- > 0)
	{
		if (keyd[i])
		{
			temp = keycode_value(keyd[i], &anchor);
			/*Debug_Val(0, "Keycode[%d][%d] temp %d 8* %d n %d -- %d\n", i, anchor,
						temp,
						(8 * i) + temp,
						keyd[i], keytocheck);*/
			if (anchor == 1)
			{
				while (anchor == 1)
				{
					if (((8 * i) + temp) == keytocheck)
						return 1;
				
					temp = keycode_value(keyd[i], &anchor);
					
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
Lib_EventPoll()
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
		Neuro_RedrawScreen();
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
	
	if ( XCheckTypedWindowEvent(dmain->display, *dmain->cwin, ButtonRelease, &event) == True)  
	{
		/* Debug_Val(0, "Button event %d\n", event.xbutton.button); */
		mouse_wheel = event.xbutton.button;
	}
	
	return 0;
}

int
Lib_EventsInit()
{
	return 0;
}

void
Lib_EventsExit()
{
	
}
