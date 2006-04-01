/* x11.c
 * X11 driver(abstraction layer) source.
 */

#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/xpm.h>

#include <extlib.h>
#include <ebuf.h>
#include <other.h>
#include <graphics.h>

#define buffer_old_method 0

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
}V_OBJECT;

static EBUF *vobjs;

static V_OBJECT *dmain;
static V_OBJECT *scldmain; /* buffer (double) */

static u32 color_key = 0; /* a variable to set the transparent color when loading bitmaps */

#if temp
static Pixmap pixel; /*a 1x1 pixel buffer fo pixels Input Output*/
#endif /* temp */

static i32 width = 800, height = 600; /* HACK WARNING TODO make this better and settable*/

static u8 Toggle_Exposed = 1;

static void
clean_Vobjects(void *src)
{
	V_OBJECT *buf;

	buf = (V_OBJECT*)src;	

	Debug_Val(10, "cleaning process ...\n");

	if (dmain->display == buf->display)
		Debug_Val(10, "This element is the main display --\n");
	
	if (buf->data)
	{
		Debug_Val(10, "Freeing pixmap data\n");
		XFreePixmap(dmain->display, buf->data);
	}

	if (buf->shapemask)
	{
		Debug_Val(10, "Freeing pixmap mask\n");
		XFreePixmap(dmain->display, buf->shapemask);
	}

	if (buf->raw_data)
	{
		Debug_Val(10, "destroying XImage raw data\n");
		XDestroyImage(buf->raw_data);
	}
	
	if (buf->GC)
	{
		Debug_Val(10, "Freeing Graphic Context\n");
		XFreeGC(buf->display, buf->GC);
	}
	
	if (buf->win)
	{
		Debug_Val(10, "Destroying Window\n");
		XDestroyWindow(buf->display, buf->win);
	}
	
	if (buf->display)
	{
		Debug_Val(10, "Closing display\n");
		XCloseDisplay(buf->display);
	}
	Debug_Val(10, "cleaning process done ...\n");
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

/*  video constructor destructor  */
/* will need to include the screen width and height also */
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
			200, 200, width, height, 1, DefaultDepth(tmp->display, tmp->screen),
			CopyFromParent, CopyFromParent, CWBackingStore | CWBackPixel, &wattrib);
			
		tmp->cwin = &tmp->win;
	
		XMapWindow(tmp->display, tmp->win);
	}

	XSelectInput(tmp->display, *tmp->cwin, ExposureMask | KeyPressMask | ButtonPressMask | FocusChangeMask);

	XFlush(tmp->display);

	/* Debug_Val(0, "graphics exposures %d\n", tmp->wValue.graphics_exposures); */
	tmp->wValue.graphics_exposures = 0;
	tmp->GC = XCreateGC(tmp->display, *tmp->cwin, GCGraphicsExposures, &tmp->wValue);
	
	dmain = tmp;
	*screen = tmp;

	
	if (screen_buf)
	{
		V_OBJECT *tmp2;
		
		Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
		
		tmp2 = Neuro_GiveCurEBuf(vobjs);
		
#if buffer_old_method
		/* Debug_Print("Beacon 3"); */
		tmp2->data = XCreatePixmap(tmp->display, *tmp->cwin, width, height, 
				DefaultDepth(tmp->display, tmp->screen));

		/* Debug_Print("Beacon 4"); */
		tmp2->cwin = &tmp2->data;
		
		tmp2->raw_data = XGetImage(tmp->display, *tmp2->cwin, 0, 0, width, height, DefaultDepth(tmp->display, tmp->screen), ZPixmap);
		/* XInitImage(tmp2->raw_data); */
		/* Debug_Print("Beacon 5"); */
#else /* NOT buffer_old_method */
		
		Debug_Print("Beacon 3");
		tmp2->raw_data = XCreateImage(tmp->display, 
				XDefaultVisual(tmp->display, tmp->screen), 
				DefaultDepth(tmp->display, tmp->screen), 
				ZPixmap, 0, NULL, width, height, 32, 0);

		Debug_Print("Beacon 4");

		tmp2->raw_data->data = calloc(1, tmp2->raw_data->bytes_per_line * height);

		tmp2->data = XCreatePixmap(tmp->display, *tmp->cwin, width, height, 
				DefaultDepth(tmp->display, tmp->screen));

		Debug_Print("Beacon 5");
		
		XPutImage(tmp->display, tmp2->data, tmp->GC, tmp2->raw_data, 0, 0, 
				0, 0, width, height);
		
		tmp2->cwin = &tmp2->data;

		Debug_Print("Beacon 6");
#endif /* NOT buffer_old_method */
		
		
		scldmain = tmp2;
		*screen_buf = tmp2;
	}
	else
	{
		scldmain = tmp;
	}

	/* Debug_Val(0, "Screen addr %d buffer addr %d\n", dmain, scldmain); */
	return 0;
}

u32 
Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b)
{
	return Neuro_GiveRGB(r, g, b);
}

void
Lib_SetColorKey(v_object *vobj, u32 key)
{
	color_key = key;
}

void 
Lib_PutPixel(v_object *srf, int x, int y, u32 pixel)
{
	V_OBJECT *tmp;
#if temp
	XImage *buf;
#endif /* temp */
	/* i32 h, w; */
	Pixel pix;
	XColor scrncolor;
	XColor exactcolor;
	
	tmp = (V_OBJECT*)srf;
	
	XAllocNamedColor(dmain->display, DefaultColormap(dmain->display, dmain->screen),
			"white", &scrncolor, &exactcolor);

	pix = scrncolor.pixel;
	
	/* Neuro_GiveImageSize(tmp, &w, &h); */

	
	/* a better depth input is needed, use the DefaultDepth macro to find it */
	/* buf = XGetImage(dmain->display, *tmp->cwin, x, y, 1, 1, , ZPixmap);

	buf->f.put_pixel(buf, x, y, pixel);*/

	/* XDrawPoint(dmain->display, *tmp->cwin, *dmain->cGC, x, y); */

	Debug_Val(0, "neuro white color %d\n", Neuro_GiveRGB24(255, 255, 255));
	if (tmp->raw_data)
		XPutPixel(tmp->raw_data, x, y, pix);
}

u32 
Lib_GetPixel(v_object *srf, int x, int y)
{
	/* XImage *buf; */
	V_OBJECT *tmp;
	/* i32 h, w; */
	unsigned long color = 0;

	tmp = (V_OBJECT*)srf;

	if (tmp->raw_data == NULL)
	{
		Error_Print("the XImage raw_data is empty");
		return 1;
	}
	
	/*Neuro_GiveImageSize(tmp, &w, &h);*/

	/* buf = XGetImage(dmain->display, *tmp->cwin, x, y, 1, 1, DefaultDepth(dmain->display, dmain->screen), ZPixmap); */

	/* color = tmp->raw_data->f.get_pixel(tmp->raw_data, x, y); */
	
	color = XGetPixel(tmp->raw_data, x, y);
	
	Debug_Val(0, "(%d,%d) Color Found %d\n", x, y, color);
	return color;
	/* return 1; */
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
	
		
	
	if (vsrc->shapemask)
		XSetClipMask(dmain->display, dmain->GC, vsrc->shapemask);
	XSetClipOrigin(dmain->display, dmain->GC, ClipX, ClipY);
		

	_err = XCopyArea(dmain->display, *vsrc->cwin, *vdst->cwin, dmain->GC, 
			Rsrc.x, Rsrc.y, Rsrc.w, Rsrc.h,
			Rdst.x, Rdst.y);

	XSetClipMask(dmain->display, dmain->GC, None);
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

	/* Debug_Val(0, "V_OBJECT size %d\n", sizeof(V_OBJECT)); */
	if (Neuro_EBufIsEmpty(vobjs))
	{
		*img = NULL;
		return;
	}
	
	setBitmapColorKey(color_key);
	readBitmapFileToPixmap(path, &temp);
	
	if (!temp)
	{
		Debug_Val(0, "Error loading the file %s, it might not exist or its not a bitmap\n", path);
		return;
	}

	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));

	tmp = Neuro_GiveCurEBuf(vobjs);

	buffer = (char**)Neuro_GiveEBufCore(temp);	
	
	initbuf = buffer;
	
	_err = XpmCreatePixmapFromData(dmain->display, *dmain->cwin, initbuf, 
			&tmp->data, &tmp->shapemask, &tmp->attrib);
			
	/*_err = XpmCreatePixmapFromData(dmain->display, *dmain->cwin, initbuf, 
		&tmp->data, &tmp->shapemask, NULL);*/
	
	tmp->cwin = &tmp->data;
	/* tmp->cwin = &tmp->shapemask; */
	if (_err == 0)
	{
		i32 h, w;

		*img = tmp;

		Neuro_GiveImageSize(tmp, &w, &h);
		tmp->raw_data = XGetImage(dmain->display, *tmp->cwin, 0, 0, w, h, DefaultDepth(dmain->display, dmain->screen), ZPixmap);
		Debug_Val(0, "Successfully loaded the file %s\n", path);
	}
	else
		Debug_Val(0, "Error loading the file %s with error %d\n", path, _err);
	
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

	tmp2->data = XCreatePixmap(dmain->display, *dmain->cwin, width, height, DefaultDepth(dmain->display, dmain->screen));
	tmp2->cwin = &tmp2->data;
		
	tmp2->raw_data = XGetImage(dmain->display, *tmp2->cwin, 0, 0, width, height, DefaultDepth(dmain->display, dmain->screen), ZPixmap);
	XInitImage(tmp2->raw_data);

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

	/* support only black filling for now 
	 * TODO support the use of u32 color
	 */

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

	XFillRectangle(dmain->display, *tmp->cwin, dmain->GC, 
			Vsrc.x, Vsrc.y, Vsrc.w, Vsrc.h);
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
	
	buf = vobj;
	
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
	
	XQueryPointer(dmain->display, *dmain->cwin, &croot, &cchild, &rx, &ry, x, y, &mask);

	/* Debug_Val(0, "lib mouse state %d\n", mask); */

	/* TODO might need to make this more extended... however, the 
	 * speed gain by using this method is worth it :)
	 */
	if (mask & 0x00000600)
		value = 6;

	if (mask & 0x00000500)
		value = 5;
	
	if (mask & 0x00000400)
		value = 4;

	if (mask & 0x00000300)
		value = 3;
	
	if (mask & 0x00000200)
		value = 2;
	
	if (mask & 0x00000100)
		value = 1;
	
	/* Debug_Val(0, "mouse status raw %d filtered %d\n", mask, value); */
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
