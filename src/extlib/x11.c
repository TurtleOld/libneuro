/* x11.c
 * X11 driver(abstraction layer) source.
 */

#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/xpm.h>

#include <extlib.h>
#include <ebuf.h>
#include <other.h>


typedef struct V_OBJECT
{
	Display *display;
	i32 screen;
	GC GC;

	GC *cGC; /* pointer to the current GC in use */
	Window *cwin; /* pointer to the current window in use or pixmap */

	Window rwin; /* the root window */
	
	Window win;
	GC wGC;
	XGCValues wValue;
	
	XImage *raw_data;
	Pixmap data;
	Pixmap shapemask;
	XpmAttributes attrib;
}V_OBJECT;

static EBUF *vobjs;

static V_OBJECT *dmain;
static V_OBJECT *scldmain; /* buffer (double) */

#if temp
static Pixmap pixel; /*a 1x1 pixel buffer fo pixels Input Output*/
#endif /* temp */

static i32 width = 800, height = 600; /* HACK WARNING TODO make this better and settable*/

static void
clean_Vobjects(void *src)
{
	V_OBJECT *buf;

	buf = (V_OBJECT*)src;

	/*if (dmain->display == buf->display)
		Info_Print("Cleaning the main display");*/

	/* Info_Print("cleaning process ..."); */
	/* Info_Print("pixmap data"); */
	if (buf->data)
		XFreePixmap(dmain->display, buf->data);

	/* Info_Print("shapemask"); */
	if (buf->shapemask)
		XFreePixmap(dmain->display, buf->shapemask);
	
	/* Info_Print("display main window Graphic context (GC)"); */
	if (buf->wGC)
		XFreeGC(buf->display, buf->wGC);
	
	/* Info_Print("window"); */
	if (buf->win)
		XDestroyWindow(buf->display, buf->win);
	
	/* Info_Print("display"); */
	if (buf->display)
		XCloseDisplay(buf->display);
	/* Info_Print("cleaning process done"); */
}

static char
keycode_value(char num)
{
	switch(num)
	{
		case 1:
			return 0;
		break;
		
		case 2:
			return 1;
		break;

		case 4:
			return 2;
		break;

		case 8:
			return 3;
		break;

		case 16:
			return 4;
		break;

		case 32:
			return 5;
		break;

		case 64:
			return 6;
		break;

		case 128:
			return 7;
		break;

		case -128:
			return 7;
		break;

		default:
			return -1;
		break;
	}
}

/*  video constructor destructor  */
/* will need to include the screen width and height also */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{	
	V_OBJECT *tmp;
	XSetWindowAttributes wattrib;
	
	Neuro_CreateEBuf(&vobjs);
	Neuro_SetcallbEBuf(vobjs, clean_Vobjects);
	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
	
	tmp = Neuro_GiveCurEBuf(vobjs);
	
	tmp->display = XOpenDisplay(NULL);
	tmp->screen = XDefaultScreen(tmp->display);
	tmp->GC = XDefaultGC(tmp->display, tmp->screen);

	tmp->rwin = XRootWindow(tmp->display, tmp->screen);

	/* tmp->cwin = &tmp->rwin; */
	wattrib.backing_store = WhenMapped;
	wattrib.background_pixel = BlackPixel(tmp->display, tmp->screen);
	
	tmp->win = XCreateWindow(tmp->display, tmp->rwin,
			200, 200, width, height, 1, DefaultDepth(tmp->display, tmp->screen),
			CopyFromParent, CopyFromParent, CWBackingStore | CWBackPixel, &wattrib);
		
	if (tmp->cwin == NULL)
	{
		tmp->cwin = &tmp->win;
	
		XMapWindow(tmp->display, tmp->win);
	}

	XSelectInput(tmp->display, *tmp->cwin, ExposureMask | KeyPressMask | ButtonPressMask);

	XFlush(tmp->display);
	
	/*tmp->wGC = XCreateGC(tmp->display, *tmp->cwin, 
			GCFunction | GCFillStyle | GCGraphicsExposures, 
			&tmp->wValue);
	*/

	
	tmp->wGC = XCreateGC(tmp->display, *tmp->cwin, GCGraphicsExposures, &tmp->wValue);
		
	tmp->cGC = &tmp->wGC;
	
	dmain = tmp;
	*screen = tmp;

	/*Debug_Print("before");
	tmp->raw_data = XGetImage(tmp->display, *tmp->cwin, 0, 0, width, height, 32, ZPixmap);
	Debug_Print("after");*/
#if temp	
	XCreatePixmap(tmp->display, *tmp->cwin, 1, 1, DefaultDepth(tmp->display, tmp->screen));
#endif /* temp */
	
	if (screen_buf)
	{
		V_OBJECT *tmp2;
		Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
		
		tmp2 = Neuro_GiveCurEBuf(vobjs);

		tmp2->data = XCreatePixmap(tmp->display, *tmp->cwin, width, height, DefaultDepth(tmp->display, tmp->screen));
		tmp2->cwin = &tmp2->data;
		
		Debug_Print("before");
		tmp2->raw_data = XGetImage(tmp->display, *tmp2->cwin, 0, 0, width, height, DefaultDepth(tmp->display, tmp->screen), ZPixmap);
		XInitImage(tmp2->raw_data);
		Debug_Print("after");


		scldmain = tmp2;
		*screen_buf = tmp2;
	}
	else
	{
		scldmain = tmp;
	}

	Debug_Val(0, "Screen addr %d buffer addr %d\n", dmain, scldmain);
	return 0;
}

u32 
Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b)
{
	return 0;
}

void
Lib_SetColorKey(v_object *vobj, u32 key)
{
	
}

void 
Lib_PutPixel(v_object *srf, int x, int y, u32 pixel)
{
	V_OBJECT *tmp;
#if temp
	XImage *buf;
#endif /* temp */
	/* i32 h, w; */
	
	tmp = (V_OBJECT*)srf;
	
	/* Neuro_GiveImageSize(tmp, &w, &h); */

#if temp
	/* a better depth input is needes, use the DefaultDepth macro to find it */
	buf = XGetImage(dmain->display, *tmp->cwin, x, y, 1, 1, 32, ZPixmap);

	buf->f.put_pixel(buf, x, y, pixel);
#endif /* temp */

	/* XDrawPoint(dmain->display, *tmp->cwin, *tmp->cGC, x, y); */
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
		return -1;
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
	
	/* Debug_Val(0, "Blit start\n"); */
#if temp
	if (source == dmain)
	{
		Error_Print("You cannot draw the screen to another v_object\n");
		/* this is not allowed */
		return;
	}
#endif /* temp */
	
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
		/* Neuro_GiveImageSize(destination, &w, &h); */
			
		/*
		if (dst->x <= 0)
			dst->x = 1;
		if (dst->y <= 0)
			dst->y = 1;
		*/
		
		/*
		if (dst->x > w)
			dst->x = w;
		if (dst->y > h)
			dst->y = h;
		*/
		Rdst.x = dst->x;
		Rdst.y = dst->y;
		Rdst.h = dst->h;
		Rdst.w = dst->w;
	}
		
	
	/* if (vsrc->shapemask) */
	XSetClipMask(dmain->display, *dmain->cGC, vsrc->shapemask);
	/*XSetClipMask(dmain->display, *dmain->cGC, None);*/
	XSetClipOrigin(dmain->display, *dmain->cGC, Rdst.x, Rdst.y);
	
	
	/* TODO change vsrc->data and vdst->cwin to pointers which will change 
	 * depending on the type of v_object the v_object is. Either core
	 * or pixmap...
	 */
	/*
	if (vdst == scldmain)
		Debug_Val(0, "copy from unknown to screen buffer\n");
	
	if (vdst == dmain && vsrc == scldmain)
		Debug_Val(0, "copy from screen buffer to the window\n");
	*/

	/* Debug_Val(0, "XcopyArea attempt data\"%d\" cwin\"%d\" src(%d,%d,%d,%d) dst(%d,%d)\n",
			vsrc->data, *vdst->cwin,
			Rsrc.x, Rsrc.y, Rsrc.w, Rsrc.h,
			Rdst.x, Rdst.y );
	*/

	_err = XCopyArea(dmain->display, *vsrc->cwin, *vdst->cwin, *dmain->cGC, 
			Rsrc.x, Rsrc.y, Rsrc.w, Rsrc.h,
			Rdst.x, Rdst.y);
	/* Debug_Val(0, "XcopyArea done\n"); */

	/*
	if (_err != 0)
	{
		Error_Print("Blit seems to have failed");
		Debug_Val(0, "Debug value is %d\n", _err);
	}
	*/
	XSetClipMask(dmain->display, *dmain->cGC, None);
	/* Debug_Val(0, "Blit done\n"); */
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
	/*
	while (*buffer)
	{
		Debug_Val(1, "%s\n", *buffer);
		i++;
		buffer++;
	}
	Debug_Val(1, "%s\nreal total == %d\n", *initbuf, i);
	*/
	
	/*
	tmp->attrib.valuemask = (XpmReturnPixels | XpmReturnExtensions | XpmExactColors | XpmCloseness);
	tmp->attrib.exactColors = False;
	tmp->attrib.closeness = 40000;
	*/
	
	_err = XpmCreatePixmapFromData(dmain->display, *dmain->cwin, initbuf, &tmp->data, &tmp->shapemask, &tmp->attrib);
	
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
	return NULL;
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

	XFillRectangle(dmain->display, *tmp->cwin, *dmain->cGC, 
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

void
Lib_GetVObjectData(v_object *vobj, u32 *flags, i32 *h, i32 *w, u32 *pitch, 
		void **pixels, Rectan **clip_rect, u8 *bpp, 
		u32 *Rmask, u32 *Gmask, u32 *Bmask,u32 *Amask)
{
	V_OBJECT *buf;
	
	if (!vobj)
		return;
	
	buf = vobj;
	
	if (h)
	{
		if (buf == dmain || buf == scldmain)
		{
			*h = height;
		}
		else
		{
			*h = buf->attrib.height;
		}
	}
	
	if (w)
	{
		if (buf == dmain || buf == scldmain)
		{
			*w = width;
		}
		else
		{
			*w = buf->attrib.width;
		}
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
	
	XQueryKeymap(dmain->display, keyd);

	while (i-- > 0)
	{
		if (keyd[i])
		{
			if ((8 * i) + keycode_value(keyd[i]) == XKeysymToKeycode(dmain->display, key))
				return 1;
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
Lib_PollEvent(void *event)
{
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
