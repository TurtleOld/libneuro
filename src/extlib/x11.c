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

	Window *win;
	GC wGC;
	XGCValues wValue;
	
	Pixmap data;
	Pixmap shapemask;
	XpmAttributes attrib;
}V_OBJECT;

EBUF *vobjs;

V_OBJECT *dmain;

/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{	
	V_OBJECT *tmp;
	
	Neuro_CreateEBuf(&vobjs);
	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));
	
	tmp = Neuro_GiveCurEBuf(vobjs);
	
	tmp->display = XOpenDisplay(NULL);
	tmp->screen = XDefaultScreen(tmp->display);
	tmp->GC = XDefaultGC(tmp->display, tmp->screen);
	
	tmp->win = XCreateSimpleWindow(display, XRootWindow(tmp->display, tmp->screen),
			200, 200, 800, 600, 4,
			BlackPixel(tmp->display, tmp->screen),
			WhitePixel(tmp->display, tmp->screen));

	tmp->wGC = XCreateGC(tmp->display, tmp->win, 0, &tmp->wValue);
	
	
	dmain = tmp;
	*screen = tmp;
	
	return 0;
}

void
Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst)
{

}

void
Lib_LoadBMP(const char *path, v_object **img)
{
	EBUF *temp;
	V_OBJECT *tmp;

	readBitmapFileToPixmap(path, &temp);
	
	Neuro_AllocEBuf(vobjs, sizeof(V_OBJECT*), sizeof(V_OBJECT));

	tmp = Neuro_GiveCurEBuf(vobjs);
	
	XpmCreatePixmapFromData(dmain->display, dmain->GC, Neuro_GiveEBufCore(temp), &tmp->data, &tmp->shapemask, &tmp->attrib);
	
	*img = tmp;
	
	Neuro_CleanEBuf(&temp);
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

}

void
Lib_FillRect(v_object *source, Rectan *src, u32 color)
{

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

}

void
Lib_VideoExit()
{
	Neuro_CleanEBuf(&vobjs);
}

/*----------------- Input Events -----------------*/

u8 *
Lib_GetKeyState(i32 *numkeys)
{
	return NULL;
}

u8
Lib_GetMouseState(i32 *x, i32 *y)
{
	return 0;
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
