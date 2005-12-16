/* none.c
 * the source file permitting to compile this library without any lower
 * extern library(this is used for debugging).
 */



#ifdef USE_X11

#include <neuro/extlib.h>

#include <stdlib.h>

#include <X11.h>

/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{	
	return 0;
}

void
Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst)
{

}

void
Lib_LoadBMP(const char *path, v_object **img)
{

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

#endif /* USE_X11 */
