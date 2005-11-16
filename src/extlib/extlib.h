/* Library Abstraction Layer header */

#ifndef __EXTLIB_H
#define __EXTLIB_H

#include "engine.h"

extern int Lib_VideoInit(v_object **screen, v_object **screen_buf);

extern void Lib_VideoExit();

extern int Lib_EventInit();

extern void Lib_EventExit();


extern void Lib_LockVObject(v_object *vobj);

extern void Lib_UnlockVObject(v_object *vobj);

extern void Lib_FreeVobject(v_object *source);

extern void Lib_Flip(v_object *source);

extern void Lib_FillRect(v_object *source, Rectan *src, u32 color);

extern void Lib_UpdateRect(v_object *source, Rectan *src);

extern void Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst);

extern void Lib_GiveVobjectProp(v_object *source, Rectan *output);

extern void Lib_GetVObjectData(v_object *vobj, u32 *flags, i32 *h, i32 *w, u32 *pitch, void **pixels, Rectan **clip_rect, u8 *bpp, u32 *Rmask, u32 *Gmask, u32 *Bmask, u32 *Amask);

extern v_object *Lib_LoadBMP(const char *path);

extern u32 Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b);

extern void Lib_SetColorKey(v_object *vobj, u32 flag,u32 key);

extern v_object * Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask, u32 Bmask, u32 Amask);

/* a higher SDL function which is kinda an hack for event.c */
extern void Lib_EventPoll();

extern i32 Lib_PollEvent(void *event);

extern u8 *Lib_GetKeyState(i32 *numkeys);

extern u8 Lib_GetMouseState(i32 *x, i32 *y); 

#endif /* not __EXTLIB_H */
