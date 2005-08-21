/* Library Abstraction Layer header */

#ifndef __EXTLIB_H
#define __EXTLIB_H

#include "engine.h"

extern int Lib_VideoInit(v_object **screen, v_object **screen_buf);

extern void Lib_VideoExit();

extern int Lib_EventInit();

extern void Lib_EventExit();


extern void Lib_FreeVobject(v_object *source);

extern void Lib_Flip(v_object *source);

extern void Lib_FillRect(v_object *source, Rectan *src, u32 color);

extern void Lib_UpdateRect(v_object *source, Rectan *src);

extern void Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst);

extern void Lib_GiveVobjectProp(v_object *source, Rectan *output);


#endif /* not __EXTLIB_H */
