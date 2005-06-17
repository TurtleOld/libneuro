/* events.h 
 */

#ifndef __EVENTS_H
#define __EVENTS_H

#include "engine.h"

/*! TODO */
extern void Events_AddPressedKeyEvent(u32 keysym, void (*callback)());

extern void Events_AddPressedMouseEvent(u32 button, void (*callback)());

extern void Events_AddReleasedMouseEvent(u32 button, void (*callback)());

/*! TODO */
extern void Events_Poll(void);

/*! TODO */
extern int Events_Init(void);

/*! TODO */
extern void Events_Clean(void);

extern void Events_CleanKeyb();

extern void Events_CleanMouse();

#endif /* __EVENTS_H */
