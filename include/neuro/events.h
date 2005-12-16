/* events.h 
 */

#ifndef __EVENTS_H
#define __EVENTS_H

#include "neuro_engine.h"

/*! TODO */
extern void Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)());

/*! TODO */
extern void Neuro_AddPressedMouseEvent(u32 button, void (*callback)());

/*! TODO */
extern void Neuro_AddReleasedMouseEvent(u32 button, void (*callback)());

/*! TODO */
extern void Neuro_CleanKeyboard();

/*! TODO */
extern void Neuro_CleanMouse();


/* used internally */
extern void Events_Poll(void);
extern int Events_Init(void);
extern void Events_Clean(void);

#endif /* __EVENTS_H */
