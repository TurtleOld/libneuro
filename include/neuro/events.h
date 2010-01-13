
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

/* events.h 
 */

#ifndef __EVENTS_H
#define __EVENTS_H

#include "neuro_engine.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @name
 * Neuro_Events
 *
 * @sdescri
 * A module which permits Input events like keyboard and mouse to be
 * set to a callback which is called once they are triggered.
 *
 * @description
 * Neuro Events module
 * 
 * @related
 * Neuro_AddPressedKeyEvent(3), Neuro_AddReleasedKeyEvent(3),
 * Neuro_AddPressedMouseEvent(3), Neuro_AddReleasedMouseEvent(3),
 * Neuro_AddPressedMultiKeyEvent(3), Neuro_AddReleasedMultiKeyEvent(3),
 * Neuro_CleanKeyboard(3), Neuro_CleanMouse(3), Neuro_GetMousePos(3)
 **/

/** TODO */
extern void Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)());

/** TODO */
extern void Neuro_AddReleasedKeyEvent(u32 keysym, void (*callback)());


/** TODO */
extern void Neuro_AddPressedMouseEvent(u32 button, void (*callback)(int x, int y));

/** TODO */
extern void Neuro_AddReleasedMouseEvent(u32 button, void (*callback)(int x, int y));


/**
 * @sdescri 
 * same as the normal Neuro_AddPressedKeyEvent(3) function but also 
 * sends the key number as the first argument of
 * the callback so one callback can be used for
 * more than one key at once.
 *
 * @param[in]
 * the keysym of the keyboard that is catched and sent to the callback.
 *
 * @param[in]
 * this callback is called with the keysym that was catched.
 *
 * @related
 * Neuro_AddReleasedMultiKeyEvent(3)
 */
extern void Neuro_AddPressedMultiKeyEvent(u32 keysym, void (*callback)(u32 keysym));

/** TODO */
extern void Neuro_AddReleasedMultiKeyEvent(u32 keysym, void (*callback)(u32 keysym));

/** TODO */
extern void Neuro_CleanKeyboard();

/** TODO */
extern void Neuro_CleanMouse();

/** puts the mouse coordinates into x and y */
extern void Neuro_GetMousePos(int *x, int *y);

/* porcelein function which permits peeking the trigger state of the keyboard events. */
extern int Neuro_GetKeyStatus(int keysym);

/* used internally */

extern void Events_TriggerMotion(int x, int y);
extern void Events_TriggerButton(u32 button, int x, int y, int status);
extern void Events_TriggerKey(u32 keysym, int keystatus);

extern void Events_Poll(void);
extern int Events_Init(void);
extern void Events_Clean(void);

#ifdef __cplusplus
}
#endif


#endif /* __EVENTS_H */
