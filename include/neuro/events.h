
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

/*! TODO */
extern void Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)());

/*! TODO */
extern void Neuro_AddPressedMouseEvent(u32 button, void (*callback)(int x, int y));

/*! TODO */
extern void Neuro_AddReleasedMouseEvent(u32 button, void (*callback)(int x, int y));

/*! TODO */
extern void Neuro_CleanKeyboard();

/*! TODO */
extern void Neuro_CleanMouse();


/* used internally */
extern void Events_Poll(void);
extern int Events_Init(void);
extern void Events_Clean(void);

#endif /* __EVENTS_H */
