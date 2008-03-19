
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

/* neuro.h */

#ifndef __NEURO_H
#define __NEURO_H

#include "neuro_engine.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @name
 * Neuro_Library
 *
 * @description
 * Neuro is a library that combines enough tools to abstract
 * a broad array of uses for applications. It was primarily coded
 * for providing a light renderer which can handle which images have
 * to be drawn first according to their level.
 * (a painter's algorithm if you prefer) but
 * it later started to include much more tools that were useful
 * for way more than just graphical applications. \n \n
 *
 * Libneuro has 8 sections plus additionnal projects inside it : \n
 * \\fI*\\fP \\fBextlib\\fP \n
 * \\fI*\\fP memory \n
 * \\fI*\\fP video \n
 * \\fI*\\fP events \n
 * \\fI*\\fP misc \n \n
 *
 * Libneuro was previously fully dependant on SDL but then some
 * functionalities of SDL weren't perfect and thus it was decided
 * to make libneuro have it's own abstraction for low level graphical
 * libraries in order to permit the implementation for other low
 * level libraries. And that's how the extlib (external library) section
 * of libneuro was done. The module currently contains 3 main drivers.
 * X11, SDL and none(or dummy). The X11 driver now being the main driver
 * for libneuro. The functions in the extlib module are primarily of the Lib_
 * prefix but they aren't exported to external functions so they can't be used
 * directly. The misc/other.c module contains Neuro_ bindings for all the
 * Lib_ functions. \n \n
 *
 * memory
 *
 * video
 *
 *
 *
 *
 *
 *
 **/


/* */
extern void Neuro_SetNeuroDebugFilter(const char *filter);

/* */
extern int Neuro_Poll();

/* */
extern int Neuro_Init();

/* */
extern void Neuro_Quit();

#ifdef __cplusplus
}
#endif

#endif /* __MODULE_H */
