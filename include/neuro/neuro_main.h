
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
 * <p>
 * Neuro is a library that combines enough tools to abstract
 * a broad array of uses for applications. It was primarily coded
 * for providing a light renderer which can handle which images have
 * to be drawn first according to their level.
 * (a painter's algorithm if you prefer) but
 * it later started to include much more tools that were useful
 * for way more than just graphical applications.  
 * </p>
 *
 * <p>
 * Libneuro has 6 sections : 
 * <ul>
 *	<li><b>extlib</b></li>
 *	<li>debugging --> Neuro_Debug(3)</li>
 *	<li>memory --> Neuro_EBuf(3) </li>
 *	<li>video  --> Neuro_Graphics(3) </li>
 *	<li>events --> Neuro_Events(3)</li>
 *	<li>misc</li>
 * </ul>
 * <p>
 * Additionnal optionally embedded Projects (documentation not included)
 * <ul>
 * 	<li>libneuronet --> network interface abstraction layer</li>
 * 	<li>libneurogui --> graphical user interface implementation</li>
 * </ul>
 * </p>
 * </p>
 * <p>
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
 * Lib_ functions.  
 * </p>
 *
 * <p>
 * memory
 * </p>
 *
 * <p>
 * video
 * </p>
 *
 * @examples
 *
 * -- this is used so external/internal functions can actually
 * -- stop the main loop from looping. Used to stop the program
 * -- from running basicly.
 * static u32 running = 1;
 *
 * void
 * main_loop()
 * {
 * 	while (running)
 * 	{
 * 		-- This function is an absolute necessity for using
 * 		-- libneuro as it actually make everything happen internally
 * 		Neuro_Poll();
 *
 * 		... -- your own module's polls come here
 *
 * 		-- To avoid having the CPU hit high levels from this program
 * 		-- due to this loop going endlessly.
 * 		Neuro_Sleep(5000);
 * 	}
 * }
 *
 * ...
 *
 * int _err; -- used to keep the error state of libneuro
 * 
 * _err = Neuro_Init();
 * if (_err)
 * 	return 1;
 *
 * main_loop();
 *
 *
 * Neuro_Quit();
 *
 *
 *
 * @related
 * Neuro_SetNeuroDebugFilter(3), Neuro_Poll(3), Neuro_Init(3), Neuro_Quit(3)
 *
 **/


/** 
 * @sdescri
 * Function which sets which debugging channels will be output or not
 *
 * @param[in]
 * see Neuro_Debug(3) for details on the filter format
 *
 */
extern void Neuro_SetNeuroDebugFilter(const char *filter);

/** */
extern int Neuro_Poll();

/** */
extern int Neuro_Init();

/** */
extern void Neuro_Quit();

#ifdef __cplusplus
}
#endif

#endif /* __MODULE_H */
