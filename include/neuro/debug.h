
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

/* debug.h */

#ifndef __DEBUG_H
#define __DEBUG_H

#include "neuro_engine.h"

/*! This function shouldn't be used directly because
 * Macros were made to fill automatically the arguments
 * like the file name, function name, line number.
 */
extern void Neuro_DebugPrint(char *type, char *control, char *filename, char *funcName, u32 lineNum);

/*! Prints predefined
 * messages and also makes for a very
 * easy to call function with only one 
 * argument which extends to 4 automatically 
 */
#define Debug_Print(x) Neuro_DebugPrint("Debug Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Error_Print(x) Neuro_DebugPrint("Error Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Info_Print(x) Neuro_DebugPrint("Information Message", x, __FILE__, __FUNCTION__, __LINE__)

/**
 * @sdescri flexible formatted text output function with levels
 * @description depending on the current debug level, will 
 * output those formatted debug informations.
 *
 * @related Neuro_SetDebugLevel(3)
 */
extern void Debug_Val(u8 level, char *control, ...);

/** 
 * @sdescri sets the current debug level
 *
 * @description this function's sole purpose is to set
 * the current debug level which will only change the 
 * behavior of the function Debug_Val.
 *
 * @related Debug_Val(3)
 */
extern void Neuro_SetDebugLevel(u8 level);

/* macro used to check for endianness.
 * this macro returns 1 if the system
 * is little endian and 0 if the system
 * is anything else (big endian hopefully
 * because we don't support middle endian.
 */
/*
#define IsLittleEndian() 		\
({					\
		int __i;		\
	        char *__p;		\
		__p = (char*)&__i;	\
		__i = 1;		\
		if (__p[0] == 1)	\
			1;	\
		else			\
			0;})
*/
extern int IsLittleEndian();
	

#endif /* __DEBUG_H */
