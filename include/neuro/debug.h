
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

#ifdef __cplusplus
extern "C" {
#endif

enum DEBUG_CLASS
{
	DBG_Start,

	DBG_All,
	DBG_Warn,
	DBG_Error,
	DBG_Trace,

	DBG_End
};

/* don't call this function directly, use Neuro_SetDebugFilter(3) */
extern void Neuro_SetCoreDebugFilter(char *project_name, char *filter);

#define Neuro_SetDebugFilter(x) Neuro_SetCoreDebugFilter(NEURO_PROJECT_NAMESPACE,  x)

extern void Neuro_DebugChannel(const char *project_name, const char *channel, const char *type, const char *filename, const char *funcName, u32 lineNum, u8 output_detailed, const char *control, ...);

#define NEURO_ERROR(x, y) Neuro_DebugChannel(NEURO_PROJECT_NAMESPACE, \
		NEURO_CURRENT_CHANNEL, \
	       	"error", __FILE__, __FUNCTION__, __LINE__, 1, x, y)

#define NEURO_WARN(x, y) Neuro_DebugChannel(NEURO_PROJECT_NAMESPACE, \
		NEURO_CURRENT_CHANNEL, \
		"warn", __FILE__, __FUNCTION__, __LINE__, 1, x, y)

#define NEURO_TRACE(x, y) Neuro_DebugChannel(NEURO_PROJECT_NAMESPACE, \
		NEURO_CURRENT_CHANNEL, \
	       	"trace", __FILE__, __FUNCTION__, __LINE__, 1, x, y)

#define NEURO_PROJECT_NAME(x) char *NEURO_PROJECT_NAMESPACE=x

#define NEURO_MODULE_CHANNEL(x) static char *NEURO_CURRENT_CHANNEL=x

/* This fonction can be used to
 * extend the use of the 3 debug
 * functions in this module :
 * NEURO_ERROR _WARNING and _DEBUG.
 * it can be used to actually
 * enable more than one argument
 * to be passed to the resulting
 * message.
 *
 * example would be :
 * NEURO_ERROR("%s", Neuro_s("Debug failed with code %d at loop #%d", errno, cycle_num));
 *
 * the string would be grown as 
 * needed and it is automatically
 * freed at Debug_Clean(); call.
 * (done by Neuro_Quit();)
 */
extern char *Neuro_s(const char *control, ...);

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

extern int Debug_Init();
extern void Debug_Clean();


#ifdef __cplusplus
}
#endif

#endif /* NOT __DEBUG_H */
