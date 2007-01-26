
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

/* debug.c
 * Module : Debug
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

/*-------------------- Local Headers Including ---------------------*/
#include <ebuf.h>

/*-------------------- Main Module Header --------------------------*/
#include "debug.h"


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

static u8 debug_level = 0;

static EBUF *debug_l;

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

void
Neuro_DebugPrint(char *type, char *control, char *filename, char *funcName, u32 lineNum)
{
	/*
	va_list args;
	char *msg = calloc(1, 520);
	
	va_start(args, control);
	vasprintf(msg, control, args);
	va_end(args);
	*/
	fprintf(stderr, "%s : %s:%s:%d -- %s\n", type, filename, funcName, lineNum, control);
	/* fprintf(stderr, "%s\n", control); */
	
	/* free(msg); */
}

/* */
void
Debug_Val(u8 level, char *control, ...)
{
	va_list args;
	/* char *msg;*/
	
	if (debug_level >= level)
	{
		/* msg = calloc(1, 520); */
		va_start(args, control);
		/* vasprintf(msg, control, args); */
		vfprintf(stderr, control, args);
		va_end(args);

		/* free(msg); */
	}
}

void
Neuro_SetDebugLevel(u8 level)
{
	debug_level = level;
}

int
IsLittleEndian()
{
	int i = 1;
	char *p;

	p = (char*)&i;
	if (p[0] == 1)
		return 1;
	else
		return 0;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Debug_Init()
{
	Neuro_CreateEBuf(&debug_l);
	return 0;
}

void
Debug_Clean()
{
	Neuro_CleanEBuf(&debug_l);
}
