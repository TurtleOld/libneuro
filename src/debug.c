
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
#include <stdlib.h>
#include <stdio.h> /* printf vfprintf */
#include <stdarg.h> /* va_start va_end */
#include <string.h> /* strcmp */

/*-------------------- Local Headers Including ---------------------*/
#include <ebuf.h>

/*-------------------- Main Module Header --------------------------*/
#include "debug.h"


/*--------------------      Other       ----------------------------*/

typedef struct DEBUG_CHANNEL
{
	char *channel; /* should only contain unallocated strings */
}DEBUG_CHANNEL;

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

void
Debug_Channel(const char *channel, char *type, char *filename, 
		char *funcName, u32 lineNum, u8 output_detailed, char *control, ...)
{
	va_list args;
	DEBUG_CHANNEL *buf;
	u32 total = 0;

	if (Neuro_EBufIsEmpty(debug_l))
		return;

	total = Neuro_GiveEBufCount(debug_l) + 1;

	while (total-- > 0)
	{

		buf = Neuro_GiveEBuf(debug_l, total);

		if (!strcmp(channel, buf->channel) || !strcmp(type, buf->channel))
		{
			if (output_detailed == 1)
				fprintf(stderr, "%s : (%s) %s:%s:%d -- ", type, channel, filename, funcName, lineNum);

			va_start(args, control);
			vfprintf(stderr, control, args);
			va_end(args);

			fprintf(stderr, "\n"); /* we do a line feed */
		}
	}
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

void
Debug_VerboseChannel(char *channel)
{
	DEBUG_CHANNEL *buf;

	if (!channel)
		return;

	Neuro_AllocEBuf(debug_l, sizeof(DEBUG_CHANNEL*), sizeof(DEBUG_CHANNEL));

	buf = Neuro_GiveCurEBuf(debug_l);

	buf->channel = channel;
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
