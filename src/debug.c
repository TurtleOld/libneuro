
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
#include <global.h>
#include <ebuf.h>

/*-------------------- Main Module Header --------------------------*/
#include "debug.h"


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("debug");

typedef struct DEBUG_CHANNEL
{
	/* should only contain unallocated strings ie only "" strings. */
	char *namespace; /* the project's namespace this channel is in <-- NEURO_PROJECT_NAME()*/
	char *channel; /* the module name, example : bitmap <-- NEURO_MODULE_CHANNEL() */
	
	u32 class; /* the class, see debug.h -- enum DEBUG_CLASS */
	
	u32 rule; /* 1:+    0:- */
}DEBUG_CHANNEL;

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

static u8 debug_level = 0;

static EBUF *debug_l;


static const char *Debug_Classes[] = {
	"all",
	"warn",
	"error",
	"trace"
};

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

/* filter should be in the format class<+|->channel (ex: warn+bitmap)
 * and it can contain a virtually unlimited amount of 
 * space separated elements.
 */
void
Neuro_CoreSetFilter(char *project_name, char *filter)
{
	/* this function's purpose is to populate
	 * our filtering ebuffer debug_l.
	 *
	 * class and channel can be replaced by a special
	 * string : all - which will always be matched.
	 * the string all will be replaced by the character
	 * '*' to accelerate matching.
	 *
	 * the class type will be normalized to small cases
	 * in case the input instructions used capital letters.
	 * we will also drop and output a warn if an input
	 * class contains invalid characters, only letters will
	 * be accepted.
	 *
	 *
	 * the first thing we will do is separate every elements
	 * of the filter using Neuro_SepChr2(). Then we will check
	 * every of them by using the function memchr() to get the
	 * pointer of the character that is either + or -. In case
	 * theres none in the element, we drop it and output a warning
	 * but we still check the others. We then use again Neuro_SepChr2()
	 * for every elements with their corresponding type <+|->.
	 *
	 * This done, we need to loop the ebuffor debug_l to see if
	 * the filter to be added is already present (in that case, we
	 * drop the filter and don't output anything) or if the filter
	 * is contradictory to an existing one, we simply modify the
	 * existing one to become the new filter.
	 */


	DEBUG_CHANNEL *buf;

	if (!project_name || !filter)
	{
		NEURO_ERROR("Invalid argument used", NULL);
		return;
	}

	if (Neuro_EBufIsEmpty(debug_l))
	{
		Neuro_CleanEBuf(&debug_l);
		Neuro_CreateEBuf(&debug_l);
	}

	Neuro_AllocEBuf(debug_l, sizeof(DEBUG_CHANNEL*), sizeof(DEBUG_CHANNEL));

	buf = Neuro_GiveCurEBuf(debug_l);

	if (buf)
		buf->channel = filter;
	else
		NEURO_ERROR("Out of Memory", NULL);	
}

void
Neuro_DebugChannel(const char *project_name, const char *channel, char *type, char *filename, 
		char *funcName, u32 lineNum, u8 output_detailed, char *control, ...)
{
	va_list args;
	DEBUG_CHANNEL *buf;
	u32 total = 0;

	/* we allow the call of this function even 
	 * if the init wasn't priorly called.
	 */
	if (!debug_l)
	{
		Neuro_CreateEBuf(&debug_l);

		Neuro_SetFilter("Error");
		Neuro_SetFilter("Warn");

		NEURO_WARN("temporary default debugging set -- 2", NULL);
	}

	if (Neuro_EBufIsEmpty(debug_l))
		return;

	total = Neuro_GiveEBufCount(debug_l) + 1;

	while (total-- > 0)
	{

		buf = Neuro_GiveEBuf(debug_l, total);

		if (!strcmp(channel, buf->channel) || !strcmp(type, buf->channel))
		{
			if (output_detailed == 1)
				fprintf(stderr, "%s : \"%s\" (%s) %s:%s:%d -- ", type, project_name, channel, filename, funcName, lineNum);

			va_start(args, control);
			vfprintf(stderr, control, args);
			va_end(args);

			fprintf(stderr, "\n"); /* we do a line feed */
		}
	}}


/*-------------------- Constructor Destructor ----------------------*/

int
Debug_Init()
{
	Neuro_CreateEBuf(&debug_l);
	
	Neuro_SetFilter("Error");
	Neuro_SetFilter("Warn");

	NEURO_WARN("temporary default debugging set", NULL);

	return 0;
}

void
Debug_Clean()
{
	Neuro_CleanEBuf(&debug_l);
}
