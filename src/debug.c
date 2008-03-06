
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
#include <stdio.h> /* printf() vfprintf() vasprintf()*/
#include <stdarg.h> /* va_start() va_end() */
#include <string.h> /* strcmp()  strlen()*/

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <ebuf.h>
#include <other.h> /* Neuro_SepChr2() */

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
}DEBUG_CHANNEL;

/* this is to avoid having a warning that the function 
 * isn't set.
 */
#if HAVE_VASPRINTF
extern int vasprintf(char **, const char *, va_list);
#endif /* HAVE_VASPRINTF */

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

static u8 debug_level = 0;

static EBUF *debug_l;

/* this variable is used in the Neuro_s() function */
static char *string_maker;


#define DEBUG_CLASS_AMOUNT 4
static const int Debug_ClassMasks[] = {
	0x00000007,
	0x00000001,
	0x00000002,
	0x00000004
};

static const char *Debug_Classes[] = {
	"all",
	"warn",
	"error",
	"trace"
};

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

static void
clean_debug_channel(void *src)
{
	DEBUG_CHANNEL *tmp;

	tmp = (DEBUG_CHANNEL*)src;

	if (tmp)
	{
		if (tmp->namespace)
			free(tmp->namespace);

		if (tmp->channel)
			free(tmp->channel);
	}
}

static int
elem_getclass(const char *class)
{
	u32 i = DEBUG_CLASS_AMOUNT;
	i32 class_type = -1;

	while (i-- > 0)
	{
		if (!strcmp(Debug_Classes[i], class))
		{
			class_type = Debug_ClassMasks[i];
			break;
		}		
	}


	return class_type;
}

static void
calibrate_string(char *string)
{
	int len = strlen(string);

	while (len-- > 0)
	{
		/*Debug_Val(0, "herein -> %c\n", string[len]);*/
		if (string[len] >= 'A' && string[len] <= 'Z')
			string[len] ^= 0x20;
	}
}

static DEBUG_CHANNEL *
filter_elem_exist(const char *namespace, const char *elem)
{
	DEBUG_CHANNEL *buf;
	u32 total = 0;

	if (Neuro_EBufIsEmpty(debug_l))
		return NULL;

	total = Neuro_GiveEBufCount(debug_l) + 1;

	while (total-- > 0)
	{
		buf = Neuro_GiveEBuf(debug_l, total);

		if (!buf->namespace)
			continue;

		if (!strcmp(namespace, buf->namespace))
		{
			if (!strcmp(elem, buf->channel))
			{
				return buf;
			}
		}
	}

	return NULL;
}

static DEBUG_CHANNEL *
filter_handleElem(char *namespace, char *elem)
{
	char *toggle = NULL;
	DEBUG_CHANNEL *buf;
	u8 type = 0;
	i32 class_type = 0;

	/* Debug_Val(0, "HANDLE \"%s\" len %d\n", elem, strlen(elem)); */

	if (strlen(elem) <= 1 || memchr(elem, ' ', strlen(elem)))
	{
		NEURO_WARN("too small filter element found", NULL);
		return NULL;
	}

	toggle = memchr(elem, '-', strlen(elem));

	if (!toggle)
	{
		toggle = memchr(elem, '+', strlen(elem));
		type = 1;
	}

	if (toggle)
		*toggle = '\0';
	else
	{
		NEURO_WARN("the character + or - is required before the channel name", NULL);
		return NULL;
	}
	
	if (toggle == elem)
	{
		elem = "all";
	}

	calibrate_string(elem);

	class_type = elem_getclass(elem);

	if (class_type < 0)
	{
		NEURO_ERROR("Invalid class used \"%s\", Valid classes: all warn error trace", elem);
		return NULL;
	}
	/* prior to creating a new element, we need to check the buffer for 
	 * an element with the same name so we can modify it instead of 
	 * creating a conflicting one.
	 */

	if ((buf = filter_elem_exist(namespace, &toggle[1])))
	{
		NEURO_TRACE("elem exists: %s", buf->channel);

		if (type == 0)
			buf->class ^= class_type;
		else
			buf->class &= class_type;

		return buf;
	}

	Neuro_AllocEBuf(debug_l, sizeof(DEBUG_CHANNEL*), sizeof(DEBUG_CHANNEL));

	buf = Neuro_GiveCurEBuf(debug_l);

	if (buf)
	{
		buf->channel = calloc(1, strlen(&toggle[1]) + 1);
		strncpy(buf->channel, &toggle[1], strlen(&toggle[1]));

		buf->namespace = calloc(1, strlen(namespace) + 1);
		strncpy(buf->namespace, namespace, strlen(namespace));
		if (type == 1)
			buf->class = class_type;
		/* Debug_Val(0, "before %s\n", elem); */
		/* Debug_Val(0, "after %s\n", elem); */
	}
	else
	{
		NEURO_ERROR("Out of Memory", NULL);
		return NULL;
	}

	return buf;
}

/*-------------------- Global Functions ----------------------------*/

void
Debug_Channel(const char *channel, const char *type, const char *filename, 
		const char *funcName, u32 lineNum, u8 output_detailed, const char *control, ...)
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
Debug_Val(u8 level, const char *control, ...)
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
 * an element that doesn't have a class will be treated as class
 * all by default.
 * and it can contain a virtually unlimited amount of 
 * space separated elements.
 */
void
Neuro_SetCoreDebugFilter(char *project_name, char *filter)
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
	 * This done, we need to loop the ebuffer debug_l to see if
	 * the filter to be added is already present (in that case, we
	 * drop the filter and don't output anything) or if the filter
	 * is contradictory to an existing one, we simply modify the
	 * existing one to become the new filter.
	 */

	if (!project_name || !filter)
	{
		NEURO_ERROR("Invalid argument used", NULL);
		return;
	}

	if (Neuro_EBufIsEmpty(debug_l))
	{
		Neuro_CleanEBuf(&debug_l);
		Neuro_CreateEBuf(&debug_l);
		Neuro_SetcallbEBuf(debug_l, clean_debug_channel);
	}

	/* we start by separating all the elements in the filter string */
	{
		EBUF *sep;
		u32 total = 0;

		sep = Neuro_SepChr2(' ', filter);

		if (Neuro_EBufIsEmpty(sep))
		{
			NEURO_WARN("No valid filter elements to parse", NULL);
			return;
		}

		total = Neuro_GiveEBufCount(sep) + 1;

		while (total-- > 0)
		{
			DEBUG_CHANNEL *tmp;
			SepChr_Data *buf;

			buf = Neuro_GiveEBuf(sep, total);

			tmp = filter_handleElem(project_name, buf->string);

			if (!tmp)
			{
				NEURO_WARN("Failed to create/modify element", NULL);
				continue;
			}

			NEURO_TRACE("created: %s", tmp->channel);
		}

		Neuro_CleanEBuf(&sep);
	}
}

void
Neuro_DebugChannel(const char *project_name, const char *channel, const char *type, const char *filename, 
		const char *funcName, u32 lineNum, u8 output_detailed, const char *control, ...)
{
	va_list args;
	DEBUG_CHANNEL *buf;
	u32 total = 0, i = 0;
	u32 print_message = 0; /* toggle */

	/* Debug_Val(0, "Herein %s %s->%s [%d]\n", project_name, channel, type, lineNum); */

	if (!project_name || !channel || !type || !filename || !funcName || !control)
	{
		const char *output = NULL;

		if (funcName)
			output = funcName;

		if (output)
			NEURO_ERROR("Invalid argument used -- caller name \"%s\"", output);
		else
			NEURO_ERROR("Invalid argument used -- unknown caller", NULL);

		return;
	}

	if (Neuro_EBufIsEmpty(debug_l))
	{	
		if (output_detailed == 1)
			fprintf(stderr, "- : (%s:%s) %s:%s:%d -- ", project_name, channel, filename, funcName, lineNum);

		fprintf(stderr, "temporary message because the debug buffer is empty\n");
		return;
	}

	total = Neuro_GiveEBufCount(debug_l) + 1;

	while (i < total)
	{
		buf = Neuro_GiveEBuf(debug_l, i);

		if (!strcmp(project_name, buf->namespace))
		{
			int class_type = 0;


			class_type = elem_getclass(type);

			if (class_type < 0)
			{
				NEURO_ERROR("Invalid class used", NULL);
				return;
			}

			/*Debug_Val(0, "class type %d\n", buf->class);
			Debug_Val(0, "%s -> %s\n", channel, buf->channel);*/

			/* the "all" type always matches every elements :) */
			if (!strcmp(channel, buf->channel) || !strcmp("all", buf->channel))
			{
				if ((buf->class & class_type) == 0)
					print_message = 0;
				else
					print_message = 1;
			}
		}

		i++;
	}

	if (print_message == 1)
	{
		u8 output_type[10];
		u32 len = strlen(type);

		if (len >= 10)
		{
			NEURO_ERROR("Type class default size of 10 is too low for present use, please raise it and recompile", NULL);
			return;
		}

		/* we set the ending NULL */

		output_type[len] = '\0';
		/* we Capitalize the type string */
		while (len-- > 0)
		{
			output_type[len] = type[len] ^ 0x20;
		}


		if (output_detailed == 1)
			fprintf(stderr, "%s : (%s:%s) %s:%s:%d -- ", output_type, project_name, channel, filename, funcName, lineNum);

		va_start(args, control);
		vfprintf(stderr, control, args);
		va_end(args);

		fprintf(stderr, "\n"); /* we do a line feed */
	}
}

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
char *
Neuro_s(const char *control, ...)
{
	int len = 0;
	va_list args;

	va_start(args, control);

	if (string_maker)
	{
		free(string_maker);
		string_maker = NULL;
	}

#if HAVE_VASPRINTF
	len = vasprintf(&string_maker, control, args);
#else /* NOT HAVE_VASPRINTF */
	string_maker = calloc(1, 512);
	len = vsnprintf(string_maker, 512, control, args);
#endif /* NOT HAVE_VASPRINTF*/
	va_end(args);

	return string_maker;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Debug_Init()
{
	if (Neuro_EBufIsEmpty(debug_l) && debug_l == NULL)
	{
		Neuro_CreateEBuf(&debug_l);
		Neuro_SetcallbEBuf(debug_l, clean_debug_channel);
	}

	return 0;
}

void
Debug_Clean()
{
	Neuro_CleanEBuf(&debug_l);

	if (string_maker)
	{
		free(string_maker);
		string_maker = NULL;
	}
}
