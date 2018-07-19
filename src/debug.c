
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
#include <string.h> /* strcmp()  strlen() memset() */

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

static EBUF *debug_l; /* contains DEBUG_CHANNEL elems */

/* this variable is used in the Neuro_s() function */
static char *string_maker;


#define DEBUG_CLASS_AMOUNT 4 /* the 5th one is very special */
static const int Debug_ClassMasks[] = {
	0x00000007, /* all */
	0x00000001, /* warn */
	0x00000002, /* error */
	0x00000004,  /* trace */
	0x00000010 /* for any class, this is to not show it */
};

static const char *Debug_Classes[] = {
	"all",
	"warn",
	"error",
	"trace"
};

#define DETAILED_DEBUG 0

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
filter_handleElem(const char *namespace, char *elem)
{
	char *toggle = NULL;
	DEBUG_CHANNEL *buf;
	u8 type = 0; /* 1 addition, 0 deletion */
	i32 class_type = 0;

	if (strlen(elem) <= 1 || memchr(elem, ' ', strlen(elem)))
	{
		WARN("too small filter element found");
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
		WARN("the character + or - is required before the channel name");
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
		ERROR(Neuro_s("Invalid class used \"%s\", Valid classes: all warn error trace", elem));
		return NULL;
	}
	/* prior to creating a new element, we need to check the buffer for 
	 * an element with the same name so we can modify it instead of 
	 * creating a conflicting one.
	 */

	if ((buf = filter_elem_exist(namespace, &toggle[1])))
	{
		TRACE(Neuro_s("elem exists: %s class %d type %d class_type %d", buf->channel, buf->class, type, class_type));

		if ((buf->class & class_type) == 0)
			buf->class ^= class_type;

		if (type == 0)
			buf->class ^= Debug_ClassMasks[4];

		TRACE(Neuro_s("new class %d", buf->class));

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

		buf->class = class_type;

		if (type == 0)
			buf->class ^= Debug_ClassMasks[4];

		/* fprintf(stdout, "Added elem with class %d -- witness no show flag %d\n", buf->class, Debug_ClassMasks[4]); */
		TRACE(Neuro_s("(%s) Element -> %s %s", namespace, elem, buf->channel));
	}
	else
	{
		ERROR("Out of Memory");
		return NULL;
	}

	return buf;
}

/*-------------------- Global Functions ----------------------------*/

int
IsLittleEndian(void)
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
Neuro_SetCoreDebugFilter(const char *project_name, const char *filter)
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
		ERROR("Invalid argument used");
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
			WARN("No valid filter elements to parse");
			return;
		}

		total = Neuro_GiveEBufCount(sep) + 1;

		while (total-- > 0)
		{
			DEBUG_CHANNEL *tmp;
			SepChr_Data *buf;

			buf = Neuro_GiveEBuf(sep, total);

			if (DETAILED_DEBUG)
				fprintf(stderr, "pushed \'%s\' to filter_handleElem\n", buf->string);
			
			tmp = filter_handleElem(project_name, buf->string);

			if (!tmp)
			{
				WARN("Failed to create/modify element");
				continue;
			}

			TRACE(Neuro_s("created: %s", tmp->channel));
		}

		Neuro_CleanEBuf(&sep);
	}
}

void
Neuro_DebugChannel(const char *project_name, const char *channel, const char *type, const char *filename, 
		const char *funcName, u32 lineNum, u8 output_detailed, const char *message)
{
	DEBUG_CHANNEL *buf;
	u32 total = 0;
	u32 print_message = 0; /* toggle */

	if (!project_name || !channel || !type || !filename || !funcName || !message)
	{
		const char *output = NULL;

		if (funcName)
			output = funcName;

		if (output)
			ERROR(Neuro_s("Invalid argument used -- caller name \"%s\"", output));
		else
			ERROR("Invalid argument used -- unknown caller");

		return;
	}

	
	/* if (Neuro_EBufIsEmpty(debug_l)) */
	if (DETAILED_DEBUG)
	{	
		if (output_detailed == 1)
			fprintf(stderr, "- : (%s:%s) %s:%s:%d type %s -- ", project_name, channel, filename, funcName, lineNum, type);

		fprintf(stderr, "debug --> ");
		fprintf(stderr, message);
		fprintf(stderr, "\n");

		/* return; */
	}

	if (Neuro_EBufIsEmpty(debug_l))
	{
		if (DETAILED_DEBUG)
			fprintf(stderr, "The buffer debug_l is empty, no debugging will be output\n");
		return;
	}

	total = Neuro_GiveEBufCount(debug_l) + 1;

	while (total-- > 0)
	{
		buf = Neuro_GiveEBuf(debug_l, total);

		if (DETAILED_DEBUG)
			fprintf(stderr, "Is \'%s\' the same as \'%s\'? ... ", 
					project_name, buf->namespace);
		if (!strcmp(project_name, buf->namespace))
		{
			int class_type = 0;
		
			if (DETAILED_DEBUG)
				fprintf(stderr, "Yes\n");

			class_type = elem_getclass(type);

			if (class_type < 0)
			{
				ERROR("Invalid class used");
				return;
			}

			if (DETAILED_DEBUG)
				fprintf(stderr, "Is \'%s | %s\' the same as \'%s\'? ... ", 
					channel, "all", buf->channel);
			/* the "all" type always matches every elements :) */
			if (!strcmp(channel, buf->channel) || !strcmp("all", buf->channel))
			{
				if (DETAILED_DEBUG)
					fprintf(stderr, "Yes extra -> [%d] & [%d] = %d\n", buf->class, class_type, buf->class & class_type);

				if (buf->class & Debug_ClassMasks[4])
					print_message = 0;
				else if (buf->class & class_type)
					print_message = 1;
			}
			else
			{
				if (DETAILED_DEBUG)
					fprintf(stderr, "No\n");
			}
		}
		else
		{
			if (DETAILED_DEBUG)
				fprintf(stderr, "No\n");
		}
	}

	if (print_message == 1)
	{
		u8 output_type[10];
		u32 len = strlen(type);

		if (len >= 10)
		{
			ERROR("Type class default size of 10 is too low for present use, please raise it and recompile");
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

		fprintf(stderr, message);

		fprintf(stderr, "\n"); /* we do a line feed */
	}
	else
	{
		if (DETAILED_DEBUG)
			fprintf(stderr, "Debug message rejected due to filter\n");
	}
}

/* This function can be used to
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
		memset(string_maker, 0, 2048);
	} else {
		string_maker = calloc(1, 2048);
	}

	len = vsnprintf(string_maker, 2048, control, args);
	va_end(args);

	return string_maker;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Debug_Init(void)
{
	if (Neuro_EBufIsEmpty(debug_l) && debug_l == NULL)
	{
		Neuro_CreateEBuf(&debug_l);
		Neuro_SetcallbEBuf(debug_l, clean_debug_channel);
	}

	return 0;
}

void
Debug_Clean(void)
{
	if (DETAILED_DEBUG)
		fprintf(stderr, "Cleaning The debug buffer.\n");
	Neuro_CleanEBuf(&debug_l);

	if (string_maker)
	{
		free(string_maker);
		string_maker = NULL;
	}
}
