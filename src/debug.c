/* debug.c
 * Module : Dbg_
 */

/*--- Extern Headers Including ---*/
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
/*--- Local Headers Including ---*/

/*--- Main Module Header ---*/
#include "debug.h"

/*--- Global Variables ---*/

/*--- Static Variables ---*/
static u8 debug_level = 0;

/*--- Static Prototypes ---*/

/*--- Static Functions ---*/

/*--- Global Functions ---*/

/*void
Neuro_DebugPrint(char *filename, char *funcName, u32 lineNum, const char *control, ...)
*/
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

/*--- Poll ---*/

/*--- Constructor Destructor ---*/

