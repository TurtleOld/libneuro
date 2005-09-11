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

/*--- Static Prototypes ---*/

/*--- Static Functions ---*/

/*--- Global Functions ---*/

/*void
Neuro_DebugPrint(char *filename, char *funcName, u32 lineNum, const char *control, ...)
*/
void
Neuro_DebugPrint(char *control, char *filename, char *funcName, u32 lineNum)
{
	/*
	va_list args;
	char *msg = calloc(1, 520);
	
	va_start(args, control);
	vasprintf(msg, control, args);
	va_end(args);
	*/
	/* fprintf(stderr, "%s:%s:%d -- %s\n", filename, funcName, lineNum, control); */
	fprintf(stderr, "%s\n", control);
	
	/* free(msg); */
}

/*--- Poll ---*/

/*--- Constructor Destructor ---*/

