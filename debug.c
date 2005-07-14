/* debug.c
 * Module : Dbg_
 */

/*--- Extern Headers Including ---*/
#include <stdio.h>

/*--- Local Headers Including ---*/

/*--- Main Module Header ---*/
#include "debug.h"

/*--- Global Variables ---*/

/*--- Static Variables ---*/

/*--- Static Prototypes ---*/

/*--- Static Functions ---*/

/*--- Global Functions ---*/

void
Dbg_Print(char *message, char *funcName, u32 lineNum)
{
	fprintf(stderr, "%s:%d -- %s\n", funcName, lineNum, message);
}

/*--- Poll ---*/

/*--- Constructor Destructor ---*/

