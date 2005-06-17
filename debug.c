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
Dbg_Print(u8 *message, u8 *funcName, u32 lineNum)
{
	fprintf(stderr, "%s in %s:%d\n", message, funcName, lineNum);
}

/*--- Poll ---*/

/*--- Constructor Destructor ---*/

