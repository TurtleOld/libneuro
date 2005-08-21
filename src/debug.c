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

inline void
Neuro_DebugPrint(char *message, char *filename, char *funcName, u32 lineNum)
{
	fprintf(stderr, "%s:%s:%d -- %s\n", filename, funcName, lineNum, message);
}

/*--- Poll ---*/

/*--- Constructor Destructor ---*/

