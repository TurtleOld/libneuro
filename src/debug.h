/* debug.h */

#ifndef __DEBUG_H
#define __DEBUG_H

#include "engine.h"

/*! TODO */
extern void Neuro_DebugPrint(char *message, char *filename, char *funcName, u32 lineNum);

/*! TODO */
/*
#define Dbg(x) Dbg_Print(x, __FUNCTION__, __LINE__)
*/
/* #define DbgP(v ,w, x, y, z) error_at_line(v, w, x, y, "%s", z) */
/*
#define DbgP error_at_line
*/

#endif /* __DEBUF_H */
