/* debug.h */

#ifndef __DEBUG_H
#define __DEBUG_H

#include "engine.h"

/*! TODO */
extern void Dbg_Print(u8 *message, u8 *funcName, u32 lineNum);

/*! TODO */
/* #define DbgP(x, y, z) Dbg_Print(x, y, z) */
/* #define DbgP(v ,w, x, y, z) error_at_line(v, w, x, y, "%s", z) */
#define DbgP error_at_line


#endif /* __DEBUF_H */
