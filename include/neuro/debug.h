/* debug.h */

#ifndef __DEBUG_H
#define __DEBUG_H

#include "neuro_engine.h"

/*! TODO */
/*extern void Neuro_DebugPrint(char *filename, char *funcName, u32 lineNum, const char *control, ...) __attribute__ ((__always_inline__));*/

extern void Neuro_DebugPrint(char *type, char *control, char *filename, char *funcName, u32 lineNum);

/*! TODO */
/*#define DbgP(x) Neuro_DebugPrint(__FILE__, __FUNCTION__, __LINE__, x)*/
/*#define DbgP(x) Neuro_DebugPrint(x, NULL, NULL, 0)*/
#define Debug_Print(x) Neuro_DebugPrint("Debug Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Error_Print(x) Neuro_DebugPrint("Error Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Info_Print(x) Neuro_DebugPrint("Information Message", x, __FILE__, __FUNCTION__, __LINE__)

/*
#define Dbg(x) Dbg_Print(x, __FUNCTION__, __LINE__)
*/
/* #define DbgP(v ,w, x, y, z) error_at_line(v, w, x, y, "%s", z) */
/*
#define DbgP error_at_line
*/

#endif /* __DEBUF_H */
