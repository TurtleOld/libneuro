/* debug.h */

#ifndef __DEBUG_H
#define __DEBUG_H

#include "neuro_engine.h"

/*! This function shouldn't be used directly because
 * Macros were made to fill automatically the arguments
 * like the file name, function name, line number.
 */
extern void Neuro_DebugPrint(char *type, char *control, char *filename, char *funcName, u32 lineNum);

/*! Prints predefined
 * messages and also makes for a very
 * easy to call function with only one 
 * argument which extends to 4 automatically 
 */
#define Debug_Print(x) Neuro_DebugPrint("Debug Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Error_Print(x) Neuro_DebugPrint("Error Message", x, __FILE__, __FUNCTION__, __LINE__)
#define Info_Print(x) Neuro_DebugPrint("Information Message", x, __FILE__, __FUNCTION__, __LINE__)

/* depending on the current debug level, will 
 * output those formatted debug informations.
 */
extern void Debug_Val(u8 level, char *control, ...);

/* sets the current debug level */
extern void Neuro_SetDebugLevel(u8 level);
	

#endif /* __DEBUG_H */
