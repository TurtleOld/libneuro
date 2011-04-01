/* util.h */

#ifndef __UTIL_H
#define __UTIL_H

#include "common.h"

/* complement to EBUF, same as Neuro_SCleanEBuf but without it's flaw
 * however, this is not an efficient solve for the problem.
 *
 * we delete the first element and reorder the elements back to their 
 * normal ordering.
 */
extern void Util_SCleanEBuf(EBUF *input, void *element);

#endif /* NOT __UTIL_H */
