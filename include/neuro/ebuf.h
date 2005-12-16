#ifndef __EBUF_H
#define __EBUF_H

#include "neuro_engine.h"
#include <stdlib.h>

/*  
 *  The EBUF module. Stands for Engine Buffer, it is named this
 *  way because it is an abstraction of the memory allocation, 
 *  reallocation method. It is very useful because it cuts 
 *  needed code for those by alot(and also cuts on very frequent errors).
 *  This module can be used for example on an array of names.
 *  It will provide the programmer with an easy way to make
 *  such an array to grow dynamically. This module also has a
 *  way to remove array frames from the array.
 */

/* 
 * This macro is to set the number of extra instances 
 * of memory needs to be allocated per allocations.
 * In theory, this is absolutely not needed but since
 * the allocation proccess is slow, the less we do it,
 * the better. 10 is a good default.
 */
#define MEMORY_ALLOC_OVERH 10

/* 
 * the Engine Buffer object. A pointer variable 
 * of this type is required. 
 * Example : EBUF *myeng;
 * 
 * -- this is the struct we want to have buffered, 
 * -- meaning EBUF will keep and create instances of this struct.
 * 
 * typedef struct ST
 * {
 * 	char *someString;
 * }ST;
 */
typedef struct EBUF EBUF;

/* 
 * This function needs to be called only once before any other
 * functions from this module is used. It cleans the EBUF variable 
 * completely. 
 * Example : Neuro_CreateEBuf(&myeng);
 */
extern void Neuro_CreateEBuf(EBUF **eng);

/* 
 * This function is used to set a callback function to the EBUF.
 * This callback function will be called (during the cleaning
 * time) for every arrays right before they are each freed.
 * This is so you can free custom stuff that you allocated in the
 * struct you put in EBUF. The callback function will need a single
 * argument which will be a void pointer. This will point to the
 * array(single element of the big array) which is being freed.
 * Example : 
 * 
 * static void
 * callbackclean(void *src)
 * {
 * 	ST *temp;
 * 	temp = (ST*)src;
 * 	if (src->someString)
 *		free(src->someString);
 * }
 *
 * -- in some initialisation function you put :
 * Neuro_SetcallbEBuf(myeng, &callbackclean);
 */
extern void Neuro_SetcallbEBuf(EBUF *eng, void (*callback)(void *src));

/* allocation and reallocation */
extern void Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj);

/* if you start with Create, you got to finish up with this function */
extern void Neuro_CleanEBuf(EBUF **eng);

/* clean a single element */
extern void Neuro_SCleanEBuf(EBUF *eng, void *object);

/* give the count of elements in the array */
extern u32 Neuro_GiveEBufCount(EBUF *eng);

/* give the array number of the element [object] */
extern i32 Neuro_GiveEBufElem(EBUF *eng, void *object);

/* give the real address of the element of the number [elem] */
extern void **Neuro_GiveEBufAddr(EBUF *eng, u32 elem);

/* give the element corresponding to the number [elem] */
extern void *Neuro_GiveEBuf(EBUF *eng, u32 elem);

/* copy the content of an element to another */
extern void Neuro_SetEBuf(EBUF *eng, void **to, void *from);

/* copy the content of an EBUF variable to another EBUF variable
 * Note : this is very fast because it is only address copy.
 */
extern void Neuro_CopyEBuf(EBUF *to, EBUF *from);

/* resets the EBUF variable WITHOUT FREEING IT -- Warning this 
 * is a mem leak if you didn't copy the content to another one 
 */
extern void Neuro_ResetEBuf(EBUF *eng);

/* this is a simple boolean returning function that returns
 * 1 if [eng] is empty and 0 if it holds stuff. 
 */
extern u8 Neuro_EBufIsEmpty(EBUF *eng);

#endif /* not __EBUF_H */
