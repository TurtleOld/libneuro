
/*    
 * libneuro, a light weight abstraction of high or lower libraries 
 * and toolkit for applications.
 * Copyright (C) 2005-2006  Nicholas Niro, Robert Lemay
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef __EBUF_H
#define __EBUF_H

#include "neuro_engine.h"
#include <stdlib.h> /* for the size_t type */

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

/** Neuro_CreateEBuf
 * @sdescri 
 * sole constructor of an EBUF element.
 *
 * @description 
 * This function initializes and makes operationnal
 * an EBUF element.
 * 
 * @param[in] 
 * the address of an EBUF pointer.
 *
 * @examples 
 *
 * static EBUF *myeng; \n \n
 * 
 * ... \n \n
 *
 * Neuro_CreateEBuf(&myeng);
 *
 * @related 
 * Neuro_CleanEBuf(3), Neuro_AllocEBuf(3), Neuro_SetcallbEBuf(3), 
 * Neuro_EBufIsEmpty(3)
 *
 */
extern void Neuro_CreateEBuf(EBUF **eng);

/** Neuro_SetcallbEBuf
 * @sdescri 
 * Set a callback that will be called for each array elements
 * when the EBUF element is cleaning itself after a call to Neuro_CleanEBuf
 * or Neuro_SCleanEBuf.
 *
 * @description 
 * This function is used to set a callback function to the EBUF element.
 * This callback function will be called (during the cleaning
 * time) for every arrays right before they are each freed.
 * This is so you can free custom stuff that you allocated in the
 * struct you put in EBUF. The callback function will need a single
 * argument which will be a void pointer. This will point to the
 * array(single element of the big array) which is being freed.
 *
 *
 * @param[in]
 * an EBUF element.
 *
 * @param[in]
 * a callback which itself contain an argument to a void pointer.
 *
 * @examples
 *
 * typedef struct ST \n
 * { \n
 * 	char *someString; \n
 * }ST; \n \n \n
 *
 *
 * static EBUF *myeng; \n \n
 *
 * static void \n
 * callbackclean(void *src) \n
 * { \n
 * 	ST *temp; \n \n
 *
 * 	temp = (ST*)src; \n \n
 *
 * 	if (temp) \n
 * 	{ \n
 * 		if (temp->someString) \n
 * 			free(temp->someString); \n
 * 	} \n
 * } \n \n
 *
 * ... \n \n
 *
 * Neuro_CreateEBuf(&myeng); \n \n
 * Neuro_SetcallbEBuf(myeng, &callbackclean); \n \n
 *
 * ... \n \n
 *
 * ST *foo; \n \n
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST)); \n \n
 *
 * foo = Neuro_GiveCurEBuf(myeng); \n \n
 *
 * foo->someString = (char*)malloc(50); \n \n
 *
 * ... \n \n
 *
 * Neuro_CleanEBuf(&myeng);
 *
 * @related
 * Neuro_CreateEBuf(3), Neuro_CleanEBuf(3)
 *
 */
extern void Neuro_SetcallbEBuf(EBUF *eng, void (*callback)(void *src));

/** Neuro_AllocEBuf
 * @sdescri
 * Simple Allocation and reallocation of an EBuf element.
 *
 * @description
 * This function is pretty much the most useful function in the lot,
 * it actually creates a new "slot" which can then be used to add
 * data to an EBUF element. This function and Neuro_MultiAllocEBuf
 * are the two only functions which allocate/reallocate memory in
 * an EBUF element. 
 *
 * Take good note that an EBUF element can buffer
 * any kinds of structure (so it can dynamically grow easily and avoid memory
 * leaks). To support this, this function needs only 2 informations 
 * about the structure : 
 * the size of its pointer form and the size of its normal form. It might
 * also be possible to work with other variable types than structures, although
 * it is not really recommended unless you know what you are doing.
 *
 * @param[in]
 * an EBUF element.
 *
 * @param[in]
 * the size of the pointer form of the structure (usually sizeof can be used to find it).
 *
 * @param[in]
 * the size of the normal form of the structure (usually sizeof can be used to find it).
 *
 * @examples
 * typedef struct ST \n
 * { \n
 * 	int foobar; \n
 * }ST; \n \n \n
 *
 *
 * static EBUF *myeng; \n \n
 *
 *
 * ... \n \n
 *
 * Neuro_CreateEBuf(&myeng); \n \n
 *
 * ... \n \n
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST)); \n \n
 *
 * ... \n \n
 *
 * Neuro_CleanEBuf(&myeng);
 *
 * @related
 * Neuro_GiveEBufCount(3), Neuro_GiveEBuf(3), Neuro_GiveCurEBuf(3), 
 * Neuro_MultiAllocEBuf(3)
 */
extern void Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj);


/** Neuro_MultiAllocEBuf
 * @sdescri
 * initial allocation of a bigger amount of slots than one.
 * 
 * @description
 * This function works exactly the same as Neuro_AllocEBuf except
 * for one single thing : It can allocate more than one slot at once.
 * Take very good note that this will only work if the EBUF object hasn't
 * been allocated before, ie only if it is empty. For security purpose, this 
 * function won't work if theres already allocated slots in the EBUF.
 *
 * @param[in]
 * an EBUF element.
 *
 * @param[in]
 * the amount of slots to allocate.
 *
 * @param[in]
 * the size of the pointer form of the structure (usually sizeof can be used to find it).
 *
 * @param[in]
 * the size of the normal form of the structure (usually sizeof can be used to find it).* 
 *
 * @examples
 * see the example in Neuro_AllocEBuf(3)
 *
 * @related
 * Neuro_GiveEBufCount(3), Neuro_GiveEBuf(3), Neuro_GiveCurEBuf(3), 
 * Neuro_AllocEBuf(3)
 */
extern void Neuro_MultiAllocEBuf(EBUF *eng, u32 amount, size_t sptp, size_t sobj);

/** Neuro_CleanEBuf
 * @sdescri 
 * Standard destructor of an EBUF element.
 *
 * @description 
 *
 * 
 * @param[in] 
 * the address of an EBUF pointer.
 *
 * @examples 
 *
 * static EBUF *myeng; \n \n
 * 
 * ... \n \n
 *
 * Neuro_CreateEBuf(&myeng);
 *
 * ... \n \n
 *
 * Neuro_CleanEBuf(&myeng);
 *
 * @related 
 * Neuro_CreateEBuf(3), Neuro_AllocEBuf(3), Neuro_SetcallbEBuf(3), 
 * Neuro_SCleanEBuf(3)
 *
 */
extern void Neuro_CleanEBuf(EBUF **eng);

/* clean a single element 
 * Neuro_CreateEBuf(3), Neuro_AllocEBuf(3), Neuro_SetcallbEBuf(3), 
 * Neuro_CleanEBuf(3)
 *
 */
extern void Neuro_SCleanEBuf(EBUF *eng, void *object);

/* give the count of elements in the array 
 * Neuro_AllocEBuf(3), Neuro_GiveEBuf(3),
 * Neuro_GiveCurEBuf(3), Neuro_EBufIsEmpty(3)
 */
extern u32 Neuro_GiveEBufCount(EBUF *eng);

/* give the array number of the element [object] 
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufCore(3), Neuro_SetEBuf(3),
 * Neuro_CopyEBuf(3), Neuro_ResetEBuf(3)
 */
extern int Neuro_GiveEBufElem(EBUF *eng, void *object, u32 *elem);

/* gives the last element of the buffer 
 * Neuro_AllocEBuf(3), Neuro_MultiAllocEBuf(3)
 */
extern void *Neuro_GiveCurEBuf(EBUF *eng);

/* give the real address of the element of the number [elem] 
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufElem(3)
 * Neuro_GiveEBufCore(3), Neuro_SetEBuf(3),
 * Neuro_CopyEBuf(3), Neuro_ResetEBuf(3)
 */
extern void **Neuro_GiveEBufAddr(EBUF *eng, u32 elem);

/* give the element corresponding to the number [elem] 
 * Neuro_AllocEBuf(3), Neuro_MultiAllocEBuf(3), 
 * Neuro_GiveEBufCount(3)
 */
extern void *Neuro_GiveEBuf(EBUF *eng, u32 elem);

/* give the core buffer of the EBuf element 
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufAddr(3), Neuro_SetEBuf(3),
 * Neuro_CopyEBuf(3), Neuro_ResetEBuf(3)
 */
extern void **Neuro_GiveEBufCore(EBUF *eng);


/* copy the content of an element to another 
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufCore(3), Neuro_GiveEBufCore(3),
 * Neuro_CopyEBuf(3), Neuro_ResetEBuf(3)
 *
 */
extern void Neuro_SetEBuf(EBUF *eng, void **to, void *from);

/* copy the content of an EBUF variable to another EBUF variable
 * Note : this is very fast because it is only address copy.
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufCore(3), Neuro_SetEBuf(3),
 * Neuro_SetEBuf(3), Neuro_ResetEBuf(3)
 */
extern void Neuro_CopyEBuf(EBUF *to, EBUF *from);

/* resets the EBUF variable WITHOUT FREEING IT -- Warning this 
 * is a mem leak if you didn't copy the content to another one 
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufCore(3), Neuro_SetEBuf(3),
 * Neuro_CopyEBuf(3), Neuro_CopyEBuf(3)
 */
extern void Neuro_ResetEBuf(EBUF *eng);

/* this is a simple boolean returning function that returns
 * 1 if [eng] is empty and 0 if it holds stuff. 
 *
 * Neuro_GiveEBufCount(3)
 */
extern u8 Neuro_EBufIsEmpty(EBUF *eng);

#endif /* not __EBUF_H */
