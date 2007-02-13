
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

/**
 * @name
 * Neuro_EBuf
 *
 * @description
 * IN SHORT : \n \n
 * The EBUF module. Stands for Engine Buffer. 
 * Its primary use is to make the allocation and 
 * reallocation of structures quick and easy;
 * It provides secure and easy ways to 
 * dynamically add, remove and modify entries 
 * from a buffer. 
 * \n \n \n
 *
 *
 * INTRODUCTION : \n \n
 * This manual presents the way the module has
 * to be used (which functions to call and in what order) briefly,
 * without however, going into too much details. The goal is to 
 * give a general insight as to which functions are needed to 
 * attain a result. \n \n \n
 *
 *
 * BEGINNING : \n \n
 * To make use of even the slightest of this module, the first 
 * requirement is to create a pointer to an EBUF type. \n \n
 *
 * here's an example : EBUF *abuffer; \n \n
 *
 * You cannot use a direct variable like : EBUF abuffer  --
 * for parity reasons(safegard of the data integrity),
 * the content of the structure is to remain hidden
 * from external programs. \n 
 *
 * The EBuf module is completely reentrant, meaning it does 
 * not contain global data in the module, making it
 * thread safe. \n \n \n
 *
 *
 * INITIALISING AND FREEING : \n \n
 * Now that the EBUF pointer is created, the next step is to 
 * initialize it by using Neuro_CreateEBuf(3). Among other, 
 * it allocated enough memory for its own internal data and 
 * then resets them to default initial values. \n
 *
 * When no longer needed, the pointer should be freed by using
 * Neuro_CleanEBuf(3). It frees the EBUF's internal buffer 
 * completely. In the case that the use of the EBUF pointer
 * is needed after it is freed, the call of Neuro_CreateEBuf(3)
 * is required again. In the case that just one element 
 * in the buffer needs to be freed, the use of
 * Neuro_SCleanEBuf(3) is required. \n 
 *
 * In addition to normal freeing, you can also set a callback
 * right after the initialization using Neuro_SetcallbEBuf(3). 
 * The callback will be called for every elements that the 
 * EBUF contains right before they are freed (by either calling 
 * Neuro_CleanEBuf(3) or Neuro_SCleanEBuf(3)), permitting 
 * the manual freeing of allocated pointers inside the structure. 
 * Among other, this permits EBUF pointers to contain other
 * EBUF pointers and/or manually allocated pointers and the
 * hability to free them cleanly.\n \n \n
 *
 * ALLOCATING NEW DATA : \n \n
 * 
 * by now, we have initalised and possibly set a callback to the
 * instance, but we are still missing an important step :
 * creating a structure template which the instance EBUF will 
 * contain. Theres only two ways to allocate : Neuro_AllocEBuf(3)
 * and Neuro_MultiAllocEBuf(3)
 * A basic structure template was used in the example in
 * the man page for the function Neuro_AllocEBuf(3) which is 
 * exactly the function we need to use to allocate a new element.
 *
 * (allocate a new element? didn't we just allocated one with
 * Neuro_CreateEBuf(3)?  -- Yes! but the use of EBUF is to easily
 * create dynamic arrays of structures. Hence the use of the term
 * element to call a single array that we will allocate.)
 *
 * READ AND WRITE : \n \n
 * reading from an ebuf element is quite easy, it works kind of
 * the same as when you call malloc(). After calling Neuro_AllocEBuf(3)
 * or Neuro_MultiAllocEBuf(3), you can use either Neuro_GiveEBuf(3) 
 * or Neuro_GiveCurEBuf(3). Those two functions return a void pointer
 * which you simply put into the correct variable. You can then read
 * or write from the structure directly.
 *
 **/

/* 
 * This macro is to set the number of extra instances 
 * of memory needs to be allocated per allocations.
 * In theory, this is absolutely not needed but since
 * the allocation proccess is slow, the less we do it,
 * the better. 10 is a good default.
 */
#define MEMORY_ALLOC_OVERH 10

#ifdef __cplusplus
extern "C" {
#endif

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
 * an EBUF pointer.
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
 * Neuro_SetcallbEBuf(myeng, callbackclean); \n \n
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
 * an EBUF pointer.
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
 * an EBUF pointer.
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

/** Neuro_SCleanEBuf
 * @sdescri
 * cleans/frees one element.
 *
 * @description
 * This function's purpose is to clean elements in a similar 
 * manner as Neuro_CleanEBuf(3) but it only 
 * cleans one element. Take note that the callback
 * which can be set using the function Neuro_SetcallbEBuf(3) is also called with this function.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @param[in]
 * the pointer to an element contained in the EBUF buffer which needs to be cleaned.
 * 
 * @examples
 *
 * typedef struct ST \n
 * { \n
 * 	char *someString; \n
 * }ST; \n \n \n
 *
 * static EBUF *myeng; \n \n 
 *
 * ... \n \n
 *
 * Neuro_CreateEBuf(&myeng); \n \n
 *
 * ... \n \n
 *
 * ST *buf; \n \n
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST)); \n \n
 *
 * buf = Neuro_GetCurEBuf(myeng); \n \n
 *
 * buf->someString = "hello"; \n \n
 *
 * -- lets say we no longer need the variable -- \n
 * -- we can clean it using this function -- \n
 * Neuro_SCleanEBuf(myeng, buf); \n \n
 *
 * ... \n \n
 *
 * Neuro_CleanEBuf(&myeng); \n \n
 *
 * @related
 * Neuro_CreateEBuf(3), Neuro_AllocEBuf(3), Neuro_SetcallbEBuf(3), Neuro_CleanEBuf(3), Neuro_GiveEBuf(3), Neuro_GiveCurEBuf(3)
 *
 */
extern void Neuro_SCleanEBuf(EBUF *eng, void *object);

/** Neuro_GiveEBufCount
 *
 * @sdescri
 * give the count of elements in the array 
 *
 * @description
 * This function returns the number of elements contained
 * in the core buffer. Each elements in the buffer are ordered
 * in the order they were "pushed" into the buffer thus this
 * value can be used to loop the buffer for all the elements
 * one after the other. NOTE This function returns 0 when theres only
 * one element and it can't return negative values so it cannot 
 * be used to know if the buffer is empty or not! Use the function
 * Neuro_EBufIsEmpty(3) to check if its empty or not.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @returnval
 * the amount of elements in the EBUF element.
 *
 * @examples
 *
 * -- in this example, I ommited the initial creation of the buffer, -- \n
 * -- the final cleaning of the buffer, -- \n
 * -- the cleaning callback function, -- \n
 * -- the allocation of the buffer and -- \n
 * -- the struct ST and its typedef. -- \n
 * -- SEE the man page for the function Neuro_AllocEBuf(3) for those -- \n
 * -- and SEE the man page for Neuro_SetcallbEBuf(3) for the callback -- \n \n
 *
 * ST *buf; \n
 * unsigned int total = 0; \n \n
 *
 * -- we check if the buffer is empty -- \n
 * if (Neuro_EBufIsEmpty(myeng)) \n
 * 	return; \n \n
 *
 * -- we get the total number of elements in myeng -- \n
 * -- and also increment it by one because we will -- \n
 * -- loop the elements from finish to start -- \n
 * total = Neuro_GiveEBufCount(myeng) + 1; \n \n
 *
 * while (total-- > 0) \n
 * { \n
 * 	buf = Neuro_GiveEBuf(myeng, total); \n \n
 *
 * 	... \n \n
 *
 * } \n
 *
 * @related
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

/** Neuro_GiveEBuf()
 *
 * @sdescri
 * give the element corresponding to the number [elem]
 *
 * @description
 * This function is the default way of getting the 
 * pointer of an allocated element. The pointer can 
 * be used to access the data and read/write to it.
 *
 * @param[in] 
 * an EBUF pointer.
 *
 * @param[in]
 * this argument corresponds to the element number
 * that you want to fetch from the buffer.
 *
 * @returnval
 * a void pointer that points to the data.
 *
 * @examples
 *
 *
 *
 *
 * @related
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

#ifdef __cplusplus
}
#endif

#endif /* not __EBUF_H */
