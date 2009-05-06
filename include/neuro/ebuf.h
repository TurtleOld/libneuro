
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
 * <h2>IN SHORT :</h2>
 *
 * <p>
 * <i>EBuf</i> stands for Engine Buffer. 
 * Its primary use is to make structures <i>allocation</i> 
 * and <i>reallocation</i> quick and easy; It provides 
 * <i>secure</i> and easy ways to dynamically <i>add</i>, 
 * <i>remove</i> and <i>modify</i> entries from a buffer. 
 * </p>
 *   
 * <h2>INTRODUCTION :</h2>
 * <p>
 * This <i>manual</i> presents the way the module has
 * to be used (which <i>functions</i> to call and in what order) 
 * <i>briefly</i>,
 * without going into too much details. The goal of this <i>manual</i>
 * is to give a general insight as to which functions are 
 * needed to attain a result.
 * </p>
 *
 * <h2>BEGINNING :</h2>
 * <p>
 * To make use of even the slightest of this module, the first 
 * requirement is to create a <i>pointer</i> to an EBUF(3) type.  
 * </p>
 *
 * <p><i>here's an example</i> : EBUF *abuffer;</p>
 *
 * <p>
 * You cannot use a direct variable like : EBUF abuffer  --
 * for parity reasons(safegard of the data integrity),
 * the content of the structure is to remain hidden
 * from external programs.  
 * </p>
 *
 * <p>
 * The EBuf module is completely reentrant, meaning it does 
 * not contain global data in the module, making it
 * thread safe.   
 * </p>
 *
 * <h2>INITIALISING AND FREEING :</h2>
 *
 * <p>
 * Now that the EBUF(3) pointer is created, the next step is to 
 * initialize it by using Neuro_CreateEBuf(3). Among other, 
 * it allocates enough memory for its own internal data and 
 * then resets them to default initial values. 
 * </p>
 *
 * <p>
 * When no longer needed, the pointer should be freed by using
 * Neuro_CleanEBuf(3). It frees the EBUF(3)'s internal buffer 
 * completely. In the case that the use of the EBUF(3) pointer
 * is needed after it is freed, the call of Neuro_CreateEBuf(3)
 * is required again. In the case that just one element 
 * in the buffer needs to be freed, the use of
 * Neuro_SCleanEBuf(3) is required.  
 * </p>
 *
 * <p>
 * In addition to normal freeing, you can also set a callback
 * right after the initialization using Neuro_SetcallbEBuf(3). 
 * The callback will be called for every elements that the 
 * EBUF(3) contains right before they are freed (by either calling 
 * Neuro_CleanEBuf(3) or Neuro_SCleanEBuf(3)), permitting 
 * the manual freeing of allocated pointers inside the structure. 
 * Among other, this permits EBUF(3) pointers to contain other
 * EBUF(3) pointers and/or manually allocated pointers and the
 * hability to free them cleanly.  
 * </p>
 *
 * <p>
 * <b>NOTE :</b> Neuro_SCleanEBuf(3) <i>doesn't</i> keep the data order in the
 * buffer at all. It actually is made to break the data order in
 * order to be more efficient.  
 * </p>
 *
 * <p>
 * <i>here's an example</i>, 3 elements were allocated :
 * elements 1, 2 and 3. The element number 4 is added to this
 * buffer to form (1, 2, 3 and 4). The element number 2 is then
 * removed from the buffer, which would <i>normally</i> do
 * (1, 3 and 4) <i>but no</i>, this actually does (1, 4 and 3).  
 * </p>
 *
 * <p>
 * The last element is <i>always</i> moved to the position of the
 * deleted element which actually breaks the order of the data
 * in the buffer completely (but is necessary for performance reasons).  
 * </p>
 *
 * <p>
 * <i>In order to fix this sorting quirk</i>, all the steps in order to do so
 * are stated in the <i>MOVING DATA</i> section of this very manual.
 * </p>
 *   
 * <h2>ALLOCATING NEW DATA :</h2>
 *
 * <p>
 * by now, we have initalised and possibly set a callback to the
 * instance, but we are still missing an important step :
 * creating a structure template which the instance EBUF(3) will 
 * contain. Theres only two ways to allocate : Neuro_AllocEBuf(3)
 * and Neuro_MultiAllocEBuf(3)
 * A basic structure template was used in the example in
 * the man page for the function Neuro_AllocEBuf(3) which is 
 * exactly the function we need to use to allocate a new element. 
 * </p>
 *   
 *
 * <h2>READ AND WRITE :</h2>
 *
 * <p>
 * reading/writing from an ebuf element is quite easy, it works kind of
 * the same as when you call malloc(). After calling Neuro_AllocEBuf(3)
 * or Neuro_MultiAllocEBuf(3), you can use either Neuro_GiveEBuf(3) 
 * or Neuro_GiveCurEBuf(3). Those two functions return a void pointer
 * which you simply put into the correct variable. You can then read
 * or write from the structure directly. If you want to work on all the
 * data of the buffer, you need to fetch how much elements are contained.
 * For the purpose the function Neuro_GiveEBufCount(3) has to be used.
 * Then, when looping, a simple call to Neuro_GiveEBuf(3) has to be used
 * to get each data structs.  
 * </p>
 * 
 * <h2>MOVING DATA :</h2>
 *<p>
 * on certain occasions, you might want to change the order by which
 * the data in the EBUF(3) buffer is organised. This is needed when you
 * want to sort the elements. For this effect, we actually need to use
 * the function Neuro_SetEBuf(3) to copy a certain element to a 
 * precise address on the buffer. This can't be used directly, we 
 * actually need to get the address of the element we want to change.
 * To get the address of a certain element, we need to use the function
 * Neuro_GiveEBufAddr(3). 
 * Here's how we do it :  
 * </p>
 *
 * @examples
 *
 * -- in this code example, we will transfer the last element into -- 
 * -- a certain position in the buffer and transfer that certain -- 
 * -- position into the last place. --  
 *
 * -- ST is a structure we use throughout the EBUF(3) man pages -- 
 * -- if you have no idea whats it for, check a few man paged -- 
 * -- on EBUF(3), especially the Neuro_SetcallbEBuf(3) one. -- 
 * -- we assume this pointer already points to an EBUF(3) element -- 
 * ST *element; 
 * u32 elem_value;  
 *
 * ...  
 * 
 * -- we need the array number of the element to call the next -- 
 * -- functions -- 
 * if (Neuro_GiveEBufElem(myeng, element, &elem_value)) 
 * 	return; -- an error occured so we bail out -- 
 *
 * -- we then copy the last element into that certain's element address -- 
 * Neuro_SetEBuf(myeng, Neuro_GiveEBufAddr(myeng, elem_value), Neuro_GiveCurEBuf(myeng));  
 *
 * -- now that we copied the address of the last element into -- 
 * -- our certain element. we need to copy our certain element into -- 
 * -- the last position. --  
 *
 * Neuro_SetEBuf(myeng, Neuro_GiveEBufAddr(myeng, Neuro_GiveEBufCount(myeng)), element);  
 *
 * And thats it folks! Now, next time we loop the buffer, the order will have
 * a certain element at the end and the last element in what ever position our
 * certain element was.
 *   
 *
 * <b>PLEASE NOTE :</b> you <b>can't</b> use the returned value of the function 
 * Neuro_GiveEBufCount(3) to figure if the EBUF(3) is <b>empty</b> (ie when it 
 * doesn't contain any elements or when still unallocated).
 * <b>for this</b>, you have to use the function Neuro_EBufIsEmpty(3) which returns
 * 1 if the buffer can <b>not</b> be used and 0 if it <b>can</b>. Any use of an EBUF(3)
 * which returns 1 when you call Neuro_EBufIsEmpty(3) can create <b>very</b>
 * unwanted results and <b>very</b> likely cause a segmentation fault.
 *
 * @related
 * EBUF(3), Neuro_CreateEBuf(3), Neuro_SetcallbEBuf(3), 
 * Neuro_AllocEBuf(3), Neuro_MultiAllocEBuf(3),
 * Neuro_CleanEBuf(3), Neuro_SCleanEBuf(3),
 * Neuro_GiveEBufCount(3), Neuro_GiveEBufElem(3),
 * Neuro_GiveCurEBuf(3), Neuro_GiveEBufAddr(3),
 * Neuro_GiveEBuf(3), Neuro_GiveEBufCore(3),
 * Neuro_SetEBuf(3), Neuro_CopyEBuf(3),
 * Neuro_ResetEBuf(3), Neuro_EBufIsEmpty(3)
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

/**
 * @description
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
 * static EBUF *myeng;  
 * 
 * ...  
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
 * typedef struct ST 
 * { 
 * 	char *someString; 
 * }ST;   
 *
 *
 * static EBUF *myeng;  
 *
 * static void 
 * callbackclean(void *src) 
 * { 
 * 	ST *temp;  
 *
 * 	temp = (ST*)src;  
 *
 * 	if (temp) 
 * 	{ 
 * 		if (temp->someString) 
 * 			free(temp->someString); 
 * 	} 
 * }  
 *
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);  
 * Neuro_SetcallbEBuf(myeng, callbackclean);  
 *
 * ...  
 *
 * ST *foo;  
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST));  
 *
 * foo = Neuro_GiveCurEBuf(myeng);  
 *
 * foo->someString = (char*)malloc(50);  
 *
 * ...  
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
 * typedef struct ST 
 * { 
 * 	int foobar; 
 * }ST;   
 *
 *
 * static EBUF *myeng;  
 *
 *
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);  
 *
 * ...  
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST));  
 *
 * ...  
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
 * static EBUF *myeng;  
 * 
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);
 *
 * ...  
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
 * typedef struct ST 
 * { 
 * 	char *someString; 
 * }ST;   
 *
 * static EBUF *myeng;   
 *
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);  
 *
 * ...  
 *
 * ST *buf;  
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST));  
 *
 * buf = Neuro_GetCurEBuf(myeng);  
 *
 * buf->someString = "hello";  
 *
 * -- lets say we no longer need the variable -- 
 * -- we can clean it using this function -- 
 * Neuro_SCleanEBuf(myeng, buf);  
 *
 * ...  
 *
 * Neuro_CleanEBuf(&myeng);  
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
 * -- in this example, I ommited the initial creation of the buffer, -- 
 * -- the final cleaning of the buffer, -- 
 * -- the cleaning callback function, -- 
 * -- the allocation of the buffer and -- 
 * -- the struct ST and its typedef. -- 
 * -- SEE the man page for the function Neuro_AllocEBuf(3) for those -- 
 * -- and SEE the man page for Neuro_SetcallbEBuf(3) for the callback --  
 *
 * ST *buf; 
 * unsigned int total = 0;  
 *
 * -- we check if the buffer is empty -- 
 * if (Neuro_EBufIsEmpty(myeng)) 
 * 	return;  
 *
 * -- we get the total number of elements in myeng -- 
 * -- and also increment it by one because we will -- 
 * -- loop the elements from finish to start -- 
 * total = Neuro_GiveEBufCount(myeng) + 1;  
 *
 * while (total-- > 0) 
 * { 
 * 	buf = Neuro_GiveEBuf(myeng, total);  
 *
 * 	...  
 *
 * } 
 *
 * @related
 * Neuro_AllocEBuf(3), Neuro_GiveEBuf(3),
 * Neuro_GiveCurEBuf(3), Neuro_EBufIsEmpty(3)
 */
extern u32 Neuro_GiveEBufCount(EBUF *eng);

/** Neuro_GiveEBufElem
 * @sdecri
 * output the array number of the element object 
 *
 * @description
 * This function is used to get the array number
 * of a structure pointer which is inside
 * the buffer eng. If the element is not 
 * contained inside the buffer, this will
 * return 1 and 0 on success. Make sure to
 * put either a real integer or an allocated
 * one's address for the *elem argument.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @param[in]
 * a void pointer of an element 
 * of which the array number will be put
 * in the output integer.
 *
 * @param[out]
 * the array number of the element in the
 * void pointer.
 *
 * @returnval
 * either 0 or 1. 1 on error (when the pointer object is not
 * found in the buffer eng or when the buffer eng is NULL).
 * 0 on no error.
 *
 * @examples
 * 
 * typedef struct ST 
 * { 
 * 	char *someString; 
 * }ST;   
 *
 * -- we create a struct element -- 
 * -- called an_element into which -- 
 * -- we will put the address of an ebuf allocated element -- 
 * -- this method can be used to track certain key elements inside the buffer. -- 
 *
 * static ST *an_element;  
 *
 * static EBUF *myeng;  
 *
 * static void 
 * callbackclean(void *src) 
 * { 
 * 	ST *temp;  
 *
 * 	temp = (ST*)src;  
 *
 * 	if (temp) 
 * 	{ 
 * 		if (temp->someString) 
 * 			free(temp->someString); 
 * 	} 
 * }  
 *
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);  
 * Neuro_SetcallbEBuf(myeng, callbackclean);  
 *
 * ...  
 *
 * ST *foo;  
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST));  
 *
 * foo = Neuro_GiveCurEBuf(myeng);  
 *
 *
 * foo->someString = (char*)malloc(50);  
 *
 * -- we set our variable to the address that the pointer foo points to -- 
 * -- this makes it behave the exact same as if it was actually -- 
 * -- the pointer foo... we can then access the data from it -- 
 *  -- for any purpose and fast. -- 
 * an_element = foo;  
 * 
 * ...  
 *
 * -- in a certain function, we use the content of the pointer an_element -- 
 * -- but for certain cases we also need to get its array number. -- 
 * -- This only works if this particular pointer was allocated in an EBUF. --  
 *
 * u32 elem_value = 0;  
 *
 * if (Neuro_GiveEBufElem(myeng, an_element, &elem_value)) 
 * { 
 * 	printf("an error happened, the element (an_element) wasn't found in the buffer (myeng)\n"); 
 * 	return; 
 * }  
 *
 * -- now, if there wasn't any errors, the variable an_element -- 
 * -- contains the array number for the pointer an_element. -- 
 * -- this number can be used with the function Neuro_GiveEBuf(3) or Neuro_GiveEBufAddr(3).  
 *
 * ...  
 *
 * Neuro_CleanEBuf(&myeng);
 *
 * 
 *
 * @related
 * Neuro_GiveEBufAddr(3), Neuro_GiveEBuf(3)
 */
extern int Neuro_GiveEBufElem(EBUF *eng, void *object, u32 *elem);

/** Neuro_GiveCurEBuf
 * @sdescri
 * outputs the last element of the buffer.
 *
 * @description
 * before this function, we had to get the number of
 * elements from the buffer using Neuro_GiveEBufCount(3)
 * and then call Neuro_GiveEBuf(3) to get the element.
 * Now, this function can be used to have the exact same
 * result but in just one call.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @returnval
 * the pointer of the element. It returns NULL on error.
 *
 * @examples
 * -- see the man page for Neuro_GiveEBuf(3) for how this -- 
 * -- function can be used. Also, almost all the functions -- 
 * -- use this function in their examples so just check how --
 * -- they use it... usually, this function is called right -- 
 * -- after you allocate the buffer using Neuro_AllocEBuf(3), -- 
 * -- because we need to get the pointer so we can populate it. -- 
 *
 * @related
 * Neuro_AllocEBuf(3), Neuro_MultiAllocEBuf(3),
 * Neuro_GiveEBuf(3)
 */
extern void *Neuro_GiveCurEBuf(EBUF *eng);

/** Neuro_GiveEBufAddr
 * 
 * @sdescri
 * gives the real address of the element of the array number elem.
 *
 * @description
 * this function is essential in the event that you want
 * to change the order into which the EBUF buffer is.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @param[in]
 * the array number of the element you want the pointer address.
 *
 * @returnval
 * the pointer to a pointer which contains the address of the
 * element's pointer.
 *
 * @examples
 * see the Neuro_EBuf(3) man page on section MOVING DATA for
 * an example on how to use this function. 
 * 
 * @related
 * Neuro_GiveEBufElem(3), Neuro_SetEBuf(3),
 */
extern void **Neuro_GiveEBufAddr(EBUF *eng, u32 elem);

/** Neuro_GiveEBuf()
 *
 * @sdescri
 * gives the element corresponding to the number input in
 * elem.
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
 * the pointer to the element from the EBUF. As a void pointer
 * so it is possible to "morph" it into any other pointer type.
 *
 * @examples
 * 
 * typedef struct ST 
 * { 
 * 	char *someString; 
 * }ST;   
 *
 *
 * static EBUF *myeng;  
 *
 * static void 
 * callbackclean(void *src) 
 * { 
 * 	ST *temp;  
 *
 * 	temp = (ST*)src;  
 *
 * 	if (temp) 
 * 	{ 
 * 		if (temp->someString) 
 * 			free(temp->someString); 
 * 	} 
 * }  
 *
 * ...  
 *
 * Neuro_CreateEBuf(&myeng);  
 * Neuro_SetcallbEBuf(myeng, callbackclean);  
 *
 * ...  
 *
 * ST *foo;  
 *
 * Neuro_AllocEBuf(myeng, sizeof(ST*), sizeof(ST));  
 *
 * foo = Neuro_GiveCurEBuf(myeng);  
 *
 * foo->someString = (char*)malloc(50);  
 *
 * ...  
 *
 * -- this code outputs to the default output channel -- 
 * -- the content of every elements of the buffer -- 
 * u32 total = 0; 
 * ST *temp = NULL;  
 *
 * if (Neuro_EBufIsEmpty(myeng)) 
 * 	return;  
 *
 * total = Neuro_GiveEBufCount(myeng) + 1;  
 *
 * while (total-- > 0) 
 * { 
 * 	temp = Neuro_GiveEBuf(myeng, total);  
 * 
 * 	printf("some value : %s\n", temp->someString); 
 * }  
 *
 * ...  
 *
 * Neuro_CleanEBuf(&myeng); 
 *
 * @related
 * Neuro_AllocEBuf(3), Neuro_MultiAllocEBuf(3), 
 * Neuro_GiveEBufCount(3)
 */
extern void *Neuro_GiveEBuf(EBUF *eng, u32 elem);

/** Neuro_GiveEBufCore
 * @sdescri
 * give the core buffer of the EBuf element 
 *
 * @description
 * this function should never be used, it returns
 * the actual pointer of the CORE of an EBUF element.
 * The need of this function is very close to Nill
 * and it is strongly advised to avoid the use of this
 * function at all cost.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @returnval
 * the CORE of the EBUF's main (and only) buffer.
 *
 * @example
 * this function was initially made for bitmap loading
 * in neuro extlib, it permitted to input simple string
 * elements and then the ability to give this core 
 * to the pixmap library so it would load the bitmap
 * from it. This use was considered a BIG HACK. The neuro
 * bitmap loading code stopped using this function quite
 * a long time ago so it is unused anywhere.
 *
 * Neuro_SCleanEBuf(3) Neuro_GiveEBufAddr(3)
 * Neuro_GiveEBufAddr(3), Neuro_SetEBuf(3),
 * Neuro_CopyEBuf(3), Neuro_ResetEBuf(3)
 */
extern void **Neuro_GiveEBufCore(EBUF *eng);


/** Neuro_SetEBuf
 *
 * @sdescri
 * copy the actual content to another position in the
 * buffer.
 *
 * @description
 * this function is a key element for the task of sorting
 * elements among the other elements in the buffer.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @param[in]
 * the actual address of the pointer of an element from the EBUF.
 * This informations can ONLY be given by the function
 * Neuro_GiveEBufAddr(3).
 *
 * @param[in]
 * the pointer of the element to copy to the address to.
 *
 * @examples
 * see the Neuro_EBuf(3) man page on section MOVING DATA for
 * an example on how to use this function. *
 *
 *
 * Neuro_GiveEBufAddr(3), Neuro_GiveEBufElem(3)
 */
extern void Neuro_SetEBuf(EBUF *eng, void **to, void *from);

/** Neuro_CopyEBuf
 * @sdecri
 * copy the content of an EBUF variable to another EBUF variable.
 *
 * @description
 * nothing unusual with this function, it simply cleanly copies the
 * content of an already created EBUF element to another one
 * that wasn't created yet. If copy an EBUF to another that
 * already contains data, you'll create a big memory leak.
 * Note : this function is very fast because it is only copies addresses.
 * Note2 : Also note that only one of the two needs to be cleaned;
 * they both contain the same addresses.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @param[in]
 * an EBUF pointer which gets copied into the EBUF to.
 *
 * @examples
 * static EBUF *myeng;  
 *
 * ...  
 * 
 * Neuro_CreateEBuf(&myeng);  
 *
 * ...  
 *
 * EBUF *clone;
 *
 * Neuro_CopyEBuf(clone, myeng);
 *
 * ...  
 *
 * Neuro_CleanEBuf(&myeng);
 *
 *
 * @related
 * Neuro_ResetEBuf(3)
 */
extern void Neuro_CopyEBuf(EBUF *to, EBUF *from);

/** Neuro_ResetEBuf
 * @sdescri
 * resets the EBUF variable WITHOUT FREEING IT -- Warning this 
 * is a mem leak if you didn't copy the content to another one 
 *
 * @description
 * use the function Neuro_CopyEBuf(3) to backup the addresses for 
 * the cleaning process prior to using this function please.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @related
 * Neuro_CopyEBuf(3)
 */
extern void Neuro_ResetEBuf(EBUF *eng);

/** Neuro_EBufIsEmpty
 * @sdecri
 * this is a simple boolean returning function that returns
 * 1 if [eng] is empty and 0 if it holds stuff. 
 *
 * @description
 * this function is the only way to know if an EBUF is empty
 * or not, you CAN'T use the function Neuro_GiveEBufCount(3)
 * to figure that.
 *
 * @param[in]
 * an EBUF pointer.
 *
 * @returnval
 * 1 if the EBUF is empty and should not be used and 0 if it
 * was created and populated.
 *
 * @related
 * Neuro_GiveEBufCount(3)
 */
extern u8 Neuro_EBufIsEmpty(EBUF *eng);

#ifdef __cplusplus
}
#endif

#endif /* not __EBUF_H */
