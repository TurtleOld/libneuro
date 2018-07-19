
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

/* ebuf.c
 *
 * interface to the EBUF header. 
 * EBUF is used in a same manner as we use linked lists;
 * It is an easy way to have a growing structure array.
 * The EBUF header elements are hidden to external programs.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>
#include <string.h> /* memset */

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/
#include <ebuf.h>

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("ebuf");

/* take note that the typedef is in the ebuf.h header!!
 *
 * this is the core EBUF header. It is  
 * 16 bytes in size and handles structures
 * so their use is memory leak less, safe and
 * easy. There is only one of this header
 * structure per EBUF iteration. The actual
 * data we hold is in the **buffer variable!
 */
struct EBUF
{
	/* this pointer will point to the structures
	 * the external programs give us. This is where the
	 * data is kept and grows.
	 */
	void **buffer;
	
	/* how many overhead memory inside the **buffer we have
	 * allocated in advance.
	 */
	u32 mem;

	/* the current total number of elements inside **buffer */
	u32 total;

	/* initial sizes as we now support only the first size used only */
	size_t initial_sptp;
	size_t initial_sobj;

	/* callback that is not necessarily needed. This callback 
	 * is used if the input structure itself has allocated elements
	 * that needs to be freed for every iteration.
	 */
	void (*callback)(void *src);
};

/* doesn't seem to be ISO C ANSI compliant. 
 * I'm leaving the code here for future 
 * reference. 
 */
/*
#define GIVEEBUF(__eng, __elem)	\
({ 						\
	register void ***__buf;			\
	void *__result;				\
	if (__elem > __eng->total)		\
		__result = NULL;		\
	__buf = (void***)&__eng->buffer;	\
	if ((*__buf)[__elem])			\
		__result = (*__buf)[__elem];	\
	else					\
		__result = NULL; 		\
 	__result; })
*/
	
/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/	

void
Neuro_CreateEBuf(EBUF **eng)
{
	*eng = Neuro_CreateEBuf2();
#if old
	/* we allocate only the header EBUF structure */
	*eng = (EBUF*)calloc(1, sizeof(EBUF));
	
	/* set ALL the elements in the header EBUF structure 
	 * to their default NULL or 0 values.
	 */
	(*eng)->mem = 0;
	(*eng)->total = 0;
	(*eng)->buffer = NULL;
	(*eng)->callback = NULL;
#endif /* old */
}

EBUF *
Neuro_CreateEBuf2()
{
	EBUF *output = NULL;

	output = (EBUF*)calloc(1, sizeof(EBUF));

	/* set ALL the elements in the header EBUF structure 
	 * to their default NULL or 0 values.
	 */
	output->mem = 0;
	output->total = 0;
	output->buffer = NULL;
	output->callback = NULL;

	return output;
}

void
Neuro_SetcallbEBuf(EBUF *eng, void (*callback)(void *src))
{
	eng->callback = callback;
}

/* sptp stands for : size of the pointer type of the structure 
 * the external program will pass to EBUF. example :
 * sizeof(struct foo *) or sizeof(foo *)
 *
 * sobj stands for : size of the actual structure the external
 * program will pass to EBUF. example : sizeof(struct foo)
 * or sizeof(foo)
 *
 * TODO This function should have had a return value to support
 * error events... How are we supposed to know if there is 
 * not enough memory available like this?
 */
void
Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj)
{
	void ***buf;
	u32 *total;
	u32 *mem;
	u32 i = 0;

	if (!eng)
		return;
	
	buf = &eng->buffer;
	total = &eng->total;
	mem = &eng->mem;
	
	/*
	Debug_Val(0, "debug : %d\n", sptp);
	Debug_Val(0, "before mem %d\n", mem);
	*/

	if (!buf)
	{
		ERROR("Tried to allocate in a NULL buffer");
		return;
	}
	
	/* we will allocate or reallocate the ** 
	 * which is what points to the * elements
	 */
	if (*buf == NULL)
	{
		/* we allocate extra amount of elements to speed up 
		 * allocation time. MEMORY_ALLOC_OVERH is set in the
		 * ebuf.h header.
		 */
		*buf = calloc(MEMORY_ALLOC_OVERH, sptp);
		*total = 0;

		/* we set the mem EBUF element to how many overhead 
		 * times we could add data without the need to reallocate.
		 */
		*mem = MEMORY_ALLOC_OVERH - 1;

		i = MEMORY_ALLOC_OVERH;

		eng->initial_sptp = sptp;
		eng->initial_sobj = sobj;

		while(i-- > 0)
		{
			(*buf)[i] = calloc(1, sobj);
		}
	}
	else if ((*mem * sptp) < sptp)
	{
		if (sptp != eng->initial_sptp || sobj != eng->initial_sobj)
		{
			ERROR("We no longer support multiple size of objects. Each object need to be exactly the size of the first one inserted into the buffer.");
			return;
		}

		if (*buf == NULL)
		{
			/* theres a big problem */
			ERROR("Memory buffer unknown error");
			return;
		}

		/* we again allocate more memory than needed */
		*buf = realloc(*buf, sptp * (*total + MEMORY_ALLOC_OVERH));

		/* keep track of how many times we don't need to reallocate */
		*mem = MEMORY_ALLOC_OVERH - 1;

		i = *total + MEMORY_ALLOC_OVERH;

		while(i-- > *total)
		{
			(*buf)[i] = calloc(1, sobj);
		}
	}
	else
	{
		if (sptp != eng->initial_sptp || sobj != eng->initial_sobj)
		{
			ERROR("We no longer support multiple size of objects. Each object need to be exactly the size of the first one inserted into the buffer.");
			return;
		}
		/* We now will use an emplacement that was allocated in advance.
		 * Since we will use an element, we decrement the overhead EBUF
		 * mem variable value.
		 */
		*mem -= 1;

		memset((*buf)[*total], 0, sobj);
	}
	
	/* Debug_Val(0, "EBUF -- after mem %d\n", *mem); */
	
	/* in addition to allocating the **buffer pointer, we also 
	 * need to allocate the actual structure it will point to.
	 *
	 * in technical terms, we now allocate the * .
	 */

	/* this is now being allocated with the overhead */
	/* (*buf)[*total] = calloc(1, sobj); */
	
	*total = *total + 1;
}

void
Neuro_MultiAllocEBuf(EBUF *eng, u32 amount, size_t sptp, size_t sobj)
{
	void ***buf;
	u32 *total;
	u32 *mem;
	int i = 0;
	
	buf = &eng->buffer;
	total = &eng->total;
	mem = &eng->mem;
	
	/*
	if (amount == 1)
	{
		Neuro_AllocEBuf(eng, sptp, sobj);
		return;
	}*/

	i = amount;

	while (i-- > 0)
	{
		Neuro_AllocEBuf(eng, sptp, sobj);
	}

#if temp
	/* we only do anything if the buffer doesn't contain anything 
	 * to avoid memory leaks.
	 */
	if (*buf == NULL)
	{
		*buf = calloc(amount, sptp);
		*total = amount - 1;
		*mem = 0;

	
		i = *total + 1;

		/* we need to allocate every * elements in the ** */
		while (i-- > 0)
		{
			(*buf)[i] = calloc(1, sobj);
		}
	}
#endif /* temp */
}

void
Neuro_SCleanEBuf(EBUF *eng, void *object)
{
	void *buf;
	u32 elem;
	u32 total;

	if (!eng || !object)
	{
		WARN("Element eng or object is NULL");
		return;
	}
	
	total = Neuro_GiveEBufCount(eng);
	
	/* normally, EBUF functions input integers which are 
	 * corresponding to the ** array. But in this function
	 * we input the actual pointer to the element. 
	 * 
	 * This puts the actual integer corresponding to the
	 * pointer object from the ** array and puts it in elem.
	 */
	if (Neuro_GiveEBufElem(eng, object, &elem))
	{
		WARN("Element wasn't found in the buffer");
		return;
	}
	
	/* we call the callback that will clean 
	 * allocated pointers in the structure if 
	 * theres one.
	 */
	if (eng->callback != NULL)
		eng->callback(object);
	else
		TRACE("There's no cleaning callback for this buffer (this could be normal)");
	
	/* now this is problematic, for performance reasons, it's better *not* to free the object. */
	/*free(object);*/


	/* now that the object is freed, we will attempt to fill its 
	 * emplacement with the last one.
	 */
	
	/* check to see if the one we want to remove is the last one */ 
	if (total != elem)
	{
		/* get the last one */
		buf = Neuro_GiveEBuf(eng, total);
	
		/* make the one we just deleted point to the last one */
		Neuro_SetEBuf(eng, Neuro_GiveEBufAddr(eng, elem), buf);
	}
	
	/* make the last element point to NULL so it can be reused*/
	/* Neuro_SetEBuf(eng, Neuro_GiveEBufAddr(eng, total), NULL); */

	/* make the last element point to our unused address */
	Neuro_SetEBuf(eng, Neuro_GiveEBufAddr(eng, total), object);

	eng->mem++; /* add an extra mem because we now have an extra slot free */
	
	eng->total--;
}

void
Neuro_CleanEBuf2(EBUF *eng)
{
	void *buf;
	u32 i;	
	
	if (!eng)
		return;
		
	buf = &eng->buffer;
	i = eng->total + eng->mem;
	
	/* loop all the elements in the EBUF **, call the
	 * callback(if theres one!) with the actual element 
	 * as the argument and finally free the element.
	 *
	 * in short : free every * in the ** array.
	 */
	while (i-- > 0)
	{
		
		/* buf = Neuro_GiveEBuf(eng, i); */
		buf = eng->buffer[i];
		
		if (buf)
		{
			if (eng->callback)
			{
				eng->callback(buf);
			}
			free(buf);
		}
		/* Debug_Val(0, "#%d -- cleaned\n", i); */
	}

	/* free the actual ** array */
	if (eng->buffer != NULL)
	{
		free(eng->buffer);
		eng->buffer = NULL;
	}
	
	/* Debug_Val(0, "cleaned %d elements\n", eng->total); */
	eng->total = 0;
	eng->mem = 0;

	free(eng);
}


void
Neuro_CleanEBuf(EBUF **engi)
{
	EBUF *eng;
	
	/*
	 * this is to avoid very big and puzzling call to the **engi element like
	 * (*engi)->mem  or engi[0]->mem. This is for cleaner code.
	 */
	eng = *engi;
	
	if (!eng)
		return;

	Neuro_CleanEBuf2(eng);

	*engi = NULL;
	
#if old
	void *buf;
	EBUF *eng;
	u32 i;
	
	/*
	 * this is to avoid very big and puzzling call to the **engi element like
	 * (*engi)->mem  or engi[0]->mem. This is for cleaner code.
	 */
	eng = *engi;
	
	if (!eng)
		return;
		
	buf = &eng->buffer;
	i = eng->total;
	
	/* loop all the elements in the EBUF **, call the
	 * callback(if theres one!) with the actual element 
	 * as the argument and finally free the element.
	 *
	 * in short : free every * in the ** array.
	 */
	while (i-- > 0)
	{
		
		buf = Neuro_GiveEBuf(eng, i);
		
		if (buf)
		{
			if (eng->callback)
			{
				eng->callback(buf);
			}
			free(buf);
		}
		/* Debug_Val(0, "#%d -- cleaned\n", i); */
	}

	/* free the actual ** array */
	if (eng->buffer != NULL)
	{
		free(eng->buffer);
		eng->buffer = NULL;
	}
	
	/* Debug_Val(0, "cleaned %d elements\n", eng->total); */
	eng->total = 0;
	eng->mem = 0;

	/* free the actual EBUF structure */
	if (*engi != NULL)
	{
		free(*engi);
		*engi = NULL;
	}
#endif /* old */


}



u32 
Neuro_GiveEBufCount(EBUF *eng)
{
	if (!eng)
	       return 0;

	return (eng->total > 0 ? eng->total -1 : 0);
}

void *
Neuro_GiveEBuf(EBUF *eng, u32 elem)
{	
	if (!eng)
		return NULL;

	if (Neuro_EBufIsEmpty(eng))
		return NULL;

	if (elem > eng->total)
		return NULL;

	
	if (eng->buffer[elem])
		return eng->buffer[elem];
	else
		return NULL;
}

void *
Neuro_GiveCurEBuf(EBUF *eng)
{	
	if (!eng)
		return NULL;

	if (Neuro_EBufIsEmpty(eng))
		return NULL;

	
	if (eng->buffer[eng->total - 1])
		return (eng->buffer[eng->total - 1]);
	else
		return NULL;
}

void **
Neuro_GiveEBufCore(EBUF *eng)
{
	if (!eng)
		return NULL;
	
	return eng->buffer;
}

/* returns 1 on error and 0 if all is ok 
 *
 * for a certain pointer, we return its corresponding array
 * number.
 */
int
Neuro_GiveEBufElem(EBUF *eng, void *object, u32 *elem)
{
	void *buf;
	u32 i;

	if (!eng || !object)
	{
		*elem = 0;
		return 1;
	}
		
	buf = &eng->buffer;
	i = eng->total;
	
	while (i-- > 0)
	{
		buf = Neuro_GiveEBuf(eng, i);
		if (buf == object)
		{
			*elem = i;
			return 0;
		}
	}

	
	return 1;
}

/* give the address of an EBUF * element */
void **
Neuro_GiveEBufAddr(EBUF *eng, u32 elem)
{
	void ***buf;
	
	if (!eng)
		return NULL;
	
	
	buf = (void***)&eng->buffer;
	
	if ((*buf)[elem])
		return &(*buf)[elem];
	else
		return NULL;
}

/* to move EBUF elements from a position to another 
 * watch out for memory leaks!
 */
void
Neuro_SetEBuf(EBUF *eng, void **to, void *from)
{
	if (!eng || !to || !from)
		return;

	*to = from;
	
	return;
}

/* copy an EBUF header to another */
void
Neuro_CopyEBuf(EBUF *to, EBUF *from)
{
	if (!to || !from)
		return;
	
	to->buffer = from->buffer;
	to->mem = from->mem;
	to->total = from->total;
}

/* reset the EBUF element 
 *
 * NOTE : should only be called if the EBUF address
 * was passed to another EBUF or it was copied to another
 * using the Neuro_CopyEBuf function!!!
 * Else, there will be a big memory leak.
 */
void
Neuro_ResetEBuf(EBUF *eng)
{
	if (!eng)
		return;
	
	eng->buffer = NULL;
	eng->total = 0;
	eng->mem = 0;
}

/* returns 1 if the EBUF is empty, 0 if its NOT empty and
 * 2 if the EBUF passed does not exist. */
u8
Neuro_EBufIsEmpty(EBUF *eng)
{
	if (!eng)
		return 2;

	return (eng->total == 0 ? 1 : 0);
}


