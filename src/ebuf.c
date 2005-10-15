/* ebuf.c
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>
#include <stdio.h>

/*-------------------- Local Headers Including ---------------------*/

/*-------------------- Main Module Header --------------------------*/
#include "ebuf.h"

/*--------------------      Other       ----------------------------*/
struct EBUF
{
	void **buffer;
	u32 mem;
	u32 total;
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
EBUF *
Neuro_CreateEBuf()
{
	EBUF *temp;

	temp = (EBUF*)calloc(1, sizeof(EBUF));

	temp->mem = 0;
	temp->total = 0;
	temp->buffer = NULL;

	return temp;
}

void
Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj)
{
	void ***buf;
	u32 *total;
	u32 *mem;
	
	buf = &eng->buffer;
	total = &eng->total;
	mem = &eng->mem;
	
	if (*mem > MEMORY_ALLOC_OVERH)
	{
		printf("Theres a huge problem, the memory over head allocation doesnt seem to work properly -- debug value : %d\n", *mem);
		return;
	}
	/*
	printf("debug : %d\n", sptp);
	printf("before mem %d\n", mem);
	*/
	if (*buf == NULL)
	{
		*buf = calloc(MEMORY_ALLOC_OVERH, sptp);
		*total = 0;
		*mem = MEMORY_ALLOC_OVERH;
	}
	else if ((*mem * sptp) <= sptp)
	{
		*buf = realloc(*buf, sptp * (MEMORY_ALLOC_OVERH * (*total + 1)));
		*mem = MEMORY_ALLOC_OVERH;
	}
	else
		*mem -= 1;
	/*
	printf("after mem %d\n", mem);
	*/
	(*buf)[*total] = calloc(1, sobj);
	
	*total = *total + 1;
}

void
Neuro_CleanEBuf(EBUF **engi)
{
	void *buf;
	EBUF *eng;
	u32 i;
	
	eng = *engi;
	if (!eng)
		return;
		
	buf = &eng->buffer;
	i = eng->total;

	while (i-- > 0)
	{
		buf = Neuro_GiveEBuf(eng, i);
		if (buf)
			free(buf);
		/* printf("#%d -- cleaned\n", i); */
	}

	if (eng->buffer != NULL)
	{
		free(eng->buffer);
		eng->buffer = NULL;
	}
	
	/* printf("cleaned %d elements\n", eng->total); */
	eng->total = 0;
	eng->mem = 0;

	if (*engi != NULL)
	{
		free(*engi);
		*engi = NULL;
	}
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
	register void ***buf;
	
	if (!eng)
		return NULL;

	if (elem > eng->total)
		return NULL;
	
	buf = (void***)&eng->buffer;
	
	if ((*buf)[elem])
		return (*buf)[elem];
	else
		return NULL;
}

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

void
Neuro_SetEBuf(EBUF *eng, void **to, void *from)
{
	if (!eng || !to || !from)
		return;

	*to = from;
	
	return;
}

void
Neuro_CopyEBuf(EBUF *to, EBUF *from)
{
	if (!to || !from)
		return;
	
	to->buffer = from->buffer;
	to->mem = from->mem;
	to->total = from->total;
}

void
Neuro_ResetEBuf(EBUF *eng)
{
	if (!eng)
		return;
	
	eng->buffer = NULL;
	eng->total = 0;
	eng->mem = 0;
}

u8
Neuro_EBufIsEmpty(EBUF *eng)
{
	if (!eng)
		return 2;

	return (eng->total == 0 ? 1 : 0);
}


