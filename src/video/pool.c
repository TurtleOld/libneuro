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

/* pool.c
 * Module : Pool
 *
 * memory management system, not really performant though, its actually better
 * to free and reallocate then trying not to free...
 */

/*-------------------- Extern Headers Including --------------------*/


/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/
#include "pool.h"


/*--------------------      Other       ----------------------------*/

enum POOL_TYPES
{
	POOL_AVAILABLE = 0, /* an available spot */
	POOL_RAWENGINE = 1, /* RAW_ENGINE */
	POOL_QUEUE, /* INSTRUCTION_ENGINE */
	/*POOL_PIXELS,*/ /* PIXEL_ENGINE */
	
	POOL_LAST
};

/* this is the memory pool structure 
 * made to keep track of unused memory
 * so it can be used again if necessary.
 *
 * ** note **
 * this test (although working well technically) was
 * an awful performance failure so it was dropped.
 * 
 * the code was kept for future reference,
 * see the macro use_memory_pool to toggle the use
 * of this feature.
 */
typedef struct STRUCT_POOL
{
	u8 type;
	void *data;
}STRUCT_POOL;

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/
/* pool used to reuse allocated memory */
static EBUF *_pool; 


/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

/* function to put a new element in the pool */
void
Pool_PushData(u8 type, void *data)
{
	STRUCT_POOL *tmp;
	u32 total = 0;

	/* Debug_Val(0, "Pushed a struct to be in the pool\n"); */
	if (!Neuro_EBufIsEmpty(_pool))
	{
		total = Neuro_GiveEBufCount(_pool) + 1;

		/* loop the buffer to attempt to put the data 
		 * into an available spot
		 */
		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(_pool, total);
	
			if (tmp->type == POOL_AVAILABLE)
			{
				/* Debug_Val(0, "putting data into an available slot\n"); */
				tmp->type = type;
				tmp->data = data;
	
				return;
			}
		}
	}

	/* Debug_Val(0, "no more available slots, we need to allocate a new one\n"); */

	/* if we are here, it means there was no available
	 * spot found. We will have to create a new one.
	 * and put the data in it.
	 */
	Neuro_AllocEBuf(_pool, sizeof(STRUCT_POOL*), sizeof(STRUCT_POOL));

	tmp = Neuro_GiveCurEBuf(_pool);

	tmp->type = type;
	tmp->data = data;
}


/* returns a pointer corresponding to the type or
 * NULL if none found 
 */
void *
Pool_PullData(u8 type)
{
	STRUCT_POOL *tmp;
	u32 total = 0;
	void *ret = NULL;

	if (Neuro_EBufIsEmpty(_pool))
		return NULL;
	
	total = Neuro_GiveEBufCount(_pool) + 1;

	while (total-- > 0)
	{
		tmp = Neuro_GiveEBuf(_pool, total);

		if (tmp->type == type)
		{
			tmp->type = POOL_AVAILABLE;
			ret = tmp->data;
			tmp->data = NULL;
			
			return ret;
		}
	}

	return NULL;
}
/*-------------------- Poll ----------------------------------------*/

void
Pool_Poll()
{
	
}

/*-------------------- Constructor Destructor ----------------------*/

int
Pool_Init()
{
	Neuro_CreateEBuf(&_pool);
	return 0;
}

void
Pool_Clean()
{
	Neuro_CleanEBuf(&_pool);
}
