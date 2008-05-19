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



/* painter.c
 * The core painter's algorithm module.
 */

/* 
 * this code is my own version of the painter's algorithm. 
 * It is thus used to draw images in an order you have to
 * input when "pushing" an image, called layers. 
 * This module also requires 3 other variables which contain
 * the size of the image, a pointer to the image and the 
 * destination of the image.
 *
 * To make the painter's algorithm, I used 2 types of data. 
 * I use raw data which contains exactly what the external 
 * process gives us and then theres the instruction data which
 * contains a pointer to a single raw data and a pointer
 * to the next instruction data.
 *
 * The instruction datas order are changed every time a new
 * image is pushed. It is organized in a growing order, 
 * the smallest layers are the first ones and the biggest
 * are the last instructions drawn.
 *
 * Now, you see, the major advantage of this is we can also
 * redraw images that were below another one which would for
 * example be deleted. But for this, we have do an expansive 
 * bounds search for images which have to be redrawn.
 *
 * Also note that raw datas have different types that trigger
 * behaviors. Static and dynamic are the two most important
 * types, then theres the temporary type and the rest
 * are pretty much sub types for those that I mentioned earlier.
 */


/*-------------------- Extern Headers Including --------------------*/
#include <string.h> /* memcpy */

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <extlib.h> /* we only use Lib_GetScreenSize */
#include <ebuf.h> /* we use all the "normal" allocation function from this */
#include <other.h> /* we call all the bounding check functions from it
			and also the bound fix ones */

/*-------------------- Main Module Header --------------------------*/
#include "video.h"
#include <graphics.h>

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("video");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

static INSTRUCTION_ENGINE *first_element;
static INSTRUCTION_ENGINE *last_element;

static EBUF *Raw;
static EBUF *Queue;

/* buffered screen size for fast consultation */
static Rectan screenSize;

/* a rectangle meant to test the bound fix algorithm */
/* static Rectan test_BoundFix; */

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/* A function that handles "fixes" needed on images
 * to make them constrainted in the main screen.
 * returns 0 if all is ok and 1 if the image needs
 * to be dropped.
 */
static u8
BoundFixChecker(Rectan *indep, Rectan *isrc, Rectan *idst)
{
	Rectan bufa;

	bufa.x = idst->x;
	bufa.y = idst->y;
	bufa.w = isrc->w - 1;
	bufa.h = isrc->h - 1;
	
	switch (Neuro_BoundsCheck(indep, &bufa))
	{
		case 0: /* the v_object is inside the screen, all is ok */
		break;

		case 1: /* the v_object is outside the screen, we need to drop this instruction */
		{
			/* Debug_Print("a drawing instruction was dropped because its destination is outbound"); */
			return 1;
		}
		break;

		/* here is the fun, the v_object is overlapping the screen 
		 * and we got to only draw the part that is still inside 
		 * the screen 
		 */
		case 2: 
		{
			/* to do this, I'm thinking we could use 2 similar functions 
			 * the 1st one would check and "correct" the parts of the
			 * v_object that is not inside the screen vertically and
			 * the 2nd one would do the same but horizontally.
			 * The only thing to do is to change the isrc(Rectan)'s 
			 * values.
			 */
			Neuro_VerticalBoundCrop(indep, isrc, idst);
			Neuro_HorizontalBoundCrop(indep, isrc, idst);
			/* Info_Print("Fixed the coordinates/size of the drawing instruction."); */
		}
		break;

		default:
		{
			/* Debug_Print("a drawing instruction was dropped because its destination is outbound"); */
			return 1;
		}
		break;
	}
	
	return 0;
}

/* compute the instruction_engine buffer every 
 * time a new raw element is added.
 */
static INSTRUCTION_ENGINE *
computeRawEngine(RAW_ENGINE *toadd)
{
	register EBUF *tmp;	
	register INSTRUCTION_ENGINE *buf = NULL, *cur = NULL, *last = NULL;
	register u32 current; /* current number of elements in instruction */	

	tmp = Queue;
	
	/*if (use_memory_pool)
		buf = Pull_Data_From_Pool(POOL_QUEUE);*/
	
	if (buf == NULL)
	{
		/* Debug_Val(0, "computeRawEngine buf is empty so we allocate for %d\n", toadd->layer); */
		Neuro_AllocEBuf(tmp, sizeof(INSTRUCTION_ENGINE*), sizeof(INSTRUCTION_ENGINE));
		buf = Neuro_GiveCurEBuf(tmp);
	}
	/*else
		Debug_Val(0, "computeRawEngine recycled an object! %d\n", toadd->layer);*/


	current = Neuro_GiveEBufCount(tmp);
	
	buf->current = toadd;
	buf->next = NULL;


	/* this is the special case for a TDRAW_SDESTROY element 
	 * every one of those elements will be put into the 
	 * layer 0.
	 */
	if (buf->current->type == TDRAW_SDESTROY)
	{
		buf->current->layer = 0;
	}

	/* 
	 * special case so volatile types always are put in the 
	 * beginning of the queue. Both to make the process
	 * faster and to make the volatiles have precedence
	 * over everything to avoid conflicts.
	 */
	if (buf->current->type == TDRAW_VOLATILE)
	{
		/* we make the type be of the layer 0 so it is 
		 * always the first in the list.
		 *
		 * the layer 0 is reserved stricly for volatile
		 * types.
		 */

		buf->current->layer = 1;
	}

	if (debug_instruction_buffer)
		NEURO_TRACE("%s", Neuro_s("Push --> layer %d type %d", toadd->layer, toadd->type));
	
	if (last_element != NULL)
	{
		if (last_element->current == NULL)
		{
			NEURO_ERROR("%s", Neuro_s("dropping this call -- last_element current == %d - debug %d", 
				current, 
				(int)last_element->current));
			return NULL;
		}

		if (last_element->current->layer <= buf->current->layer)
		{
			last_element->next = buf;
			last_element = buf;
			/*Debug_Val(0, "proof layer %d real layer %d ptr %d\n",
					(*buf)[current]->current->layer,
					(*last_element)->current->layer,
					(int)(*last_element)->current);*/
			/* Debug_Val(0, "Placed the object at the end of the queue\n"); */	

			return buf;
		}
	}
	else
	{
		first_element = buf;
		last_element = buf;
		/* Debug_Val(0, "Just placed the frame as the first element, starting the queue\n"); */
		return buf;
	}

	cur = first_element;
	/* cur = Neuro_GiveEBuf(tmp, first); */
	while (cur != NULL)
	{
		/* Debug_Val(0, "looped cur %d buf %d\n", cur->current->layer, buf->current->layer); */
		if (cur->current->layer > buf->current->layer)
		{
			/*printf("Event current layer %d > toadd layer %d\n",  
					cur->current->layer, 
					toadd->layer); 
			*/
			
			/* to avoid death loops */
			/*if (cur->next == buf)
				cur->next = NULL;*/
			
			if (cur == first_element)
			{
#if temp
				/* switch **buf with the current position */
				temp = Neuro_GiveEBuf(tmp, first);
				Neuro_SetEBuf(tmp, Neuro_GiveEBufAddr(tmp, first), buf);
				Neuro_SetEBuf(tmp, Neuro_GiveEBufAddr(tmp, current), temp);

				/*printf("Beginning LL change : cur %d, buf[0][0] %d\n", 
						(int)cur, 
						(int)buf[0]);
				*/
				if (cur->current == NULL || cur->current->surface_ptr == NULL)
					cur = Neuro_GiveEBuf(tmp, first);
				if (Neuro_GiveEBuf(tmp, first) == buf)
				{
					Error_Print("huge problem, it is going to put its next element as the same node as itself, creating a death loop!!\n");
					cur->next = NULL;
				}
				else	
					cur->next = temp;
#endif /* temp */
				/*Debug_Val(0, "New First element proclaimed %d\n", 
						buf->current->layer);*/
				buf->next = first_element;
				first_element = buf;
			}
			else
				buf->next = cur;
			if (last != NULL)
			{
				last->next = buf;
			}
			break;
		}
		else
		{
			/* Debug_Val(0, "nothing to be done\n"); */
		}
		last = cur;
		cur = cur->next;
	}
	/* printf("End of the looping process\n"); */
	
	
	if (debug_instruction_buffer)
	{
		NEURO_TRACE("BEGIN inside computeRawEngine debug print", NULL);
		Graphics_DebugPrintQueue();
		NEURO_TRACE("END inside computeRawEngine debug print", NULL);
	}

	return buf;
}


/*-------------------- Global Functions ----------------------------*/

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 */
INSTRUCTION_ENGINE *
Graphics_AddDrawingInstruction(u32 layer, u8 type, Rectan *isrc, Rectan *idst, void *isurface)
{
	RAW_ENGINE *buf = NULL;
	Rectan tIsrc, tIdst;

	if (isurface == NULL || isrc == NULL || idst == NULL)
		return NULL;

	memcpy(&tIsrc, isrc, sizeof(Rectan));
	memcpy(&tIdst, idst, sizeof(Rectan));	

	if (BoundFixChecker(&screenSize, &tIsrc, &tIdst) == 1)
	{
		/* Debug_Val(10, "a drawing instruction was dropped because its destination is outbound"); */
		return NULL;
	}
	
#if retain_image_inipos
	tIsrc.x = isrc->x;
	tIsrc.y = isrc->y;
	tIsrc.w = isrc->w;
	tIsrc.h = isrc->h;

	tIdst.x = idst->x;
	tIdst.y = idst->y;
#endif /* retain_image_inipos */
	
	/* a square in the middle of the screen to test 
	 * the bound fix checker on an object other than 
	 * the screen itself. 
	 */
	/* BoundFixChecker(&test_BoundFix, &tIsrc, &tIdst); */
	
	/*if (use_memory_pool)
		buf = Pull_Data_From_Pool(POOL_RAWENGINE);*/

	if (buf == NULL)
	{
		Neuro_AllocEBuf(Raw, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
		buf = Neuro_GiveCurEBuf(Raw);
	}
	
	/* we reserve the first 5 spots for our need 
	 * we might reserve more than 5
	 * if the need comes out.
	 */
	buf->layer = layer + 5;
	buf->type = type;
	memcpy(&buf->src, &tIsrc, sizeof(Rectan));

	buf->dx = tIdst.x;
	buf->dy = tIdst.y;

	buf->surface_ptr = isurface;	

	/* sets a flag to tell when changed things 
	 * and I think the whole screen will be 
	 * redrawn.
	 */
	Graphics_RedrawScreen();

	/* if (buf->type == TDRAW_STATIC || buf->type == TDRAW_DYNAMIC)
		Graphics_SetAllToRedraw();*/

	return computeRawEngine((RAW_ENGINE*)buf);
}

INSTRUCTION_ENGINE *
Graphics_PushRaw(RAW_ENGINE *raw)
{
	return computeRawEngine((RAW_ENGINE*)raw);
}


INSTRUCTION_ENGINE *
Graphics_GetFirstElem()
{
	return first_element;
}

void
Graphics_SetFirstElem(INSTRUCTION_ENGINE *elem)
{
	first_element = elem;
}

INSTRUCTION_ENGINE *
Graphics_GetLastElem()
{
	return last_element;
}

void
Graphics_SetLastElem(INSTRUCTION_ENGINE *elem)
{
	last_element = elem;
}

EBUF *
Graphics_GetRawBuffer()
{
	return Raw;
}

EBUF *
Graphics_GetQueueBuffer()
{
	return Queue;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Graphics_PainterInit()
{
	i32 screenwidth, screenheight;
	
	Neuro_CreateEBuf(&Raw);
	Neuro_CreateEBuf(&Queue);

	/* get the screen size from the "official" source */
	Lib_GetScreenSize(&screenwidth, &screenheight);

	/* populate a buffer variable that keeps the size of the screen for fast consultation. */
	screenSize.x = 0;
	screenSize.y = 0;
	screenSize.w = screenwidth;
	screenSize.h = screenheight;
	
	return 0;
}

int
Graphics_PainterReset()
{
	Neuro_CleanEBuf(&Raw);
	Neuro_CleanEBuf(&Queue);

	Neuro_CreateEBuf(&Raw);
	Neuro_CreateEBuf(&Queue);

	first_element = NULL;
	last_element = NULL;

	return 0;
}

void
Graphics_PainterClean()
{
	NEURO_TRACE("Raw total %d", Neuro_GiveEBufCount(Raw));
	NEURO_TRACE("Queue total %d", Neuro_GiveEBufCount(Queue));

	Neuro_CleanEBuf(&Raw);	
	Neuro_CleanEBuf(&Queue);

	first_element = NULL;
	last_element = NULL;

}
