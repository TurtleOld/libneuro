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

/* coredraw.c
 * the core drawing algorithm.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <string.h> /* memcpy */

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <extlib.h> /* we mainly use the blitting function and screen updating functions */
#include <ebuf.h> /* we mainly use the single item cleaning function Neuro_SCleanEBuf */

/*-------------------- Main Module Header --------------------------*/
#include "video.h"
#include <graphics.h>


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("video/coredraw");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/* a function to resolve a bug that made the volatile objects
 * mysteriously disapear. It was because the last element of the
 * Dynamic object was not refreshed so if an element was just
 * before the Dynamic object, it just made it disapear :).
 * 
 * This function searches the element just before the input element
 * and returns it.
 */
static INSTRUCTION_ENGINE *
get_Previous_Object_To_Object(INSTRUCTION_ENGINE *indep)
{
	INSTRUCTION_ENGINE *cur, *last = NULL;
	
	cur = Graphics_GetFirstElem();

	if (cur == NULL)
		return NULL;

	if (indep == cur)
		return NULL;

	while (cur)
	{
		
		if (cur == indep)
		{
			/* Debug_Val(0, "found the previous elem 0x%x, (his next 0x%x)\n", 
					last, last->next);*/
			return last;
		}

		
		last = cur;
		cur = cur->next;
	}


	return NULL;
}

/* del_mask : 0 is don't clean any buffers (big memory leak) 
 * 1 is clean only the raw engine
 * 2 is clean only the queue(instruction) engine
 * 3 is clean both
 */
static void
Core_clean_object(INSTRUCTION_ENGINE *cur, int dont_redraw_section, u8 del_mask)
{
	Rectan buf;
	INSTRUCTION_ENGINE *last = NULL;
	EBUF *verify_m;

	/* this functions needs to destroy an element
	 * from both the instruction buffer and the raw
	 * buffer. 
	 *
	 * the first step we do is fill a special Rectan
	 * rectangle with the position and size of 
	 * the image. Then, we draw this position and
	 * size black on screen. After that is done, we
	 * have to redraw previous objects (if theres any).
	 *
	 *
	 */

	if (debug_track_fonts)
	{
		if (cur->current->layer >= 99999)
		{
			NEURO_TRACE("Destroying font - &0x%x", cur);
		}
	}

	/* Graphics_DebugQueueIntegrityCheck(); */

	if (!cur)
		return;

	/* Debug_Val(0, "destroying element address 0x%x\n", cur); */
	
	buf.x = cur->current->dx;
	buf.y = cur->current->dy;
	buf.w = cur->current->src.w;
	buf.h = cur->current->src.h;
			
	/*if (background)
		Lib_BlitObject(background, &buf, sclScreen2, &buf);
	else
		Lib_FillRect(sclScreen2, &buf, 0);
	*/

	if (dont_redraw_section == 0)
	{
		Lib_FillRect(Neuro_GetScreenBuffer(), &buf, 0);

		Graphics_RedrawSection(cur);
	}

	if (debug_clean_instruction_buffer)
	{
		Neuro_CreateEBuf(&verify_m);
		Graphics_DebugBufferQueue(verify_m);
	}

	if (debug_clean_instruction_buffer)
	{
		NEURO_TRACE("*initial values - Amount of elems %d", 
				Neuro_GiveEBufCount(verify_m) - 1);
		Graphics_DebugPrintMissing(verify_m);
	}

	/* only set the previous element (last) if the element we need to
	 * destroy isn't the first one 
	 */
	if (cur != Graphics_GetFirstElem())
		last = get_Previous_Object_To_Object(cur);
	if (last)
	{
		/* Debug_Val(0, "changing last's(0x%x) next 0x%x to 0x%x\n", last, 
				last->next, cur->next);*/

		last->next = cur->next;
	}	
		
	/* check to see if the element cur is either first_element 
	 * or last_element and if so, we will destituate it.
	 */
	if (cur == Graphics_GetFirstElem())
	{
		Graphics_SetFirstElem(cur->next);
	}

	if (cur == Graphics_GetLastElem())
	{
		if (last)
		{
			last->next = NULL;
			Graphics_SetLastElem(last);
		}
		else
			Graphics_SetLastElem(NULL);
	}

	if (debug_clean_instruction_buffer)
	{
		NEURO_TRACE("*before real destroy", NULL);
		Graphics_DebugPrintMissing(verify_m);
	}
		
	/*if (use_memory_pool)
		Push_Data_To_Pool(POOL_QUEUE, cur);
	else*/
	{
		/* INSTRUCTION_ENGINE *temp; */
		
		/*	
		temp = cur;
		cur = cur->next;
		*/
		
		/* Debug_Val(0, "before queue total %d\n", 
				Neuro_GiveEBufCount(_Queue) + 1);*/

		if (del_mask == 1 || del_mask == 3)
			Neuro_SCleanEBuf(Graphics_GetRawBuffer(), cur->current);
		/* Debug_Val(0, "-- element address 0x%x destroyed\n", cur); */
		if (del_mask == 2 || del_mask == 3)
			Neuro_SCleanEBuf(Graphics_GetQueueBuffer(), cur);

		/* Debug_Val(0, "after queue total %d\n", 
				Neuro_GiveEBufCount(_Queue) + 1); */
		/* continue; */
	}

	if (debug_clean_instruction_buffer)
	{
		NEURO_TRACE("**After the destroy", NULL);
		/*Debug_Val(0, "Amount of elems in verify %d in queue %d\n", 
				Neuro_GiveEBufCount(verify_m) - 1, 
				Neuro_GiveEBufCount(_Queue) - 1);*/
		NEURO_TRACE("**Full output", NULL);

		Graphics_DebugPrintQueue();

		Graphics_DebugPrintMissing(verify_m);
		Neuro_CleanEBuf(&verify_m);
	}

	/* Graphics_DebugQueueIntegrityCheck(); */
}

static void
clean_object(INSTRUCTION_ENGINE *cur, int dont_redraw_section)
{
	Core_clean_object(cur, dont_redraw_section, 3);
}

/*-------------------- Global Functions ----------------------------*/

/* take note that we don't do any checks for valid pointers and such 
 * this function is supposed to be a backbone to an interface function
 * which itself does the parity checks.
 */
void
Graphics_DestroyElement(INSTRUCTION_ENGINE *elem)
{
	RAW_ENGINE *tmp;
	Rectan dst;

	/* we destroy only the queue buffer because we want to make 
	 * the emplacement of the element into a more comfortable 
	 * place for the algorithm to avoid glitches.
	 */

	tmp = elem->current;
	
	/* we clean only the queue element and keep the raw engine buffer */
	Core_clean_object(elem, 1, 2);

	dst.x = tmp->dx;
	dst.y = tmp->dy;

	tmp->type = TDRAW_SDESTROY;

	/* we put the raw buffer back into the queue */
	Graphics_PushRaw(tmp);
}

void
Graphics_CoreDrawAll()
{
	Rectan isrc, idst;
	INSTRUCTION_ENGINE *cur, *last = NULL;
	u32 safety = 10000; /* safety decrementor to avoid death loop */
		
	if (Neuro_EBufIsEmpty(Graphics_GetQueueBuffer()))
		return;	

	cur = Graphics_GetFirstElem();

	/* start the real drawing */
	while (cur)
	{	

		if (safety <= 0)
		{
			NEURO_ERROR("To avoid a death loop, had to bail out of the instruction loop after 1000 loops", NULL);
			break; /* safety break */
		}
		else
			safety--;

		if (check_integrity_on_draw)
		{
			NEURO_TRACE("Data integrity check before drawing", NULL);
			Graphics_DebugQueueIntegrityCheck();
		}

		memcpy(&isrc, &cur->current->src, sizeof(Rectan));

		idst.x = cur->current->dx;
		idst.y = cur->current->dy;

		if (debug_instruction_buffer)
			NEURO_TRACE("%s", Neuro_s("%s Flushing type %d layer %d\n", __FUNCTION__, cur->current->type, cur->current->layer));
		
		/* draw the surface_ptr to the screen buffer. */
		switch (cur->current->type)
		{
			case TDRAW_STATIC:
			{
				
				/* Debug_Val(0, "static draw (%d,%d) %dx%d to (%d,%d)\n", 
						isrc.x, isrc.y, isrc.w, isrc.h, 
						idst.x, idst.y);

				*/


				Lib_BlitObject(cur->current->surface_ptr, &isrc, Neuro_GetScreenBuffer(), 
						&idst);
				
				cur->current->type = TDRAW_SDRAWN;

				/* Graphics_RedrawSection(cur); */

				/* Debug_Val(0, "drawn static\n"); */

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
					{
						NEURO_TRACE("%s", Neuro_s("Drawing font - Coord (%d,%d) &0x%x\n", 
								cur->current->dx,
								cur->current->dy, cur));
					}
				}
			}
			break;

			case TDRAW_SDRAWN:
			{
				/* nothing needed for this type */
				/* Debug_Val(0, "already drawn\n"); */
			}
			break;

			case TDRAW_SREDRAW:
			{
				/* Debug_Val(0, "address of surface 0x%x\n", cur->current->surface_ptr); */

				/* now we redraw the actual element */
				Lib_BlitObject(cur->current->surface_ptr, &isrc, Neuro_GetScreenBuffer(), 
						&idst);

				/* we cleanly redrawn the static element so we
				 * set the element's flag to drawn
				 */
				cur->current->type = TDRAW_SDRAWN;

				/* then we redraw the stuff that could have been 
				 * there and actually need to be visible(and are above
				 * our element, ie layers).
				 *
				 * broken for some reason
				 */
				Graphics_RedrawSection(cur);

				/* Debug_Val(0, "Redrawn a static element\n"); */
			}
			break;

			case TDRAW_SDESTROY:
			{
				INSTRUCTION_ENGINE *tmp;
				/*
				if (cur->current->layer > 1000)
					Debug_Print("Cleaned a static image");
				*/

				tmp = cur;

				cur = cur->next;

				clean_object(tmp, 0);

				/* we restart the loop completely since we need to draw
				 * the volatile types first before drawing the rest..
				 */
				cur = Graphics_GetFirstElem();
				
				if (cur)
					continue;
			}
			break;
			
			case TDRAW_DYNAMIC:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						Neuro_GetScreenBuffer(), &idst);

				cur->current->type = TDRAW_DYNAMIC_CLEAN;
				
				if (dynamic_debug)
					NEURO_TRACE("Dynamic : Tagging addr %x to clean", cur);
				/* Debug_Val(0, "drawn dynamic\n"); */
			}
			break;
			
#if temp
			case TDRAW_DYNAMIC_CLEAN:
			{
				/* Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);*/

				/* clean_object(cur); */
			}
			break;
#endif /* temp */

			case TDRAW_VOLATILE:
			{
				/*Debug_Val(0, "Volatile draw (%d,%d) %dx%d to (%d,%d) image 0x%x\n", 
						isrc.x, isrc.y, isrc.w, isrc.h,
						idst.x, idst.y, 
						cur->current->surface_ptr);
				*/

				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						Neuro_GetScreenBuffer(), &idst);

				/* NOTE that we CAN't USE clean_object() BECAUSE 
				 * it actually makes the image all BLACK
				 * so it nullifies the action of this COMPLETELY.
				 */
				if (last)
					last->next = cur->next;
		
				/* check to see if the element cur is either first_element 
				 * or last_element and if so, we will destituate it.
				 */
				if (cur == Graphics_GetFirstElem())
				{
					Graphics_SetFirstElem(cur->next);
				}

				if (cur == Graphics_GetLastElem())
				{
					if (last)
					{
						last->next = NULL;
						Graphics_SetLastElem(last);
					}
				}
			
			
				/*if (use_memory_pool)
					Push_Data_To_Pool(POOL_QUEUE, cur);
				else*/
				{
					INSTRUCTION_ENGINE *temp;
				
					temp = cur;
					cur = cur->next;

					Neuro_SCleanEBuf(Graphics_GetRawBuffer(), temp->current);
					Neuro_SCleanEBuf(Graphics_GetQueueBuffer(), temp);
					
					if (cur)
						continue;
				}
			}
			break;

			
			default:
			{
				NEURO_ERROR("Draw unknown type %d\n", cur->current->type);
			}
			break;
		}

		if (cur)
		{
			last = cur;
			if (cur->next == NULL && cur != Graphics_GetLastElem())
			{
				NEURO_ERROR("cur->next is NULL AND it isn't the last element, bad, very bad...", NULL);
			}
			cur = cur->next;
		}
		else
			break;
	}


	Graphics_SetDrawnLastCycle();
	/* Lib_FillRect(sclScreen, &test_BoundFix, 0); */
}

void
Graphics_SetAllToRedraw()
{
	INSTRUCTION_ENGINE *cur;
	
	cur = Graphics_GetFirstElem();

	if (cur == NULL)
		return;

	while (cur)
	{
		if (cur->current->type == TDRAW_SDRAWN)
			cur->current->type = TDRAW_STATIC; /* TDRAW_SREDRAW seems to do a flaw */

		cur = cur->next;
	}

	if (debug_instruction_buffer)
		NEURO_TRACE("Just Set all the instructions to be redrawn", NULL);
}

/* Graphics_CoreCleanAll might have cleaned objects
 * that should be drawn so we will redraw those in this
 * function. 
 * returns non zero if a volatile type was pushed
 */
int
Graphics_RedrawSection(INSTRUCTION_ENGINE *indep)
{
	Rectan buf, indep_body;
	INSTRUCTION_ENGINE *cur;
	int bounds_ret = 0;
	int output = 0;


	indep_body.x = indep->current->dx;
	indep_body.y = indep->current->dy;
	indep_body.w = indep->current->src.w;
	indep_body.h = indep->current->src.h;
	
	cur = Graphics_GetFirstElem();

	if (cur == NULL)
		return 0;

	while (cur)
	{
		
		if (cur == indep)
		{
			cur = cur->next;
			continue;
		}
		
		if (!cur->current)
		{
			NEURO_ERROR("%s", Neuro_s("BAD : the instruction 0x%x has an empty content! indep 0x%x its next element 0x%x", 
					cur, indep, indep->next));
			/* odd error, this ain't supposed to happen :L */
			return 0;
		}

		if (debug_track_fonts)
		{
			if (cur->current->layer >= 99999 && indep->current->layer >= 99999)
			{
				NEURO_TRACE("%s", Neuro_s("INITIAL Redrawing font -- Font type %d (%d,%d) &0x%x\n", cur->current->type, 
						buf.x, buf.y, cur));
			}
		}
		
		if (cur->current->type == TDRAW_SDRAWN)
		{
			
			buf.x = cur->current->dx;
			buf.y = cur->current->dy;
			buf.w = cur->current->src.w;
			buf.h = cur->current->src.h;
			
			bounds_ret = Neuro_BoundsCheck(&indep_body, &buf);
			/* bounds_ret = 2; */	

			if (debug_track_fonts)
			{
				if (cur->current->layer >= 99999 && indep->current->layer >= 99999)
				{
					NEURO_TRACE("%s", Neuro_s("INITIAL 2 Redrawing font -- bounds_ret %d current (%d,%d) indep (%d,%d)\n", 
							bounds_ret, 
							buf.x, buf.y,
							indep_body.x, indep_body.y));
				}	
			}

			if (bounds_ret == 0)
			{
				Rectan bufa;
				
				bufa.x = cur->current->dx;
				bufa.y = cur->current->dy;
				bufa.w = 0;
				bufa.h = 0;

				Neuro_PushVolatileDraw(cur->current->layer, &cur->current->src, 
						&bufa, cur->current->surface_ptr);
			
				/*Debug_Val(0, "dynamic is inside this object\n");*/

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						NEURO_TRACE("Redrawing font #0", NULL);
				}

				output = 1;
			}

			if (bounds_ret == 2)
			{
				Rectan isrc, idst;
				
				memcpy(&isrc, &cur->current->src, sizeof(Rectan));

				idst.x = cur->current->dx;
				idst.y = cur->current->dy;
				idst.w = 0;
				idst.h = 0;

				Neuro_VerticalBoundCrop(&indep_body, &isrc, &idst);
				Neuro_HorizontalBoundCrop(&indep_body, &isrc, &idst);

				Neuro_PushVolatileDraw(cur->current->layer, 
						&isrc, &idst, cur->current->surface_ptr);

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						NEURO_TRACE("Redrawing font #2", NULL);
				}
				
				output = 1;
			}

			if (bounds_ret == 3)
			{
				Rectan isrc, idst, hack;
				
				memcpy(&isrc, &cur->current->src, sizeof(Rectan));

				idst.x = cur->current->dx;
				idst.y = cur->current->dy;
				idst.w = 0;
				idst.h = 0;
				
				
				if (indep_body.x > buf.x)
				{
					isrc.x += indep_body.x - buf.x;
					idst.x += indep_body.x - buf.x;

					/*isrc.y += buf.y - indep_body.y;
					idst.y += buf.y - indep_body.y;*/

					isrc.w -= buf.w - indep_body.w;
					/* isrc.h += indep_body.h - buf.h; */
				}
				else
				{
					/*isrc.x += buf.x - indep_body.x;
					idst.x += buf.x - indep_body.x;*/

					isrc.y += indep_body.y - buf.y;
					idst.y += indep_body.y - buf.y;

					/* isrc.w += indep_body.w - buf.w; */
					isrc.h -= buf.h - indep_body.h;
				}
				
				/* Neuro_PushVolatileDraw(cur->current->layer, 
						&isrc, &idst, cur->current->surface_ptr);*/
				
				/* temporary hack that seems to work, we draw the whole 
				 * static image... this needs testing and a better 
				 * algorithm.
				 */
				
				hack.x = cur->current->dx;
				hack.y = cur->current->dy;
				hack.w = 0;
				hack.h = 0;

				Neuro_PushVolatileDraw(cur->current->layer, &cur->current->src, 
						&hack, cur->current->surface_ptr);

				/*Debug_Val(0, "we have a case 3 situation!\n");*/

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						NEURO_TRACE("Redrawing font #3", NULL);
				}
				
				output = 1;
			}

			/* indep is inside cur */
			if (bounds_ret == 4)
			{
				Rectan bufa;
				Rectan nsrc;

				bufa.x = indep_body.x;
				bufa.y = indep_body.y;
				bufa.w = 0;
				bufa.h = 0;

				nsrc.x = indep_body.x - cur->current->dx;
				nsrc.y = indep_body.y - cur->current->dy;
				nsrc.w = indep_body.w;
				nsrc.h = indep_body.h;	

				/*
				bufa.x = 0;
				bufa.y = 0;
				bufa.w = 0;
				bufa.h = 0;

				nsrc.x = 0;
				nsrc.y = 0;
				nsrc.w = cur->current->src.w;
				nsrc.h = cur->current->src.h;
				*/

				Neuro_PushVolatileDraw(cur->current->layer, &nsrc, 
						&bufa, cur->current->surface_ptr);

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						NEURO_TRACE("Redrawing font #4", NULL);
				}

				output = 1;
			}
			
			/* Debug_Val(0, "Redraw Section debug #%d\n", bounds_ret); */
		}	
		cur = cur->next;
	}

	return output;
}

void
Graphics_CoreCleanAll()
{
	INSTRUCTION_ENGINE *cur;

	cur = Graphics_GetFirstElem();
		
	if (cur == NULL)
		return;
	
	/* "reset" the emplacement of the last position of the image
	 * with the background if theres one or with the color black 
	 * if none.
	 */
	while (cur)
	{			
		clean_object(cur, 1);
		
		cur = cur->next;
	}
	
	Graphics_ResetScreenDraw();
}

/* only clean those with the type TDRAW_DYNAMIC_CLEAN */
void
Graphics_CoreCleanDoneDynamics()
{
	INSTRUCTION_ENGINE *cur;

	cur = Graphics_GetFirstElem();
		
	if (cur == NULL)
		return;
	
	/* "reset" the emplacement of the last position of the image
	 * with the background if theres one or with the color black 
	 * if none.
	 */
	while (cur)
	{
		if (cur->current->type == TDRAW_DYNAMIC_CLEAN)
		{
			clean_object(cur, 0);
		}
		
		cur = cur->next;
	}
}

void
Graphics_FreeVObject(v_object *source)
{
	INSTRUCTION_ENGINE *cur, *tmp;

	/* this function was made to avoid the painter's algorithm
	 * to have an element contain an image surface that was
	 * freed before it was processed resulting in a segmentation
	 * fault.
	 */

	cur = Graphics_GetFirstElem();

	while (cur)
	{
		tmp = cur->next;

		if (cur->current->surface_ptr == source)
		{
			/*if (cur->current->type == TDRAW_SDRAWN)
				Neuro_DestroyDraw(cur);
			else*/
				Core_clean_object(cur, 0, 3);
		}

		cur = tmp;
	}


	Lib_FreeVobject(source);
}

