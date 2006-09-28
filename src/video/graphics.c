
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

/* graphics.c
 * Module : Graphic_
 * 
 * could code in an engine :)
 * an engine would require a priority list(layers)
 * and the pointer to the SDL_Surface(of the image) variable.
 * (will definitely add more variables to RAW_ENGINE, 
 * just think about sprites.)
 * could use 2 buffers : one is the raw dump of the images
 * to show and the other one is the instruction buffer.
 * the raw buffer would be scanned to see which one of
 * the image have to be shown next and will then be put
 * in the instruction buffer which will be read one after 
 * the other. 
 * The raw buffer will contain all the stuff and the 
 * instruction buffer will only contain the pointers
 * to the data in the raw buffer and also a pointer to
 * the next pointer(a linked list).
 *
 * will need to organise loaded images. I am really
 * not sure which method I should use :L . Theres 2
 * image types : normal images and sprites. Sprites
 * could have a different format type. Sprites will
 * need to have a set number of frames which will 
 * depend on the image size. 
 *
 *
 * for now, I will add simple ways to load the images
 * so I can concentrate on the engine's drawing and
 * eventually do the space stars.
 *
 * Should I keep categories for the loaded images or 
 * just load them all in the same emplacement. 
 * Could also have images loading in the modules and 
 * they would send the pointer to the raw_instruction 
 * buffer... hmm sounds like a good idea :).
 *
 * Ok, now we need to be able to set the number of time
 * the screen will be redrawn per seconds. Added
 * a macro FRAMES_PER_SECOND which will hold the maximum
 * amount of screen that must be drawn per seconds.
 *
 * A single second has 1000 milliseconds we will use
 * 100 however because 1k is a bit too much. So the idea
 * is to divide the 60 frames in the 100, we have :
 * 60 / 100 = 0.6 . Will have to do an alghorithm to
 * know that 60 frames per seconds is in fact 1 frame 
 * per 2 "ticks" in 100. Small thought, to do 100 we
 * could do 1000 % 100 :). 1 frame per 2 "ticks" in 
 * 100 would make it 50 fps(we can't draw 0.6 frame 
 * nor 1.2 frame). How should I do it then? One way would
 * be to have a float variable raise until it gets to
 * 60. no I dont think it will work. Perhaps have an 
 * integer go to 60 and have % 1. the % 1 will then be
 * additionned by the absolute of 60 / 100 so if it is
 * 200(for example) it should work.
 *
 * 0 % 1 = 0 + 2 = 2
 * 1 % 1 = 1 + 2 = 3 <--- no it wont work
 * 2 % 1 = 0 + 2 = 2
 *
 * Ok ill explain once again what I need, I need an
 * algorithm that will take 60(default fps) and 100
 * and break the instructions (that r spread around the 100)
 * into 60 parts. 60 fps would in fact fall to 50 because
 * it would be 1 frame per 2 parts of the 100 which 
 * gives 50. This means that some parts of the 100
 * will need to do the instruction twice, 10 to be exact.
 * those 10 will need to be spread evenly around the
 * 100. something like this
 *  0 1 0 1 0 1 0 1 0 2 0 1 0 1 0 1 0 1 0 2 (every 10 theres a 2)
 *
 * lol I made it! I once again complicated everything when
 * it was so obvious when I retought it. I left my above idea
 * for future seeing of how I was offtrack ;P.
 *
 * On second thought, the before last paragraph will be
 * used to implement a frames per second limiter so it
 * was still useful.
 */

/* this code is my own version of the painter's algorithm. 
 * It is thus used to draw images in an order you have to
 * input when "pushing" an image, called the layer. 
 * This module also requires 3 other variables which contain
 * the size of the image, a pointer to the image and the 
 * destination of the image.
 *
 * To handle the problem, I used 2 types of data. I use raw
 * data which contains exactly what the external process
 * gives us and then theres the instruction data which
 * contains a pointer to a single raw data and a pointer
 * to the next instruction data.
 *
 * The instruction datas order are changed everytime a new
 * image is pushed. It is organised in a growing order, 
 * the smallest layers are the first ones and the biggest
 * are the last instructions drawn.
 *
 * Now, you see, the major advantage of this is we can also
 * redraw images that were below another one which would for
 * example be deleted. But for this, we do an expansive bounds
 * search for images which have to be redrawn.
 *
 * Also note that raw datas have different types that trigger
 * behaviors. Static and dynamic and the two most important
 * types, when then have the temporary type and the rest
 * is pretty much sub types for those that I mentionned earlier.
 */

#define debug_instruction_buffer 0
#define debug_clean_instruction_buffer 0
#define verbose_missing_output 0
#define dynamic_debug 0
#define check_integrity_on_draw 0

#define debug_track_fonts 1
#define debug_track_special_font 1 /* child of the above, won't work if the above is 0 */
#define debug_track_special_font_x 6 /* ditto */
#define debug_track_special_font_y 6 /* above above ditto */

#define screen_buffer 1
#define second_screen_buffer 0
#define retain_image_inipos 0
 
#define use_memory_pool 0
 
/*--- Extern Headers Including ---*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h> /* to calculate the frames per second (fps) */
 
/*--- Local Headers Including ---*/
#include <extlib.h>
#include <ebuf.h>

/*--- local module main header ---*/
#include "video.h"

/*--- Main Module Header ---*/
#include <graphics.h>
 
/*--- Global Variables ---*/

/*--- Static Variables ---*/

static v_object *screen; /* the screen surface */
static v_object *sclScreen; /* attempt to do a double buffered screen (for the static type) */
static v_object *sclScreen2; /* another screen buffer used for the dynamic type */

static v_object *background; /* the background image */

static INSTRUCTION_ENGINE *first_element;
static INSTRUCTION_ENGINE *last_element;

static EBUF *_Drawing;
static EBUF *_Raw;
static EBUF *_Queue;

/* screen size */
static Rectan screenSize;

static u32 temp_count;

/* a rectangle meant to test the bound fix algorithm */
static Rectan test_BoundFix;

static u32 fps; /* used increment every cycles */
static u32 lFps; /* used to give to other function the current's cycle fps count. */
static u32 ltime; /* used for the fps count, it is used to know when 1 second passed. */
static u32 fps_limit; /* used to limit the fps to this count. */
static u8 fps_dotincr; /* used to after dot increment for the fps limiter algorithm */
/*static u8 fps_incr; *//* used to increment for the fps limiter algorithm */
static u32 frameSkip = 0; /* holds the number of frames we have to skip. */
static u32 frameSkip_tmp = 0; /* the count of frames skipped already. */

/* 1 is that we have drawn the last cycle and 0 is no */
static u8 drawn_last_cycle;

/* 1 is that we don't draw anything in this cycle */
static u8 dont_draw_this_cycle;

/* 1 is that we got new draw instruction this cycle so we have to draw. */
static u8 draw_this_cycle;

/* 1 is that the Draw operations are safe, 
 * else they should not be considered safe 
 * and should be ignored 
 */
static u8 safe_draw_operation;

/* last frame we redrawn the pixels? */
/* static u8 lastPdraw; */

/* debug variable to find what happens 
 * with a font instruction type
 */
static INSTRUCTION_ENGINE *dfont;

/*--- Static Prototypes ---*/

static void cleanDrawing();
static void cleanQueue();
static void cleanRaw();

static INSTRUCTION_ENGINE *computeRawEngine(RAW_ENGINE *toadd);

/* debug print of the instruction queue */
static void print_queue() __attribute__ ((__unused__));
static void buffer_queue(EBUF *src) __attribute__ ((__unused__));
static void print_missing(EBUF *src) __attribute__ ((__unused__));

/* draw the objects on the screen */
static void draw_objects();
/* clean the previously drawn object on the screen */
static void clean_drawn_objects();
/* update only a part of the screen */
static void updScreen(Rectan *rect);
/* security, check if a rect is in bound with the main screen */
static int secureBoundsCheck(Rectan *rect) __attribute__ ((__always_inline__, __unused__));
/* clean the screen of the handled pixels drawn */
/* static void cleanPixels(); */

/* test function for the bounds fixer algorithm */
static u8 BoundFixChecker(Rectan *indep, Rectan *isrc, Rectan *idst);
/* the old function that used to push images into this engine 
 * now used as a backbone.
 */
/* static INSTRUCTION_ENGINE *Neuro_AddDrawingInstruction(u32 layer, u8 type, Rectan *isrc, Rectan *idst, void *isurface); */


static void Raw_Engine_All_To_Pool();
static void Queue_All_To_Pool();


static int redraw_erased_for_object(INSTRUCTION_ENGINE *indep);

static void clean_object(INSTRUCTION_ENGINE *cur);

/*--- Static Functions ---*/

/* check if the rectangle can be securly blit to the main screen */
static int
secureBoundsCheck(Rectan *rect)
{
	return Neuro_BoundsCheck(&screenSize, rect);
}

static void
updScreen(Rectan *rect)
{
	Lib_UpdateRect(screen, rect);
}

static void
cleanDrawing()
{
	Neuro_CleanEBuf(&_Drawing);
}

static void 
cleanRaw()
{
	Neuro_CleanEBuf(&_Raw);
}

static void 
cleanQueue()
{
	Neuro_CleanEBuf(&_Queue);
	first_element = NULL;
	last_element = NULL;
}

/* debug function */
static void
print_queue() 
{
	INSTRUCTION_ENGINE *cur;
	
	if (Neuro_EBufIsEmpty(_Queue))
		return;
	/* cur = Neuro_GiveEBuf(_Queue, 0); */
	cur = first_element;
	/* printf("Queue address %d\n", (int)cur); */
	
	while (cur != NULL)
	{
		/*if (cur->current->type == TDRAW_SDRAWN)
		{
			cur = cur->next;
			continue;
		}*/
		
		Debug_Val(0, "layer #%d address &%x type %d\n", cur->current->layer, cur, 
				cur->current->type);
		
		if (cur->next == first_element)
		{
			Debug_Val(0, "Error- this element points to the beginning element\n");
			break;
		}
		else
			cur = cur->next;
	}
}

static void
buffer_queue(EBUF *src)
{
	INSTRUCTION_ENGINE *cur;
	
	if (Neuro_EBufIsEmpty(_Queue))
		return;

	cur = first_element;
	
	if (verbose_missing_output == 1)
	{
		Debug_Val(0, "Outputting initial queue for missing data\n");
	}

	while (cur != NULL)
	{	
		debug_status *tmp = NULL;
		Neuro_AllocEBuf(src, sizeof(debug_status*), sizeof(debug_status));

		tmp = Neuro_GiveCurEBuf(src);

		/* memcpy(tmp, cur, sizeof(INSTRUCTION_ENGINE*)); */
		tmp->missing = (u32)cur;

		if (verbose_missing_output == 1)
		{
			Debug_Val(0, "layer #%d address &%x (%x) type %d\n", cur->current->layer, cur, 
					tmp->missing, cur->current->type);
		}
		
		if (cur->next == first_element)
		{
			Debug_Val(0, "Error- this element points to the beginning element\n");
			break;
		}
		else
			cur = cur->next;
	}
}

static void
print_missing(EBUF *src)
{
	INSTRUCTION_ENGINE *cur;
	debug_status *tmp;
	EBUF *missing_list;
	u32 total;
	u8 found = 0;
	
	if (Neuro_EBufIsEmpty(_Queue))
		return;

	if (Neuro_EBufIsEmpty(src))
		return;

	Neuro_CreateEBuf(&missing_list);

	cur = first_element;
	
	while (cur != NULL)
	{
		total = Neuro_GiveEBufCount(src) - 1;
		found = 0;

		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(src, total);

			/* Debug_Val(0, "is 0x%x the same as 0x%x? ", tmp->missing, cur); */
			if (tmp->missing == (u32)cur)
			{
				/* Debug_Val(0, "yes\n"); */
				found++;
			}
			/*else
				Debug_Val(0, "no\n");*/
		}

		if (found == 0)
		{
			struct debug_status *dstmp;
			Neuro_AllocEBuf(missing_list, sizeof(struct debug_status*), sizeof(struct debug_status));

			dstmp = Neuro_GiveCurEBuf(missing_list);

			dstmp->missing = (u32)cur;

			dstmp->duplicates = 0;
		}
		else if (found > 1)
		{
			struct debug_status *dstmp;
			Neuro_AllocEBuf(missing_list, sizeof(struct debug_status*), sizeof(struct debug_status));

			dstmp = Neuro_GiveCurEBuf(missing_list);

			dstmp->missing = (u32)cur;

			dstmp->duplicates = found;
		}
		
		if (cur->next == first_element)
		{
			Error_Print("This element points to the beginning element\n");
			break;
		}
		else
			cur = cur->next;
	}

	/* now that we filled the missing_list buffer, we can output
	 * its data. 
	 */

	Debug_Print("Debug queue status report");

	if (!Neuro_EBufIsEmpty(missing_list))
	{
		total = Neuro_GiveEBufCount(missing_list);
		Debug_Val(0, "We found %d missing/destroyed/duplicate addresses on %d\n", total, 
			Neuro_GiveEBufCount(_Queue));
	}
	else
	{
		Debug_Val(0, "NO missing/destroyed/duplicate addresses!\n");
		total = 0;
		Neuro_CleanEBuf(&missing_list);
		return;
	}
	
	total--;

	while (total-- > 0)
	{
		struct debug_status *dstmp = NULL;

		dstmp = Neuro_GiveEBuf(missing_list, total);

		if (!dstmp)
			continue;

		if (dstmp->duplicates == 0)
			Debug_Val(0, "the address 0x%x is missing\n", dstmp->missing);
		else
			Debug_Val(0, "the address 0x%x is present %d times\n", 
					dstmp->duplicates);
	}

	Neuro_CleanEBuf(&missing_list);
}

static void
Queue_Integrity_Check()
{
	INSTRUCTION_ENGINE *cur; /* ordered element from the queue */
	u32 qtotal; /* the queue engine buffer total decrementor */
	INSTRUCTION_ENGINE *raw; /* unordered element from the buffer */
	RAW_ENGINE *data; /* the raw data pointer */
	u32 rtotal; /* the raw engine buffer total decrementor */
	u8 temp; /* used in the tests */

	/* those are booleans for the report this function will give */
	u8 correct_queue_integ = 0; /* integrity of the queue data; 
				      ie the content of each exist and
				      it exists in the raw_engine buffer */
	u8 correct_last_elem = 0; /* specific */
	u8 correct_queue_and_buffer = 0; /* the queue contains all the elements 
					from its buffer */
	u8 correct_raw_and_queue = 0; /* the raw data are all contained in the 
					queue */
	u8 correct_order_queue = 0; /* the queue has the correct ordering */

	/* this function will check the queue's 
	 * elements with all the unordered elements
	 * from the ebuf. 
	 * 
	 * -- correct_queue_integ
	 * It will first check if all the elements in 
	 * the queue exist in the ebuf and if the raw 
	 * content they contain also exist.
	 *
	 * -- correct_last_elem
	 * It will also check if the first and last 
	 * elements are the same as the variables pointers
	 * for those.
	 *
	 * -- correct_queue_and_buffer
	 * It will then see if all the unordered ebuf queue
	 * elements are contained (linked) in the ordered queue.
	 *
	 * -- correct_raw_and_queue
	 * It will check if ALL the elements from the RAW_ENGINE
	 * buffer are contained in the unordered queue buffer.
	 *
	 * -- correct_order_queue
	 * It will check if the order of the ordered queue is good.
	 *
	 * Take good note that this integrity check should in fact
	 * have its own module because we can't currently easily
	 * see if the first_element is correct or no...
	 *
	 * We currently ASSUME the first_element is correct and non
	 * NULL or else we don't do any tests.
	 */

	if (first_element == NULL)
	{
		Error_Print("failed, first_element is NULL");
		return;
	}

	/* we start to check the queue integrity 
	 * -- correct_queue_integ
	 */
	cur = first_element;
	correct_queue_integ = 1;

	while (cur != NULL)
	{

		if (cur->current == NULL)
			correct_queue_integ = 0;

		cur = cur->next;
	}

	/* now we check if the last element is the
	 * correct one.
	 * -- correct_last_elem
	 */
	cur = first_element;

	while (cur != NULL)
	{

		if (cur->next == NULL)
		{
			if (cur == last_element)
				correct_last_elem = 1;

			break;
		}

		cur = cur->next;
	}

	/* It will then see if all the unordered ebuf queue
	 * elements are contained (linked) in the ordered queue.
	 * -- correct_queue_and_buffer
	 */
	correct_queue_and_buffer = 1;

	qtotal = Neuro_GiveEBufCount(_Queue) - 1;

	while (qtotal-- > 0)
	{
		raw = Neuro_GiveEBuf(_Queue, qtotal);

		cur = first_element;
		temp = 0;

		while (cur != NULL)
		{
			if (cur == raw)
				temp = 1;

			cur = cur->next;
		}

		if (temp == 0)
			correct_queue_and_buffer = 0;
	}

	/* It will check if ALL the elements from the RAW_ENGINE
	 * buffer are contained in the unordered queue buffer.
	 * -- correct_raw_and_queue
	 */
	rtotal = Neuro_GiveEBufCount(_Raw) - 1;
	correct_raw_and_queue = 1;

	while (rtotal-- > 0)
	{
		data = Neuro_GiveEBuf(_Raw, rtotal);

		qtotal = Neuro_GiveEBufCount(_Queue) - 1;

		temp = 0;

		while (qtotal-- > 0)
		{
			raw = Neuro_GiveEBuf(_Queue, qtotal);
			
			if (raw->current == data)
				temp = 1;
		}

		if (temp == 0)
			correct_raw_and_queue = 0;
	}
	/*
	 * It will check if the order of the ordered queue is good.
	 * -- correct_order_queue
	 */
	correct_order_queue = 1;
	
	cur = first_element;
	temp = 0;

	while (cur != NULL)
	{
		
		if (cur->current)
		{
			if (cur->current->layer < temp)
				correct_order_queue = 0;

			temp = cur->current->layer;
		}

		cur = cur->next;
	}

	/* now we output our report */
	Debug_Print("Data Integrity Check Report");

	Debug_Val(0, "Queue integrity : %d\n", correct_queue_integ);
	Debug_Val(0, "Correct last element : %d\n", correct_last_elem);
	Debug_Val(0, "Queue and its buffer : %d\n", correct_queue_and_buffer);
	Debug_Val(0, "Raw and Queue buffer presence : %d\n", correct_raw_and_queue);
	Debug_Val(0, "Order of the queue : %d\n", correct_order_queue);
}

/* compute the instruction_engine everytime
 * a new raw element is added.
 *
 * convertion : done
 * testing : works
 */
static INSTRUCTION_ENGINE *
computeRawEngine(RAW_ENGINE *toadd)
{
	register EBUF *tmp;	
	register INSTRUCTION_ENGINE *buf = NULL, *cur = NULL, *last = NULL;
	register u32 current; /* current number of elements in instruction */	

	temp_count++;

	tmp = _Queue;
	
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
	
	if (debug_instruction_buffer)
		Debug_Val(0, "Push --> %d\n", toadd->layer);
	
	if (last_element != NULL)
	{
		if (last_element->current == NULL)
		{
			printf("CAUGHT ERROR in last_element current == %d -- debug %d\nDropping this call\n", 
				current, 
				(int)last_element->current);
			return NULL;
		}
		
		/* printf("last_element layer %d\n", last_element->current->layer); */

		if (last_element->current->layer <= buf->current->layer)
		{
			last_element->next = buf;
			last_element = buf;
			/*printf("proof layer %d real layer %d ptr %d\n",
					(*buf)[current]->current->layer,
					(*last_element)->current->layer,
					(int)(*last_element)->current);*/
			/* Debug_Val(0, "Placed the object at the end of the queue\n"); */

			if (debug_track_fonts)
			{
				if (buf->current->layer >= 99999)
				{
					Debug_Print("Font added");
					Debug_Val(0, "infos : (%d,%d) &0x%x\n", 
							buf->current->dx, buf->current->dy, 
							buf);

					if (debug_track_special_font)
					{
						if (buf->current->dx == debug_track_special_font_x &&
							buf->current->dy == debug_track_special_font_y)
						{
							dfont = buf;
						}
					}

					/* check point for debugging */
				}
			}

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

#if temp
	/* search for the first element and if we find any,
	 * put its number in "first"
	 */
	while (current-- > 0)
	{
		cur = Neuro_GiveEBuf(tmp, current);

		if (cur->current)
		{
			if (cur->current->surface_ptr)
			{
				first = current;
				break;
			}
		}
	}
#endif /* temp */
	
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
					printf("huge problem, it is going to put its next element as the same node as itself, creating a death loop!!\n");
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
		Debug_Val(0, "BEGIN inside computeRawEngine debug print\n");
		print_queue();
		Debug_Val(0, "END inside computeRawEngine debug print\n");
	}

	if (debug_track_fonts)
	{
		if (buf->current->layer >= 99999)
		{
			Debug_Print("Font added");
			Debug_Val(0, "infos : (%d,%d) &0x%x\n", 
					buf->current->dx, buf->current->dy, 
					buf);

			if (debug_track_special_font)
			{
				if (buf->current->dx == debug_track_special_font_x &&
					buf->current->dy == debug_track_special_font_y)
				{
					dfont = buf;
				}
			}

			/* check point for debugging */
		}
	}

	return buf;
}

/* 
 * convertion : done
 * testing : works
 */
static void
draw_objects()
{
	Rectan isrc, idst;
	INSTRUCTION_ENGINE *cur, *last = NULL;
		
	if (Neuro_EBufIsEmpty(_Queue))
		return;	

	cur = first_element;

	/* start the real drawing */
	while (cur)
	{	
		if (debug_track_special_font)
		{
			if (dfont)
			{
				if (dfont->current)
				{
					Debug_Val(0, "Special font's type : %d\n", 
							dfont->current->type);
				}
			}
		}


		if (check_integrity_on_draw)
		{
			Debug_Print("Data integrity check before drawing");
			Queue_Integrity_Check();
		}

		memcpy(&isrc, &cur->current->src, sizeof(Rectan));

		idst.x = cur->current->dx;
		idst.y = cur->current->dy;
		
		/* draw the surface_ptr to the screen buffer. */
		switch (cur->current->type)
		{
			case TDRAW_STATIC:
			{
				
				/*Debug_Val(0, "after x y (%d,%d) size %d %d\n", 
						buf.x, buf.y, buf.w, buf.h);
				*/

				Lib_BlitObject(cur->current->surface_ptr, &isrc, sclScreen, 
						&idst);			
				
				cur->current->type = TDRAW_SDRAWN;
				/* Debug_Val(0, "drawn static\n"); */

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
					{
						Debug_Print("Drawing font");
						Debug_Val(0, "Coord (%d,%d) &0x%x\n", 
								cur->current->dx,
								cur->current->dy, cur);
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
				Lib_BlitObject(cur->current->surface_ptr, &isrc, sclScreen2, 
						&idst);

				/* then we redraw the stuff that could have been 
				 * there and actually need to be visible(and are above
				 * our element, ie layers).
				 */
				redraw_erased_for_object(cur);

				/* we cleanly redrawn the static element so we
				 * set the element's flag to drawn
				 */
				cur->current->type = TDRAW_SDRAWN;

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

				clean_object(tmp);
				
				if (cur)
					continue;
				else
					return;
			}
			break;
			
			case TDRAW_DYNAMIC:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);

				cur->current->type = TDRAW_DYNAMIC_CLEAN;
				
				if (dynamic_debug)
					Debug_Val(0, "Dynamic : Tagging addr %x to clean\n", cur);
				/* Debug_Val(0, "drawn dynamic\n"); */
			}
			break;
			
			/*case TDRAW_DYNAMIC_CLEAN:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);
			}
			break;*/

			case TDRAW_VOLATILE:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);
				
				if (last)
					last->next = cur->next;
		
				/* check to see if the element cur is either first_element 
				 * or last_element and if so, we will destituate it.
				 */
				if (cur == first_element)
					first_element = cur->next;

				if (cur == last_element)
				{
					last->next = NULL;
					last_element = last;
				}
			
			
				/*if (use_memory_pool)
					Push_Data_To_Pool(POOL_QUEUE, cur);
				else*/
				{
					INSTRUCTION_ENGINE *temp;
				
					temp = cur;
					cur = cur->next;

					Neuro_SCleanEBuf(_Raw, temp->current);
					Neuro_SCleanEBuf(_Queue, temp);
					continue;
				}
			}
			break;

			
			default:
			{
				Debug_Val(0, "ERROR Draw unknown type %d\n", cur->current->type);
			}
			break;
		}

		last = cur;
		if (cur->next == NULL && cur != last_element)
		{
			Error_Print("cur->next is NULL AND it isn't the last element, bad, very bad...");
		}
		cur = cur->next;
	}

	drawn_last_cycle = 1;
	/* Lib_FillRect(sclScreen, &test_BoundFix, 0); */
}

/* clean_drawn_objects() might have cleaned objects
 * that should be drawn so we will redraw those in this
 * function. 
 * returns non zero if a volatile type was pushed
 */
static int
redraw_erased_for_object(INSTRUCTION_ENGINE *indep)
{
	Rectan buf, indep_body;
	INSTRUCTION_ENGINE *cur;
	int bounds_ret = 0;
	int output = 0;


	indep_body.x = indep->current->dx;
	indep_body.y = indep->current->dy;
	indep_body.w = indep->current->src.w;
	indep_body.h = indep->current->src.h;
	

	if (first_element == NULL)
		return 0;
	
	cur = first_element;

	while (cur)
	{		
		
		if (cur == indep)
		{
			cur = cur->next;
			continue;
		}
		
		if (!cur->current)
		{
			Debug_Val(0, "BAD : the instruction 0x%x has an empty content!\n", 
					cur);

			Debug_Val(0, "DEBUG data : indep 0x%x  its next element 0x%x\n",
					indep, indep->next);
			/* odd error, this ain't supposed to happen :L */
			return 0;
		}

		if (debug_track_fonts)
		{
			if (cur->current->layer >= 99999 && indep->current->layer >= 99999)
			{
				Debug_Print("INITIAL Redrawing font");

				Debug_Val(0, "Font type %d (%d,%d) &0x%x\n", cur->current->type, 
						buf.x, buf.y, cur);
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
					Debug_Print("INITIAL 2 Redrawing font");
					Debug_Val(0, "bounds_ret %d current (%d,%d) indep (%d,%d)\n", 
							bounds_ret, 
							buf.x, buf.y,
							indep_body.x, indep_body.y);
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
						Debug_Print("Redrawing font #0");
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

				Neuro_VerticalBoundFix(&indep_body, &isrc, &idst);
				Neuro_HorizontalBoundFix(&indep_body, &isrc, &idst);

				Neuro_PushVolatileDraw(cur->current->layer, 
						&isrc, &idst, cur->current->surface_ptr);

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						Debug_Print("Redrawing font #2");
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

				/* Debug_Val(0, "we have a case 3 situation!\n"); */

				if (debug_track_fonts)
				{
					if (cur->current->layer >= 99999)
						Debug_Print("Redrawing font #3");
				}
				
				output = 1;
			}	
			
			/* Debug_Val(0, "object end\n"); */
		}	
		cur = cur->next;
	}

	return output;
}

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
	
	if (first_element == NULL)
		return NULL;

	if (indep == first_element)
		return NULL;

	cur = first_element;

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

static void
clean_object(INSTRUCTION_ENGINE *cur)
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
			Debug_Print("Destroying font");
			Debug_Val(0, "&0x%x\n", cur);
		}
	}

	/* Queue_Integrity_Check(); */

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
					
	Lib_FillRect(sclScreen2, &buf, 0);

	redraw_erased_for_object(cur);

	if (debug_clean_instruction_buffer)
	{
		Neuro_CreateEBuf(&verify_m);
		buffer_queue(verify_m);
	}

	if (debug_clean_instruction_buffer)
	{
		Debug_Print("*initial values");
		Debug_Val(0, "Amount of elems %d\n", Neuro_GiveEBufCount(verify_m) - 1);
		print_missing(verify_m);
	}

	/* only set the previous element (last) if the element we need to
	 * destroy isn't the first one 
	 */
	if (cur != first_element)
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
	if (cur == first_element)
		first_element = cur->next;

	if (cur == last_element)
	{
		if (last)
		{
			last_element = last;
			last_element->next = NULL;
		}
		else
			last_element = NULL;
	}

	if (debug_clean_instruction_buffer)
	{
		Debug_Print("*before real destroy");
		print_missing(verify_m);
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

		Neuro_SCleanEBuf(_Raw, cur->current);
		/* Debug_Val(0, "-- element address 0x%x destroyed\n", cur); */
		Neuro_SCleanEBuf(_Queue, cur);

		/* Debug_Val(0, "after queue total %d\n", 
				Neuro_GiveEBufCount(_Queue) + 1); */
		/* continue; */
	}

	if (debug_clean_instruction_buffer)
	{
		Debug_Print("**After the destroy");
		Debug_Val(0, "Amount of elems in verify %d in queue %d\n", 
				Neuro_GiveEBufCount(verify_m) - 1, 
				Neuro_GiveEBufCount(_Queue) - 1);
		Debug_Print("**Full output");

		print_queue();

		print_missing(verify_m);
		Neuro_CleanEBuf(&verify_m);
	}

	/* Queue_Integrity_Check(); */
}

/* */
static void
clean_drawn_objects()
{
	Rectan buf;
	INSTRUCTION_ENGINE *cur, *last = NULL;

		
	if (first_element == NULL)
		return;

	cur = first_element;
	
	/* "reset" the emplacement of the last position of the image
	 * with the background if theres one or with the color black 
	 * if none.
	 */
	while (cur)
	{				
		buf.x = cur->current->dx;
		buf.y = cur->current->dy;
		buf.w = cur->current->src.w;
		buf.h = cur->current->src.h;
			
		if (cur->current->type == TDRAW_DYNAMIC_CLEAN)
		{
			/*if (background)
				Lib_BlitObject(background, &buf, sclScreen2, &buf);
			else
				Lib_FillRect(sclScreen2, &buf, 0);
			*/
					
			Lib_FillRect(sclScreen2, &buf, 0);
			
			if (redraw_erased_for_object(cur))
			{
				last = get_Previous_Object_To_Object(cur);
			}

			if (dynamic_debug)
				Debug_Val(0, "Dynamic : cleaning address %x\n", cur);


			if (last)
				last->next = cur->next;
		
			/* check to see if the element cur is either first_element 
			 * or last_element and if so, we will destituate it.
			 */
			if (cur == first_element)
				first_element = cur->next;

			if (cur == last_element)
			{
				if (last)
				{
					last_element = last;
					last_element->next = NULL;
				}
				else
					last_element = NULL;
			}
			
			
			/*if (use_memory_pool)
				Push_Data_To_Pool(POOL_QUEUE, cur);
			else*/
			{
				INSTRUCTION_ENGINE *temp;
				
				temp = cur;
				cur = cur->next;
				/*Debug_Val(0, "before queue total %d\n", 
						Neuro_GiveEBufCount(_Queue));*/

				Neuro_SCleanEBuf(_Raw, temp->current);
				Neuro_SCleanEBuf(_Queue, temp);

				/* Debug_Val(0, "after queue total %d\n", 
						Neuro_GiveEBufCount(_Queue));*/
				continue;
			}
		}
		
		last = cur;
		cur = cur->next;
	}
	
	drawn_last_cycle = 0;
}


static void
clean_queue()
{	
	/*
	first_element = NULL;
	last_element = NULL;
	*/

	if (debug_instruction_buffer)
	{
		Debug_Val(0, "-BEGIN debug print\n");
		print_queue();
		Debug_Val(0, "-END debug print\n");
	}
}

/* a test to see if the bounds fix algo works 
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
			Neuro_VerticalBoundFix(indep, isrc, idst);
			Neuro_HorizontalBoundFix(indep, isrc, idst);
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

static void
Raw_Engine_All_To_Pool()
{
	/*
	register RAW_ENGINE *buf;
	register u32 total;

	if (Neuro_EBufIsEmpty(_Raw))
		return;
	
	total = Neuro_GiveEBufCount(_Raw) + 1;
	
	while (total-- > 0)
	{
		buf = Neuro_GiveEBuf(_Raw, total);

		if (buf->surface_ptr)
		{
			buf->surface_ptr = NULL;
			Push_Data_To_Pool(POOL_RAWENGINE, buf);
		}
	}
	*/
}

static void
Queue_All_To_Pool()
{
#if temp
	INSTRUCTION_ENGINE *buf;

	if (!first_element)
		return;
	
	buf = first_element;
	
	while (buf != NULL)
	{
		/* Debug_Val(0, "queue dump\n"); */
		buf->current = NULL;
		Push_Data_To_Pool(POOL_QUEUE, buf);	

		buf = buf->next;
	}
#endif /* temp */
}

/*--- Global Functions ---*/


/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 *
 *   convertion : done
 *   testing : works
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
		Neuro_AllocEBuf(_Raw, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
		buf = Neuro_GiveCurEBuf(_Raw);
	}
	
	buf->layer = layer;
	buf->type = type;
	memcpy(&buf->src, &tIsrc, sizeof(Rectan));

	buf->dx = tIdst.x;
	buf->dy = tIdst.y;

	buf->surface_ptr = isurface;	

	draw_this_cycle = 1;

	return computeRawEngine((RAW_ENGINE*)buf);
}

u8
Graphics_DrawIsPresent(v_elem *elem)
{
	INSTRUCTION_ENGINE *tmp = NULL;
	u32 total = 0;

	if (!elem)
		return 0;

	total = Neuro_GiveEBufCount(_Queue) + 1;

	while(total-- > 0)
	{
		tmp = Neuro_GiveEBuf(_Queue, total);

		if (tmp == elem)
			return 1;
	}

	return 0;
}

u8 
Graphics_GetSafeDrawOp()
{
	return safe_draw_operation;
}

void
Graphics_SetSafeDrawOp(u8 safe)
{
	if (safe > 1)
		safe = 1;

	safe_draw_operation = safe;
}

int
Graphics_RedrawSection(INSTRUCTION_ENGINE *indep)
{
	return redraw_erased_for_object(indep);
}

/* might become obsolete */
void
Neuro_SetFrameSkip(u32 frameskip)
{
	frameSkip = frameskip;
}

/* might become obsolete */
void
Neuro_SetFpsLimit(u32 fpsLimit)
{
	fps_limit = fpsLimit;
}

/* we need to move that to the interface function */
void
Neuro_GiveScreenSize(u32 *width, u32 *height)
{
	/* will be changed so programs that call Neuro will be 
	 * able to change the resolution dynamically when it starts
	 * so the screen size values will be in variables rather than
	 * hardcoded in macros.
	 */
	/**width = SCREEN_X;
	*height = SCREEN_Y;*/

	Lib_GetScreenSize(width, height);
}

void
Neuro_RedrawScreen()
{
	draw_this_cycle = 1;
}

/* clean the whole screen */
void
Neuro_RefreshScreen()
{
	Rectan buf;
	i32 h, w;

	if (background)
	{
		Neuro_GiveImageSize(background, &w, &h);

		buf.x = 0;
		buf.y = 0;
		buf.w = (i16)w;
		buf.h = (i16)h;
		
		Lib_BlitObject(background, &buf, sclScreen, NULL);
	}
	else
		Lib_FillRect(sclScreen, NULL, 0);

	/* Debug_Val(0, "before Pool total %d\n", Neuro_GiveEBufCount(_pool)); */
	/*if (use_memory_pool)
	{
		Raw_Engine_All_To_Pool();
		Queue_All_To_Pool();
	}
	else*/
	{
		Neuro_CleanEBuf(&_Raw);
		Neuro_CleanEBuf(&_Queue);

		Neuro_CreateEBuf(&_Raw);
		Neuro_CreateEBuf(&_Queue);

	}
	/*Debug_Val(0, "Real Elements total %d\n", temp_count);
	temp_count = 0;
	Debug_Val(0, "Raw total %d\n", Neuro_GiveEBufCount(_Raw));
	Debug_Val(0, "Queue total %d\n", Neuro_GiveEBufCount(_Queue));
	Debug_Val(0, "after Pool total %d\n", Neuro_GiveEBufCount(_pool));*/
	
	first_element = NULL;
	last_element = NULL;


	safe_draw_operation = 0;
}

void
Neuro_GiveFPS(t_tick *output)
{
	*output = lFps;
}

/* use this function to set the background 
 * --will soon become obsolete--
 */
void
Neuro_AddBackground(v_object *isurface)
{
	Rectan buf;
	Rectan src;


	background = isurface;
	Lib_GiveVobjectProp(background, &buf);

	src.x = 0;
	src.y = 0;
	src.h = buf.h;
	src.w = buf.w;
	
	Lib_BlitObject(background, NULL, sclScreen, &src);
	Lib_Flip(sclScreen);
}

void 
Neuro_AddDirectDrawing(Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Lib_BlitObject(isurface, isrc, sclScreen, idst);
	
	/*Lib_BlitObject(sclScreen2, NULL, screen, NULL);
	updScreen(0);
	Lib_Flip(screen);*/
}

/* external modules call this function
 * to add their callback functions to 
 * be ran in this engine's loop.
 *
 * convertion : done
 * testing : works 100%
 */
void 
Neuro_AddDrawingElement(void (*func)())
{
	DRAWING_ELEMENTS *buf = NULL;
	
	Neuro_AllocEBuf(_Drawing, sizeof(DRAWING_ELEMENTS*), sizeof(DRAWING_ELEMENTS));

	buf = Neuro_GiveEBuf(_Drawing, Neuro_GiveEBufCount(_Drawing));
	
	buf->callback = func;
	
}

void *
Neuro_GetScreenBuffer()
{
	return (void*)sclScreen;
}

/*--- Poll ---*/

/* convertion : done
 * testing : works
 */
void
Graphics_Poll()
{	
	if (debug_instruction_buffer || dynamic_debug)
		Debug_Val(0, "cycle\n");

	/* we will call a function in the module pixels in a near future */
	/*
	if (clean_pixel_in_this_cycle)
	{
		cleanPixels();
		clean_pixel_in_this_cycle = 0;
	}
	*/


	if (ltime + 1 <= time(NULL))
	{
		lFps = fps;
		fps = 0;
		ltime = time(NULL);
	}
	else
	{
		/* lFps = 0; */
		fps++;
	}
	
	if (fps_limit > 0 /*&& fps_limit <= fps*/) 
	{
		/* in this case, we toggle a variable so nothing will be drawn
		 * this cycle 
		 */
		fps_dotincr += fps_limit;

		if (fps_dotincr >= 100)
		{
			fps++;
		
			if (fps_limit <= 100)
				fps_dotincr -= 100;
			else
				fps_dotincr -= 100 * (int)(fps_limit / 100);
		}
		else
			dont_draw_this_cycle = 1;
	}

	if (!Neuro_EBufIsEmpty(_Drawing))
	{
		DRAWING_ELEMENTS *bufa;
		u32 total;
		
		total = Neuro_GiveEBufCount(_Drawing) + 1;

		while (total-- > 0)
		{
			bufa = Neuro_GiveEBuf(_Drawing, total);
			(*bufa->callback)();
		}
	}
	
	if (debug_instruction_buffer)
	{
		Debug_Val(0, "--BEGIN debug print\n");
		print_queue();
		Debug_Val(0, "--END debug print\n");
	}

	/* construct the instruction buffer */
	
	/* flush the instruction completely in the order 
	 * presented and clean the raw engine buffer 
	 */
	if (frameSkip_tmp <= 0)
	{
		frameSkip_tmp = frameSkip;
	}
	else
	{
		dont_draw_this_cycle = 1;
		frameSkip_tmp--;
	}

	
	if (!dont_draw_this_cycle)
	{	
		if (drawn_last_cycle)
			clean_drawn_objects();
		
		if (debug_instruction_buffer)
		{
			Debug_Val(0, "*BEGIN debug print\n");
			print_queue();
			Debug_Val(0, "*END debug print\n");
		}
		
		if (draw_this_cycle)
			draw_objects();
	}
	else
		dont_draw_this_cycle = 0;

	
	/* update the full screen */
	if (draw_this_cycle && !dont_draw_this_cycle)
	{	
		if (screen_buffer)
			Lib_BlitObject(sclScreen, NULL, screen, NULL);
		
		if (second_screen_buffer == 0)
			updScreen(0);
		
		draw_this_cycle = 0;
	}
	
	/* clean some of the most important buffers */
	clean_queue();
	
	if (second_screen_buffer)
	{
		Lib_BlitObject(sclScreen2, NULL, screen, NULL);
		updScreen(0);
	}
}

/*--- Constructor Destructor ---*/
int
Graphics_Init()
{
	int _err_ = 0;
	u32 screenwidth, screenheight;

	ltime = time(NULL);

	Lib_GetScreenSize(&screenwidth, &screenheight);
	
	if (screen_buffer)
	{
		_err_ = Lib_VideoInit(&screen, &sclScreen);

		if (_err_ == 1)
		{
			Error_Print("Lib_VideoInit failed");
			return 1;
		}
	}
	else
	{
		_err_ = Lib_VideoInit(&screen, NULL);
		
		if (_err_ == 1)
		{
			Error_Print("Lib_VideoInit failed");
			return 1;
		}
		
		
		sclScreen = screen;
	}

	if (second_screen_buffer)
	{
		sclScreen2 = Lib_CreateVObject(0, screenwidth, screenheight, 
				Lib_GetDefaultDepth(), 0, 0, 0, 0);

		if (sclScreen2 == NULL)
		{
			Error_Print("Couldn't create a v object : sclScreen2");
			return 1;
		}
		
		Lib_SetColorKey(sclScreen2, 0);
	}
	else
	{
		sclScreen2 = sclScreen;
	}

	
	Neuro_CreateEBuf(&_Drawing);
	Neuro_CreateEBuf(&_Raw);
	Neuro_CreateEBuf(&_Queue);

	/*
	if (use_memory_pool)
		Pool_Init();
	*/
	
	screenSize.x = 0;
	screenSize.y = 0;
	screenSize.w = screenwidth;
	screenSize.h = screenheight;
	
	test_BoundFix.x = screenSize.w / 2;
	test_BoundFix.y = 500 + 10;
	test_BoundFix.w = 20;
	test_BoundFix.h = 7 * 6;
	
	return _err_;
}

/* convertion : done
 * testing : seems to work
 */
void 
Graphics_Clean()
{	
	
	Debug_Val(0, "Raw total %d\n", Neuro_GiveEBufCount(_Raw));
	Debug_Val(0, "Queue total %d\n", Neuro_GiveEBufCount(_Queue));
	/* Debug_Val(0, "Pool total %d\n", Neuro_GiveEBufCount(_pool)); */
	
	cleanDrawing();
	cleanRaw();
	cleanQueue();

	/*
	if (use_memory_pool)
		Pool_Clean();
	*/
	
	if (screen)
	{
		Lib_FreeVobject(screen);
		screen = NULL;
	}
	if (sclScreen && screen_buffer)
	{
		Lib_FreeVobject(sclScreen);
		sclScreen = NULL;
	}
	if (sclScreen2 && second_screen_buffer)
	{
		Lib_FreeVobject(sclScreen2);
		sclScreen2 = NULL;
	}

	Lib_VideoExit();
}
