
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

#define debug_instruction_buffer 0
#define dynamic_debug 0

#define screen_buffer 1
#define second_screen_buffer 0
#define retain_image_inipos 0

#define handle_double_rectangle 0
 
#define use_memory_pool 0
 
/*--- Extern Headers Including ---*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h> /* to calculate the frames per second (fps) */
 
/*--- Local Headers Including ---*/
#include <extlib.h>
#include <ebuf.h>

/*--- Main Module Header ---*/
#include "graphics.h"
 
/*--- Global Variables ---*/

/*--- Static Variables ---*/

enum drawings_type
{
	TDRAW_STATIC = 1,
	TDRAW_DYNAMIC,
	TDRAW_DYNAMIC_CLEAN,
	TDRAW_SDRAWN, /* static but already drawn */

	TDRAW_END
};
 
typedef struct RAW_ENGINE
{
	u8 layer; /* the drawing level that the surface is drawn */
	u8 type; /* 1 is static, 2 is dynamic, 3 is static but already drawn*/
	u8 double_rectangle; /* is set to 1 if we have to draw 2 identical rectangle
			      * used with the overriding. something like this
			      * (ASCII art)
			      *
			      * 1100011
			      * 1100011
			      * 1100011
			      * 1100011
			      *
			      * or
			      *
			      * 1111111
			      * 0000000
			      * 0000000
			      * 1111111
			      */
	Rectan src;
	Rectan dst; /* will need to memcpy the data because 
			* this will be used beyond the 
			* scope of the calling function.
			*/
	void *override; /* if used, will point to a Rectan element that will override 
			 * src and dst.
			 */
	void *override2;
	void *surface_ptr; /* only the pointer needed cause its "static" */
}RAW_ENGINE;

/* this linked list will be computed 
 * unless, RAW_ENGINE is empty, every frames. 
 * It will not be possible to change entries 
 * in it once they r in, new stuff r added 
 * to the end of this linked list and done
 * stuff r removed, exactly like a fifo pipe.
 */
typedef struct INSTRUCTION_ENGINE
{
	RAW_ENGINE *current;
	struct INSTRUCTION_ENGINE *next;
}INSTRUCTION_ENGINE;

typedef struct DRAWING_ELEMENTS
{
	void (*callback)(void);
}DRAWING_ELEMENTS;

typedef struct PIXEL_ENGINE
{
	u32 x, y;
}PIXEL_ENGINE;

enum POOL_TYPES
{
	POOL_AVAILABLE = 0, /* an available spot */
	POOL_RAWENGINE = 1, /* RAW_ENGINE */
	POOL_QUEUE, /* INSTRUCTION_ENGINE */
	POOL_PIXELS, /* PIXEL_ENGINE */
	
	POOL_LAST
};

typedef struct STRUCT_POOL
{
	u8 type;
	void *data;
}STRUCT_POOL;

static v_object *screen; /* the screen surface */
static v_object *sclScreen; /* attempt to do a double buffered screen (for the static type) */
static v_object *sclScreen2; /* another screen buffer used for the dynamic type */

static v_object *background; /* the background image */


static INSTRUCTION_ENGINE *first_element;
static INSTRUCTION_ENGINE *last_element;

static EBUF *_Drawing;
static EBUF *_Raw;
static EBUF *_Queue;
static EBUF *_Pixel;

/* pool used to reuse allocated memory */
static EBUF *_pool; 

/* screen size */
static Rectan screenSize;

u32 temp_count;

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

/* 1 is that the pixels will be cleaned during this cycle and 0 is no it won't */
static u8 clean_pixel_in_this_cycle;

/* 1 is that we have drawn the last cycle and 0 is no */
static u8 drawn_last_cycle;

/* 1 is that we don't draw anything in this cycle */
static u8 dont_draw_this_cycle;

/* 1 is that we got new draw instruction this cycle so we have to draw. */
static u8 draw_this_cycle;

/* last frame we redrawn the pixels? */
/* static u8 lastPdraw; */


/*--- Static Prototypes ---*/

static void cleanDrawing();
static void cleanQueue();
static void cleanRaw();

static void computeRawEngine(RAW_ENGINE *toadd);

/* debug print of the instruction queue */
static void print_queue() __attribute__ ((__unused__));

/* draw the objects on the screen */
static void draw_objects();
/* clean the previously drawn object on the screen */
static void clean_drawn_objects();
/* update only a part of the screen */
static void updScreen(Rectan *rect);
/* security, check if a rect is in bound with the main screen */
static int secureBoundsCheck(Rectan *rect) __attribute__ ((__always_inline__));
/* clean the screen of the handled pixels drawn */
static void cleanPixels();

/* test function for the bounds fixer algorithm */
static u8 BoundFixChecker(Rectan *indep, Rectan *isrc, Rectan *idst);
/* the old function that used to push images into this engine 
 * now used as a backbone.
 */
static void AddDrawingInstruction(u8 layer, u8 type, Rectan *isrc, Rectan *idst, void *isurface);

/* returns a pointer corresponding to the type or
 * NULL if none found 
 */
static void *Pull_Data_From_Pool(u8 type);
/* function to put a new element in the pool */
static void Push_Data_To_Pool(u8 type, void *data);


static void Raw_Engine_All_To_Pool();
static void Queue_All_To_Pool();

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

static void
cleanRawEngineElement(void *eng)
{
	RAW_ENGINE *buf;

	buf = (RAW_ENGINE*)eng;

	if (buf)
	{
		if (buf->override)
			free(buf->override);
		if (buf->override2)
			free(buf->override2);
	}
}

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
		if (cur->current->type == TDRAW_SDRAWN)
		{
			cur = cur->next;
			continue;
		}
		
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
} __attribute__ ((__unused__))

/* compute the instruction_engine everytime
 * a new raw is added.
 *
 * convertion : done
 * testing : works
 */
static void
computeRawEngine(RAW_ENGINE *toadd)
{
	register EBUF *tmp;	
	register INSTRUCTION_ENGINE *buf = NULL, *cur = NULL, *last = NULL;
	register u32 current; /* current number of elements in instruction */	

	temp_count++;

	tmp = _Queue;
	
	buf = Pull_Data_From_Pool(POOL_QUEUE);
	
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
			return;
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
			return;
		}
	}
	else
	{
		first_element = buf;
		last_element = buf;
		/* Debug_Val(0, "Just placed the frame as the first element, starting the queue\n"); */
		return;
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
}

static void
cleanPixels()
{
	EBUF *tmp;
	Rectan buf;
	PIXEL_ENGINE *pix;
	u32 current;
	
	tmp = _Pixel;

	current = Neuro_GiveEBufCount(tmp);
	
	if (current <= 0)
		return;

	
	if (!Neuro_EBufIsEmpty(tmp))
	{
		while (current-- > 0)
		{
			pix = Neuro_GiveEBuf(tmp, current);
			buf.x = pix->x;
			buf.y = pix->y;
			
			buf.w = 1;
			buf.h = 1;
			
			Lib_BlitObject(background, &buf, sclScreen, &buf);
		}
	}
	Neuro_CleanEBuf(&_Pixel);
}

/* 
 * convertion : done
 * testing : works
 */
static void
draw_objects()
{
	Rectan isrc, idst;
	INSTRUCTION_ENGINE *cur;
		
	if (Neuro_EBufIsEmpty(_Queue))
		return;	

	cur = first_element;

	/* start the real drawing */
	while (cur)
	{		
		memcpy(&isrc, &cur->current->src, sizeof(Rectan));
		memcpy(&idst, &cur->current->dst, sizeof(Rectan));

		if (cur->current->override)
		{
			Rectan *buf;

			buf = (Rectan*)cur->current->override;
			
			isrc.x = buf->x;
			isrc.y = buf->y;
			isrc.h = buf->h;
			isrc.w = buf->w;

			free(cur->current->override);
			cur->current->override = NULL;
			
			

			buf = (Rectan*)cur->current->override2;
			
			idst.x = buf->x;
			idst.y = buf->y;
			idst.h = buf->h;
			idst.w = buf->w;

			free(cur->current->override2);
			cur->current->override2 = NULL;		
		}
		
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
			
#if handle_double_rectangle
				if (cur->current->double_rectangle == 1)
				{
					/* special case where we have to draw 
					 * an identical rectangle on the other
					 * side of the image.
					 */
					cur->current->double_rectangle = 0;
					
					/* find out if its an horizontal or
					 * vertical mirror we need.
					 */
					if (isrc.w == cur->current->src.w)
					{
						/* this is the horizontal mirror. */
						
						isrc.y = abs(isrc.y - cur->current->src.h);
					}

					if (isrc.h == cur->current->src.h)
					{
						/* this is the vertical mirror. */
						
						isrc.x = abs(isrc.x - cur->current->src.w);
					}
					
					Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen, &idst);
				}
#endif /* handle_double_rectangle */
				
				cur->current->type = TDRAW_SDRAWN;
			}
			break;
			
			case TDRAW_DYNAMIC:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);

				cur->current->type = TDRAW_DYNAMIC_CLEAN;
				
				if (dynamic_debug)
					Debug_Val(0, "Dynamic : Tagging addr %x to clean\n", cur);
			}
			break;
			
			/*case TDRAW_DYNAMIC_CLEAN:
			{
				Lib_BlitObject(cur->current->surface_ptr, &isrc, 
						sclScreen2, &idst);
			}
			break;*/

			
			default:
			break;
		}


		cur = cur->next;
	}

	drawn_last_cycle = 1;
	/* Lib_FillRect(sclScreen, &test_BoundFix, 0); */
}

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 *
 *   convertion : done
 *   testing : works
 */
static void 
AddDrawingInstruction(u8 layer, u8 type, Rectan *isrc, Rectan *idst, void *isurface)
{
	RAW_ENGINE *buf = NULL;
	Rectan tIsrc, tIdst;
	

	memcpy(&tIsrc, isrc, sizeof(Rectan));
	memcpy(&tIdst, idst, sizeof(Rectan));	

	if (BoundFixChecker(&screenSize, &tIsrc, &tIdst) == 1)
	{
		Debug_Val(10, "a drawing instruction was dropped because its destination is outbound");
		return;
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
	
	buf = Pull_Data_From_Pool(POOL_RAWENGINE);
	if (buf == NULL)
	{
		Neuro_AllocEBuf(_Raw, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
		buf = Neuro_GiveCurEBuf(_Raw);
	}
	
	buf->layer = layer;
	buf->type = type;
	memcpy(&buf->src, &tIsrc, sizeof(Rectan));
	memcpy(&buf->dst, &tIdst, sizeof(Rectan));
	buf->surface_ptr = isurface;
	
	computeRawEngine((RAW_ENGINE*)buf);

	draw_this_cycle = 1;
}

/* clean_drawn_objects() might have cleaned objects
 * that should be drawn so we will redraw those in this
 * function.
 */
static void
redraw_erased_for_object(INSTRUCTION_ENGINE *indep)
{
	Rectan buf, indep_body;
	INSTRUCTION_ENGINE *cur;
	int bounds_ret = 0;


	indep_body.x = indep->current->dst.x;
	indep_body.y = indep->current->dst.y;
	indep_body.w = indep->current->src.w;
	indep_body.h = indep->current->src.h;
	

	if (first_element == NULL)
		return;
	
	cur = first_element;

	while (cur)
	{		
		
		if (cur == indep)
		{
			cur = cur->next;
			continue;
		}
			
		if (cur->current->type == TDRAW_SDRAWN)
		{
			
			buf.x = cur->current->dst.x;
			buf.y = cur->current->dst.y;
			buf.w = cur->current->src.w;
			buf.h = cur->current->src.h;
			
			bounds_ret = Neuro_BoundsCheck(&indep_body, &buf);
			/* bounds_ret = 2; */	
			
			if (bounds_ret == 0)
			{	
				/*Lib_BlitObject(cur->current->surface_ptr, &cur->current->src, 
						sclScreen, &cur->current->dst);*/

				cur->current->type = TDRAW_STATIC;
			}

			if (bounds_ret == 2 || bounds_ret == 3)
			{
				Rectan *isrc, *idst;

				
				isrc = calloc(1, sizeof(Rectan));
				idst = calloc(1, sizeof(Rectan));
					
				memcpy(isrc, &cur->current->src, sizeof(Rectan));
				memcpy(idst, &cur->current->dst, sizeof(Rectan));

				Neuro_VerticalBoundFix(&indep_body, isrc, idst);
				Neuro_HorizontalBoundFix(&indep_body, isrc, idst);
				
				cur->current->override = isrc;
				cur->current->override2 = idst;

				cur->current->type = TDRAW_STATIC;
					
				if (bounds_ret == 3)
					cur->current->double_rectangle = 1;
				
			}
			
			/* Debug_Val(0, "object end\n"); */
		}	
		cur = cur->next;
	}
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
		buf.x = cur->current->dst.x;
		buf.y = cur->current->dst.y;
		buf.w = cur->current->src.w;
		buf.h = cur->current->src.h;
			
		if (cur->current->type == TDRAW_DYNAMIC_CLEAN)
		{
			/*if (background)
				Lib_BlitObject(background, &buf, sclScreen2, &buf);
			else
				Lib_FillRect(sclScreen2, &buf, 0);
			*/
		
			if (dynamic_debug)
				Debug_Val(0, "Dynamic : cleaning address %x\n", cur);
			
			Lib_FillRect(sclScreen2, &buf, 0);
			
			redraw_erased_for_object(cur);

			if (last)
				last->next = cur->next;
		
			/* check to see if the element cur is either first_element 
			 * or last_element and if so, we will destituate it.
			 */
			if (cur == first_element)
				first_element = cur->next;

			if (cur == last_element)
			{
				last_element = last;
				last_element->next = NULL;
			}
			
			
			if (use_memory_pool)
				Push_Data_To_Pool(POOL_QUEUE, cur);
			else
			{
				INSTRUCTION_ENGINE *temp;
				
				temp = cur;
				cur = cur->next;

				Neuro_SCleanEBuf(_Raw, temp->current);
				Neuro_SCleanEBuf(_Queue, temp);
				continue;
			}
		}
#if temp
		else
		{
			if (cur->current->type == TDRAW_DYNAMIC)
			{
				cur->current->type = TDRAW_DYNAMIC_CLEAN;
				
				if (dynamic_debug)
					Debug_Val(0, "Dynamic : Tagging addr %x to clean\n", cur);
			}
		}
#endif /* temp */
		
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
}

static void
Queue_All_To_Pool()
{
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
}

/* returns a pointer corresponding to the type or
 * NULL if none found 
 */
static void *
Pull_Data_From_Pool(u8 type)
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

/* function to put a new element in the pool */
static void
Push_Data_To_Pool(u8 type, void *data)
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

/*--- Global Functions ---*/

void
Neuro_SetFrameSkip(u32 frameskip)
{
	frameSkip = frameskip;
}

void
Neuro_SetFpsLimit(u32 fpsLimit)
{
	fps_limit = fpsLimit;
}

void
Neuro_GiveScreenSize(u32 *width, u32 *height)
{
	/* will be changed so programs that call Neuro will be 
	 * able to change the resolution dynamically when it starts
	 * so the screen size values will be in variables rather than
	 * hardcoded in macros.
	 */
	*width = SCREEN_X;
	*height = SCREEN_Y;
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
	if (use_memory_pool)
	{
		Raw_Engine_All_To_Pool();
		Queue_All_To_Pool();
	}
	else
	{
		Neuro_CleanEBuf(&_Raw);
		Neuro_CleanEBuf(&_Queue);

		Neuro_CreateEBuf(&_Raw);
		Neuro_CreateEBuf(&_Queue);

		Neuro_SetcallbEBuf(_Raw, cleanRawEngineElement);
	}
	/*Debug_Val(0, "Real Elements total %d\n", temp_count);
	temp_count = 0;
	Debug_Val(0, "Raw total %d\n", Neuro_GiveEBufCount(_Raw));
	Debug_Val(0, "Queue total %d\n", Neuro_GiveEBufCount(_Queue));
	Debug_Val(0, "after Pool total %d\n", Neuro_GiveEBufCount(_pool));*/
	
	first_element = NULL;
	last_element = NULL;
}

void
Neuro_GiveFPS(t_tick *output)
{
	*output = lFps;
}

void
Neuro_PushStaticDraw(u8 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	AddDrawingInstruction(layer, TDRAW_STATIC, isrc, idst, isurface);
}

void
Neuro_PushDynamicDraw(u8 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	AddDrawingInstruction(layer, TDRAW_DYNAMIC, isrc, idst, isurface);
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
	/* Lib_Flip(screen); */
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

void
Neuro_PutPixel(u32 x, u32 y, u32 pixel)
{
	EBUF *tmp;
	PIXEL_ENGINE *buf;
	u32 current;
	Rectan check;

	
	check.x = x;
	check.y = y;
	check.h = 1;
	check.w = 1;
	
	if (secureBoundsCheck(&check))
	{
		printf("Unsecure Pixel position have been catched, dropping the instruction\n");
		return;
	}

	tmp = _Pixel;
	Neuro_AllocEBuf(tmp, sizeof(PIXEL_ENGINE*), sizeof(PIXEL_ENGINE));
	
	current = Neuro_GiveEBufCount(tmp);
	buf = Neuro_GiveEBuf(tmp, current);
	
	Neuro_RawPutPixel(sclScreen, x, y, pixel);
	
	buf->x = x;
	buf->y = y;
}

void *
Neuro_GetScreenBuffer()
{
	return (void*)sclScreen;
}

u32
Neuro_GetPixel(u32 x, u32 y)
{
	return Neuro_RawGetPixel(screen, x, y);
}

void
Neuro_CleanPixels()
{
	clean_pixel_in_this_cycle = 1;
}

/*--- Poll ---*/

/* convertion : done
 * testing : works
 */
void
Graphics_Poll()
{	
	if (debug_instruction_buffer)
		Debug_Val(0, "cycle\n");

	if (clean_pixel_in_this_cycle)
	{
		cleanPixels();
		clean_pixel_in_this_cycle = 0;
	}	
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
	int _err_;
	ltime = time(NULL);
	
	_err_ = 0;
	/* will need to be configurable from the projects that use Neuro */
	
	if (screen_buffer)
	{
		_err_ = Lib_VideoInit(&screen, &sclScreen);
	}
	else
	{
		_err_ = Lib_VideoInit(&screen, NULL);
		sclScreen = screen;
	}

	if (second_screen_buffer)
	{
		sclScreen2 = Lib_CreateVObject(0, SCREEN_X, SCREEN_Y, Lib_GetDefaultDepth(), 0, 
				0, 0, 0);
		Lib_SetColorKey(sclScreen2, 0);
	}
	else
	{
		sclScreen2 = sclScreen;
	}

	
	Neuro_CreateEBuf(&_Drawing);
	Neuro_CreateEBuf(&_Raw);
	Neuro_CreateEBuf(&_Queue);
	Neuro_CreateEBuf(&_Pixel);


	Neuro_SetcallbEBuf(_Raw, cleanRawEngineElement);
	
	if (use_memory_pool)
		Neuro_CreateEBuf(&_pool);

	
	screenSize.x = 0;
	screenSize.y = 0;
	screenSize.w = SCREEN_X;
	screenSize.h = SCREEN_Y;
	
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
	Debug_Val(0, "Pool total %d\n", Neuro_GiveEBufCount(_pool));
	
	cleanDrawing();
	cleanRaw();
	cleanQueue();
	Neuro_CleanEBuf(&_Pixel);

	Neuro_CleanEBuf(&_pool);
	
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
