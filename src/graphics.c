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
 */

#define debug_instruction_buffer 0
 
#define cleanEngineBuffer Neuro_CleanEngineBuffer
#define allocEngineBuf Neuro_AllocEngineBuf

 
/*--- Extern Headers Including ---*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h> /* to calculate the frames per second (fps) */
 
/*--- Local Headers Including ---*/
#include "typedef.h"
#include "extlib.h"

/*--- Main Module Header ---*/
#include "graphics.h"
 
/*--- Global Variables ---*/

/*--- Static Variables ---*/

struct ENGINEBUF
{
	void **buffer;
	u32 mem;
	u32 total;
};
 
typedef struct RAW_ENGINE
{
	u8 layer; /* the drawing level that the surface is drawn */
	Rectan src;
	Rectan dst; /* will need to memcpy the data because 
			* this will be used beyond the 
			* scope of the calling function.
			*/
	void *surface_ptr; /* only the pointer needed cause its "static" */
}RAW_ENGINE;

/* this linked list will be computed 
 * unless, RAW_ENGINE is empty, every frames. 
 * It will not be possible to change entries 
 * int it once they r in, new stuff r added 
 * to the end of this linked list and done
 * stuff r removed, exactly like a fifo pipe.
 */
typedef struct INSTRUCTION_ENGINE
{
	RAW_ENGINE *current;
	struct INSTRUCTION_ENGINE *next;
}INSTRUCTION_ENGINE;

typedef void (*DRAWING_ELEMENTS)(void);

typedef struct PIXEL_ENGINE
{
	u32 x, y;
}PIXEL_ENGINE;

static v_object *screen; /* the screen surface */
static v_object *sclScreen; /* attempt to do a double buffered screen */

static v_object *background; /* the background image */


static INSTRUCTION_ENGINE *last_element;

static ENGINEBUF _Drawing;
static ENGINEBUF _Raw;
static ENGINEBUF _Queue;
static ENGINEBUF _Pixel;

/* buffered structs */
static ENGINEBUF *b_Raw;
static ENGINEBUF *b_Queue;


/* temporary debugging variable, please remove when debugging is done */
static int f_count;


static int fps;
static int lFps;
static int ltime;

/* 1 is that the pixels will be cleaned during this cycle and 0 is no it won't */
static u8 clean_pixel_in_this_cycle;

/* last frame we redrawn the pixels? */
/* static u8 lastPdraw; */


/*--- Static Prototypes ---*/

static void cleanDrawing();
static void cleanQueue();
static void cleanRaw();

static void computeRawEngine(RAW_ENGINE *toadd);

/* debug print of the instruction queue */
static void print_queue() __attribute__ ((__unused__));

static void flush_queue();
/* update only a part of the screen */
static void updScreen(Rectan *rect);
/* security, check if a rect is in bound with the main screen */
static inline int secureBoundsCheck(Rectan *rect) __attribute__ ((__always_inline__));
/* clean the screen of the handled pixels drawn */
static void cleanPixels();

/*--- Static Functions ---*/

/* check if the rectangle can be securly blit to the main screen */
static inline int
secureBoundsCheck(Rectan *rect)
{
	/* normal bounds check */
	if (rect->x >= SCREEN_X || rect->y >= SCREEN_Y)
		return 1;
	/* now see if the width and height coords are in bound too */
	if (rect->x + rect->w >= SCREEN_X || rect->y + rect->h >= SCREEN_Y)
		return 1;
	return 0;
}

static void
updScreen(Rectan *rect)
{
	Lib_UpdateRect(screen, rect);
}

static void
cleanDrawing()
{
	cleanEngineBuffer(&_Drawing);
}

static void 
cleanRaw()
{
	cleanEngineBuffer(&_Raw);
}

static void 
cleanQueue()
{
	cleanEngineBuffer(&_Queue);	
}

static void
print_queue()
{
	INSTRUCTION_ENGINE *cur = *_Queue.buffer;
	
	while (cur != NULL)
	{
		printf("layer #%d\n", cur->current->layer);
		if (cur->next == *_Queue.buffer)
		{
			printf("Error- this element points to the beginning element\n");
			break;
		}
		else
			cur = cur->next;
	}
}

/* compute the instruction_engine everytime
 * a new raw is added.
 *
 * convertion : done
 * testing : seems to work
 */
static void
computeRawEngine(RAW_ENGINE *toadd)
{
	register ENGINEBUF *tmp;	
	register INSTRUCTION_ENGINE ***buf = NULL, *cur = NULL, *last = NULL, *temp = NULL;
	register u32 current; /* current number of elements in instruction */	


	tmp = &_Queue;	
	allocEngineBuf(tmp, sizeof(INSTRUCTION_ENGINE*), sizeof(INSTRUCTION_ENGINE));
	
	buf = (INSTRUCTION_ENGINE***)&tmp->buffer;	
	current = tmp->total - 1;
	
	/* printf("Raw computing Cycle ptr %d\n", (int)(*buf)[current]); */
	
	/* add the data to the end of the queue */
	/* printf("will place layer %d in the correct order\n", toadd->layer); */
	(*buf)[current]->current = toadd;
	(*buf)[current]->next = NULL;	

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

		/* if ((*last_element)->current->layer <= (*buf)[current]->current->layer) */
		{
			last_element->next = (*buf)[current];
			last_element = (*buf)[current];
			/*printf("proof layer %d real layer %d ptr %d\n",
					(*buf)[current]->current->layer,
					(*last_element)->current->layer,
					(int)(*last_element)->current);*/
			/* printf("Placed the frame at the end of the queue\n"); */
			return;
		}
	}
	else
	{
		last_element = (*buf)[current];
		/* printf("Just placed the frame as the first element, starting the queue\n"); */
		return;
	}

	cur = **buf;
	while (cur != NULL)
	{
		/*  printf("looped %d\n", (int)cur); */
		if (cur->current->layer > (*buf)[current]->current->layer)
		{
			/*printf("Event current layer %d > toadd layer %d\n",  
					cur->current->layer, 
					toadd->layer); 
			*/
			
			/* to avoid death loops */
			if (cur->next == (*buf)[current])
				cur->next = NULL;
			
			if (cur == **buf)
			{
				/* switch **buf with the current position */
				temp = **buf;
				**buf = (*buf)[current];
				(*buf)[current] = temp;

				/*printf("Beginning LL change : cur %d, buf[0][0] %d\n", 
						(int)cur, 
						(int)buf[0]);
				*/
				cur = **buf;
				if (**buf == (*buf)[current])
				{
					printf("huge problem, it is going to put its next element as the same node as itself, creating a death loop!!\n");
					cur->next = NULL;
				}
				else	
					cur->next = temp;
			}
			else
				(*buf)[current]->next = cur;
			if (last != NULL)
			{
				last->next = (*buf)[current];
			}
			break;
		}
		else
		{
			/* printf("nothing to be done\n"); */
		}
		last = cur;
		cur = cur->next;
	}
	/* printf("End of the looping process\n"); */
#if debug_instruction_buffer
		printf("BEGIN inside computeRawEngine debug print\n");
		print_queue();
		printf("END inside computeRawEngine debug print\n");
#endif /* debug_instruction_buffer */

}

static void
cleanPixels()
{
	ENGINEBUF *tmp;
	Rectan buf;
	PIXEL_ENGINE ***pix;
	u32 current;
	
	tmp = &_Pixel;
	current = tmp->total;
	pix = (PIXEL_ENGINE***)&tmp->buffer;

	if (current <= 0)
		return;

	/* printf("inside %s %d\n", __FUNCTION__, current); */
	
	if (background)
	{
		while (current-- > 0)
		{
			/* printf("%d\n", current); */
			buf.x = (*pix)[current]->x;
			buf.y = (*pix)[current]->y;
			
			buf.w = 1;
			buf.h = 1;
			
			Lib_BlitObject(background, &buf, sclScreen, &buf);
			/* printf("%d\n", Other_GetPixel(background, buf.x, buf.y));*/
			/* Neuro_PutPixel(buf.x, buf.y, 
					Other_GetPixel(background, buf.x, buf.y));
			*/
		}
	}
	cleanEngineBuffer(&_Pixel);
}

/* 
 * convertion : done
 * testing : seems to work
 */
static void
flush_queue()
{
	ENGINEBUF *tmp;
	Rectan buf;
	INSTRUCTION_ENGINE *cur;

	tmp = b_Queue;
	
	if (tmp && background)
	{
		
		if (tmp->buffer)
			cur = *tmp->buffer;
		else
			return;
		/* printf("cycle\n"); */
	
		/* "reset" the emplacement of the last position of the image
		 * with the background if theres one or with the color black 
		 * if none.
		 */
		while (cur)
		{
			/* printf("flushing an instruction : layer %d\n", cur->current->layer); */
			
			buf.x = cur->current->dst.x;
			buf.y = cur->current->dst.y;
			buf.w = cur->current->src.w - 1;
			buf.h = cur->current->src.h - 1;
			if (!secureBoundsCheck(&buf))
			{
				if (background)
				{
					Lib_BlitObject(background, &cur->current->dst, sclScreen, 
						&cur->current->dst);
	
				}
				/* updScreen(&buf); */
			}
			else
			{
				printf("[reset] catched an unsecure coordinate %d %d %d %d\n", buf.x, buf.y, buf.w - 1, buf.h - 1);
			}
			/* if (cur->next == NULL)
				printf("the next elements seems to be NULL\n");
			*/
			cur = cur->next;
		}
	}
	else if (!background)
	{
		Lib_FillRect(sclScreen, 0, 0);
	}
	
	/* start the real drawing */
	tmp = &_Queue;
	
	if (tmp->buffer)
		cur = *tmp->buffer;
	else
		return;
	
	while (cur)
	{
		/* printf("flushing an instruction : layer %d\n", cur->current->layer); */
		buf.x = cur->current->dst.x;
		buf.y = cur->current->dst.y;
		buf.w = cur->current->src.w - 1;
		buf.h = cur->current->src.h - 1;
		if (!secureBoundsCheck(&buf))
		{
			Lib_BlitObject(cur->current->surface_ptr, 
					&cur->current->src, sclScreen, 
					&cur->current->dst);
		}
		else
		{
			printf("catched an unsecure coordinate %d %d %d %d\n", buf.x, buf.y, buf.w - 1, buf.h - 1);
		}
		/* if (cur->next == NULL)
			printf("the next elements seems to be NULL\n");
		*/
		cur = cur->next;
	}
	f_count = 0;
}

static void
clean_queue()
{
	/*
	cleanRaw();
	cleanQueue();
	*/
	cleanEngineBuffer(b_Raw);
	cleanEngineBuffer(b_Queue);
	b_Raw = &_Raw;
	b_Queue = &_Queue;
}

/*--- Global Functions ---*/

void
Neuro_AllocEngineBuf(ENGINEBUF *eng, size_t sptp, size_t sobj)
{
	void ***buf = NULL;
	u32 total = 0;
	u32 mem = 0;
	
	buf = &eng->buffer;
	total = eng->total;
	mem = eng->mem;
	
	if (mem > MEMORY_ALLOC_OVERH)
	{
		printf("Theres a huge problem, the memory over head allocation doesnt seem to work properly -- debug value : %d\n", mem);
		return;
	}
	/*
	printf("debug : %d\n", sptp);
	printf("before mem %d\n", mem);
	*/
	if (!*buf)
	{
		*buf = calloc(MEMORY_ALLOC_OVERH, sptp);
		total = 0;
		mem = MEMORY_ALLOC_OVERH;
	}
	else if ((mem * sptp) < sptp)
	{
		*buf = realloc(*buf, sptp * (MEMORY_ALLOC_OVERH + total + 1));
		mem = MEMORY_ALLOC_OVERH;
	}
	else
		mem -= 1;
	/*
	printf("after mem %d\n", mem);
	*/
	(*buf)[total] = calloc(1, sobj);
	
	total++;
	eng->total = total;
	eng->mem = mem;
}

void
Neuro_CleanEngineBuffer(ENGINEBUF *eng)
{
	void ***buf;
	u32 i;
	
	if (!eng)
		return;
		
	buf = &eng->buffer;
	i = eng->total;

#if cleanDbg
	printf("total to clean : %d\n", i);
#endif /* cleanDbg */

	while (i-- > 0)
	{
#if cleanDbg
		printf("cleaning i:%d\nptr %d\n", i, (int)(*buf)[i]);
#endif /* cleanDbg */
		if ((*buf)[i])
			free((*buf)[i]);
#if cleanDbg
		else
			printf("the element %d is NULL\n", i);
#endif /* cleanDbg */
	}

	if (*buf)
		free(*buf);
	*buf = NULL;
	
	eng->total = 0;
	eng->buffer = NULL;
	eng->mem = 0;
	last_element = NULL;
	/* printf("cleaned the Engine buffers\n"); */
}

void
Neuro_GiveFPS(t_tick *output)
{
	*output = lFps;
}

/* use this function to set the background */
void
Neuro_AddBackground(v_object *isurface)
{
	background = isurface;
	Rectan buf;
	Rectan src;

	Lib_GiveVobjectProp(background, &buf);

	src.x = 0;
	src.y = 0;
	src.h = buf.h;
	src.w = buf.w;
	
	Lib_BlitObject(background, NULL, sclScreen, &src);
	Lib_Flip(sclScreen);
}

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 *
 *   convertion : done
 *   testing : seg fault at 22 elements
 */
void 
Neuro_AddDrawingInstruction(u8 layer, Rectan *isrc, Rectan *idst, void *isurface)
{
	register ENGINEBUF *tmp = NULL;
	register RAW_ENGINE ***buf = NULL;
	register u32 current;

	/* printf("new layer %d\n", layer); */
	tmp = &_Raw;
	/* printf("-- raw element adding (%d), current total is %d\n", layer, tmp->total); */
	
	/*
	if (f_count < 20)
	{
		f_count++;
		printf("frame count %d total %d\n", f_count, tmp->total);
	}
	else
		return;
	*/
		
	allocEngineBuf(tmp, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
	buf = (RAW_ENGINE***)&tmp->buffer;
	current = tmp->total - 1;
	
	(*buf)[current]->layer = layer;
	memcpy(&(*buf)[current]->src, isrc, sizeof(Rectan));
	memcpy(&(*buf)[current]->dst, idst, sizeof(Rectan));
	(*buf)[current]->surface_ptr = isurface;
	computeRawEngine((RAW_ENGINE*)(*buf)[current]);
}

void 
Neuro_AddDirectDrawing(Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Lib_BlitObject(isurface, isrc, screen, idst);
	/* SDL_UpdateRect(screen, dst->x, dst->y, src->w, src->h); */
	/* printf("%d %d %d %d\n", dst->x, dst->y, src->w, src->h); */
	/* SDL_UpdateRect(screen, 0, 0, 0, 0); */
	Lib_Flip(screen);
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
	ENGINEBUF *tmp = NULL;
	DRAWING_ELEMENTS ***buf = NULL;
	u32 current;
	
	tmp = &_Drawing;
	allocEngineBuf(tmp, sizeof(DRAWING_ELEMENTS*), sizeof(DRAWING_ELEMENTS));
	
	buf = (DRAWING_ELEMENTS***)&tmp->buffer;
	current = tmp->total - 1;

	*(*buf)[current] = func;

	/* printf("proof %d real %d\n", (int)*(*buf)[current], (int)func); */
}

void
Neuro_PutPixel(u32 x, u32 y, u32 pixel)
{
	ENGINEBUF *tmp;
	PIXEL_ENGINE ***buf;
	u32 current;
	Rectan check;

	
	check.x = x;
	check.y = y;
	check.h = 1;
	check.w = 1;
	
	if (secureBoundsCheck(&check))
	{
		DbgP("Unsecure Pixel position have been catched, dropping the instruction\n");
		return;
	}

	tmp = &_Pixel;
	allocEngineBuf(tmp, sizeof(PIXEL_ENGINE*), sizeof(PIXEL_ENGINE));
	
	buf = (PIXEL_ENGINE***)&tmp->buffer;
	current = tmp->total - 1;
	
	Neuro_RawPutPixel(sclScreen, x, y, pixel);
	
	(*buf)[current]->x = x;
	(*buf)[current]->y = y;
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
	ENGINEBUF *tmp;
	DRAWING_ELEMENTS ***buf;
	
	u32 loo = 0;
	const u32 frameSkipMax = 0;
	static u32 frameSkip = 0;
	/* printf("Cycle\n"); */

	tmp = &_Drawing;
	buf = (DRAWING_ELEMENTS***)&tmp->buffer;
	
	if (tmp->total == 0)
		return;

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
		lFps = 0;

	/* printf("function pointer %d proof %d\n", (int)drawing_elements_buffer[0][loo], 
			(int)&Neuro_ShowImage); */
	/* printf("debug of the Neuro_Poll function -> drawingelements buffer : number of functions in buffer == %d\n", drawing_elements_buffer_count); */
	while (loo < tmp->total)
	{
		/* printf("callback ptr %d #%d\n", (int)(*(*buf)[loo]), loo); */
		if (*(*buf)[loo] != NULL)
		{
			/* (*test_ptr)(); */
			/* Neuro_ShowImage(); */
			/* printf("calling callback #%d on %d\n", loo, tmp->total); */
			(*(*buf)[loo])();
		}
		loo++;
	}
	
#if debug_instruction_buffer
	print_queue();
#endif /* debug_instruction_buffer */

	/* construct the instruction buffer */
	
	/* flush the instruction completely in the order 
	 * presented and clean the raw engine buffer 
	 */
	if (frameSkip <= 0)
	{
		flush_queue();
		frameSkip = frameSkipMax;
	}
	else
		frameSkip--;
	
	/* clean some of the most important buffers */
	clean_queue();

	/* update the full screen */
	/* SDL_UpdateRect(screen, 0, 0, 0, 0); */
	/* SDL_Flip(screen); */

	Lib_BlitObject(sclScreen, NULL, screen, NULL);

	/* SDL_UpdateRect(screen, 0, 0, 0, 0); */
	updScreen(0);
		
	/* SDL_Flip(screen); */

	fps++;
}

/*--- Constructor Destructor ---*/
int
Graphics_Init()
{
	int _err_;
	ltime = time(NULL);

	_err_ = 0;
	/* will need to be configurable */
	_err_ = Lib_VideoInit(&screen, &sclScreen);
	
	/* screen = SDL_SetVideoMode(SCREEN_X, SCREEN_Y, 16, SDL_SWSURFACE);

	if (screen == NULL)
	{
		perror("SDL_SetVideoMode()");
		return 1;
	}
	*/

	/* sclScreen = SDL_CreateRGBSurface(SDL_SWSURFACE, SCREEN_X, SCREEN_Y, screen->format->BitsPerPixel, screen->format->Rmask, screen->format->Gmask, screen->format->Bmask, screen->format->Amask); */
	
	/* 
	 * printf("the size of SDL_Rect %d, the size of RAW_ENGINE %d the size of Rectan %d the size of SDL_Surface %d the size of u8 %d\n", 
			sizeof(SDL_Rect), 
			sizeof(RAW_ENGINE), 
			sizeof(Rectan),
			sizeof(SDL_Surface*),
			sizeof(u8)); 
	*/
	return _err_;
}

/* convertion : done
 * testing : seems to work
 */
void 
Graphics_Clean()
{	
	cleanDrawing();
	cleanRaw();
	cleanQueue();
	cleanEngineBuffer(b_Queue);
	cleanEngineBuffer(b_Raw);
	cleanEngineBuffer(&_Pixel);
	Lib_FreeVobject(screen);
	Lib_FreeVobject(sclScreen);
	
	Lib_VideoExit();
}
