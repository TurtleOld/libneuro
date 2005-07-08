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

#define debug_instruction_buffer 1
 
/*--- Extern Headers Including ---*/
#include <stdlib.h>
#include <string.h>
#include <SDL.h>

/*--- Local Headers Including ---*/

/*--- Main Module Header ---*/
#include "graphics.h"

/*--- Global Variables ---*/

/*--- Static Variables ---*/

typedef struct ENGINEBUF
{
	void **buffer;
	u32 mem;
	u32 total;
}ENGINEBUF;
 
typedef struct RAW_ENGINE
{
	char layer; /* the drawing level that the surface is drawn */
	SDL_Rect src;
	SDL_Rect dst; /* will need to memcpy the data because 
			* this will be used beyond the 
			* scope of the calling function.
			*/
	SDL_Surface *surface_ptr; /* only the pointer needed cause its "static" */
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

static SDL_Surface *screen;
static SDL_Surface *sclScreen;


static INSTRUCTION_ENGINE **last_element;

static ENGINEBUF _Drawing;
static ENGINEBUF _Raw;
static ENGINEBUF _Queue;

static Uint32 color_black; /* to fill the screen after each frames */

static int temporary; /* temporary debugging variable, please remove when debugging is done */

/*--- Static Prototypes ---*/

static void cleanEngineBuffer(ENGINEBUF *eng);
static void cleanDrawing();
static void cleanQueue();
static void cleanRaw();

static void computeRawEngine(RAW_ENGINE *toadd);
#if obsolete
static void compute_instructionqueue();
#endif /* obsolete */

#if debug_instruction_buffer
static void print_queue();
#endif /* debug_instruction_buffer */

static void flush_queue();
/* update only a part of the screen */
static void updScreen(SDL_Rect *rect);
/* security, check if a rect is in bound with the main screen */
static int secureBoundsCheck(SDL_Rect *rect);

/*--- Static Functions ---*/

/* check if the rectangle can be securly blit to the main screen */
static int
secureBoundsCheck(SDL_Rect *rect)
{
	/* normal bounds check */
	if (rect->x < 0 || rect->y < 0 || rect->x >= SCREEN_X || rect->y >= SCREEN_Y)
		return 1;
	/* now see if the width and height coords are in bound too */
	if (rect->x + rect->w >= SCREEN_X || rect->y + rect->h >= SCREEN_Y)
		return 1;
	return 0;
}

static void
updScreen(SDL_Rect *rect)
{
	SDL_UpdateRect(screen, rect->x, rect->y, rect->w, rect->h);
}


static void
cleanEngineBuffer(ENGINEBUF *eng)
{
	void ***buf;
	u32 i;
	
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

	free(*buf);
	*buf = NULL;
	
	eng->total = 0;
	eng->buffer = NULL;
	eng->mem = 0;
	last_element = NULL;
	/* printf("cleaned the Engine buffers\n"); */
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
allocEngineBuf(ENGINEBUF *eng, size_t sptp, size_t sobj)
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
		*buf = realloc(*buf, sptp * (MEMORY_ALLOC_OVERH + total));
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

#if debug_instruction_buffer
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
#endif /* debug_instruction_buffer */

/* compute the instruction_engine everytime
 * a new raw is added.
 *
 * convertion : done
 * testing : TODO
 */
static void
computeRawEngine(RAW_ENGINE *toadd)
{
	ENGINEBUF *tmp;	
	INSTRUCTION_ENGINE ***buf = NULL, *cur = NULL, *last = NULL, *temp = NULL;
	u32 current; /* current number of elements in instruction */
		
	tmp = &_Queue;
	allocEngineBuf(tmp, sizeof(INSTRUCTION_ENGINE*), sizeof(INSTRUCTION_ENGINE));
	
	buf = (INSTRUCTION_ENGINE***)&tmp->buffer;	
	current = tmp->total - 1;
	
	/* printf("Raw computing Cycle ptr %d\n", (int)(*buf)[current]); */
	
	/* add the data to the end of the queue */
	printf("will place layer %d in the correct order\n", toadd->layer);
	(*buf)[current]->current = toadd;
	(*buf)[current]->next = NULL;	

	if (last_element != NULL)
	{
		if ((*last_element)->current == NULL)
		{
			printf("CAUGHT ERROR in last_element current == %d -- debug %d\nDropping this call\n", 
					current, 
					(int)(*last_element)->current);
			return;
		}
		/* printf("last_element layer %d\n", last_element->current->layer); */
		if ((*last_element)->current->layer <= (*buf)[current]->current->layer)
		{
			(*last_element)->next = (*buf)[current];
			last_element = &(*buf)[current];
			printf("proof layer %d real layer %d ptr %d\n",
					(*buf)[current]->current->layer,
					(*last_element)->current->layer,
					(int)(*last_element)->current);
			return;
		}
	}
	else
	{
		last_element = &(*buf)[current];
		return;
	}

	cur = **buf;
	while (cur != NULL)
	{
		printf("looped %d\n", (int)cur);
		if (cur->current->layer > (*buf)[current]->current->layer)
		{
			printf("Event current layer %d > toadd layer %d\n",  
					cur->current->layer, 
					toadd->layer); 
			
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
			printf("nothing to be done\n");
		}
		last = cur;
		cur = cur->next;
	}
#if debug_instruction_buffer
	printf("BEGIN inside computeRawEngine debug print\n");
	print_queue();
	printf("END inside computeRawEngine debug print\n");
#endif /* debug_instruction_buffer */

}

/* 
 * convertion : done
 * testing : TODO
 */
static void
flush_queue()
{
	ENGINEBUF *tmp = &_Queue;
	SDL_Rect buf;
	INSTRUCTION_ENGINE *cur;
	int temporar = 0;
	
	cur = *tmp->buffer;
	/* printf("cycle\n"); */
	
	while (cur)
	{
		printf("flushing an instruction : layer %d\n", cur->current->layer);
		buf.x = cur->current->dst.x;
		buf.y = cur->current->dst.y;
		buf.w = cur->current->src.w - 1;
		buf.h = cur->current->src.h - 1;
		if (!secureBoundsCheck(&buf))
		{
			SDL_BlitSurface(cur->current->surface_ptr, &cur->current->src, screen, &cur->current->dst);
			updScreen(&buf);
		}
		else
		{
			printf("catched an unsecure coordinate %d %d %d %d\n", buf.x, buf.y, buf.w - 1, buf.h - 1);
		}
		if (cur->next == NULL)
			printf("the next elements seems to be NULL\n");
		cur = cur->next;
		temporar++;
	}
	if (temporar < temporary)
		printf("theres a problem with the linked list, the number of calls to it doesnt reflect the number of elements in the linked list\n");
	temporary = 0;
}

static void
clean_queue()
{
	cleanRaw();
	cleanQueue();
}

/*--- Global Functions ---*/

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 *
 *   convertion : done
 *   testing : TODO
 */
void 
Graphics_AddDrawingInstruction(u8 layer, void *isrc, void *idst, void *isurface)
{
	ENGINEBUF *tmp = NULL;
	RAW_ENGINE ***buf = NULL;
	u32 current;
	SDL_Rect *src;
	SDL_Rect *dst;
	SDL_Surface *surface;

	printf("new layer %d\n", layer);
	tmp = &_Raw;
	allocEngineBuf(tmp, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
	buf = (RAW_ENGINE***)&tmp->buffer;
	current = tmp->total - 1;
	
	src = (SDL_Rect*)isrc;
	dst = (SDL_Rect*)idst;
	surface = (SDL_Surface*)isurface;		
	
	(*buf)[current]->layer = layer;
	memcpy(&(*buf)[current]->src, src, sizeof(SDL_Rect));
	memcpy(&(*buf)[current]->dst, dst, sizeof(SDL_Rect));
	(*buf)[current]->surface_ptr = surface;
	computeRawEngine((RAW_ENGINE*)(*buf)[current]);
	temporary++;
}

void 
Graphics_AddDirectDrawing(void *isrc, void *idst, void *isurface)
{
	SDL_Rect *dst = (SDL_Rect*)idst;
	SDL_Rect *src = (SDL_Rect*)isrc;
	SDL_BlitSurface((SDL_Surface*)isurface, src, screen, dst);
	/* SDL_UpdateRect(screen, dst->x, dst->y, src->w, src->h); */
	/* printf("%d %d %d %d\n", dst->x, dst->y, src->w, src->h); */
	SDL_UpdateRect(screen, 0, 0, 0, 0);
}

/* external modules call this function
 * to add their callback functions to 
 * be ran in this engine's loop.
 *
 * convertion : done
 * testing : TODO
 */
void 
Graphics_AddDrawingElement(void (*func)())
{
	ENGINEBUF *tmp = NULL;
	DRAWING_ELEMENTS ***buf = NULL;
	u32 current;
	
	tmp = &_Drawing;
	allocEngineBuf(tmp, sizeof(DRAWING_ELEMENTS*), sizeof(DRAWING_ELEMENTS));
	
	buf = (DRAWING_ELEMENTS***)&tmp->buffer;
	current = tmp->total - 1;

	*(*buf)[current] = func;

	printf("proof %d real %d\n", (int)*(*buf)[current], (int)func);
}

/*--- Poll ---*/

/* convertion : done
 * testing : TODO
 */
void
Graphics_Poll()
{
	ENGINEBUF *tmp;
	DRAWING_ELEMENTS ***buf;
	SDL_Rect rect;
	u32 loo = 0;
	const u32 frameSkipMax = 0;
	static u32 frameSkip = 0;
	/* printf("Cycle\n"); */

	tmp = &_Drawing;
	buf = (DRAWING_ELEMENTS***)&tmp->buffer;
	
	if (tmp->total == 0)
		return;
	
	rect.x = 0;
	rect.y = 0;
	rect.w = screen->w;
	rect.h = screen->h;
	/* SDL_FillRect(screen, &rect, color_black); */
	SDL_FillRect(screen, 0, 0);
	SDL_UpdateRect(screen, 0, 0, SCREEN_X, SCREEN_Y);
	/* printf("function pointer %d proof %d\n", (int)drawing_elements_buffer[0][loo], 
			(int)&Graphics_ShowImage); */
	/* printf("debug of the Graphics_Poll function -> drawingelements buffer : number of functions in buffer == %d\n", drawing_elements_buffer_count); */
	while (loo < tmp->total)
	{
		printf("callback ptr %d #%d\n", (int)(*(*buf)[loo]), loo);
		if (*(*buf)[loo] != NULL)
		{
			/* (*test_ptr)(); */
			/* Graphics_ShowImage(); */
			printf("calling callback #%d on %d\n", loo, tmp->total);
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

	/* SDL_BlitSurface(screen, &rect, screen, &rect); */
	/* SDL_UpdateRect(screen, 0, 0, 0, 0); */
	/* SDL_Flip(screen); */
}

/*--- Constructor Destructor ---*/
int
Graphics_Init()
{
	/* will need to be configurable */
	screen = SDL_SetVideoMode(SCREEN_X, SCREEN_Y, 16, SDL_HWSURFACE);

	if (screen == NULL)
	{
		perror("SDL_SetVideoMode()");
		return 1;
	}

	sclScreen = SDL_CreateRGBSurface(SDL_SWSURFACE, SCREEN_X, SCREEN_Y, 16, screen->format->Rmask, screen->format->Gmask, screen->format->Bmask, screen->format->Amask);
	
	color_black = SDL_MapRGB(screen->format, 0, 0, 0);
	return 0;
}

/* convertion : done
 * testing : TODO
 */
void 
Graphics_Clean()
{	
	cleanDrawing();
	cleanRaw();
	cleanQueue();	
	
	SDL_FreeSurface(sclScreen);
}
