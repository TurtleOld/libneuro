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
#define last_element_method 1
 
/*--- Extern Headers Including ---*/
#include <stdlib.h>
#include <string.h>
#include <SDL.h>

/*--- Local Headers Including ---*/

/*--- Main Module Header ---*/
#include "graphics.h"

/*--- Global Variables ---*/

/*--- Static Variables ---*/
typedef struct
{
	char layer; /* if its -1 it is in the INSTRUCTION_ENGINE buffer */
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
typedef struct queue_engine
{
	RAW_ENGINE *current;
	struct queue_engine *next;
}INSTRUCTION_ENGINE;

typedef void (*DRAWING_ELEMENTS)(void);

static SDL_Surface *screen;
static SDL_Surface *sclScreen;


static DRAWING_ELEMENTS ***drawing_elements_buffer;
static unsigned int drawing_elements_buffer_count = 0;

static RAW_ENGINE ***raw_engine_instruction;
static unsigned int raw_engine_instruction_count = 0;
static unsigned int raw_engine_instruction_mem = 0;

static INSTRUCTION_ENGINE ***queue_engine_instruction;
static unsigned int queue_engine_instruction_count = 0;
static unsigned int queue_engine_instruction_mem = 0;
static INSTRUCTION_ENGINE *last_element;


static Uint32 color_black; /* to fill the screen after each frames */


/*--- Static Prototypes ---*/

static void clean_drawingelement_buffer();
static void clean_rawengine();
static void clean_instructionengine();
static void clean_lesser_rawengine();

static void compute_raw_engine(RAW_ENGINE *toadd);
static void compute_instructionqueue();

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
clean_drawingelement_buffer()
{
	unsigned int a = drawing_elements_buffer_count;
	DRAWING_ELEMENTS ***buf = drawing_elements_buffer;
	while (a-- > 0)
	{
		if (buf[0][a] != NULL)
		{
			free(buf[0][a]);
		}
	}
	free(buf[0]);
}

static void 
clean_lesser_rawengine()
{
	unsigned int a = raw_engine_instruction_count;
	while (a-- > 0)
	{
		/* printf("a %d\n", a); */
		free(raw_engine_instruction[0][a]);
		raw_engine_instruction[0][a] = NULL;
	}
	raw_engine_instruction_mem += (sizeof(RAW_ENGINE*) * raw_engine_instruction_count);
	raw_engine_instruction_count = 0;
}

static void 
clean_lesser_instructionengine()
{
	unsigned int a = queue_engine_instruction_count;
	while (a-- > 0)
	{
		/* printf("a %d\n", a); */
		free(queue_engine_instruction[0][a]);
		queue_engine_instruction[0][a] = NULL;
	}
	
	queue_engine_instruction_mem += (sizeof(INSTRUCTION_ENGINE*) * queue_engine_instruction_count);
	queue_engine_instruction_count = 0;
	last_element = NULL;
}

static void
clean_rawengine()
{
	clean_lesser_rawengine();

	/*
	 if (*raw_engine_instruction)
		free(*raw_engine_instruction);
	*raw_engine_instruction = NULL;
	*/
}

static void
clean_instructionengine()
{	
	clean_lesser_instructionengine();
	
	/*
	if (*queue_engine_instruction)
		free(*queue_engine_instruction);
	*queue_engine_instruction = NULL;
	*/
}




/* compute the raw engine and add the stuff to
 * the instruction engine all at once -obsolete-
 */
static void
compute_instructionqueue()
{
	INSTRUCTION_ENGINE ***buf = queue_engine_instruction;
	RAW_ENGINE **raw = *raw_engine_instruction;
	unsigned int indep = raw_engine_instruction_count; /* the current frames to write to the
        						    * screen	
							    */
	unsigned int loo1 = 0, loo2 = 0, loo3 = 0; /* integers used for the loops */
	unsigned int current = 0; /* current number of elements in instruction */
	
		
	if (*raw_engine_instruction == NULL || raw_engine_instruction_count == 0)
		return;

	if (*buf == NULL)
	{
		*buf = (INSTRUCTION_ENGINE**)calloc(1, sizeof(INSTRUCTION_ENGINE*));
		queue_engine_instruction_count = 0;
	}
	else
	{
		clean_instructionengine();
		compute_instructionqueue();
		return;
	}

	while (loo1 < indep)
	{
		/* this loop will do another loop 
		 * which will hold another loop.
		 * this loop will loop around all the
		 * elements in the raw_engine buffer.
		 * and loop to see if theres an element
		 * of lower level. if theres one, the lowest
		 * one will be put in the instruction_engine
		 * buffer.
		 */
		
		buf[0][current] = (INSTRUCTION_ENGINE*)calloc(1, sizeof(INSTRUCTION_ENGINE));
		
		while (loo2 < LAYER_CHECK_PER_FRAMES)
		{
			while (loo3 < indep)
			{
				if (raw[loo3]->layer != -1)
				{
					
				}
				loo3++;
			}
			loo2++;
		}
		loo1++;
	}
	queue_engine_instruction_count = current;
}

#if debug_instruction_buffer
static void
print_queue()
{
	INSTRUCTION_ENGINE *cur = **queue_engine_instruction;
	
	while (cur != NULL)
	{
		printf("layer #%d\n", cur->current->layer);
		if (cur->next == queue_engine_instruction[0][0])
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
 */
static void
compute_raw_engine(RAW_ENGINE *toadd)
{
	INSTRUCTION_ENGINE ***buf = queue_engine_instruction, *cur = NULL, *last = NULL, *temp = NULL;
	unsigned int current = queue_engine_instruction_count; /* current number of elements in instruction */
		
	if (*buf == NULL)
	{
		*buf = (INSTRUCTION_ENGINE**)calloc(10, sizeof(INSTRUCTION_ENGINE*));
		queue_engine_instruction_count = 0;
		queue_engine_instruction_mem = sizeof(INSTRUCTION_ENGINE*) * 10;
	}
	else if (queue_engine_instruction_mem < sizeof(INSTRUCTION_ENGINE*))
	{
		/* printf("debug before realloc mem %d count total %d\n", queue_engine_instruction_mem, queue_engine_instruction_count); */
		*buf = (INSTRUCTION_ENGINE**)realloc(*buf, (sizeof(INSTRUCTION_ENGINE*) * (10 + current)));
		queue_engine_instruction_mem = sizeof(INSTRUCTION_ENGINE*) * 10;

	}

	if (*buf == NULL)
	{
		printf("Out of memory! \n");
		return;
	}
	cur = **buf;
	/* printf("Cycle\n"); */
	
	/* printf("compute_raw_engine current mem %d\n", queue_engine_instruction_mem); */
	/* if (queue_engine_instruction_mem > 0) */
	queue_engine_instruction_mem -= sizeof(INSTRUCTION_ENGINE*);

	buf[0][current] = (INSTRUCTION_ENGINE*)calloc(1, sizeof(INSTRUCTION_ENGINE));
	
	/* add the data to the end of the queue */
	buf[0][current]->current = toadd;
	/* printf("New layer %d\n", toadd->layer); */
	buf[0][current]->next = NULL;
#if last_element_method
	if (last_element != NULL)
	{
		if (last_element->current == NULL)
		{
			printf("CAUGHT ERROR in last_element current == %d\nDropping this call\n", current);
			return;
		}
		/* printf("last_element layer %d\n", last_element->current->layer); */
		if (last_element->current->layer <= buf[0][current]->current->layer)
		{
			last_element->next = buf[0][current];
			queue_engine_instruction_count++;
			last_element = buf[0][current];
			/* printf("proof layer %d real layer %d\n",
					buf[0][current]->current->layer,
					last_element->current->layer);
			*/
			return;
		}
	}
	else
	{
		last_element = buf[0][current];
		return;
	}
#else /* not last_element_method */
	if (current > 0)
	{
		/* search for the last element */
		while (cur != NULL)
		{
			if (cur->next == NULL)
			{
				/* to prevent yet another death loop 
				 * also bail out if the instruction priority
				 * is not higher than the last one.
				 * to make things faster, keep a static ptr
				 * to the last element and refresh it everytime.
				 */
				if (cur->current->layer <= buf[0][current]->current->layer)
				{
					cur->next = buf[0][current];
					queue_engine_instruction_count++;
					return;
				}
				break;
			}
			cur = cur->next;
		}
		/*
		printf("debug previous layer #%d\n", buf[0][current - 1]->current->layer);
		buf[0][current - 1]->next = buf[0][current];
		*/
	}
#endif /* not last_element_method */

	cur = **buf;
	while (cur != NULL)
	{
		/* printf("looped\n"); */
		if (cur->current->layer > toadd->layer)
		{
			/* printf("Event current layer %d > toadd layer %d\n",  
					cur->current->layer, 
					toadd->layer); 
			*/
			/* to avoid death loops */
			if (cur->next == buf[0][current])
				cur->next = NULL;
			
			if (cur == buf[0][0])
			{
				temp = cur;
				buf[0][0] = buf[0][current];
				buf[0][current] = temp;
				/*printf("Beginning LL change : cur %d, buf[0][0] %d\n", 
						(int)cur, 
						(int)buf[0][0]);
				*/
				cur = buf[0][0];
				cur->next = temp;
			}
			else
				buf[0][current]->next = cur;
			if (last != NULL)
			{
				last->next = buf[0][current];
			}
			break;
		}
		last = cur;
		cur = cur->next;
	}
#if debug_instruction_buffer
	printf("BEGIN inside compute_raw_engine debug print\n");
	print_queue();
	printf("END inside compute_raw_engine debug print\n");
#endif /* debug_instruction_buffer */

	queue_engine_instruction_count++;
}

static void
flush_queue()
{
	SDL_Rect buf;
	if (*queue_engine_instruction == NULL)
		return;
	if (**queue_engine_instruction == NULL)
		return;
	INSTRUCTION_ENGINE *cur = **queue_engine_instruction;

	while (cur != NULL)
	{
		
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
		cur = cur->next;
	}
	/*clean_lesser_rawengine();
	clean_lesser_instructionengine();*/
}

static void
clean_queue()
{
	clean_lesser_rawengine();
	clean_lesser_instructionengine();
}

/*--- Global Functions ---*/

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 */
void 
Graphics_AddDrawingInstruction(u8 layer, void *isrc, void *idst, void *isurface)
{
	RAW_ENGINE ***buf = raw_engine_instruction;
	u32 current = raw_engine_instruction_count;
	SDL_Rect *src = (SDL_Rect*)isrc;
	SDL_Rect *dst = (SDL_Rect*)idst;
	SDL_Surface *surface = (SDL_Surface*)isurface;
	
	if (*buf == NULL)
	{
		*buf = (RAW_ENGINE**)calloc(9, sizeof(RAW_ENGINE*));
		raw_engine_instruction_count = 0;
		raw_engine_instruction_mem = sizeof(RAW_ENGINE*) * 8;
	}
	else if (raw_engine_instruction_mem < sizeof(RAW_ENGINE*))
	{
		/* printf("current mem %d\n", raw_engine_instruction_mem); */
		*buf = (RAW_ENGINE**)realloc(*buf, (sizeof(RAW_ENGINE*) * (5 + current)));
		raw_engine_instruction_mem = sizeof(RAW_ENGINE*) * 3;
	}
	
	if (*buf == NULL)
	{
		printf("Out of memory! \n");
		return;
	}
	
	/* printf("ext current mem %d\n", raw_engine_instruction_mem); */
	if (raw_engine_instruction_mem > 0)
		raw_engine_instruction_mem -= sizeof(RAW_ENGINE*);
	buf[0][current] = (RAW_ENGINE*)calloc(1, sizeof(RAW_ENGINE));
	if (buf[0][current] == NULL)	
	{
		printf("Out of memory! \n");
		return;
	}

	
	buf[0][current]->layer = layer;
	memcpy(&buf[0][current]->src, src, sizeof(SDL_Rect));
	memcpy(&buf[0][current]->dst, dst, sizeof(SDL_Rect));
	buf[0][current]->surface_ptr = surface;

	compute_raw_engine((RAW_ENGINE*)buf[0][current]);
	
	raw_engine_instruction_count++;
}

void 
Graphics_AddDirectDrawing(void *isrc, void *idst, void *isurface)
{
	SDL_Rect *dst = (SDL_Rect*)idst;
	SDL_Rect *src = (SDL_Rect*)isrc;
	SDL_BlitSurface((SDL_Surface*)isurface, src, screen, dst);
	SDL_UpdateRect(screen, dst->x, dst->y, src->w, src->h);
	/* printf("%d %d %d %d\n", dst->x, dst->y, src->w, src->h); */
	/* SDL_UpdateRect(screen, 0, 0, 0, 0); */
}


void 
Graphics_AddDrawingElement(void (*func)())
{
	int current = drawing_elements_buffer_count;
	DRAWING_ELEMENTS ***buf = (DRAWING_ELEMENTS***)drawing_elements_buffer;

	if (*buf == NULL)
	{
		*buf = (DRAWING_ELEMENTS**)calloc(1, sizeof(DRAWING_ELEMENTS*));
		drawing_elements_buffer_count = 0;
	}
	else
	{
		*buf = (DRAWING_ELEMENTS**)realloc(*buf, sizeof(DRAWING_ELEMENTS*) * (current + 1));
	}
	if (*buf == NULL)
	{
		printf("Graphics_AddDrawingElement Out of memory! \n");
		return;
	}
	buf[0][current] = (DRAWING_ELEMENTS*)calloc(1, sizeof(DRAWING_ELEMENTS));
	/* printf("DEBUG %d %d\n", sizeof(DRAWING_ELEMENTS*), sizeof(DRAWING_ELEMENTS));
	getchar(); */
	*buf[0][current] = func;
	/* drawing_elements_buffer[0][current] = func; */

	drawing_elements_buffer_count++;
}

/*--- Poll ---*/
void
Graphics_Poll()
{
	SDL_Rect rect;
	unsigned int loo = 0;
	const int frameSkipMax = 0;
	static unsigned int frameSkip = 0;
	/* printf("Cycle\n"); */

	if (drawing_elements_buffer_count == 0)
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
	while (loo < drawing_elements_buffer_count)
	{
		if (drawing_elements_buffer[0][loo] != NULL)
		{
			/* (*test_ptr)(); */
			/* Graphics_ShowImage(); */
			(*drawing_elements_buffer[0][loo])();
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
	
	drawing_elements_buffer = (DRAWING_ELEMENTS ***)calloc(1, sizeof(DRAWING_ELEMENTS**));

	raw_engine_instruction = (RAW_ENGINE ***)calloc(1, sizeof(DRAWING_ELEMENTS**));
	
	queue_engine_instruction = (INSTRUCTION_ENGINE ***)calloc(1, sizeof(DRAWING_ELEMENTS**));
	
	/*
	drawing_elements_buffer = (DRAWING_ELEMENTS ***)HVar_CreateBuffer("Drawing_Elements", 
			clean_drawingelement_buffer);

	raw_engine_instruction = (RAW_ENGINE ***)HVar_CreateBuffer("Raw_Engine", 
			clean_rawengine);
	
	queue_engine_instruction = (INSTRUCTION_ENGINE ***)HVar_CreateBuffer("Instruction_Engine", 
			clean_instructionengine);
	*/
	return 0;
}

void 
Graphics_Clean()
{	
	clean_drawingelement_buffer();
	clean_rawengine();
	clean_instructionengine();
	
	if (drawing_elements_buffer)
		free(drawing_elements_buffer);
	if (raw_engine_instruction)
		free(raw_engine_instruction);
	if (queue_engine_instruction)
		free(queue_engine_instruction);
	
	SDL_FreeSurface(sclScreen);
}

