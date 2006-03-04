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
#define debug_instruction_buffer2 0
 
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
 
typedef struct RAW_ENGINE
{
	u8 layer; /* the drawing level that the surface is drawn */
	Rectan src;
	Rectan dst; /* will need to memcpy the data because 
			* this will be used beyond the 
			* scope of the calling function.
			*/
	void *surface_ptr; /* only the pointer needed cause its "static" */
	v_object *former_area; /* contains an allocated variable that contains 
				* what we drawn right before the engine drawn 
				* anything to this position. 
				*/
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

typedef struct DRAWING_ELEMENTS
{
	void (*callback)(void);
}DRAWING_ELEMENTS;

typedef struct PIXEL_ENGINE
{
	u32 x, y;
}PIXEL_ENGINE;

static v_object *screen; /* the screen surface */
static v_object *sclScreen; /* attempt to do a double buffered screen */

static v_object *background; /* the background image */


static INSTRUCTION_ENGINE *last_element;

static EBUF *_Drawing;
static EBUF *_Raw;
static EBUF *_Queue;
static EBUF *_Pixel;

/* buffered structs */
static EBUF *b_Raw;
static EBUF *b_Queue;

/* screen size */
static Rectan screenSize;

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

/* 1 is that we got instructed by external program to clean this cycle */
static u8 clean_this_cycle;

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
static void print_queue2() __attribute__ ((__unused__));

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
	last_element = NULL;
}

static void
print_queue() 
{
	INSTRUCTION_ENGINE *cur;
	
	if (Neuro_EBufIsEmpty(_Queue))
		return;
	cur = Neuro_GiveEBuf(_Queue, 0);
	printf("Queue address %d\n", (int)cur);
	
	while (cur != NULL)
	{
		printf("layer #%d\n", cur->current->layer);
		if (cur->next == Neuro_GiveEBuf(_Queue, 0))
		{
			printf("Error- this element points to the beginning element\n");
			break;
		}
		else
			cur = cur->next;
	}
} __attribute__ ((__unused__))

static void
print_queue2() 
{
	INSTRUCTION_ENGINE *cur;
	
	if (Neuro_EBufIsEmpty(b_Queue))
		return;
	cur = Neuro_GiveEBuf(b_Queue, 0);
	printf("b Queue address %d\n", (int)cur);


	while (cur != NULL)
	{
		printf("b layer #%d\n", cur->current->layer);
		if (cur->next == Neuro_GiveEBuf(b_Queue, 0))
		{
			printf("b Error- this element points to the beginning element\n");
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
	register INSTRUCTION_ENGINE *buf = NULL, *cur = NULL, *last = NULL, *temp = NULL;
	register u32 current; /* current number of elements in instruction */	


	if (dont_draw_this_cycle && drawn_last_cycle)
	{
		return;
		/*if (drawn_last_cycle)
			clean_drawn_objects();*/
	}

	
	tmp = _Queue;	
	Neuro_AllocEBuf(tmp, sizeof(INSTRUCTION_ENGINE*), sizeof(INSTRUCTION_ENGINE));
	
	current = Neuro_GiveEBufCount(tmp);

	buf = Neuro_GiveEBuf(tmp, current);
	
	buf->current = toadd;
	buf->next = NULL;
	
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
			/* printf("Placed the frame at the end of the queue\n"); */
			return;
		}
	}
	else
	{
		last_element = buf;
		/* printf("Just placed the frame as the first element, starting the queue\n"); */
		return;
	}

	cur = Neuro_GiveEBuf(tmp, 0);
	while (cur != NULL)
	{
		/*  printf("looped %d\n", (int)cur); */
		if (cur->current->layer > buf->current->layer)
		{
			/*printf("Event current layer %d > toadd layer %d\n",  
					cur->current->layer, 
					toadd->layer); 
			*/
			
			/* to avoid death loops */
			if (cur->next == buf)
				cur->next = NULL;
			
			if (cur == Neuro_GiveEBuf(tmp, 0))
			{
				/* switch **buf with the current position */
				temp = Neuro_GiveEBuf(tmp, 0);
				Neuro_SetEBuf(tmp, Neuro_GiveEBufAddr(tmp, 0), buf);
				Neuro_SetEBuf(tmp, Neuro_GiveEBufAddr(tmp, current), temp);

				/*printf("Beginning LL change : cur %d, buf[0][0] %d\n", 
						(int)cur, 
						(int)buf[0]);
				*/
				cur = Neuro_GiveEBuf(tmp, 0);
				if (Neuro_GiveEBuf(tmp, 0) == buf)
				{
					printf("huge problem, it is going to put its next element as the same node as itself, creating a death loop!!\n");
					cur->next = NULL;
				}
				else	
					cur->next = temp;
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
	EBUF *tmp;
	Rectan buf;
	INSTRUCTION_ENGINE *cur;

	tmp = b_Queue;
	
#if debug_instruction_buffer2
	printf("B BEGIN debug print\n");
	print_queue2();
	printf("B END debug print\n");
#endif /* debug_instruction_buffer2 */
		
	/* start the real drawing */
	tmp = _Queue;
	
	if (!Neuro_EBufIsEmpty(tmp))
		cur = Neuro_GiveEBuf(tmp, 0);
	else
		return;
	
	while (cur)
	{
		/*u8 bpp;
		u32 rmask, gmask, bmask, amask;
		Rectan bufa;*/
		
		buf.x = cur->current->dst.x;
		buf.y = cur->current->dst.y;
		buf.w = cur->current->src.w;
		buf.h = cur->current->src.h;

		/* copy the emplacement of where the surface will be drawn to
		 * so we can revert to it when we clean. I'm not totally sure 
		 * if we should use sclScreen(the buffer) or Screen(the screen itself).
		 * I'll use sclScreen and see how it goes.
		 */
		
		/*Lib_GetVObjectData(sclScreen, NULL, NULL, NULL, NULL, NULL, NULL,
				&bpp, &rmask, &gmask, &bmask, &amask);*/
		
		/*cur->current->former_area = Lib_CreateVObject(0x00000000, buf.w, 
				buf.h, bpp, rmask, gmask, bmask, amask);*/

		/*
		bufa.x = 0;
		bufa.y = 0;
		bufa.w = cur->current->src.w;
		bufa.h = cur->current->src.h;
		*/
		
		/*Lib_BlitObject(sclScreen, &buf, cur->current->former_area, &bufa);*/
		
		
		/* draw the surface_ptr to the screen buffer. */
		Lib_BlitObject(cur->current->surface_ptr, &cur->current->src, sclScreen, 
					&cur->current->dst);

		/* as a test, we will draw the former_area buffer to see if it works...
		 * it didnt work :L 
		 */
		/*Lib_BlitObject(cur->current->former_area, &bufa, sclScreen, &cur->current->dst);*/
		cur = cur->next;
	}

	drawn_last_cycle = 1;
	/* Lib_FillRect(sclScreen, &test_BoundFix, 0); */
}


/* */
static void
clean_drawn_objects()
{
	EBUF *tmp;
	Rectan buf;
	INSTRUCTION_ENGINE *cur;

	tmp = b_Queue;

	
	if (tmp)
	{
		if (!Neuro_EBufIsEmpty(tmp))
			cur = Neuro_GiveEBuf(tmp, 0);
		else
			return;
	
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
			
			if (background)
				Lib_BlitObject(background, &buf, sclScreen, &buf);
			else
				Lib_FillRect(sclScreen, &buf, 0);
			
				
			/* Lib_BlitObject(cur->current->former_area, NULL, sclScreen, &buf); */
			
			/*
			if (cur->current->former_area)
			{
				Lib_FreeVobject(cur->current->former_area);
			}
			*/
			
			cur = cur->next;
		}
	}
	/* TODO : check if it all works without a background. */
	/*
	else if (!background)
	{
		Lib_FillRect(sclScreen, 0, 0);
	}
	*/
	drawn_last_cycle = 0;
}


static void
clean_queue()
{	
	Neuro_CleanEBuf(&b_Raw);
	Neuro_CleanEBuf(&b_Queue);
	
	Neuro_CreateEBuf(&b_Raw);
	Neuro_CreateEBuf(&b_Queue);
	
	Neuro_CopyEBuf(b_Raw, _Raw);
	Neuro_CopyEBuf(b_Queue, _Queue);
	
	Neuro_ResetEBuf(_Raw);
	Neuro_ResetEBuf(_Queue);

	last_element = NULL;
#if debug_instruction_buffer2
	printf("-B BEGIN debug print\n");
	print_queue2();
	printf("-B END debug print\n");
#endif /* debug_instruction_buffer2 */

#if debug_instruction_buffer
	printf("-BEGIN debug print\n");
	print_queue();
	printf("-END debug print\n");
#endif /* debug_instruction_buffer2 */

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
Neuro_RefreshScreen()
{
	Rectan buf;
	i32 h, w;

	Neuro_GiveImageSize(background, &w, &h);

	buf.x = 0;
	buf.y = 0;
	buf.w = (i16)w;
	buf.h = (i16)h;
			
	if (background)
		Lib_BlitObject(background, &buf, sclScreen, NULL);
	else
		Lib_FillRect(sclScreen, NULL, 0);
}

void
Neuro_CycleClean()
{
	clean_this_cycle = 1;
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

/* - layer is the priority by which it much be drawn.
 * - src should be used to know which part of the 
 *     surface has to be drawn(for sprites mostly).
 * - dst is the destination X Y on the screen
 * - surface is the pointer of the loaded image. 
 *
 *   convertion : done
 *   testing : works
 */
void 
Neuro_AddDrawingInstruction(u8 layer, Rectan *isrc, Rectan *idst, void *isurface)
{
	register EBUF *tmp = NULL;
	register RAW_ENGINE *buf = NULL;
	register u32 current;
	Rectan tIsrc, tIdst;

	tmp = _Raw;
	

	memcpy(&tIsrc, isrc, sizeof(Rectan));
	memcpy(&tIdst, idst, sizeof(Rectan));	

	if (BoundFixChecker(&screenSize, &tIsrc, &tIdst) == 1)
	{
		Debug_Val(10, "a drawing instruction was dropped because its destination is outbound");
		return;
	}	
	
	/* a square in the middle of the screen to test 
	 * the bound fix checker on an object other than 
	 * the screen itself. 
	 */
	/* BoundFixChecker(&test_BoundFix, &tIsrc, &tIdst); */
	
	Neuro_AllocEBuf(tmp, sizeof(RAW_ENGINE*), sizeof(RAW_ENGINE));
	
	current = Neuro_GiveEBufCount(tmp);
	buf = Neuro_GiveEBuf(tmp, current);
	
	buf->layer = layer;
	memcpy(&buf->src, &tIsrc, sizeof(Rectan));
	memcpy(&buf->dst, &tIdst, sizeof(Rectan));
	buf->surface_ptr = isurface;
	computeRawEngine((RAW_ENGINE*)buf);

	draw_this_cycle = 1;
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
		
		total = Neuro_GiveEBufCount(_Drawing);
		total++;
		while (total-- > 0)
		{
			bufa = Neuro_GiveEBuf(_Drawing, total);
			(*bufa->callback)();
		}
	}
	
#if debug_instruction_buffer
	printf("--BEGIN debug print\n");
	print_queue();
	printf("--END debug print\n");
#endif /* debug_instruction_buffer */

	/* construct the instruction buffer */
	
	/* flush the instruction completely in the order 
	 * presented and clean the raw engine buffer 
	 */
	if (frameSkip_tmp <= 0)
	{
		if (!dont_draw_this_cycle)
		{
			/*if (drawn_last_cycle)
				clean_drawn_objects();*/

			if (clean_this_cycle)
			{
				clean_drawn_objects();

			}
	
			if (draw_this_cycle)
				draw_objects();
		}
		else
			dont_draw_this_cycle = 0;
		
		frameSkip_tmp = frameSkip;
	}
	else
		frameSkip_tmp--;
	
	/* clean some of the most important buffers */
	clean_queue();

	/* update the full screen */
	if (draw_this_cycle || clean_this_cycle)
	{
		Lib_BlitObject(sclScreen, NULL, screen, NULL);

		updScreen(0);
		clean_this_cycle = 0;
		draw_this_cycle = 0;
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
	_err_ = Lib_VideoInit(&screen, &sclScreen);	
	
	Neuro_CreateEBuf(&_Drawing);
	Neuro_CreateEBuf(&_Raw);
	Neuro_CreateEBuf(&_Queue);
	Neuro_CreateEBuf(&_Pixel);
	Neuro_CreateEBuf(&b_Queue);
	Neuro_CreateEBuf(&b_Raw);

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
	cleanDrawing();
	cleanRaw();
	cleanQueue();
	Neuro_CleanEBuf(&b_Queue);
	Neuro_CleanEBuf(&b_Raw);
	Neuro_CleanEBuf(&_Pixel);
	Lib_FreeVobject(screen);
	Lib_FreeVobject(sclScreen);
	
	Lib_VideoExit();
}
