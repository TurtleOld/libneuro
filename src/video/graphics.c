
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
 * main Graphics source file and main interface
 * for this module.
 */

/*--- Extern Headers Including ---*/
#include <time.h> /* to calculate the frames per second (fps) */
 
/*--- Local Headers Including ---*/
#include <extlib.h> /* we need to init the video and clean it... also use the blitter */
#include <ebuf.h> /* no longer needed */

/*--- local module main header ---*/
#include "video.h"

/*--- Main Module Header ---*/
#include <graphics.h>
 
/*--- Global Variables ---*/

/*--- Static Variables ---*/

static v_object *screen; /* the screen surface */
static v_object *sclScreen; /* attempt to do a double buffered screen (for the static type) */
static v_object *sclScreen2; /* another screen buffer used for the dynamic type */


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
static u8 safe_draw_operation = 1;

/* last frame we redrawn the pixels? */
/* static u8 lastPdraw; */


/*--- Static Prototypes ---*/

/* update only a part of the screen */
static void updScreen(Rectan *rect);

/*--- Static Functions ---*/

static void
updScreen(Rectan *rect)
{
	Lib_UpdateRect(screen, rect);
}

/*--- Global Functions ---*/

/* move that to coredraw.c */
u8
Graphics_DrawIsPresent(v_elem *elem)
{
	INSTRUCTION_ENGINE *tmp = NULL;
	u32 total = 0;

	if (safe_draw_operation == 0)
		return 0;

	if (!elem)
		return 0;


	if (!elem->current)
		return 0;

	if (!elem->current->surface_ptr)
		return 0;

	total = Neuro_GiveEBufCount(Graphics_GetQueueBuffer()) + 1;

	while(total-- > 0)
	{
		tmp = Neuro_GiveEBuf(Graphics_GetQueueBuffer(), total);

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
	Lib_GetScreenSize(width, height);
}

void
Neuro_RedrawScreen()
{
	draw_this_cycle = 1;
}

void
Graphics_ResetScreenDraw()
{
	draw_this_cycle = 0;
}

void
Graphics_SetDrawnLastCycle()
{
	drawn_last_cycle = 1;
}

/* clean the whole screen */
void
Neuro_RefreshScreen()
{
	Lib_FillRect(sclScreen, NULL, 0);

	Graphics_PainterReset();

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
	/* obsoleted */
}

void 
Neuro_AddDirectDrawing(Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Lib_BlitObject(isurface, isrc, sclScreen, idst);
	
	/*Lib_BlitObject(sclScreen2, NULL, screen, NULL);
	updScreen(0);
	Lib_Flip(screen);*/
}

void *
Neuro_GetScreenBuffer()
{
	return (void*)sclScreen;
}

/*--- Poll ---*/

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
	
	if (debug_instruction_buffer)
	{
		Debug_Val(0, "--BEGIN debug print\n");
		Graphics_DebugPrintQueue();
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
		{
			Graphics_CoreCleanDoneDynamics();
			drawn_last_cycle = 0;
		}
		
		if (debug_instruction_buffer)
		{
			Debug_Val(0, "*BEGIN debug print\n");
			Graphics_DebugPrintQueue();
			Debug_Val(0, "*END debug print\n");
		}

		/* we check to see if the first element 
		 * is a volatile type, if it is we will
		 * make the draw_this_cycle flag 
		 * positive.
		 */
		if (draw_this_cycle == 0)
		{
			INSTRUCTION_ENGINE *tmp;

			tmp = Graphics_GetFirstElem();

			if (tmp)
			{
				if (tmp->current->type == TDRAW_VOLATILE)
					draw_this_cycle = 1;
			}
		}
		
		
		if (draw_this_cycle)
			Graphics_CoreDrawAll();
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



	Graphics_PainterInit();
	
	return _err_;
}

void 
Graphics_Clean()
{		

	Graphics_PainterClean();

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
