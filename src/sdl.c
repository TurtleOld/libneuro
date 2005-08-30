/* sdl.c
 * contains all the "useful" or used functions by the engines.
 * those functions are called by calling a tertiarry function which will
 * be the same for all the graphics/sound/... libraries. The idea of this
 * way is to provide an uniform way to implement other libraries(Library Abstraction Layer).
 * functions which can be used for different libraries will be of the module
 * Low_. example : Low_Init(). Only one library can be used at a time.
 * Options also will have to be set expetially for the initialisation functions.
 * Im not sure how exactly we should do it yet.
 */

#if USE_SDL

#include <SDL.h>
#include <endian.h>

#include "extlib.h"

/* SDL variable types used
 * -- Graphics.c --
 * SDL_Rect 	-> Rectan
 * SDL_Surface 	-> v_object
 * 
 * -- Events.c
 */

/* SDL functions used
 *  -- Graphics.c --
 * SDL_UpdateRect
 * SDL_BlitSurface
 * SDL_FillRect
 * SDL_Flip
 * SDL_SetVideoMode
 * SDL_CreateRGBSurface
 * SDL_FreeSurface
 * 
 *  -- Events.c --
 *  
*/

typedef struct options_list
{
	u32 Xsize, Ysize; /* screen size */
	u8 bpp; /* bytes per pixel */
	u32 Primary_screen_flags; /* flags for the primary screen */
	u32 Secondary_screen_flags; /* flags for the secondary (buffer) screen */
}options_list;


static options_list options = {
	800, 600, 16, 
	SDL_SWSURFACE, 
	SDL_SWSURFACE
};



int
Lib_EventsInit()
{
	/* nothing needed, done in the video init */
	return 0;
}

void
Lib_EventsExit()
{
	/* nothing needed, done in the video exit */
}


/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{
	int _err_;
	SDL_Surface *temp1, *temp2;
	
	_err_ = SDL_Init(SDL_INIT_VIDEO);
	if (_err_)
		return _err_;

	temp1 = SDL_SetVideoMode(options.Xsize, options.Ysize, options.bpp, options.Primary_screen_flags);
	if (temp1 == NULL)
		return 1;
	
	
	{
		u32 Rmask, Gmask, Bmask, Amask;

#if __BYTE_ORDER == __LITTLE_ENDIAN
		Rmask = 0x000000ff;
		Gmask = 0x0000ff00;
		Bmask = 0x00ff0000;
		Amask = 0xff000000;
#endif /* __BYTE_ORDER == __LITTLE_ENDIAN */

#if __BYTE_ORDER == __BIG_ENDIAN
		Rmask = 0xff000000;
		Gmask = 0x00ff0000;
		Bmask = 0x0000ff00;
		Amask = 0x000000ff;
#endif /* __BYTE_ORDER == __BIG_ENDIAN */

#if __BYTE_ORDER == __PDP_ENDIAN
		Rmask = 0x00ff0000;
		Gmask = 0xff000000;
		Bmask = 0x000000ff;
		Amask = 0x0000ff00;
#endif /* __BYTE_ORDER == __PDP_ENDIAN */
		
		/*temp2 = SDL_CreateRGBSurface(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, Rmask, Gmask, Bmask, Amask);*/
		temp2 = SDL_CreateRGBSurface(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, temp1->format->Rmask, temp1->format->Gmask, temp1->format->Bmask, temp1->format->Amask);

		if (temp2 == NULL)
			return 1;
	}
	*screen = temp1;
	*screen_buf = temp2;	
	return 0;
}

void
Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst)
{
	if (!source || !destination)
		return;
	
	SDL_BlitSurface((SDL_Surface*)source, (SDL_Rect*)src, (SDL_Surface*)destination, (SDL_Rect*)dst);
}

void
Lib_UpdateRect(v_object *source, Rectan *src)
{
	if (!source)
		return;
	
	if (src)
		SDL_UpdateRect((SDL_Surface*)source, src->x, src->y, src->w, src->h);
	else
		SDL_UpdateRect((SDL_Surface*)source, 0, 0, 0, 0);
}

void
Lib_FillRect(v_object *source, Rectan *src, u32 color)
{
	SDL_FillRect((SDL_Surface*)source, (SDL_Rect*)src, color);
}

void
Lib_Flip(v_object *source)
{
	SDL_Flip((SDL_Surface*)source);
	/* Lib_UpdateRect(source, 0); */
}

void
Lib_FreeVobject(v_object *source)
{
	SDL_FreeSurface((SDL_Surface*)source);
}

void
Lib_GiveVobjectProp(v_object *source, Rectan *output)
{
	SDL_Surface *temp;

	temp = (SDL_Surface*)source;

	output->x = 0;
	output->y = 0;
	output->h = temp->h;
	output->w = temp->w;	
}

void
Lib_VideoExit()
{
	SDL_Quit();
}

#endif /* USE_SDL */
