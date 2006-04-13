
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

#include <SDL/SDL.h>
/* #include <endian.h> */

#include <neuro/extlib.h>
#include <neuro/other.h>

/* SDL variable types used
 * -- Graphics.c --
 * SDL_Rect 	-> Rectan
 * SDL_Surface 	-> v_object
 * 
 * -- Events.c
 * SDL_Event
 */

/* SDL functions used
 *  -- Graphics.c --
 * SDL_UpdateRect	-> Lib_UpdateRect
 * SDL_BlitSurface	-> Lib_BlitObject
 * SDL_FillRect		-> Lib_FillRect
 * SDL_Flip		-> Lib_Flip
 * SDL_SetVideoMode	-> Lib_VideoInit
 * SDL_CreateRGBSurface	-> 
 * SDL_FreeSurface	-> Lib_FreeVobject
 * 
 *  -- Events.c --
 * SDL_GetKeyState	-> Lib_GetKeyState
 * SDL_GetMouseState	-> Lib_GetMouseState
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
	SDL_HWSURFACE | SDL_DOUBLEBUF, 
	SDL_HWSURFACE
};


/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{
	int _err_ = 0;
	SDL_Surface *temp1 = NULL, *temp2 = NULL;
	
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
		temp2 = (SDL_Surface*)Lib_CreateVObject(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, temp1->format->Rmask, temp1->format->Gmask, temp1->format->Bmask, temp1->format->Amask);

		if (temp2 == NULL)
			return 1;
	}
	*screen = temp1;
	if (screen_buf)
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
Lib_LoadBMP(const char *path, v_object **img)
{
	*img = SDL_LoadBMP(path);
}

static u8 
findColor(SDL_Palette *pal, u8 r, u8 g, u8 b)
{
        /* Do colorspace distance matching */
        u32 smallest;
        u32 distance;
        i32 rd, gd, bd;
        i32 i;
        u8 pixel=0;

        smallest = ~0;
        for ( i=0; i<pal->ncolors; ++i ) 
	{
		rd = pal->colors[i].r - r;
		gd = pal->colors[i].g - g;
		bd = pal->colors[i].b - b;
		distance = (rd*rd)+(gd*gd)+(bd*bd);
		if ( distance < smallest ) 
		{
			pixel = i;
                	if ( distance == 0 )  /* Perfect match! */
				break;
         		smallest = distance;
		}
	}
	return(pixel);
}

u32 
Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b)
{
	SDL_Surface *temp;
	SDL_PixelFormat *fmt;
	
	temp = (SDL_Surface*)vobj;
	fmt = temp->format;
	
	if (fmt)
	{
		if ( fmt->palette == NULL ) 
			return (r >> fmt->Rloss) << fmt->Rshift | (g >> fmt->Gloss) << fmt->Gshift | (b >> fmt->Bloss) << fmt->Bshift | fmt->Amask;
		else 
			return findColor(fmt->palette, r, g, b);
	}
	else
	{
		return Neuro_GiveRGB(r, g, b);
	}
}

void
Lib_SetColorKey(v_object *vobj, u32 key)
{
	if (vobj == NULL)
		return;

	SDL_SetColorKey((SDL_Surface*)vobj, SDL_SRCCOLORKEY | SDL_RLEACCEL, key);
}

v_object *
Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask,
		u32 Bmask, u32 Amask)
{
	return (v_object*)SDL_CreateRGBSurface(flags, width, height, depth, Rmask, Gmask, Bmask, Amask);
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

u32
Lib_GetPixel(v_object *srf, int x, int y)
{
	u8 bpp;
	u8 *p;
	void *pixels;
	u32 pitch;
	u32 err;
	
	
	Lib_LockVObject(srf);
	
	Lib_GetVObjectData(srf, NULL, NULL, NULL, &pitch, &pixels, NULL, &bpp,
			NULL, NULL, NULL, NULL);
	
	/* bpp = surface->format->BytesPerPixel; */
	/* Here p is the address to the pixel we want to retrieve */
	/* p = (u8 *)surface->pixels + y * surface->pitch + x * bpp; */
	p = (u8 *)pixels + y * pitch + x * bpp;
	err = 0;
	
	switch(bpp) 
	{
		case 1:
		{
			err = *p;
			/* err = *(u16 *)p; */
		}
		break;
		
		case 2:
		{
			err = *(u16 *)p;
		}
		break;
		
		/*case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
				err = p[0] << 16 | p[1] << 8 | p[2];
			else
				err = p[0] | p[1] << 8 | p[2] << 16;
		}
		break;
		*/
		
		case 4:
		{
			err = *(u32 *)p;
		}
		break;
		
		default:
		{
			err = 0;       /* shouldn't happen, but avoids warnings */
		}
	}

	Lib_UnlockVObject(srf);

	return err;

}

void
Lib_PutPixel(v_object *srf, int x, int y, u32 pixel)
{
	/* SDL_Surface *surface = (SDL_Surface*)srf; */
	/* SDL_LockSurface(surface); */
	u8 bpp;
	u8 *p;
	void *pixels;
	u32 pitch;
	
	Lib_GetVObjectData(srf, NULL, NULL, NULL, &pitch, &pixels, NULL, &bpp,
			NULL, NULL, NULL, NULL);

	/* Lib_LockVObject(srf); */
	/* int bpp = surface->format->BytesPerPixel; */
	/* Here p is the address to the pixel we want to set */
	/* p = (u8*)surface->pixels + y * surface->pitch + x * bpp; */
	p = (u8 *)pixels + y * pitch + x * bpp;

	switch(bpp) 
	{
		case 1:
		{
			*p = pixel;
		}
		break;

		case 2:
		{
			*(u16*)p = pixel;
		}
		break;
		/*
		case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN) 
			{
				p[0] = (pixel >> 16) & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = pixel & 0xff;
			} 
			else 
			{
				p[0] = pixel & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = (pixel >> 16) & 0xff;
			}
		}
		break;
		*/
		case 4:
		{
			*(u32 *)p = pixel;
		}
		break;
		
	}
	/* SDL_UnlockSurface(surface); */
	/* Lib_UnlockVObject(srf); */
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
	SDL_Surface *temp;
	
	temp = (SDL_Surface*)source;

	if (temp)
	{
		/*
		if (temp->format)
		{
			if (temp->format->palette)
			{
				if (temp->format->palette->colors)
					free(temp->format->palette->colors);

				free(temp->format->palette);
			}

			free(temp->format);
		}
		*/

		/*if (temp->pixels)
			free(temp->pixels);
		*/
		

		/* free(temp); */ /* we'll let SDL_FreeSurface this job... hope it does it ;P */
	}

	/* poor SDL needs a third support, see above. This is concerning mem leaks in 
	 * the poor SDL free surface function.
	 */
	SDL_FreeSurface((SDL_Surface*)source);

}

/* this function will become obsolete soon */
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
Lib_LockVObject(v_object *vobj)
{
	SDL_LockSurface((SDL_Surface*)vobj);
}

void
Lib_UnlockVObject(v_object *vobj)
{
	SDL_UnlockSurface((SDL_Surface*)vobj);
}

u32
Lib_GetDefaultDepth()
{
	return 16; /* TEMPORARY please make get the real value from somewhere!!! TODO TODO */
}

void
Lib_GetVObjectData(v_object *vobj, u32 *flags, i32 *h, i32 *w, u32 *pitch, 
		void **pixels, Rectan **clip_rect, u8 *bpp, 
		u32 *Rmask, u32 *Gmask, u32 *Bmask,u32 *Amask)
{
	SDL_Surface *srf = (SDL_Surface*)vobj;
		
	if (flags)
		*flags = srf->flags;

	if (h)
		*h = srf->h;
	if (w)
		*w = srf->w;
	if (pitch)
		*pitch = srf->pitch;
	if (pixels)
		*pixels = srf->pixels;
	if (clip_rect)
		*clip_rect = (Rectan*)&srf->clip_rect;
	if (bpp)
		*bpp = srf->format->BytesPerPixel;
	if (Rmask)
		*Rmask = srf->format->Rmask;
	if (Gmask)
		*Gmask = srf->format->Gmask;
	if (Bmask)
		*Bmask = srf->format->Bmask;
	if (Amask)
		*Amask = srf->format->Amask;
}

void
Lib_VideoExit()
{
	SDL_Quit();
}


/*----------------- Input Events -----------------*/

u8 *
Lib_GetKeyState(i32 *numkeys)
{
	return SDL_GetKeyState(numkeys);
}

u8
Lib_CheckKeyStatus(u32 key)
{
	u8 *keyd;

	keyd = SDL_GetKeyState(NULL);
	
	if (keyd[key])
		return 1;
	
	return 0;
}

void
Lib_EventPoll()
{
	SDL_Event event;

	while(SDL_PollEvent(&event))
	{
		switch (event.type)
		{
			default:
			break;
		}
	}
}

u8
Lib_GetMouseState(i32 *x, i32 *y)
{
	return SDL_GetMouseState(x, y);
}

i32
Lib_PollEvent(void *event)
{
	return SDL_PollEvent((SDL_Event*)event);
}

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
