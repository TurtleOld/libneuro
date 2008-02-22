
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
#if USE_ZLIB
#include <zlib.h>
#endif /* USE_ZLIB */

#define NATIVE_BMP 1

#include <global.h>
#include <graphics.h>
#include <extlib.h>
#include <other.h>

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
 * SDL_GetKeyState	-> Lib_CheckKeyStatus
 * SDL_GetMouseState	-> Lib_GetMouseState
*/

NEURO_MODULE_CHANNEL("sdl_driver");

typedef struct options_list
{
	i32 Xsize, Ysize; /* screen size */
	u8 bpp; /* bytes per pixel */
	u32 Primary_screen_flags; /* flags for the primary screen */
	u32 Secondary_screen_flags; /* flags for the secondary (buffer) screen */
}options_list;


static options_list options = {
	800, 600, 16, 
	0, 
	SDL_SWSURFACE
};

static u8 mouse_wheel = 0; /* mouse wheel variable */

#define KeySymTranslation_AMOUNT 65

static u32 KeySymsTranslateTable[KeySymTranslation_AMOUNT][2] = {
	{NKB_BackSpace, 0x0008},
	{NKB_Tab, 0x0009},
	{NKB_Linefeed, 0x000A},
	{NKB_Clear, 0x000C},
	{NKB_Return, 0x000D},
	{NKB_Pause, 0x0013},
	{NKB_Scroll_Lock, 0x0014},
	{NKB_Sys_Req, 0x0015},
	{NKB_Escape, 0x001B},
	{NKB_Delete, 127},
	{NKB_Home, 0x0116},
	{NKB_Left, 0x0114},
	{NKB_Up, 0x0111},
	{NKB_Right, 0x0113},
	{NKB_Down,0x0112},
	{NKB_Page_Up, 0x0118},
	{NKB_Page_Down, 0x0119},
	{NKB_End, 279},
	{NKB_Insert, 277},
	{NKB_KP_0, 256},
	{NKB_KP_1, 257},
	{NKB_KP_2, 258},
	{NKB_KP_3, 259},
	{NKB_KP_4, 260},
	{NKB_KP_5, 261},
	{NKB_KP_6, 262},
	{NKB_KP_7, 263},
	{NKB_KP_8, 264},
	{NKB_KP_9, 265},
	{NKB_KP_Decimal, 266},
	{NKB_KP_Divide, 267},
	{NKB_KP_Multiply, 268},
	{NKB_KP_Subtract, 269},
	{NKB_KP_Add, 270},
	{NKB_KP_Enter, 271},
	{NKB_KP_Equal, 272},
	{NKB_F1, 282},
	{NKB_F2, 283},
	{NKB_F3, 284},
	{NKB_F4, 285},
	{NKB_F5, 286},
	{NKB_F6, 287},
	{NKB_F7, 288},
	{NKB_F8, 289},
	{NKB_F9, 290},
	{NKB_F10, 291},
	{NKB_F11, 292},
	{NKB_F12, 293},
	{NKB_F13, 294},
	{NKB_F14, 295},
	{NKB_F15, 296},
	{NKB_Shift_L, 304},
	{NKB_Shift_R, 303},
	{NKB_Control_L, 306},
	{NKB_Control_R, 305},
	{NKB_Caps_Lock, 301},
	{NKB_Shift_Lock, 302},
	{NKB_Meta_L, 310},
	{NKB_Meta_R, 309},
	{NKB_Alt_L, 308},
	{NKB_Alt_R, 313}, /* the real right alt didn't work (307) */
	{NKB_Super_L, 311},
	{NKB_Super_R, 312},
};

/* translates the keysym to the one the driver uses */
static u32
TranslateKey(u32 keysym)
{
	u32 output = 0;
	u32 i = KeySymTranslation_AMOUNT + 1;

	output = keysym;

	while (i-- > 0)
	{
		if (KeySymsTranslateTable[i][0] == keysym)
			return KeySymsTranslateTable[i][1];
	}

	return output;
}

static void
get_default_mask(u32 *rmask, u32 *gmask, u32 *bmask, u32 *amask)
{
	if (!rmask || !gmask || !bmask || !amask)
	{
		NEURO_ERROR("wrong mask pointer inputed", NULL);
		return;
	}

	if (IsLittleEndian())
	{
		switch (Lib_GetDefaultDepth())
		{
			case 8:
			{
				*rmask = 0x000000C0;
				*gmask = 0x0000003C;
				*bmask = 0x00000003;
				*amask = 0x00000000;
			}
			break;

			case 16:
			{
				*rmask = 0x0000f800;
				*gmask = 0x000007e0;
				*bmask = 0x0000001f;
				*amask = 0x00000000;
			}
			break;

			case 24:
			{
				*rmask = 0x00ff0000;
				*gmask = 0x0000ff00;
				*bmask = 0x000000ff;
				*amask = 0x00000000;
			}
			break;


			default:
			break;
		}
	}
	else
	{
		switch (Lib_GetDefaultDepth())
		{
			case 8:
			{
				*rmask = 0x00000003;
				*gmask = 0x0000003C;
				*bmask = 0x000000C0;
				*amask = 0x00000000;
			}
			break;

			case 16:
			{
				*rmask = 0x0000001f;
				*gmask = 0x000007e0;
				*bmask = 0x0000f800;
				*amask = 0x00000000;
			}
			break;
			
			case 24:
			{
				*rmask = 0x0000ff00;
				*gmask = 0x00ff0000;
				*bmask = 0xff000000;
				*amask = 0x00000000;
			}
			break;

			default:
			break;
		}
	}
}




void
Lib_SetScreenSize(i32 width, i32 height)
{
	options.Xsize = width;
	options.Ysize = height;
}

void
Lib_GetScreenSize(i32 *width, i32 *height)
{
	*width = options.Xsize;
	*height = options.Ysize;
}

/*  video constructor destructor  */
int
Lib_VideoInit(v_object **screen, v_object **screen_buf)
{
	int _err_ = 0;
	SDL_Surface *temp1 = NULL, *temp2 = NULL;
	
	_err_ = SDL_Init(SDL_INIT_VIDEO);

	if (_err_)
	{
		NEURO_ERROR("SDL_Init failure", NULL);
		return _err_;
	}

	temp1 = SDL_SetVideoMode(options.Xsize, options.Ysize, options.bpp, options.Primary_screen_flags);
	if (temp1 == NULL)
	{
		NEURO_ERROR("SDL_SetVideoMode failure", NULL);
		return 1;
	}
	
	
	{
		u32 Rmask, Gmask, Bmask, Amask;

		get_default_mask(&Rmask, &Gmask, &Bmask, &Amask);
		
		temp2 = SDL_CreateRGBSurface(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, Rmask, Gmask, Bmask, Amask);

		Debug_Val(0, "MASKS 0x%x 0x%x 0x%x 0x%x  witness 0x%x 0x%x 0x%x 0x%x \n",
				Rmask, Gmask, Bmask, Amask,
				temp1->format->Rmask,
				temp1->format->Gmask,
				temp1->format->Bmask,
				temp1->format->Amask);

		/* temp2 = (SDL_Surface*)Lib_CreateVObject(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, temp1->format->Rmask, temp1->format->Gmask, temp1->format->Bmask, temp1->format->Amask); */

		if (temp2 == NULL)
		{
			NEURO_ERROR("Lib_CreateVObject failure", NULL);
			return 1;
		}
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
Lib_SyncPixels(v_object *src)
{
	
}

void
Lib_LoadBMP(const char *path, v_object **img)
{
#if NATIVE_BMP
	t_tick chrono;


	chrono = Neuro_GetTickCount();
	*img = Bitmap_LoadBMP(path);

	Debug_Val(0, "Loading a bitmap took %d\n", Neuro_GetTickCount() - chrono);
#else /* NOT NATIVE_BMP */

	*img = SDL_LoadBMP(path);

#endif /* NOT NATIVE_BMP */
}

void
Lib_LoadBMPBuffer(void *data, v_object **img)
{
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
		{
			if (IsLittleEndian())
			{
				return (r >> fmt->Rloss) << fmt->Rshift | (g >> fmt->Gloss) << fmt->Gshift | (b >> fmt->Bloss) << fmt->Bshift | fmt->Amask;
			}
			else
			{
				return (r << fmt->Rloss) >> fmt->Rshift | (g << fmt->Gloss) >> fmt->Gshift | (b << fmt->Bloss) >> fmt->Bshift | fmt->Amask;

			}
		}
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

void
Lib_SetAlpha(v_object *vobj, u8 alpha)
{
	if (vobj == NULL)
		return;

	SDL_SetAlpha((SDL_Surface*)vobj, SDL_SRCALPHA | SDL_RLEACCEL, alpha);
}

v_object *
Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask,
		u32 Bmask, u32 Amask)
{
	u32 xRmask, xGmask, xBmask, xAmask;

	get_default_mask(&xRmask, &xGmask, &xBmask, &xAmask);

	if (!Rmask)
		Rmask = xRmask;
	if (!Gmask)
		Gmask = xGmask;
	if (!Bmask)
		Bmask = xBmask;
	if (!Amask)
		Amask = xAmask;

	return (v_object*)SDL_CreateRGBSurface(SDL_SWSURFACE | flags, width, height, Lib_GetDefaultDepth(), Rmask, Gmask, Bmask, Amask);
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
	
	
	/* Lib_LockVObject(srf); */
	
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
		
		case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
				err = p[0] << 16 | p[1] << 8 | p[2];
			else
				err = p[0] | p[1] << 8 | p[2] << 16;
		}
		break;
		
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

	/* Lib_UnlockVObject(srf); */

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
	return options.bpp;
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
	{
		*clip_rect = (Rectan*)&srf->clip_rect;

		(*clip_rect)->x = srf->clip_rect.x;
		(*clip_rect)->y = srf->clip_rect.y;
		(*clip_rect)->h = srf->clip_rect.h;
		(*clip_rect)->w = srf->clip_rect.w;
	}
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

u8
Lib_CheckKeyStatus(u32 key)
{
	u8 *keyd;

	keyd = SDL_GetKeyState(NULL);
	
	if (keyd[TranslateKey(key)])
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
	u8 value = 0;

	value = SDL_BUTTON(SDL_GetMouseState(x, y));

	/* to fix what appears to be a bug... might 
	 * be a cheap hack... but it works :)
	 */
	if (value > 3)
		value = 3;

	if (mouse_wheel && value == 0)
	{
		value = mouse_wheel;
		mouse_wheel = 0;
	}
	
	return value;
}

i32
Lib_PollEvent(void *s_event)
{
	SDL_Event event;
	int _err = 0;

	while(SDL_PollEvent(&event))
	{
		switch (event.type)
		{
			case SDL_MOUSEBUTTONDOWN:
			{
				if (event.button.button == SDL_BUTTON_WHEELUP
					|| event.button.button == SDL_BUTTON_WHEELDOWN)
				{
					mouse_wheel = event.button.button;
				}
			}
			break;
				
			default:
			break;
		}
	}
	
	return _err;
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
