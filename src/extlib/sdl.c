
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

#include <graphics.h>
#include <extlib.h>
#include <other.h>

/* freetype includes */
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

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
	0, 
	SDL_SWSURFACE
};

static u8 mouse_wheel = 0; /* mouse wheel variable */

static FT_Library font_lib;

void
Lib_SetScreenSize(u32 width, u32 height)
{
	options.Xsize = width;
	options.Ysize = height;
}

void
Lib_GetScreenSize(u32 *width, u32 *height)
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
		Error_Print("SDL_Init failure");
		return _err_;
	}

	temp1 = SDL_SetVideoMode(options.Xsize, options.Ysize, options.bpp, options.Primary_screen_flags);
	if (temp1 == NULL)
	{
		Error_Print("SDL_SetVideoMode failure");
		return 1;
	}
	
	
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

		Debug_Val(0, "MASKS 0x%x 0x%x 0x%x 0x%x\n", 
				temp1->format->Rmask,
				temp1->format->Gmask,
				temp1->format->Bmask,
				temp1->format->Amask);

		temp2 = (SDL_Surface*)Lib_CreateVObject(options.Secondary_screen_flags, options.Xsize, options.Ysize, options.bpp, temp1->format->Rmask, temp1->format->Gmask, temp1->format->Bmask, temp1->format->Amask);

		if (temp2 == NULL)
		{
			Error_Print("Lib_CreateVObject failure");
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

static int 
stdio_seek(SDL_RWops *context, int offset, int whence)
{

	return gzseek(context->hidden.stdio.fp, offset, whence);
	/*
	if ( gzseek(context->hidden.stdio.fp, offset, whence) == 0 ) 
	{
		return(gztell(context->hidden.stdio.fp));
	} 
	else 
	{
		SDL_Error(SDL_EFSEEK);
		return(-1);
	}
	*/
}

static int 
stdio_read(SDL_RWops *context, void *ptr, int size, int maxnum)
{
	size_t nread = 0;

	/* nread = fread(ptr, size, maxnum, context->hidden.stdio.fp);  */

	if (size == 1)
		nread = gzread(context->hidden.stdio.fp, ptr, maxnum);

	if (size == 2)
	{
		char *buf = NULL;
		
		buf = ptr;
		
		nread = gzread(context->hidden.stdio.fp, &buf[0], maxnum);
		nread += gzread(context->hidden.stdio.fp, &buf[1], maxnum);
	}

	if (size == 4)
	{
		char *buf = NULL;
		
		buf = ptr;

		nread = gzread(context->hidden.stdio.fp, &buf[0], maxnum);
		nread += gzread(context->hidden.stdio.fp, &buf[1], maxnum);
		nread += gzread(context->hidden.stdio.fp, &buf[2], maxnum);
		nread += gzread(context->hidden.stdio.fp, &buf[3], maxnum);
	}

	/*Debug_Val(0, "asked for size %d maxnum %d -- read %d\n",
			size, maxnum, nread);*/

	if ( nread == 0) {
		SDL_Error(SDL_EFREAD);
	}
	return(nread);
}

/* this won't be needed, we don't actually want to write
 * bitmaps.
 */
static int 
stdio_write(SDL_RWops *context, const void *ptr, int size, int num)
{
	size_t nwrote;

	nwrote = fwrite(ptr, size, num, context->hidden.stdio.fp);
	if ( nwrote == 0 && ferror(context->hidden.stdio.fp) ) {
		SDL_Error(SDL_EFWRITE);
	}
	return(nwrote);
}

static int 
stdio_close(SDL_RWops *context)
{
	if ( context ) {
		if ( context->hidden.stdio.autoclose ) {
			/* WARNING:  Check the return value here! */
			/* fclose(context->hidden.stdio.fp); */
			gzclose(context->hidden.stdio.fp);
		}
		free(context);
	}
	return(0);
}


void
Lib_LoadBMP(const char *path, v_object **img)
{
#if NATIVE_BMP
	t_tick chrono;


	chrono = Neuro_GetTickCount();
	*img = readBitmapFile(path);

	Debug_Val(0, "Loading a bitmap took %d\n", Neuro_GetTickCount() - chrono);
#else /* NOT NATIVE_BMP */
#if USE_ZLIB
	gzFile fp;
	SDL_RWops *ops;

	fp = gzopen(path, "rb");
	if (fp == NULL)
		return;

	ops = SDL_AllocRW();
	if (ops == NULL)
		return;

	ops->seek = stdio_seek;
	ops->read = stdio_read;
	ops->write = stdio_write;
	ops->close = stdio_close;
	ops->hidden.stdio.fp = fp;
	ops->hidden.stdio.autoclose = 1;

	*img = SDL_LoadBMP_RW(ops, 1);

	if (*img == NULL)
	{
		Debug_Val(0, "Unable to load image \"%s\" SDL says : %s\n", path, SDL_GetError());
	}

	/*if (ops)
		free(ops);*/

#else /* NOT USE_ZLIB */
	*img = SDL_LoadBMP(path);
#endif /* NOT USE_ZLIB */
#endif /* NOT NATIVE_BMP */
}

void
Lib_LoadBMPBuffer(void *data, v_object **img)
{
	SDL_RWops *ops;

	if (data == NULL)
		return;
	
	ops = SDL_AllocRW();
	if (ops == NULL)
		return;

	ops->seek = stdio_seek;
	ops->read = stdio_read;
	ops->write = stdio_write;
	ops->close = stdio_close;
	/*ops->hidden.stdio.fp = fp;
	ops->hidden.stdio.autoclose = 1;*/
	ops->hidden.unknown.data1 = data;

	*img = SDL_LoadBMP_RW(ops, 1);

	if (*img == NULL)
	{
		Debug_Val(0, "Unable to load buffer image SDL says : %s\n", SDL_GetError());
	}
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

v_object *
Lib_RenderUnicode(font_object *ttf, u32 size, u32 character, i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst)
{
	int _err = 0;
	static u32 expect = 0;
	static u32 codepoint = 0;
	v_object *output = NULL;
	u8 space_char = 0;
	FT_Face face;
	/* XImage *mask_data = NULL; */
	u32 bg_color;

	face = (FT_Face)ttf;

	if (!face)
		return NULL;

	/* our first pick for the background is pure pink */
	bg_color = Neuro_MapRGB(255, 0, 255);

	/* in case the fancy of the external program 
	 * is having pure pink color fonts (heh it can happen)
	 * we make the background color to pure black.
	 */
	if (bg_color == color)
		bg_color = Neuro_MapRGB(0, 0, 0);

	if (character == ' ')
	{
		space_char = 1;
	}
	else
	{
		if (!src || !dst)
			return NULL;
	}

	_err = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
	if (_err)
	{
		Error_Print("Couldn't select the encoding unicode");
		return NULL;
	}
	
	_err = FT_Set_Char_Size(face, size * 64, size * 64, 72, 72);
	if (_err)
	{
		Error_Print("Couldn't set face character size");
		return NULL;
	}

	/* support for multi bytes characters */	
	if (character >= 0xC0)
	{
		if (character < 0xE0)
		{
			codepoint = character & 0x1F;
			expect = 1;
		}
		else if (character < 0xF0)
		{
			codepoint = character & 0x0F;
			expect = 2;
		}
		else if (character < 0xF8)
		{
			codepoint = character & 0x07;
			expect = 3;
		}
		return NULL;
	}
	else if (character >= 0x80)
	{
		--expect;

		if (expect >= 0)
		{
			codepoint <<= 6;
			codepoint += character & 0x3F;
		}

		if (expect > 0)
			return NULL;

		expect = 0;
	}
	else
		codepoint = character;

	_err = FT_Load_Char(face, codepoint, FT_LOAD_RENDER | FT_LOAD_MONOCHROME);
	/* _err = FT_Load_Char(face, character, FT_LOAD_MONOCHROME); */

	if (_err)
	{
		Error_Print("Couldn't load character");
		return NULL;
	}

	{
		v_object *screen;
		u32 pixel, row;
		FT_BitmapGlyph bitmap;
		u8 R, G, B;

		Neuro_GiveConvertRGB(color, &R, &G, &B);
		
		if (face->glyph->format != FT_GLYPH_FORMAT_BITMAP)
		{
			Debug_Print("Unknown non bitmap format");
			return NULL;
		}
		/*if (face->glyph->format == FT_GLYPH_FORMAT_BITMAP)
		{
			Debug_Print("Bitmap format");
		}
		else
		{
			Debug_Print("Unknown non bitmap format");
		}
		*/
		
		/* Debug_Val(0, "font size %dx%d\n", face->glyph->bitmap.width,
				face->glyph->bitmap.rows); */
		if (!space_char)
		{
			/* allocate the surface */
			output = Lib_CreateVObject(0, face->glyph->bitmap.width, 
					face->glyph->bitmap.rows, 16, 0, 0, 0, 0);
		}
		
		bitmap = (FT_BitmapGlyph)face->glyph;
		pixel = 0;
		row = 0;


		screen = Neuro_GetScreenBuffer();

		if (output)
		{
			Lib_SyncPixels(output);

			Lib_LockVObject(output);
		}

		if (!space_char)
		{
			/* create the mask */
			/* XXX if (output)
				mask_data = CreateMask(output, face->glyph->bitmap.width, face->glyph->bitmap.rows);
			*/
		}
		
#if alpha_fonts
		if (face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_GRAY && !space_char)
		{
			while (row < face->glyph->bitmap.rows)
			{	
				pixel = 0;	
				
				while (pixel < face->glyph->bitmap.width)
				{
					u32 gray;
					
					if ((gray = face->glyph->bitmap.buffer[(row * face->glyph->bitmap.pitch) + pixel]))
					{
						u32 tcolor, rcolor;
						i16 tx, ty;
						
						tx = (*x + pixel) + face->glyph->metrics.horiBearingX / 64;
						ty = (*y + row) - face->glyph->metrics.horiBearingY / 64;

						/*Debug_Val(0, "tcolor %d rcolor %d alpha %d\n", tcolor, 
							rcolor, gray);*/
						tcolor = Lib_GetPixel(screen, tx, ty);
						rcolor = AlphaPixels(color, tcolor, (double)(256 - gray));
						Lib_PutPixel(screen, tx, ty, rcolor);
						
					}
					else
					{
						/* if (character == 'X')
							Debug_Val(0, "0");*/
					}

					pixel++;
				}
				
				if (character == 'X')
					Debug_Val(0, "\n");
				row++;
			}
		}
#endif /* alpha_fonts */

		if (face->glyph->bitmap.pixel_mode == FT_PIXEL_MODE_MONO && !space_char)
		{
			u16 cbits[8];
			u16 i = 8;
			i16 tx, ty;
			u8 gray;
			u8 *bm, *rstart;

			bm = face->glyph->bitmap.buffer;
			rstart = bm;

			if (IsLittleEndian())
			{
				cbits[7] = 0x80;
				cbits[6] = 0x40;
				cbits[5] = 0x20;
				cbits[4] = 0x10;
				cbits[3] = 0x08;
				cbits[2] = 0x04;
				cbits[1] = 0x02;
				cbits[0] = 0x01;
			}
			else
			{
				cbits[7] = 0x08;
				cbits[6] = 0x04;
				cbits[5] = 0x02;
				cbits[4] = 0x01;
				cbits[3] = 0x80;
				cbits[2] = 0x40;
				cbits[1] = 0x20;
				cbits[0] = 0x10;
			}
			
			pixel = 0;
			while (row < face->glyph->bitmap.rows)
			{
				gray = *bm;
				i = 8;
				while (i-- > 0)
				{			
					/* tx = (*x + pixel) + face->glyph->metrics.horiBearingX / 64;
					ty = (*y + row) - face->glyph->metrics.horiBearingY / 64; */
					/* tx = pixel + face->glyph->metrics.horiBearingX / 64;
					ty = row - face->glyph->metrics.horiBearingY / 64;*/
					tx = pixel;
					ty = row;
						
					if (gray & cbits[i])
					{
						/* Debug_Val(0, "dot at (%d,%d)\n", tx, ty); */
						Lib_PutPixel(output, tx, ty, color);
						
					}
					else
					{
						/* the background pixels... we will 
						 * make the pixels of this color 
						 * transparent.
						 */
						Lib_PutPixel(output, tx, ty, bg_color); 

					}

					if (pixel >= face->glyph->bitmap.width - 1)
					{
						row++;

						pixel = 0;
						rstart += face->glyph->bitmap.pitch;
						bm = rstart;
						bm--;
						
						break;
					}
					pixel++;
				}

				bm++;
			}
		}	


		if (!space_char)
		{
			src->x = 0;
			src->y = 0;
			src->w = face->glyph->bitmap.width;
			src->h = face->glyph->bitmap.rows;

			dst->x = *x + face->glyph->metrics.horiBearingX / 64;
			dst->y = (size + *y) - face->glyph->metrics.horiBearingY / 64;

			dst->w = 0;
			dst->h = 0;
			
#if font_mask
			if (obj && mask_data)
			{
				/* copy the mask data (for transparency) to the V_OBJECT 
				 * shapemask variable properly.
				 */
				CreatePixmap(mask_data, *obj->cwin, &obj->shapemask);

				/* we no longer need the mask_data variable, it got copied 
				 * to the good place so we destroy it.
				 */
				XDestroyImage(mask_data);
			}
#endif /* font_mask XXX */

			/* we will need to call the function Lib_SetColorKey 
			 * which will itself generate the transparency 
			 * mask.
			 */
			Lib_SetColorKey(output, bg_color);
		}

		*x = *x + face->glyph->metrics.horiAdvance / 64;
		/* *y = *y + face->glyph->metrics.vertAdvance / 64; */

		if (output)
			Lib_UnlockVObject(output);
	}

	return output;
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
			if (IsLittleEndian)
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


font_object *
Lib_LoadFontFile(const char *fonts_file_path)
{
	FT_Face face;
	int _err = 0;
	
	_err = FT_New_Face(font_lib, fonts_file_path, 0, &face);

	if (_err != 0)
	{
		return NULL;
	}

	return face;
}

void
Lib_CleanFont(font_object *font)
{
	FT_Done_Face(font);
}

int
Lib_FontsInit()
{
	return FT_Init_FreeType(&font_lib);
}

void
Lib_FontsExit()
{
	FT_Done_FreeType(font_lib);
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
