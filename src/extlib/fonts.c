/*    
 * libneuro, a light weight abstraction of high or lower libraries 
 * and toolkit for applications.
 * Copyright (C) 2005-2008 Nicholas Niro, Robert Lemay
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

/* fonts.c
 *
 * Abstracts the use of the fonts functionnalities
 * in the driver.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>
/* freetype includes */
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/
#include <extlib.h>
#include <other.h>
#include <graphics.h>

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("extlib/fonts");

#define enable_alpha 0

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

static FT_Library font_lib;

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

v_object *
Lib_RenderUnicode(font_object *ttf, u32 size, u32 character, i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst)
{
	int _err = 0;
	static u32 expect = 0;
	static u32 codepoint = 0;
	v_object *output = NULL;
	u8 space_char = 0;
	FT_Face face;
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
		NEURO_ERROR("Couldn't select the encoding unicode", NULL);
		return NULL;
	}
	
	_err = FT_Set_Char_Size(face, size * 64, size * 64, 72, 72);
	if (_err)
	{
		NEURO_ERROR("Couldn't set face character size", NULL);
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
		NEURO_ERROR("Couldn't load character", NULL);
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
			NEURO_TRACE("Unknown non bitmap format", NULL);
			return NULL;
		}
		
		NEURO_TRACE("%s", Neuro_s("font size %dx%d\n", face->glyph->bitmap.width,
				face->glyph->bitmap.rows));

		if (!space_char)
		{
			/* allocate the surface */
			output = Lib_CreateVObject(0, face->glyph->bitmap.width, 
					face->glyph->bitmap.rows, Lib_GetDefaultDepth(), 0, 0, 0, 0);
		}
		
		bitmap = (FT_BitmapGlyph)face->glyph;
		pixel = 0;
		row = 0;


		screen = Neuro_GetScreenBuffer();

		Lib_SyncPixels(output);

		Lib_LockVObject(output);

#if enable_alpha		
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
						u32 tcolor = 0, rcolor = 0;
						i16 tx, ty;
						
						tx = (*x + pixel) + face->glyph->metrics.horiBearingX / 64;
						ty = (*y + row) - face->glyph->metrics.horiBearingY / 64;

						NEURO_TRACE("%s", Neuro_s("tcolor %d rcolor %d alpha %d\n", 
							tcolor, rcolor, gray));
						tcolor = Lib_GetPixel(screen, tx, ty);
						rcolor = AlphaPixels(color, tcolor, (double)(256 - gray));
						Lib_PutPixel(screen, tx, ty, rcolor);
						
					}

					pixel++;
				}
				
				row++;
			}
		}
#endif /* enable_alpha */

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

		Lib_UnlockVObject(output);

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

			/* we will need to call the function Lib_SetColorKey 
			 * which will itself generate the transparency 
			 * mask.
			 */
			Lib_SetColorKey(output, bg_color);
		}

		*x = *x + face->glyph->metrics.horiAdvance / 64;
		/* *y = *y + face->glyph->metrics.vertAdvance / 64; */

	}

	return output;

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

/*-------------------- Constructor Destructor ----------------------*/

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
