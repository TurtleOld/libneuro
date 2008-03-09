
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

/* process.c
 * Module : Bitmap
 *
 * Code to process the bitmap data.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <ebuf.h>
#include <other.h>

#include <graphics.h> /* Neuro_PutPixel */

/*-------------------- Main Module Header --------------------------*/
#include "intern_bitmap.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("bitmap");

/* this macro is used to offset the actual workable value
 * of the bitmap's width and height values.
 *
 * A value of say 200 in width and height would actually
 * yield in the values of 0 to 199.
 */
#define SIZE_OFFSET -1

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

static void print_bitmap_infos(BITMAP_HDATA *bmap) __attribute__((unused));

/*-------------------- Static Functions ----------------------------*/

/* the magic number will probably not show correctly if big endian */
static void
print_bitmap_infos(BITMAP_HDATA *bmap)
{	
	printf("(0x%x)[%c%c] header data :\nsize %d\noffset %d\ninfoheader data :\nsize %d\nwidth %d\nheight %d\nplanes %d\nbits %d\ncompression %d\nimagesize %d\nxres %d\nyres %d\nncolors %d\nimportantcolors %d\n", 
		bmap->header.type,
		bmap->header.type & 0x00FF,
		(bmap->header.type & 0xFF00) >> 8,
		bmap->header.size,
		bmap->header.offset,
		bmap->infoheader.size,
		bmap->infoheader.width,
		bmap->infoheader.height,
		bmap->infoheader.planes,
		bmap->infoheader.bits,
		bmap->infoheader.compression,
		bmap->infoheader.imagesize,
		bmap->infoheader.xresolution,
		bmap->infoheader.yresolution,
		bmap->infoheader.ncolors,
		bmap->infoheader.importantcolours);
}

static void 
clean_bmap_color(void *eng)
{
	BITMAP_COLOR *buf;

	buf = (BITMAP_COLOR*)eng;
}

/* This function processes each bytes of the inherent bitmap
 * data pixels. It supports 4 bits per pixels types :
 * 1, 4, 8 and 24.
 *
 * input the bits per pixel of the image
 * input a 1 byte of data to process 
 */
static void
process_bitmap(BITMAP_HDATA *bmap, v_object *image, u8 *data, EBUF *bcolors, u32 *x, u32 *y, int *aux, char **buf)
{
	
	/* we will call functions depending on the bpp of the image */
	switch (bmap->infoheader.bits)
	{
		case 1:
		{
			/* will do a loop to get each 8 pixels from the data */
			
			/* this is pretty much for little endian 
			 * the minimum data size we can have for a certain width is
			 * 32 bits (4 bytes increments). Those 32 bits will be able 
			 * to hold up to 32 pixels. In case the width is higher than 
			 * 32 pixels, it will put 32 bits until the whole width
			 * can be fulfilled.
			 */
			/* we will need to have the width value because we will need
			 * to know how many bits we have to read from 32 bits (4 bytes). 
			 */
			/* aux will keep how many 32 bits still need to be done in a 
			 * certain width 
			 */
			u8 temp;
			/* double calc = 0; */
			
			/* u8 r, g, b; */
			
			const u8 values[8] = {
				0x80,
				0x40,
				0x20,
				0x10,
				0x08,
				0x04,
				0x02,
				0x01
			};
			u32 i = 0;
			u32 max = 0;
			
			if (*buf == NULL)
			{
				*buf = calloc(1, sizeof(char));
				**buf = 0;
				*aux = 0;
			}

			/* set up aux for remaining pixels */
			if (*aux == 0)
				*aux = bmap->infoheader.width;

			/* set up the number of pixels we need to process in this cycle */
			if (*aux > 8)
			{
				max = 8;
				*aux = *aux - 8;
			}
			else
			{
				max = *aux;
				*aux = 0;
			}
			
			temp = *data;

			while (i < max)
			{
				BITMAP_COLOR *cbuf = NULL;

				if (*y >= bmap->infoheader.height)
					break;

				temp = *data & values[i];
				

				if (temp)
					temp = 1;	

				cbuf = Neuro_GiveEBuf(bcolors, temp);

				Neuro_PutPixel(image, *x, 
						(bmap->infoheader.height + SIZE_OFFSET) - *y, 
						Neuro_MapRGB(cbuf->r, cbuf->g, cbuf->b));

				*x = *x + 1;
				i++;
			}

			if (*x > bmap->infoheader.width + SIZE_OFFSET)
			{
				*x = 0;
				*y = *y + 1;
			}
		}
		break;

		case 4:
		{
			/* will do a loop to get each 2 pixels from the data */
			u8 temp;
			
			const u8 values[2] = {
				0xF0,
				0x0F,
			};
			u32 i = 0;
			u32 max = 0;
		
			if (*buf == NULL)
			{
				*buf = calloc(1, sizeof(char));
				**buf = 0;
				*aux = 0;
			}
			/* set up aux for remaining pixels */
			if (*aux == 0)
				*aux = bmap->infoheader.width;
			
			/* set up the number of pixels we need to process in this cycle */
			if (*aux > 2)
			{
				max = 2;
				*aux = *aux - 2;
			}
			else
			{
				max = *aux;
				*aux = 0;
			}
			temp = *data;

			while (i < max)
			{
				BITMAP_COLOR *cbuf = NULL;
				
				if (*y >= bmap->infoheader.height)
					break;
				
				temp = *data & values[i];
				NEURO_TRACE("%s", Neuro_s("Temp value set to 0x%x -- whole val 0x%x", temp, *data));
				if (i == 0)
				{
					if (IsLittleEndian())
					{
						temp >>= 4;
						NEURO_TRACE("Little endian fixed to 0x%x", temp);
					}
					else
					{
						temp <<= 4;
						NEURO_TRACE("Big endian fixed to 0x%x", temp);
					}
				}

				NEURO_TRACE("%s", Neuro_s("status 3-3 temp val %d total %d", temp, Neuro_GiveEBufCount(bcolors)));
				cbuf = Neuro_GiveEBuf(bcolors, temp);

				if (!cbuf)
				{
					NEURO_ERROR("%s", Neuro_s("Cbuf fetched at [%d] is NULL -- data is \"%x\" witness %d", temp, *data, *data & values[i] >> 4));
				}

				if (cbuf)
				{
					NEURO_TRACE("B1 -- %s", 
						Neuro_s("img %d coord (%d,%d) (%d,%d,%d)",
					       		image, *x, 
							(bmap->infoheader.height + SIZE_OFFSET) - *y, 
							cbuf->r, cbuf->g, cbuf->b));
					Neuro_PutPixel(image, *x, 
						(bmap->infoheader.height + SIZE_OFFSET) - *y, 
						Neuro_MapRGB(cbuf->r, cbuf->g, cbuf->b));
				}
				/* NEURO_TRACE("B1 done", NULL); */
		
				*x = *x + 1;
				i++;
			}

			if (*x > bmap->infoheader.width + SIZE_OFFSET)
			{
				*x = 0;
				
				NEURO_TRACE("Changing row", NULL);
				*y = *y + 1;
			}

			NEURO_TRACE("done", NULL);
		}
		break;

		case 8:
		{
			/* will get the single pixel from the data */
			u8 temp;
			/* double calc = 0; */
			
			/* u8 r, g, b; */
			
			u32 i = 0;
			u32 max = 0;
			
			if (*buf == NULL)
			{
				*buf = calloc(1, sizeof(char));
				**buf = 0;
				*aux = 0;
			}

			/* set up aux for remaining pixels */
			if (*aux == 0)
				*aux = bmap->infoheader.width;

			/* set up the number of pixels we need to process in this cycle */
			if (*aux > 1)
			{
				max = 1;
				*aux = *aux - 1;
			}
			else
			{
				max = *aux;
				*aux = 0;
			}
			
			temp = *data;

			while (i < max)
			{
				BITMAP_COLOR *cbuf = NULL;
				temp = *data;

				if (*y >= bmap->infoheader.height)
					break;

				cbuf = Neuro_GiveEBuf(bcolors, temp);
			
				Neuro_PutPixel(image, *x, (bmap->infoheader.height + SIZE_OFFSET) - *y, 
						Neuro_MapRGB(cbuf->r, cbuf->g, cbuf->b));

				*x = *x + 1;
				i++;
			}

			if (*x > bmap->infoheader.width + SIZE_OFFSET)
			{
				*x = 0;
				
				*y = *y + 1;
			}

		}
		break;

		case 16:
		{
			/* we do not support 16 bit because I think they  
			 * use 24 bit for those. we'll see through use.
			 * if not, I really think this depth is pointless...
			 */
		}
		break;

		case 24:
		{
			/* will need to gather the data for 2 other bytes to get a
			 * single pixel. We will use the auxiliary variable to keep
			 * track of where we are at in the gathering.
			 */

			if (!*buf)
			{
				*buf = calloc(3, sizeof(u8));
				*aux = 0;
			}
	
			(*buf)[*aux] = *data;
			*aux = *aux + 1;

			if (*aux >= 3)
			{
				*aux = 0;

				if (*y >= bmap->infoheader.height)
				{
					NEURO_ERROR("attempted to draw an invalid location", NULL);
					return;
				}

				Neuro_PutPixel(image, *x, (bmap->infoheader.height + SIZE_OFFSET) - *y, 
						Neuro_MapRGB((*buf)[2], (*buf)[1], (*buf)[0]));

				*x = *x + 1;
			}

			if (*x > bmap->infoheader.width + SIZE_OFFSET)
			{
				*x = 0;

				*y = *y + 1;
			}
		}
		break;

		case 32:
		{
			/* we do not support 32 bit because it is Very uncommon if it 
			 * actually exist. Pretty much the same as 16 bpp.
			 */
		}
		break;

		default:
		{
			/* an error occured */
		}
		break;

	}
}

static BITMAP_HDATA *
parse_bitmap_header(nFILE *input)
{
	BITMAP_HDATA *buf;
	BITMAP_INFOHEADER *tmp;

	if (input == NULL)
		return NULL;
	
	buf = calloc(1, sizeof(BITMAP_HDATA));

	tmp = &buf->infoheader;
	
	fpdata16(input, &buf->header.type);
	fpdata32(input, &buf->header.size);
	fpdata16(input, &buf->header.reserved1);
	fpdata16(input, &buf->header.reserved2);
	fpdata32(input, &buf->header.offset);
	
	fpdata32(input, &tmp->size);
	fpdata32(input, (u32*)&tmp->width);
	fpdata32(input, (u32*)&tmp->height);

	fpdata16(input, &tmp->planes);
	fpdata16(input, &tmp->bits);
	fpdata32(input, &tmp->compression);
	fpdata32(input, &tmp->imagesize);
	fpdata32(input, (u32*)&tmp->xresolution);
	fpdata32(input, (u32*)&tmp->yresolution);
	fpdata32(input, &tmp->ncolors);
	fpdata32(input, &tmp->importantcolours);
	
	return buf;
}

void
process_palette(nFILE *input, BITMAP_HDATA *bmap, EBUF *bcolors)
{
	u32 i = 0;
	BITMAP_COLOR *buf;
	u32 color;
	u8 *tmp;

	while (i < bmap->infoheader.ncolors)
	{
		Neuro_AllocEBuf(bcolors, sizeof(BITMAP_COLOR*), sizeof(BITMAP_COLOR));
		
		buf = Neuro_GiveCurEBuf(bcolors);
		
		
		fpdata32(input, &color);

		tmp = (u8*)&color;

		buf->b = tmp[0];
		buf->g = tmp[1];
		buf->r = tmp[2];
		/* buf->a = tmp[3]; */
		
		i++;
	}
}


/*-------------------- Global Functions ----------------------------*/

/* ctx being the bitmap loading context
 * and loops being how many times the
 * external function wants this function
 * to load bytes from the image file. 
 */
i8
Bitmap_ProcessGradual(BMP_CTX *ctx, u32 loops)
{
	if (ctx == NULL)
	{
		NEURO_WARN("argument ctx is NULL", NULL);
		return -1;
	}

	if (ctx->f_bitmap == NULL)
	{
		NEURO_WARN("bitmap file descriptor is NULL", NULL);
		return -1;
	}

	if (ctx->i == 0)
	{

		/* When the variable ctx->i equals 0, we
		 * initialise the buffers and prepare all the
		 * variables that will be used to load the
		 * image.
		 *
		 * this part of the code should only be ran
		 * once for each bitmaps.
		 */

		ctx->bmap = parse_bitmap_header(ctx->f_bitmap);
		
		/* we do consistency checks of the image to
		 * see if it is really a bitmap.
		 */
		{
			/* print_bitmap_infos(ctx->bmap); */
			
			if (ctx->bmap->header.type != 0x4d42)
			{
				NEURO_WARN("Invalid bitmap file magic %d", ctx->bmap->header.type);
				return -1;
			}
		}

		/* if it is valid, we create the buffers */
		Neuro_CreateEBuf(&ctx->bmap_colors);
		Neuro_SetcallbEBuf(ctx->bmap_colors, &clean_bmap_color);
		
		
		/* print_bitmap_infos(ctx->bmap); */
		
		/* we set the variable psize to the size that
		 * the image data is minus the actual header size.
		 *
		 * it will be used to see when the function finished
		 * loading the data.
		 */
		ctx->i = 0;
		ctx->psize = ctx->bmap->header.size - (sizeof(BITMAP_HEADER) + sizeof(BITMAP_INFOHEADER));
		ctx->psize = ctx->psize - (ctx->bmap->infoheader.ncolors * 4);
		
		NEURO_TRACE("data %s", Neuro_s("size %d offset %d", ctx->psize, ctx->bmap->header.offset));

		/* we load the palette, if any */
		if (ctx->bmap->infoheader.ncolors > 0)
		{
			process_palette(ctx->f_bitmap, ctx->bmap, ctx->bmap_colors);
		}

		/* we create the v_object which is the libneuro
		 * representation of the image.
		 */
		{
			ctx->output = Neuro_CreateVObject(0, ctx->bmap->infoheader.width, 
					ctx->bmap->infoheader.height, 
					ctx->bmap->infoheader.bits, 0, 0, 0, 0);


			if (ctx->output == NULL)
			{
				NEURO_WARN("Created output v_object is NULL", NULL);
				return -1;
			}
		}

		/* we calculate the number of bytes there is per rows 
		 * this is mainly so we can know how much "alignment"
		 * bytes there is (which need to be skipped)
		 */
		{
			ctx->wmult = (double)ctx->bmap->infoheader.bits / 8;

			if (ctx->wmult == 0)
				ctx->wmult++;

			ctx->wmult *= (double)ctx->bmap->infoheader.width;
		}
		ctx->row_amount = ctx->wmult;

		/* we advance up to the point where the bitmap's data are. */
#if USE_ZLIB 1
		gzseek(ctx->f_bitmap, ctx->bmap->header.offset, SEEK_SET);
#else /* NOT USE_ZLIB */
		fseek(ctx->f_bitmap, ctx->bmap->header.offset, SEEK_SET);
#endif /* NOT USE_ZLIB */

		NEURO_TRACE("%s", Neuro_s("Bitmap size(psize) %d Width size(wmult) %d bitmap bpp %d", ctx->psize, ctx->wmult, ctx->bmap->infoheader.bits));

		/* we calculate the chunk of data that will be loaded for each 
		 * cycles or polls.
		 */
		ctx->cut_size = ctx->psize / 30;
		loops = ctx->cut_size;

		/* we lock the surface */
		Neuro_LockVObject(ctx->output);
	}
	

	/* this part of the function takes
	 * care of the actual loading of the
	 * bitmap file. */
	{
		i32 initial = ctx->i;
		
		while (ctx->i < (initial + loops))
		{
			u8 *data;

			/* we check to see if we went past the size of the data */
			if (ctx->i > ctx->psize)
				break;


			/* we fetch 32 bits from the file stream */
			if (ctx->advance == 0)
			{
				/* the algorithm now fetches 32 bits instead of
				 * the old 8 bits as it was before.
				 *
				 * This supplements the huge amount of code that
				 * was necessary in order to align itself to the
				 * data which was 32 bits aligned.
				 */
				fpdata32(ctx->f_bitmap, &ctx->DATA);

				/* we fetch 4 bytes of data from the bitmap file 
				 * which will be processed one after the other.
				 *
				 * This variable is used to know which
				 * byte we are processing currently.
				 */
				ctx->advance = sizeof(int);
			}

			/* since the function process_bitmap requires 
			 * a single byte and our ctx->DATA variable is an integer,
			 * we use the variable data to permit us the ease of pushing
			 * only a single byte to the function.
			 */
			data = (u8*)&ctx->DATA;


			if (ctx->row_amount > 0)
			{
				/* we push the 8 bits with various other 
				 * variables to the bits processor.
				 *
				 */
				process_bitmap(ctx->bmap, ctx->output, 
						&data[sizeof(int) - ctx->advance], 
						ctx->bmap_colors, &ctx->x, &ctx->y, 
						&ctx->aux_var, &ctx->aux_buf);

				ctx->row_amount--;
			}
			else
			{
				ctx->row_amount = ctx->wmult;

				/* a new row started and we got fresh data 
				 * for the next row so we handle them.
				 */
				if (ctx->advance == sizeof(int))
					continue;

				
				NEURO_TRACE("skipped %d bytes", sizeof(int) - ctx->advance);
				/* we flush the remaining bytes if any */
				ctx->advance = 1;
			}

			ctx->advance--;
			

			ctx->i++;
		}

		if (ctx->i >= ctx->psize)
		{ /* this bitmap finished being loaded, we free everything */

			/* to prevent further calls to be processed */
			ctx->i = -1;

			if (ctx->bmap)
				free(ctx->bmap);
			if (ctx->buf)
				free(ctx->buf);
			if (ctx->aux_buf)
				free(ctx->aux_buf);
			
			Neuro_CleanEBuf(&ctx->bmap_colors);

#if USE_ZLIB 1 
			if (ctx->f_bitmap)
				gzclose(ctx->f_bitmap);
#else /* NOT USE_ZLIB */
			if (ctx->f_bitmap)
				fclose(ctx->f_bitmap);
#endif /* NOT USE_ZLIB */

			/* we return 100% done */
			return 100;
		}

		/* we return the percentage of the file that
		 * is currently loaded. 
		 */
		return (i8)((u32)(ctx->i * 100) / ctx->psize);
	}

	/* this never happens unless the image was already loaded */
	NEURO_WARN("Useless call of the function #%d", ctx->i);
	return -1;
}


