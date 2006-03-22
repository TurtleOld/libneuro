/* bitmap process module */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include <ebuf.h>
#include <other.h>

/* #pragma pack (1) */ /* best is to avoid this */

typedef struct BITMAP_HEADER
{
	u16 type __attribute__((packed));			/* Magic identifier            */
	u32 size __attribute__((packed));			/* File size in bytes          */
	u16 reserved1, reserved2;	/* reserved */
	u32 offset __attribute__((packed));			/* Offset to image data, bytes */
}BITMAP_HEADER;

typedef struct BITMAP_INFOHEADER
{
	u32 size;			/* Header size in bytes      */
	i32 width, height;		/* Width and height of image */
	u16 planes;			/* Number of colour planes   */
	u16 bits;			/* Bits per pixel            */
	u32 compression;		/* Compression type          */
	u32 imagesize;			/* Image size in bytes       */
	i32 xresolution, yresolution;	/* Pixels per meter          */
	u32 ncolors;			/* Number of colours         */
	u32 importantcolours;		/* Important colours         */
}BITMAP_INFOHEADER;

typedef struct BITMAP_HDATA
{
	BITMAP_HEADER header __attribute__((packed));
	BITMAP_INFOHEADER infoheader __attribute__((packed));
}BITMAP_HDATA;

typedef struct BITMAP_COLOR
{
	u8 r, g ,b, a; /* red green blue and alpha(alpha won't be used eheh) */
	u8 *symbol; /* unique symbol associated with this color */
}BITMAP_COLOR;

typedef struct BITMAP_MAP
{
	BITMAP_COLOR *color;
}BITMAP_MAP;

static u32 color_key = 0; /* this is the pixel we will make it so it is transparent */

static void 
clean_bmap_color(void *eng)
{
	BITMAP_COLOR *buf;

	buf = (BITMAP_COLOR*)eng;

	if (buf->symbol)
		free(buf->symbol);
}

/*
static void
print_bitmap_infos(BITMAP_HDATA *bmap)
{	
	printf("header data :\nsize %d\noffset %d\ninfoheader data :\nsize %d\nwidth %d\nheight %d\nplanes %d\nbits %d\ncompression %d\nimagesize %d\nxres %d\nyres %d\nncolors %d\nimportantcolors %d\n", 
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
*/


static void
process_RGB(EBUF *bcolors, EBUF *bpixels, u8 ir, u8 ig, u8 ib)
{
	u8 found = 0;
	u32 ctotal = 0; /* colors and pixels frontend variables */
	BITMAP_COLOR *cbuf = NULL;
	BITMAP_MAP *pbuf = NULL;
	u8 r, g, b;
	
	r = ir;
	g = ig;
	b = ib;

	/* printf("rgb %d%d%d\n", r, g, b); */
	/* printf("bcolors status : %d\n", Neuro_EBufIsEmpty(bcolors)); */
	
	if (!Neuro_EBufIsEmpty(bcolors))
	{
		ctotal = Neuro_GiveEBufCount(bcolors);
		/* printf("%d\n", ctotal); */
		ctotal++;
		while (ctotal-- > 0)
		{
			cbuf = Neuro_GiveEBuf(bcolors, ctotal);
			if (cbuf->r == r && cbuf->g == g && cbuf->b == b)
			{
				found = 1;
				break;
			}
		}
	}
	else
	{
		/* add the color to the buffer */
		/* printf("bcolors is empty, will feed it\n"); */
		found = 0;
	}

	/* if we didn't find a corresponding color in the buffer 
	 * we add the new one to the buffer or else, we will have
	 * to make the pixel point to the corresponding color.
	 */
	if (!found)
	{
		Neuro_AllocEBuf(bcolors, sizeof(BITMAP_COLOR*), sizeof(BITMAP_COLOR));
		cbuf = Neuro_GiveCurEBuf(bcolors);
						
		cbuf->r = r;
		cbuf->g = g;
		cbuf->b = b;
						
		if (ctotal == 0)
		{
			cbuf->symbol = calloc(2, sizeof(unsigned char));
					
			cbuf->symbol[0] = ' ';
			cbuf->symbol[1] = '\0';
		}
		else
		{
			/* generate a character based on ctotal */
			Uchar(Neuro_GiveEBufCount(bcolors), &cbuf->symbol);
						
		}
	}	
				
	Neuro_AllocEBuf(bpixels, sizeof(BITMAP_MAP*), sizeof(BITMAP_MAP));

	pbuf = Neuro_GiveCurEBuf(bpixels);
				
	pbuf->color = cbuf;
				
	found = 0;

}

/* input the bits per pixel of the image
 * input a 1 byte of data to process 
 */
static void
process_bitmap(BITMAP_HDATA *bmap, u8 *palette, u8 *data, EBUF *bcolors, EBUF *bpixels, int *aux, char **buf)
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
			u8 r, g, b;
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
				temp = *data & values[i];
				if (temp)
					temp = 1;
				r = palette[(temp * 4) + 2];
				g = palette[(temp * 4) + 1];
				b = palette[(temp * 4)];
				/* printf("%d%d%d\n", r, g, b); */
				process_RGB(bcolors, bpixels, r, g, b);
				i++;
			}
		}
		break;

		case 4:
		{
			/* will do a loop to get each 2 pixels from the data */
			u8 temp;
			/* double calc = 0; */
			u8 r, g, b;
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
				temp = *data & values[i];
				if (temp > 0x0F)
					temp >>= 4;
				
				r = palette[(temp * 4) + 2];
				g = palette[(temp * 4) + 1];
				b = palette[(temp * 4)];
				/* printf("%d%d%d\n", r, g, b); */
				process_RGB(bcolors, bpixels, r, g, b);
				i++;
			}

		}
		break;

		case 8:
		{
			/* will get the single pixel from the data */
			u8 temp;
			/* double calc = 0; */
			u8 r, g, b;
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
				temp = *data;
				r = palette[(temp * 4) + 2];
				g = palette[(temp * 4) + 1];
				b = palette[(temp * 4)];
				/* printf("%d%d%d\n", r, g, b); */
				process_RGB(bcolors, bpixels, r, g, b);
				i++;
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
			 * track of were we are at in the gathering.
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
				process_RGB(bcolors, bpixels, (*buf)[2], (*buf)[1], (*buf)[0]);
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


/* returns -1 if the file doesn't exist
 * returns the size of the file otherwise
 *
 * path : input -- the file path
 * data : output -- returns the whole data in the file. REMEMBER TO FREE IT because it is
 * allocated by this function
 */
static long
buffer_file(const char *path, u8 **data)
{
	/* an extra would be to check how big the file is and allocating buf before the
	 * copy of the data.
	 */	
	FILE *file;

	long size = 0, i = 0; /* the size of the buffer */
	u8 *buf = NULL;
	
	file = fopen(path, "read");
	
	if (file == NULL)
		return -1;

	/* first get the size of the file (this is a portable solution and still fast) */
	while(!feof(file))
	{
		fgetc(file);
		size++;
	}

	fseek(file, 0, SEEK_SET); /* go back to the beginning of the file */
	/* allocate the buffer before hand */
	*data = calloc(1, size + 1);
	
	buf = *data;
	
	/* fill the buffer with the data from the file */
	while (!feof(file))
	{
		buf[i] = fgetc(file);
		i++;
	}
	
	fclose(file);
	
	return size;
}

static void
outputDataToPixmap(BITMAP_HDATA *bmap, EBUF *bcolors, EBUF *bpixels, EBUF **output_pixmap)
{
	BITMAP_COLOR *cbuf = NULL; /* colors frontend pointer */
	BITMAP_MAP *pbuf = NULL; /* pixels map frontend pointer */
	u32 symbol_count = 0; /* number of symbols maximum */
	double temp = 0; /* temporary variable */
	u32 ctotal = 0; /* colors frontend count */
	u32 ptotal = 0; /* pixels frontend count */
	char *control, *control2, *control3; /* control strings for printfish(printf ish) functions */
	int width = 0, width2 = 0; /* used in the loops */
	u8 done = 0; /* used in the loops */
	u32 i = 0;
	char *buf = NULL, *bufe = NULL;
	EBUF *bufa = NULL;
	

	ctotal = Neuro_GiveEBufCount(bcolors);
	cbuf = Neuro_GiveEBuf(bcolors, ctotal);
	

	temp = (log10(ctotal) / log10(81));
	
	symbol_count = (u32)temp;
	if (temp > symbol_count || (ctotal == 1 || !Neuro_EBufIsEmpty(bcolors)))
	{
		symbol_count++;
	}
	else
		return;
	
	Neuro_CreateEBuf(output_pixmap);
	bufa = *output_pixmap;
#if temp	
	printf("/* XPM */\nstatic char image_xpm = {\n");
#endif /* temp */
	

	Neuro_AllocEBuf(bufa, sizeof(char*), 512); /* static 512 is temporary */
	
	buf = Neuro_GiveCurEBuf(bufa);

	sprintf(buf, "%d %d %d %d", 
			bmap->infoheader.width, 
			bmap->infoheader.height, 
			ctotal + 1,
			symbol_count);
	
	control2 = calloc(40 + 25, sizeof(char));
	control3 = calloc(40 + 25, sizeof(char)); /* this is too much mem I think */
	
	sprintf(control2, "%%-%ds	c #%%02x%%02x%%02x", symbol_count);
	sprintf(control3, "%%-%ds	c %%s", symbol_count);
	
				
	i = ctotal + 1;
	
	while (i-- > 0)
	{
		int Rr, Gg, Bb;
		
		/*Rr = (color_key & 0x003e0000) >> 17;
		Gg = (color_key & 0x0001f000) >> 12;
		Bb = (color_key & 0x00000f80) >> 7;
		*/

		Rr = (color_key & 0xff000000) >> 24;
		Gg = (color_key & 0x00ff0000) >> 16;
		Bb = (color_key & 0x0000ff00) >> 8;

		
		/* Debug_Val(0, "%d COLOR KEY %d %d %d\n", color_key, Rr, Gg, Bb); */
		
		/*Rr = (Rr * 255) / 31;
		Gg = (Gg * 255) / 31;
		Bb = (Bb * 255) / 31;*/

		/* Debug_Val(0, "output2 %d COLOR KEY %d %d %d\n", color_key, Rr, Gg, Bb); */
		
		cbuf = Neuro_GiveEBuf(bcolors, i);
		
		Neuro_AllocEBuf(bufa, sizeof(char*), 512);
		buf = Neuro_GiveCurEBuf(bufa);
		
		if (cbuf->r == Rr && cbuf->g == Gg && cbuf->b == Bb)
		{
			sprintf(buf, control3, cbuf->symbol, "None");
		}
		else
		{
			sprintf(buf, control2, cbuf->symbol, cbuf->r, cbuf->g, cbuf->b);
		}
	}
	
	control = calloc(40 + 3, sizeof(char));

	sprintf(control, "%%-%ds", symbol_count);
			
	ptotal = Neuro_GiveEBufCount(bpixels);
	i = ptotal;

	/* will buffer each rows */
	bufe = calloc((symbol_count * bmap->infoheader.width) + 1, sizeof(char));
	
	if (bufe == NULL)
	{
		Error_Print("Out of memory");
		return;
	}
	
	while (!done)
	{
		if (i == 0)
		{
			/* a bit overkill ;P */
			done = 1;
			break;
		}
		else
		{
			/* printf("\"");*/ /* the leading " */
					
			width = 0;
			width2 = bmap->infoheader.width;
			while (width < bmap->infoheader.width)
			{
				u32 t;
				/* we need to inverse i */
				pbuf = Neuro_GiveEBuf(bpixels, abs((i + 1) - width2));
				width2 -= 2;
						
				
				/* sprintf(&bufe[width * symbol_count], control, pbuf->color->symbol); */
				t = 0;
				while (t < symbol_count)
				{
					if (t + 1 >= symbol_count && pbuf->color->symbol[t] == '\0')
						bufe[(width * symbol_count) + t] = ' ';
					else
						bufe[(width * symbol_count) + t] = pbuf->color->symbol[t];
					t++;
				}
				
				width++;
				if (i > 0)
					i--;
			}
			bufe[width * symbol_count] = '\0';
			
			Neuro_AllocEBuf(bufa, sizeof(char*), symbol_count * bmap->infoheader.width + 1);
			buf = Neuro_GiveCurEBuf(bufa);

			/* strncpy(buf, bufe, symbol_count * bmap->infoheader.width); */
			sprintf(buf, "%s", bufe);
		}
	}	

	free(control3);
	free(control2);
	free(control);
	free(bufe);
}

void
cleanPixmapEbuf(EBUF **pixmap)
{
	Neuro_CleanEBuf(pixmap);
}

void
setBitmapColorKey(u32 key)
{
	color_key = key;
}

void
readBitmapFileToPixmap(const char *bitmap, EBUF **output_pixmap)
{
	/* major (buffers) */
	EBUF *bmap_colors = NULL; /* the colors */
	EBUF *bmap_map = NULL; /* the map of pixels */
	u8 *buf = NULL; /* the buffer that will contain the content of the file */
	
	/* minor (mostly pointers and temporary variables) */
	u32 psize = 0; /* the full size of the pixels data */
	u8 *bdata = NULL; /* the pointer to the data */
	u8 *palette = NULL; /* the pointer to the palette if theres one */
	BITMAP_HDATA *bmap; /* this is how we will get informations about the bitmap */
	i32 i = 0, t = 0; /* incremental variable */
	long file_size = 0;
	int aux_var = 0; /* auxiliary variable that can be used by external functions */
	char *aux_buf = NULL; /* same as aux_var but a buffer */
	double msize = 0;
	double calc = 0;
	double tmp = 0;
	u32 wmult = 0;
	double pixellen = 0;
	u32 increm = 0;
	
	file_size = buffer_file(bitmap, &buf);

	if (file_size == -1 || file_size == 0)
	{
		/* error with the file reading, it doesn't seem to exist 
		 * or it is empty.
		 */
		*output_pixmap = NULL;
		return;
	}

	/* check here if the bitmap is valid or not */
		/* first check for the BM word*/
		/* then we check for the size of the file in header and size
		 * we got when reading the file
		 */

	/* if it is valid, we create the buffers */
	Neuro_CreateEBuf(&bmap_colors);
	Neuro_SetcallbEBuf(bmap_colors, &clean_bmap_color);

	Neuro_CreateEBuf(&bmap_map);
	
	/* printf("Initial test bmap_colors status : %d\n", Neuro_EBufIsEmpty(bmap_colors)); */
	
	/* assign pointers to their respective addresses */
	bmap = (BITMAP_HDATA*)buf; /* headers */
	palette = &buf[sizeof(BITMAP_HEADER) + sizeof(BITMAP_INFOHEADER)]; /* palette */
	bdata = &buf[bmap->header.offset]; /* data */
	
	/* print_bitmap_infos(bmap); */
	
	/* process the bitmap(load it in memory) */
	i = 0;
	psize = bmap->header.size - (sizeof(BITMAP_HEADER) + sizeof(BITMAP_INFOHEADER));
	psize = psize - (bmap->infoheader.ncolors * 4);
	/* printf("data size %d\n", psize); */

	/* semi static values to skip bytes that form 32 bit chunks in the data */

	pixellen = (8 / (double)bmap->infoheader.bits);
	msize = pixellen * 4;

	wmult = (u32)bmap->infoheader.bits / 8;

	if (wmult == 0)
		wmult++;

	wmult = wmult * bmap->infoheader.width;

	increm = (u32)pixellen;

	if (increm == 0)
		increm++;

	t = (u32)(bmap->infoheader.width / msize);
	tmp = msize * t;
	tmp = (double)bmap->infoheader.width - tmp;
	tmp = tmp - 0.000001; /* to avoid bugs */

	t = 0;
	
	while (i < psize)
	{			
		if (tmp > 0)
		{
			if (t >= wmult)
			{
				calc = tmp / pixellen;
				t = (u32)calc;
				if (t < calc)
				{
					t++;
				}
				t = (4 - t);
				i += t;
				
				/*
				printf("skipping %d bytes  wmult %d width %d tmp %f plen %f calc %f\n", 
						t,
						wmult, 
						bmap->infoheader.width, 
						tmp, 
						pixellen, calc);
				*/
				
				t = 0;
			}
			
			t += increm;

			if (i >= psize)
				break;
		}

		process_bitmap(bmap, palette, &bdata[i], 
				bmap_colors, bmap_map, 
				&aux_var, &aux_buf);


		/* printf("i %d psize %d\n", i, psize); */
		i++;
	}
	
	/* now that we have the bitmap all in memory, write it to the buffer */
	if (!Neuro_EBufIsEmpty(bmap_colors) && !Neuro_EBufIsEmpty(bmap_map))
	{
		outputDataToPixmap(bmap, bmap_colors, 
				bmap_map, output_pixmap);
	}
	else
	{
		printf("Error, buffers are empty\n");
	}
	
		
	if (buf)
		free(buf);
	if (aux_buf)
		free(aux_buf);
	
	Neuro_CleanEBuf(&bmap_colors);
	Neuro_CleanEBuf(&bmap_map);
}

