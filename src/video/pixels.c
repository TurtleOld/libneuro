
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

/* pixels.c
 * Module : Pixels
 *
 * not yet included in the main stream, this is a dormant module
 */

/*-------------------- Extern Headers Including --------------------*/


/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <extlib.h> /* Lib_PutPixel and Lib_GetPixel */

/*-------------------- Main Module Header --------------------------*/
#include "video.h"


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/* 1 is that the pixels will be cleaned during this cycle and 0 is no it won't */
static u8 clean_pixel_in_this_cycle;

/* the pixels buffer */
static EBUF *_Pixel;

typedef struct PIXEL_ENGINE
{
	u32 x, y;
}PIXEL_ENGINE;

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

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
		}
	}
	Neuro_CleanEBuf(&_Pixel);
}

/*-------------------- Global Functions ----------------------------*/

void
Neuro_PutPixel(v_object *vobj, u32 x, u32 y, u32 pixel)
{
	/*EBUF *tmp;
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
	buf = Neuro_GiveEBuf(tmp, current);*/
	
	Lib_PutPixel(vobj, x, y, pixel);
	/*
	buf->x = x;
	buf->y = y;
	*/
}

u32
Neuro_GetPixel(v_object *vobj, u32 x, u32 y)
{
	return Lib_GetPixel(vobj, x, y);
}

void
Neuro_CleanPixels()
{
	clean_pixel_in_this_cycle = 1;
}

/*-------------------- Poll ----------------------------------------*/

void
Pixels_Poll()
{
	
}

/*-------------------- Constructor Destructor ----------------------*/

int
Pixels_Init()
{
	Neuro_CreateEBuf(&_Pixel);

	return 0;
}

void
Pixels_Clean()
{
	Neuro_CleanEBuf(&_Pixel);
}
