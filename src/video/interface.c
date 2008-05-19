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


/* interface.c
 * interface functions to core functions 
 */

/*-------------------- Extern Headers Including --------------------*/
#include <string.h> /* memcpy */

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <graphics.h>
#include <extlib.h>

/*-------------------- Main Module Header --------------------------*/
#include "video.h"


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/
v_elem *
Neuro_PushDraw(u32 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	if (Graphics_GetSafeDrawOp() == 0)
		return NULL;


	return Graphics_AddDrawingInstruction(layer, TDRAW_STATIC, isrc, idst, isurface);
}

int
Neuro_FetchDraw(v_elem *eng, Rectan *psrc, u16 *px, u16 *py, v_object **osurface)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (psrc)
		memcpy(psrc, &eng->current->src, sizeof(Rectan));

	if (px)
		*px = eng->current->dx;

	if (py)
		*py = eng->current->dy;

	if (osurface)
	{
		/*Debug_Val(0, "surface_ptr 0x%x addr surface_ptr 0x%x\n", 
				eng->surface_ptr, &eng->surface_ptr);*/
		*osurface = eng->current->surface_ptr;
	}
	
	return 0;
}


int
Neuro_SetImgPos(v_elem *eng, u16 px, u16 py)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (!px || !py)
		return 1;

	eng->current->dx = px;
	eng->current->dy = py;	
	
	return 0;
}

int
Neuro_SetImgLayer(v_elem *eng, u32 layer)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	eng->current->layer = layer;
	
	return 0;
}

int
Neuro_SetImgSrcPos(v_elem *eng, Rectan *psrc)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (!psrc)
		return 1;

	memcpy(&eng->current->src, psrc, sizeof(Rectan));

	return 0;
}

int
Neuro_SetDraw(v_elem *eng, v_object *isurface)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (!isurface)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;
	
	eng->current->surface_ptr = isurface;

	return 0;
}

int
Neuro_CleanDraw(v_elem *eng)
{
	Rectan buf;
	v_object *screen;

	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	buf.x = eng->current->dx;
	buf.y = eng->current->dy;
	buf.w = eng->current->src.w;
	buf.h = eng->current->src.h;

	/* we start by redrawing above the static 
	 * image location to reset its image.
	 * it may be the background or black.
	 */
	/*if (background)
	{
		Lib_BlitObject(background, &buf, sclScreen2, NULL);
	}
	else*/
	
	screen = Neuro_GetScreenBuffer();

	Lib_FillRect(screen, &buf, 0);

	/* redraw_erased_for_object(eng); */
	Graphics_RedrawSection(eng);

	return 0;
}

/* this function is to tag the element to be
 * redrawn.
 */
int
Neuro_FlushDraw(v_elem *eng)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (eng->current->type == TDRAW_SDRAWN)
	{
		Rectan dst;

		dst.x = eng->current->dx;
		dst.y = eng->current->dy;
		dst.w = eng->current->src.w;
		dst.h = eng->current->src.h;


		eng->current->type = TDRAW_STATIC;

		/* flag the algorithm to tell it something changed 
		 * and an action needs to be taken.
		 */
		Graphics_RedrawScreen();
		/* draw_this_cycle = 1; */
	}
	else
		return 1;

	return 0;
}

int
Neuro_DestroyDraw(v_elem *eng)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;
	
	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (eng->current->type == TDRAW_STATIC)
		return 1;

	if (Graphics_DrawIsPresent(eng) == 0)
		return 1;

	/* eng->current->type = TDRAW_SDESTROY; */

	/* the official way of deleting an element */
	Graphics_DestroyElement(eng);

	/* clean_object(eng); */


	
	Graphics_RedrawScreen();
	/* draw_this_cycle = 1; */

	return 0;
}
