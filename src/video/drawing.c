/* drawing.c
 * interface functions to core functions 
 */

/*-------------------- Extern Headers Including --------------------*/


/*-------------------- Local Headers Including ---------------------*/
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
	Graphics_SetSafeDrawOp(1);
	return Graphics_AddDrawingInstruction(layer, TDRAW_STATIC, isrc, idst, isurface);
}

int
Neuro_FetchDraw(v_elem *eng, Rectan **psrc, u16 **px, u16 **py, v_object **osurface)
{
	if (!eng)
		return 1;

	if (!eng->current)
		return 1;

	if (Graphics_GetSafeDrawOp() == 0)
		return 1;

	if (psrc)
		*psrc = &eng->current->src;

	if (px)
		*px = &eng->current->dx;

	if (py)
		*py = &eng->current->dy;

	if (osurface)
	{
		/*Debug_Val(0, "surface_ptr 0x%x addr surface_ptr 0x%x\n", 
				eng->surface_ptr, &eng->surface_ptr);*/
		*osurface = eng->current->surface_ptr;
	}
	
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
		eng->current->type = TDRAW_SREDRAW;

		/* flag the algorithm to tell it something changed 
		 * and an action needs to be taken.
		 */
		Neuro_RedrawScreen();
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

	eng->current->type = TDRAW_SDESTROY;
	/* clean_object(eng); */
	
	Neuro_RedrawScreen();
	/* draw_this_cycle = 1; */

	return 0;
}

void
Neuro_PushStaticDraw(u32 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Graphics_AddDrawingInstruction(layer, TDRAW_STATIC, isrc, idst, isurface);
}

void
Neuro_PushDynamicDraw(u32 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Graphics_AddDrawingInstruction(layer, TDRAW_DYNAMIC, isrc, idst, isurface);
}

/* push a drawing instruction that will be deleted from the queue and raw 
 * after being drawn. This replaces the hackish override method with an 
 * ultra versatile one and much less costy ;P.
 */
void
Neuro_PushVolatileDraw(u32 layer, Rectan *isrc, Rectan *idst, v_object *isurface)
{
	Graphics_AddDrawingInstruction(layer, TDRAW_VOLATILE, isrc, idst, isurface);
}
