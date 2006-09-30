/* video.h 
 * the main local module header 
 */

#ifndef __VIDEO_H
#define __VIDEO_H

#include <graphics.h>

enum drawings_type
{
	TDRAW_BEGIN,
       	
	TDRAW_STATIC,
	TDRAW_DYNAMIC,
	TDRAW_DYNAMIC_CLEAN,
	TDRAW_SDRAWN, /* static but already drawn */
	TDRAW_SREDRAW, /* flag to make a static element redrawn */
	TDRAW_VOLATILE, /* a draw that gets deleted after being drawn (override replacement) */
	TDRAW_SDESTROY, /* the flag to destroy a static element */

	TDRAW_END
};

typedef struct RAW_ENGINE RAW_ENGINE;

struct RAW_ENGINE
{
	u32 layer; /* the drawing level that the surface is drawn */
	u8 type; /* see the drawings_type enumeration */
	Rectan src; /* size of the image */
	u16 dx, dy; /* destination coordinates */
	
	void *surface_ptr; /* points to the image held by the external application */
};

/* typedef struct INSTRUCTION_ENGINE INSTRUCTION_ENGINE; */

/* 
 * this structure is the core of the linked list
 * drawing priority algorithm. The layer for each
 * added entries is the only dependency to change
 * the drawing position of an entry.
 */
struct INSTRUCTION_ENGINE
{
	RAW_ENGINE *current;
	INSTRUCTION_ENGINE *next;
};

typedef struct DRAWING_ELEMENTS
{
	void (*callback)(void);
}DRAWING_ELEMENTS;

typedef struct debug_status
{
	u32 missing; 
	u32 duplicates; 
}debug_status;


extern INSTRUCTION_ENGINE *Graphics_AddDrawingInstruction(u32 layer, u8 type, Rectan *isrc, Rectan *idst, void *isurface);

extern u8 Graphics_DrawIsPresent(v_elem *elem);

extern int Graphics_RedrawSection(INSTRUCTION_ENGINE *indep);

extern u8  Graphics_GetSafeDrawOp();

extern void Graphics_SetSafeDrawOp(u8 safe);

/*
#include "spool.h"
#include "pixels.h"
*/

#endif /* NOT __VIDEO_H */
