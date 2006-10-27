/* video.h 
 * the main local module header 
 */

#ifndef __VIDEO_H
#define __VIDEO_H

#include <graphics.h>
#include <ebuf.h>

#define debug_instruction_buffer 1
#define debug_clean_instruction_buffer 1
#define verbose_missing_output 0
#define dynamic_debug 0
#define check_integrity_on_draw 0

#define debug_track_fonts 0
#define debug_track_special_font 1 /* child of the above, won't work if the above is 0 */
#define debug_track_special_font_x 6 /* ditto */
#define debug_track_special_font_y 6 /* above above ditto */

#define screen_buffer 1
#define second_screen_buffer 0
#define retain_image_inipos 0
 
#define use_memory_pool 0
 

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

extern void Graphics_ResetScreenDraw();

extern void Graphics_SetDrawnLastCycle();


/* inside painter.c */
extern int Graphics_PainterInit();
extern int Graphics_PainterReset();
extern void Graphics_PainterClean();

extern EBUF *Graphics_GetRawBuffer();
extern EBUF *Graphics_GetQueueBuffer();

extern void Graphics_CoreDrawAll();
extern void graphics_CoreCleanAll();

extern INSTRUCTION_ENGINE *Graphics_GetFirstElem();
extern void Graphics_SetFirstElem(INSTRUCTION_ENGINE *elem);

extern INSTRUCTION_ENGINE *Graphics_GetLastElem();
extern void Graphics_SetLastElem(INSTRUCTION_ENGINE *elem);


/* inside coredraw.c */
extern void Graphics_CoreDrawAll(); /* old draw_objects */

extern int Graphics_RedrawSection(); /* old redraw_erased_for_object */

extern void Graphics_CoreCleanAll();

extern void Graphics_CoreCleanDoneDynamics(); /* old clean_drawn_objects */

extern void Graphics_SetAllToRedraw();

/* inside debug.c */
extern void Graphics_DebugPrintQueue(); /* old print_queue */

extern void Graphics_DebugBufferQueue(EBUF *src); /* old buffer_queue */

extern void Graphics_DebugPrintMissing(EBUF *src); /* old print_missing */

extern void Graphics_DebugQueueIntegrityCheck(); /* old Queue_Integrity_Check */


/*
#include "spool.h"
#include "pixels.h"
*/

#endif /* NOT __VIDEO_H */
