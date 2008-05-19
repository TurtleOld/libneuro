
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

/*! \file graphics.h
 * \brief graphics engine header
 *
 * The graphic engine is part of the most important engines in a game. 
 * It provides function to draw images on the screen.
 */

#ifndef __GRAPHICS_H
#define __GRAPHICS_H

#include "neuro_engine.h"
#include "other.h"

#include <stdlib.h>

/*! default : 60
 * the maximum amount of frames per seconds -deprecated-
 */
#define FRAMES_PER_SECOND 60

/*! default : 2
 * how many time the computer has to check 
 * out which one of the image has the lowest
 * layer(to be placed in the instruction
 * buffer).
 */
#define LAYER_CHECK_PER_FRAMES 2

/*! The Total screen width (can be changed before compile time to set the new size) */
#define SCREEN_X 800
/*! The Total screen height (can be changed before compile time to set the new size) */
#define SCREEN_Y 600

#ifdef __cplusplus
extern "C" {
#endif

/** Neuro_Graphics
 *
 * @name 
 * Neuro_Graphics
 *
 * @description
 * IN SHORT : \n \n
 *
 * This module actually is the core of the libneuro features. 
 * Contained within is an easy to use module which handles
 *
 *
 * It
 * is a painter's algorithm which draws images at every loops 
 * without the need of further work from the client code other than
 * looping the Neuro_Poll(3) function.
 *
 *
 *
 **/

typedef struct INSTRUCTION_ENGINE INSTRUCTION_ENGINE;

typedef INSTRUCTION_ENGINE v_elem;

/** Neuro_GetScreenBuffer
 * @sdescri 
 * returns the variable containing the screen buffer.
 *
 * @description
 * This function returns the raw v_object of the screen itself.
 * This raw v_object is the buffer which the painter's algorithm
 * use before it is actually pushed to the graphic card's 
 * framebuffer
 *
 * @returnval
 * the v_object of the screen buffer 
 *
 * @related
 * Neuro_RawPutPixel(3), Neuro_RawGetPixel(3)
 *
 */
extern void *Neuro_GetScreenBuffer();

/* Neuro_PushDraw
 * @sdescri
 * this is the new generation of the drawing push functions, this will 
 * replace completely the current drawing system method if it presents
 * better performances than the old (dynamic, static and volatile) system.
 *
 * the "older" system is however kept for backward compatibility and this
 * function uses the same algorithm so it would be unwise to remove the
 * other functions.
 *
 * @description
 *
 * @param[in]
 * the layer which is the order by which this element is to be drawn in
 * relation with all the other elements in the buffer.
 *
 * @param[in]
 * the image source's rectangle used to set which part of the image is
 * to be drawn.
 *
 * @param[in]
 * the image's destination coordinate on the screen.
 *
 * @param[in]
 * a loaded image of type v_object.
 *
 * @returnval
 * a context variable used to access the element's internal values after
 * it have been pushed to the buffer. This context can be used to
 * alter values input in this function and even ultimately destroying
 * this element altogether from the buffer.
 */
extern v_elem *Neuro_PushDraw(u32 layer, Rectan *isrc, Rectan *idst, v_object *isurface);

/* 
 *
 * outputs all the core informations about an image element
 * in the buffer. 
 *
 * psrc the rectangle position of the source image.
 * px the X axis position of the image on the screen.
 * py the Y axis position of the image on the screen.
 * osurface outputs the pointer of the image.
 *
 * returns 0 if all is ok and 1 on error
 */
extern int Neuro_FetchDraw(v_elem *eng, Rectan *psrc, u16 *px, u16 *py, v_object **osurface);

/* TODO */
extern int Neuro_SetImgPos(v_elem *eng, u16 px, u16 py);

/* TODO */
extern int Neuro_SetImgSrcPos(v_elem *eng, Rectan *psrc);


/* this function's sole purpose is the input of a new surface for the
 * element. 
 */
extern int Neuro_SetDraw(v_elem *eng, v_object *isurface);

/* cleans the drawn element on screen so it is no longer visible, this 
 * action should be taken before touching the element's source
 * or destination variables or else undesirable trails will subsist
 * because no buffer is kept for the coordinates to clean.
 */
extern int Neuro_CleanDraw(v_elem *eng);

/* set the layer of a drawn element. */
extern int Neuro_SetImgLayer(v_elem *eng, u32 layer);

/* tags the element to be redrawn by the engine, without this functions, 
 * changes made with the Neuro_FetchDraw function would not be applied to
 * the element.
 */
extern int Neuro_FlushDraw(v_elem *eng);

/* destroys the element */
extern int Neuro_DestroyDraw(v_elem *eng);

/*! give the current fps */
extern void Neuro_GiveFPS(t_tick *output);


/* INTERNAL USE ONLY! */ 

/* internal use Initialisation of the Graphics Engine */
extern int Graphics_Init(void);
/* Poll of the Graphics Engine */
extern void Graphics_Poll(void);
/* Cleaning of the Graphics Engine */
extern void Graphics_Clean(void);

#ifdef __cplusplus
}
#endif

#endif /* __GRAPHICS_H */
