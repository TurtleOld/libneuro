
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

/* other.h
 */

#ifndef __OTHER_H
#define __OTHER_H

#include "neuro_engine.h"
#include "ebuf.h"

#include <stdlib.h>

extern char **Neuro_SepChr(const unsigned char chr, char *source, int *items);

/* the EBUF data uses this struct */
typedef struct SepChr_Data
{
	char *string;
}SepChr_Data;

extern EBUF *Neuro_SepChr2(const u8 chr, char *source);

/* convert color of default screen depth to 24 bit 
 * and gives each color separately in R G and B
 */
extern void Neuro_GiveConvertRGB(u32 color, u8 *R, u8 *G, u8 *B);

/* 4 functions to convert R G B values to the depth each of them do */
extern u32 Neuro_GiveRGB32(u8 R, u8 G, u8 B);
extern u32 Neuro_GiveRGB24(u8 R, u8 G, u8 B);
extern u32 Neuro_GiveRGB16(u8 R, u8 G, u8 B);
extern u32 Neuro_GiveRGB8(u8 R, u8 G, u8 B);

/* instead of using the above(Neuro_GiveRGB*), use this function that automatically 
 * check the current depth in use and use the correct one.
 */
extern u32 Neuro_MapRGB(u8 R, u8 G, u8 B);

/* uses the SDL special RGBA system, if in doubt, use
 * Neuro_MapRGB instead. (used internally)
 * returns a u32 that contains a 24bit color system. 
 */
extern u32 Neuro_GiveRGB(u8 R, u8 G, u8 B);

/* an interface to easily give the size of an image object 
 * width and height require the address of u32 to put the data on.
 */
extern void Neuro_GiveImageSize(v_object *image, i32 *width, i32 *height);

extern u32 Neuro_RawGetPixel(v_object *srf, int x, int y);

extern void Neuro_RawPutPixel(v_object *srf, int x, int y, u32 pixel);

extern void Neuro_Sleep(u32 t);

extern u32 Neuro_GetTickCount();

extern void Neuro_PrintFPS();

/* use Neuro_BoundsCheck instead */
extern u8 Neuro_DumbBoundsCheck(Rectan *indep, Rectan *depen);

/* rectangle or square bounds check function.
 * return values :
 * 0 = depen is inside indep.
 * 1 = depen and indep are not touching each others(they are far away).
 * 2 = depen is overlaping indep. 
 * 3 = depen and indep links between corners are into the other but the corners
 *     are not touching.
 */
extern u8 Neuro_BoundsCheck(Rectan *indep, Rectan *depen);

extern void Neuro_VerticalBoundFix(Rectan *indep, Rectan *isrc, Rectan *idst);

extern void Neuro_HorizontalBoundFix(Rectan *indep, Rectan *isrc, Rectan *idst);

/* generate characters (source is chgen.c in src/misc TODO might need to have the Neuro_ prefix */
extern void Uchar(int amount, unsigned char **buf);

/* internal function (source is bitmap.c in src/misc) */
extern void readBitmapFileToPixmap(const char *bitmap, EBUF **output_pixmap);
extern void readBitmapBufferToPixmap(char *data, EBUF **output_pixmap);
extern void setBitmapColorKey(u32 key);
/* internal function (source is bitmap.c in src/misc) 
 * pretty much useless, use Neuro_CleanEBuf() instead
 */
extern void cleanPixmapEbuf(EBUF **pixmap);

/* -------- Argument System ---------- */

enum
{
	OPTION_NORMAL	= 0x00000000, /* normal option which includes a callback or not */
	OPTION_ARGUMENT = 0x00000001, /* needs an argument */
	OPTION_REQUIRED = 0x00000010, /* is required to make the app run */
	OPTION_NESTED	= 0x00000100, /* can be nested with other */
	OPTION_MULTI	= 0x00001000, /* can have more than one option of this type */
	OPTION_VOID	= 0x00010000, /* when the command has no options, this option is executed */
	OPTION_QUIT	= 0x00100000  /* when this option is called, no more options r executed. */
};

extern int Neuro_ArgInit(int argc, char **argv);

extern void Neuro_ArgClean();

extern void Neuro_ArgOption(char *string, int options, void (*action)(char *data));

/* return 2 on error, 1 on normal exit requested and 0 on execution continue */
extern int Neuro_ArgProcess();

/* ---------- End of the Argument System ---------- */


#endif /* __OTHER_H */
