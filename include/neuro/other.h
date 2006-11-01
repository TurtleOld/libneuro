
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
 * 4 = indep is inside depen (reverse of 0)
 */
extern u8 Neuro_BoundsCheck(Rectan *indep, Rectan *depen);

extern void Neuro_VerticalBoundCrop(Rectan *indep, Rectan *isrc, Rectan *idst);

extern void Neuro_HorizontalBoundCrop(Rectan *indep, Rectan *isrc, Rectan *idst);

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


/* blit one surface to another one with this function */
extern void Neuro_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst);

/* free a v_object after its use is no longer needed. */
extern void Neuro_FreeVObject(v_object *source);

/* load a M$ bitmap from a file that you input 
 * pass the address of a pointer v_object :
 * v_object *image;
 *
 * and pointer for that is &image
 */
extern void Neuro_LoadBMP(const char *path, v_object **img);

/* sets the color key that will not be drawn (for transparency) of a surface.
 * this needs to be done strictly before loading a surface with X11 and 
 * can be done anytime with SDL.
 */
extern void Neuro_SetColorKey(v_object *vobj, u32 key);

/* sets the alpha (transparency) of a surface */
extern void Neuro_SetAlpha(v_object *vobj, u8 alpha);

/* syncs the pixels so subsequent input or output on them 
 * are getting correct informations.
 */
extern void Neuro_SyncPixels(v_object *src);

/* create visual surfaces with this function */
extern v_object * Neuro_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask, u32 Bmask, u32 Amask);


/* you can load a truetype (or any other that the freetype library
 * supports) with this function. 
 * returns NULL on error or a pointer to a font_object.
 */
extern font_object *Neuro_LoadFontFile(char *fonts_file_path);

/* this function is to clean a font file 
 * loaded using the function Neuro_LoadFontFile()
 */
extern void Neuro_CleanFont(font_object *font);

/* even though the input arguments seem to be quite complicated, it is not. 
 * the ttf input address can be given with the load fonts function, the size
 * is the size of the fonts you want in pixels, the character is the character
 * code you want to render.
 *
 * The x and y coordinates are a bit special, the value in those are changed so
 * characters in a string have the correct spacing(instead of overlapping).
 *
 * color is the color you want your character to be and the 2 Rectan output
 * the basic informations about the surface so it can be blit easily.
 *
 * returns the v_object pointer if the character got loaded well or
 * NULL if either there was an error or the character that was input
 * requires more than one byte to be complete.
 *
 * NOTE This function do handle spaces! ie ' '. just input the corresponding
 * x and y addresses so the function can calculate itself the size they take.
 * For this purpose, you CAN leave color, src and dst to 0 or NULL!
 * just remember to put the correct size and character (to ' ') so the size
 * of the space is correct.
 */
extern v_object *Neuro_RenderUnicode(font_object *ttf, u32 size, u32 character, 
		i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst);

/* you can set the size of the screen with this function.
 * note that this function will not work on the fly, you
 * can only use this __before__ you init Neuro! or
 * else it won't work.
 */
extern void Lib_SetScreenSize(u32 width, u32 height);

/* puts the size of the screen in current use in the input variables */
extern void Lib_GetScreenSize(u32 *width, u32 *height);

/* for pixel manipulations (input/output) this locks the lock on
 * the pixels buffer. So nothing can be done in the background on
 * them.
 */
extern void Lib_LockVObject(v_object *vobj);

/* for pixel manipulations (input/output) this unlocks the lock on
 * the pixels buffer.
 */
extern void Lib_UnlockVObject(v_object *vobj);


#endif /* __OTHER_H */
