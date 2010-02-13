/* Library Abstraction Layer header */

#ifndef __EXTLIB_H
#define __EXTLIB_H

#include "neuro_engine.h"

/* take note that those functions herein are to be used stricly by the 
 * local library and not by the external programs!
 *
 * Those functions are designed for internal use only! their where abouts
 * are not garanteed and they surely don't have any (none at all) 
 * backward compatibility! So use the Neuro_ functions, now please.
 *
 */

/* allocate stuff needed for the video */
extern int Lib_VideoInit(v_object **screen, v_object **screen_buf);

/* cleans up stuff allocated for the video */
extern void Lib_VideoExit();

/* allocated stuff needed for events */
extern int Lib_EventInit();

/* cleans up stuff allocated for events */
extern void Lib_EventExit();

/* allocates stuff needed for fonts */
extern int Lib_FontsInit();

/* cleans up stuff allocated for fonts */
extern void Lib_FontsExit();

/* you can set the size of the screen with this function.
 * note that this function will not work on the fly, you
 * can only use this __before__ you init Neuro! or
 * else it won't work.
 */
extern void Lib_SetScreenSize(i32 width, i32 height);

/* puts the size of the screen in current use in the input variables */
extern void Lib_GetScreenSize(i32 *width, i32 *height);

/* for pixel manipulations (input/output) this locks the lock on
 * the pixels buffer. So nothing can be done in the background on
 * them.
 */
extern void Lib_LockVObject(v_object *vobj);

/* for pixel manipulations (input/output) this unlocks the lock on
 * the pixels buffer.
 */
extern void Lib_UnlockVObject(v_object *vobj);

/* free a v_object after its use is no longer needed. */
extern void Lib_FreeVobject(v_object *source);

/* flip the surface/screen if its double buffered */
extern void Lib_Flip(v_object *source);

/* draw rectangles on the surface */
extern void Lib_FillRect(v_object *source, Rectan *src, u32 color);

/* used to update only parts of the surface/screen at once */
extern void Lib_UpdateRect(v_object *source, Rectan *src);

/* blit one surface to another one with this function */
extern void Lib_BlitObject(v_object *source, Rectan *src, v_object *destination, Rectan *dst);

/* SOON TO BE OBSOLETE!!
 *
 * no idea what that does... it doesn't seem to have any use... 
 */
extern void Lib_GiveVobjectProp(v_object *source, Rectan *output);

/* outputs the current depth in use */
extern u32 Lib_GetDefaultDepth();

/* get sensible informations about a surface and yes, all those needs 
 * to be filled.
 */
extern void Lib_GetVObjectData(v_object *vobj, u32 *flags, i32 *h, i32 *w, u32 *pitch, void **pixels, Rectan **clip_rect, u8 *bpp, u32 *Rmask, u32 *Gmask, u32 *Bmask, u32 *Amask);

/* load a M$ bitmap from a file that you input 
 * pass the address of a pointer v_object :
 * v_object *image;
 *
 * and pointer for that is &image
 */
extern void Lib_LoadBMP(const char *path, v_object **img);

/* load a M$ bitmap from a buffer in memory 
 * pass the address of a pointer v_object :
 * v_object *image;
 *
 * and pointer for that is &image
 */
extern void Lib_LoadBMPBuffer(void *data, v_object **img);

/* outputs the color code(properly aligned) corresponding to 
 * the three primary color inputs. For surfaces that have
 * their own palette, we support an input of the surface.
 */
extern u32 Lib_MapRGB(v_object *vobj, u8 r, u8 g, u8 b);

/* sets the color key that will not be drawn (for transparency) of a surface.
 * this needs to be done strictly before loading a surface with X11 and 
 * can be done anytime with SDL.
 */
extern void Lib_SetColorKey(v_object *vobj, u32 key);

/* sets the alpha (transparency) of a surface */
extern void Lib_SetAlpha(v_object *vobj, u8 alpha);

/* syncs the pixels so subsequent input or output on them 
 * are getting correct informations.
 */
extern void Lib_SyncPixels(v_object *src);

/* puts a pixel into surface srf at the given coordinates */
extern void Lib_PutPixel(v_object *srf, int x, int y, u32 pixel);

/* outputs the color of a pixel in the surface srf */
extern u32 Lib_GetPixel(v_object *srf, int x, int y);

/* create visual surfaces with this function */
extern v_object * Lib_CreateVObject(u32 flags, i32 width, i32 height, i32 depth, u32 Rmask, u32 Gmask, u32 Bmask, u32 Amask);

/* a higher SDL function which is kinda an hack for event.c */
extern void Lib_EventPoll();

/* the poll for the events */
extern i32 Lib_PollEvent(void *event);

/* better function to get key status */
extern u8 Lib_CheckKeyStatus(u32 key);

/* outputs the current status of the mouse and passes the coordinates to the 
 * address inputed.
 */
extern u8 Lib_GetMouseState(i32 *x, i32 *y); 

/* you can load a truetype (or any other that the freetype library
 * supports) with this function. 
 * returns NULL on error or a pointer to a font_object.
 */
extern font_object *Lib_LoadFontFile(const char *fonts_file_path);

/* this function is to clean a font file loaded 
 * using Lib_LoadFontFile
 */
extern void Lib_CleanFont(font_object *font);

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
extern v_object *Lib_RenderUnicode(font_object *ttf, u32 size, u32 character, 
		i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst);

#endif /* not __EXTLIB_H */
