/* Library Abstraction Layer header */

#ifndef __EXTLIB_H
#define __EXTLIB_H

#include "neuro_engine.h"

/* the keyboard events list 
 * taken from the X11 keymaps.
 */

/*
 * TTY Functions, cleverly chosen to map to ascii, for convenience of
 * programming, but could have been arbitrary (at the cost of lookup
 * tables in client code.
 */

#define NKB_BackSpace		0xFF08	/* back space, back char */
#define NKB_Tab			0xFF09
#define NKB_Linefeed		0xFF0A	/* Linefeed, LF */
#define NKB_Clear		0xFF0B
#define NKB_Return		0xFF0D	/* Return, enter */
#define NKB_Pause		0xFF13	/* Pause, hold */
#define NKB_Scroll_Lock		0xFF14
#define NKB_Sys_Req		0xFF15
#define NKB_Escape		0xFF1B
#define NKB_Delete		0xFFFF	/* Delete, rubout */



/* International & multi-key character composition */

#define NKB_Multi_key		0xFF20  /* Multi-key character compose */
#define NKB_Codeinput		0xFF37
#define NKB_SingleCandidate	0xFF3C
#define NKB_MultipleCandidate	0xFF3D
#define NKB_PreviousCandidate	0xFF3E

/* Cursor control & motion */

#define NKB_Home			0xFF50
#define NKB_Left			0xFF51	/* Move left, left arrow */
#define NKB_Up			0xFF52	/* Move up, up arrow */
#define NKB_Right		0xFF53	/* Move right, right arrow */
#define NKB_Down			0xFF54	/* Move down, down arrow */
#define NKB_Prior		0xFF55	/* Prior, previous */
#define NKB_Page_Up		0xFF55
#define NKB_Next			0xFF56	/* Next */
#define NKB_Page_Down		0xFF56
#define NKB_End			0xFF57	/* EOL */
#define NKB_Begin		0xFF58	/* BOL */


/* Misc Functions */

#define NKB_Select		0xFF60	/* Select, mark */
#define NKB_Print		0xFF61
#define NKB_Execute		0xFF62	/* Execute, run, do */
#define NKB_Insert		0xFF63	/* Insert, insert here */
#define NKB_Undo			0xFF65	/* Undo, oops */
#define NKB_Redo			0xFF66	/* redo, again */
#define NKB_Menu			0xFF67
#define NKB_Find			0xFF68	/* Find, search */
#define NKB_Cancel		0xFF69	/* Cancel, stop, abort, exit */
#define NKB_Help			0xFF6A	/* Help */
#define NKB_Break		0xFF6B
#define NKB_Mode_switch		0xFF7E	/* Character set switch */
#define NKB_script_switch        0xFF7E  /* Alias for mode_switch */
#define NKB_Num_Lock		0xFF7F

/* Keypad Functions, keypad numbers cleverly chosen to map to ascii */

#define NKB_KP_Space		0xFF80	/* space */
#define NKB_KP_Tab		0xFF89
#define NKB_KP_Enter		0xFF8D	/* enter */
#define NKB_KP_F1		0xFF91	/* PF1, KP_A, ... */
#define NKB_KP_F2		0xFF92
#define NKB_KP_F3		0xFF93
#define NKB_KP_F4		0xFF94
#define NKB_KP_Home		0xFF95
#define NKB_KP_Left		0xFF96
#define NKB_KP_Up		0xFF97
#define NKB_KP_Right		0xFF98
#define NKB_KP_Down		0xFF99
#define NKB_KP_Prior		0xFF9A
#define NKB_KP_Page_Up		0xFF9A
#define NKB_KP_Next		0xFF9B
#define NKB_KP_Page_Down		0xFF9B
#define NKB_KP_End		0xFF9C
#define NKB_KP_Begin		0xFF9D
#define NKB_KP_Insert		0xFF9E
#define NKB_KP_Delete		0xFF9F
#define NKB_KP_Equal		0xFFBD	/* equals */
#define NKB_KP_Multiply		0xFFAA
#define NKB_KP_Add		0xFFAB
#define NKB_KP_Separator		0xFFAC	/* separator, often comma */
#define NKB_KP_Subtract		0xFFAD
#define NKB_KP_Decimal		0xFFAE
#define NKB_KP_Divide		0xFFAF

#define NKB_KP_0			0xFFB0
#define NKB_KP_1			0xFFB1
#define NKB_KP_2			0xFFB2
#define NKB_KP_3			0xFFB3
#define NKB_KP_4			0xFFB4
#define NKB_KP_5			0xFFB5
#define NKB_KP_6			0xFFB6
#define NKB_KP_7			0xFFB7
#define NKB_KP_8			0xFFB8
#define NKB_KP_9			0xFFB9



/*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 */

#define NKB_F1			0xFFBE
#define NKB_F2			0xFFBF
#define NKB_F3			0xFFC0
#define NKB_F4			0xFFC1
#define NKB_F5			0xFFC2
#define NKB_F6			0xFFC3
#define NKB_F7			0xFFC4
#define NKB_F8			0xFFC5
#define NKB_F9			0xFFC6
#define NKB_F10			0xFFC7
#define NKB_F11			0xFFC8
#define NKB_L1			0xFFC8
#define NKB_F12			0xFFC9
#define NKB_L2			0xFFC9
#define NKB_F13			0xFFCA
#define NKB_L3			0xFFCA
#define NKB_F14			0xFFCB
#define NKB_L4			0xFFCB
#define NKB_F15			0xFFCC
#define NKB_L5			0xFFCC
#define NKB_F16			0xFFCD
#define NKB_L6			0xFFCD
#define NKB_F17			0xFFCE
#define NKB_L7			0xFFCE
#define NKB_F18			0xFFCF
#define NKB_L8			0xFFCF
#define NKB_F19			0xFFD0
#define NKB_L9			0xFFD0
#define NKB_F20			0xFFD1
#define NKB_L10			0xFFD1
#define NKB_F21			0xFFD2
#define NKB_R1			0xFFD2
#define NKB_F22			0xFFD3
#define NKB_R2			0xFFD3
#define NKB_F23			0xFFD4
#define NKB_R3			0xFFD4
#define NKB_F24			0xFFD5
#define NKB_R4			0xFFD5
#define NKB_F25			0xFFD6
#define NKB_R5			0xFFD6
#define NKB_F26			0xFFD7
#define NKB_R6			0xFFD7
#define NKB_F27			0xFFD8
#define NKB_R7			0xFFD8
#define NKB_F28			0xFFD9
#define NKB_R8			0xFFD9
#define NKB_F29			0xFFDA
#define NKB_R9			0xFFDA
#define NKB_F30			0xFFDB
#define NKB_R10			0xFFDB
#define NKB_F31			0xFFDC
#define NKB_R11			0xFFDC
#define NKB_F32			0xFFDD
#define NKB_R12			0xFFDD
#define NKB_F33			0xFFDE
#define NKB_R13			0xFFDE
#define NKB_F34			0xFFDF
#define NKB_R14			0xFFDF
#define NKB_F35			0xFFE0
#define NKB_R15			0xFFE0

/* Modifiers */

#define NKB_Shift_L		0xFFE1	/* Left shift */
#define NKB_Shift_R		0xFFE2	/* Right shift */
#define NKB_Control_L		0xFFE3	/* Left control */
#define NKB_Control_R		0xFFE4	/* Right control */
#define NKB_Caps_Lock		0xFFE5	/* Caps lock */
#define NKB_Shift_Lock		0xFFE6	/* Shift lock */

#define NKB_Meta_L		0xFFE7	/* Left meta */
#define NKB_Meta_R		0xFFE8	/* Right meta */
#define NKB_Alt_L		0xFFE9	/* Left alt */
#define NKB_Alt_R		0xFFEA	/* Right alt */
#define NKB_Super_L		0xFFEB	/* Left super */
#define NKB_Super_R		0xFFEC	/* Right super */
#define NKB_Hyper_L		0xFFED	/* Left hyper */
#define NKB_Hyper_R		0xFFEE	/* Right hyper */




#define NKB_space               0x020
#define NKB_exclam              0x021
#define NKB_quotedbl            0x022
#define NKB_numbersign          0x023
#define NKB_dollar              0x024
#define NKB_percent             0x025
#define NKB_ampersand           0x026
#define NKB_apostrophe          0x027
#define NKB_quoteright          0x027	/* deprecated */
#define NKB_parenleft           0x028
#define NKB_parenright          0x029
#define NKB_asterisk            0x02a
#define NKB_plus                0x02b
#define NKB_comma               0x02c
#define NKB_minus               0x02d
#define NKB_period              0x02e
#define NKB_slash               0x02f
#define NKB_0                   0x030
#define NKB_1                   0x031
#define NKB_2                   0x032
#define NKB_3                   0x033
#define NKB_4                   0x034
#define NKB_5                   0x035
#define NKB_6                   0x036
#define NKB_7                   0x037
#define NKB_8                   0x038
#define NKB_9                   0x039
#define NKB_colon               0x03a
#define NKB_semicolon           0x03b
#define NKB_less                0x03c
#define NKB_equal               0x03d
#define NKB_greater             0x03e
#define NKB_question            0x03f
#define NKB_at                  0x040
#define NKB_A                   0x041
#define NKB_B                   0x042
#define NKB_C                   0x043
#define NKB_D                   0x044
#define NKB_E                   0x045
#define NKB_F                   0x046
#define NKB_G                   0x047
#define NKB_H                   0x048
#define NKB_I                   0x049
#define NKB_J                   0x04a
#define NKB_K                   0x04b
#define NKB_L                   0x04c
#define NKB_M                   0x04d
#define NKB_N                   0x04e
#define NKB_O                   0x04f
#define NKB_P                   0x050
#define NKB_Q                   0x051
#define NKB_R                   0x052
#define NKB_S                   0x053
#define NKB_T                   0x054
#define NKB_U                   0x055
#define NKB_V                   0x056
#define NKB_W                   0x057
#define NKB_X                   0x058
#define NKB_Y                   0x059
#define NKB_Z                   0x05a
#define NKB_bracketleft         0x05b
#define NKB_backslash           0x05c
#define NKB_bracketright        0x05d
#define NKB_asciicircum         0x05e
#define NKB_underscore          0x05f
#define NKB_grave               0x060
#define NKB_quoteleft           0x060	/* deprecated */
#define NKB_a                   0x061
#define NKB_b                   0x062
#define NKB_c                   0x063
#define NKB_d                   0x064
#define NKB_e                   0x065
#define NKB_f                   0x066
#define NKB_g                   0x067
#define NKB_h                   0x068
#define NKB_i                   0x069
#define NKB_j                   0x06a
#define NKB_k                   0x06b
#define NKB_l                   0x06c
#define NKB_m                   0x06d
#define NKB_n                   0x06e
#define NKB_o                   0x06f
#define NKB_p                   0x070
#define NKB_q                   0x071
#define NKB_r                   0x072
#define NKB_s                   0x073
#define NKB_t                   0x074
#define NKB_u                   0x075
#define NKB_v                   0x076
#define NKB_w                   0x077
#define NKB_x                   0x078
#define NKB_y                   0x079
#define NKB_z                   0x07a
#define NKB_braceleft           0x07b
#define NKB_bar                 0x07c
#define NKB_braceright          0x07d
#define NKB_asciitilde          0x07e

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
