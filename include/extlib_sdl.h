#ifndef __EXTLIB_SDL_H
#define __EXTLIB_SDL_H

/* override keymap macros to support SDL */
#ifdef XK_BackSpace

#undef XK_BackSpace
#undef XK_Tab
#undef XK_Linefeed
#undef XK_Clear
#undef XK_Return
#undef XK_Pause
#undef XK_Scroll_Lock
#undef XK_Sys_Req
#undef XK_Escape
#undef XK_Delete
#undef XK_Up
#undef XK_Down
#undef XK_Right
#undef XK_Left
#undef XK_Home
#undef XK_Page_Up
#undef XK_Page_Down

/* numeric keypad */
#undef XK_KP_0
#undef XK_KP_1
#undef XK_KP_2
#undef XK_KP_3
#undef XK_KP_4
#undef XK_KP_5
#undef XK_KP_6
#undef XK_KP_7
#undef XK_KP_8
#undef XK_KP_9
#undef XK_KP_Decimal
#undef XK_KP_Divide
#undef XK_KP_Multiply
#undef XK_KP_Subtract
#undef XK_KP_Add
#undef XK_KP_Enter
#undef XK_KP_Equal

/* numlock elements */
#undef XK_KP_Insert
#undef XK_KP_End
#undef XK_KP_Down
#undef XK_KP_Page_Down
#undef XK_KP_Left
#undef XK_KP_Begin
#undef XK_KP_Right
#undef XK_KP_Home
#undef XK_KP_Up
#undef XK_KP_Page_Up
#undef XK_KP_Delete

#endif /* XK_BackSpace */



#define XK_BackSpace		0x0008	/* back space, back char */
#define XK_Tab			0x0009
#define XK_Linefeed		0x000A	/* Linefeed, LF */
#define XK_Clear		0x000C
#define XK_Return		0x000D	/* Return, enter */
#define XK_Pause		0x0013	/* Pause, hold */
#define XK_Scroll_Lock		0x0014
#define XK_Sys_Req		0x0015
#define XK_Escape		0x001B
#define XK_Delete		0x007E	/* Delete, rubout */

#define XK_Up			0x0111	/* Move up, up arrow */
#define XK_Down			0x0112	/* Move down, down arrow */
#define XK_Right		0x0113	/* Move right, right arrow */
#define XK_Left			0x0114	/* Move left, left arrow */
#define XK_Home			0x0116
/*#define XK_Prior		0x0155*/	/* Prior, previous */
#define XK_Page_Up		0x0118
/*#define XK_Next			0x0156*/	/* Next */
#define XK_Page_Down		0x0119
/*#define XK_End			0x0157*/	/* EOL */
/*#define XK_Begin		0x0158*/	/* BOL */

/* numeric keypad */
#define XK_KP_0			256
#define XK_KP_1			257
#define XK_KP_2			258
#define XK_KP_3			259
#define XK_KP_4			260
#define XK_KP_5			261
#define XK_KP_6			262
#define XK_KP_7			263
#define XK_KP_8			264
#define XK_KP_9			265
#define XK_KP_Decimal		266
#define XK_KP_Divide		267
#define XK_KP_Multiply		268
#define XK_KP_Subtract		269
#define XK_KP_Add		270
#define XK_KP_Enter		271
#define XK_KP_Equal		272

/* numlock elements */
/*
#define XK_KP_Insert
#define XK_KP_End
#define XK_KP_Down
#define XK_KP_Page_Down
#define XK_KP_Left
#define XK_KP_Begin
#define XK_KP_Right
#define XK_KP_Home
#define XK_KP_Up
#define XK_KP_Page_Up
#define XK_KP_Delete
*/

#endif /* NOT __EXTLIB_SDL_H */
