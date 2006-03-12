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

#endif /* NOT __EXTLIB_SDL_H */
