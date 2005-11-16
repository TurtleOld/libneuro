/* events.c 
 * Module Events_
 * Handling of the Input events(keyboard, mouse, ...)
 *
 * This is an engine and it will require a function to be able to
 * set which keyboard events will go with which functions.
 */

/*--- Extern Headers Including ---*/
#include <stdlib.h>

/*--- Local Headers Including ---*/
#include "extlib.h"

/*--- Main Module Header ---*/
#include "events.h"

/*--- Global Variables ---*/

/*--- Static Variables ---*/

/* keyboard buffers */
typedef struct
{
	u32 key;
	void (*callback)();
}PEVENTLIST;

struct
{
	u32 total;
	PEVENTLIST *kevents;
}KEventList;

/* mouse buffers */
typedef struct
{
	u32 button;
	u8 lastState; /* used to know if its been previously clicked or released  0 is nothing, 1 is that it was clicked previously */
	void (*callbackClick)(); /* callback to call when clicked */
	void (*callbackRelease)(); /* callback to call when released */
}MEVENTLIST;

typedef struct EventList
{
	u32 total;
	MEVENTLIST *mevents;
}EventList;

EventList MEventList;

/*--- Static Prototypes ---*/



/*--- Static Functions ---*/


static void
clean_keyboard()
{
	if (KEventList.kevents)
	{
		free(KEventList.kevents);
		KEventList.kevents = NULL;
		KEventList.total = 0;
	}	
}

static void
clean_mouse()
{
	if (MEventList.mevents)
	{
		free(MEventList.mevents);
		MEventList.mevents = NULL;
		MEventList.total = 0;
	}
}

/* this function is to check if keys are still being pushed.
 * It calls a function to get an array of all the keys on a
 * keyboard. We then check the keys we want to see the status of.
 * they give 1 if they r still being pushed and 0 if they aren't. 
 */
static void
handle_keys()
{
	u8 *key;
	u32 i;
	
	if (KEventList.kevents == NULL)
		return;
	
	key = Lib_GetKeyState(NULL);
	i = KEventList.total;

	if (!key)
		return;
	
	while (i-- > 0)
	{
		if (key[KEventList.kevents[i].key])
		{
			(KEventList.kevents[i].callback)();
		}
	}
#if temp
	if (key[SDLK_UP])
		Player_UpWard();
	
	if (key[SDLK_DOWN])
		Player_DownWard();
	
	if (key[SDLK_LEFT])
		Player_LeftWard();

	if (key[SDLK_RIGHT])
		Player_RightWard();

	if (key[SDLK_KP8])
		Camera_UpWard();

	if (key[SDLK_KP6])
		Camera_RightWard();

	if (key[SDLK_KP2])
		Camera_DownWard();

	if (key[SDLK_KP4])
		Camera_LeftWard();
#endif /* temp */
}

static void
handle_mouse()
{
	u8 button;
	int x, y;
	u32 i;
	
	
	if (!MEventList.mevents)
	{
		/* printf("the list is empty, bailing out\n"); */
		return;
	}
	
	button = Lib_GetMouseState(&x, &y);
	
	i = MEventList.total;

	/* printf("mouse (%d,%d) button %d \n", x, y, button); */
	
	while (i-- > 0)
	{
		if (button)
		{
			if (button == MEventList.mevents[i].button)
			{
				if (!MEventList.mevents[i].lastState)
				{
					(MEventList.mevents[i].callbackClick)(x, y);
					MEventList.mevents[i].lastState = 1;
				}
				/*else
				{
					(MEventList.mevents[i].callbackRelease)(x, y);
					MEventList.mevents[i].lastState = 0;
				}*/
			}
		}
		else
		{
			if (MEventList.mevents[i].lastState == 1)
			{
				(MEventList.mevents[i].callbackRelease)(x, y);
				if (MEventList.mevents)
					MEventList.mevents[i].lastState = 0;
			}
		}
	}
}

static void
mouseListChange(u32 button, void (*callback)(), u32 listCursor, u8 click_release)
{
	MEventList.mevents[listCursor].button = button;
	if (!click_release)
		MEventList.mevents[listCursor].callbackClick = callback;
	if (click_release)
		MEventList.mevents[listCursor].callbackRelease = callback;
}

/*--- Global Functions ---*/

void 
Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)())
{
	if (KEventList.kevents == NULL)
	{
		KEventList.kevents = (PEVENTLIST*)calloc(1, sizeof(PEVENTLIST));
		KEventList.total = 0;
	}
	else
	{
		KEventList.kevents = (PEVENTLIST*)realloc(KEventList.kevents, sizeof(PEVENTLIST) * (1 + KEventList.total));
	}
	
	KEventList.kevents[KEventList.total].key = keysym;
	KEventList.kevents[KEventList.total].callback = callback;
	
	KEventList.total++;
}

void
Neuro_AddPressedMouseEvent(u32 button, void (*callback)())
{
	u32 i = 0;
	
	/* printf("adding mouse event\n"); */
	if (MEventList.mevents == NULL)
	{
		MEventList.mevents = (MEVENTLIST*)calloc(1, sizeof(MEVENTLIST));
		MEventList.total = 0;
	}
	else
	{
		i = MEventList.total;
		while (i-- > 0)
		{
			if (button == MEventList.mevents[i].button)
			{
				mouseListChange(button, callback, i, 0);
				return;
			}
		}
		MEventList.mevents = (MEVENTLIST*)realloc(MEventList.mevents, sizeof(MEVENTLIST) * (1 + MEventList.total));
	}
	mouseListChange(button, callback, MEventList.total, 0);
	MEventList.total++;
}

void
Neuro_AddReleasedMouseEvent(u32 button, void (*callback)())
{
	u32 i = 0;
	if (MEventList.mevents == NULL)
	{
		MEventList.mevents = (MEVENTLIST*)calloc(1, sizeof(MEVENTLIST));
		MEventList.total = 0;
	}
	else
	{
		i = MEventList.total;
		while (i-- > 0)
		{
			if (button == MEventList.mevents[i].button)
			{
				mouseListChange(button, callback, i, 1);
				return;
			}
		}
		MEventList.mevents = (MEVENTLIST*)realloc(MEventList.mevents, sizeof(MEVENTLIST) * (1 + MEventList.total));
	}
	mouseListChange(button, callback, MEventList.total, 1);
	MEventList.total++;
}

void
Neuro_CleanKeyboard()
{
	clean_keyboard();
}

void
Neuro_CleanMouse()
{
	clean_mouse();
}


/*--- Poll ---*/
void
Events_Poll()
{	
	Lib_EventPoll();

	handle_keys();
	handle_mouse();
}

/*--- Constructor Destructor ---*/
int
Events_Init()
{
	return 0;
}

void
Events_Clean()
{
	clean_keyboard();
	clean_mouse();
}
