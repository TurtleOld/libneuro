/* events.c 
 * Module Events_
 * Handling of the Input events(keyboard, mouse, ...)
 *
 * This is an engine and it will require a function to be able to
 * set which keyboard events will go with which functions.
 */

#include <extlib.h>
#include <events.h>
#include <ebuf.h>

/* keyboard buffers */
typedef struct KEYBEVENT
{
	u32 key;
	void (*callback)();
}KEYBEVENT;

/* mouse buffers */
typedef struct MOUSEEVENT
{
	u32 button;
	u8 lastState; /* used to know if its been previously clicked or released  0 is nothing, 1 is that it was clicked previously */
	void (*callbackClick)(int x, int y); /* callback to call when clicked */
	void (*callbackRelease)(int x, int y); /* callback to call when released */
}MOUSEEVENT;

static EBUF *_klist; /* keyboard buffer */
static EBUF *_mlist; /* mouse buffer */

/* this function is to check if keys are still being pushed.
 * It calls a function to get an array of all the keys on a
 * keyboard. We then check the keys we want to see the status of.
 * they give 1 if they r still being pushed and 0 if they aren't. 
 */
static void
handle_keys()
{
#if old_method
	u8 *key;
#endif /* old_method */
	u32 total;
	KEYBEVENT *tmp;
	
	if (Neuro_EBufIsEmpty(_klist))
		return;
	
	total = Neuro_GiveEBufCount(_klist) + 1;
	
#if old_method
	key = Lib_GetKeyState(NULL);

	if (!key)
		return;
		
	while (total-- > 0)
	{
		tmp = Neuro_GiveEBuf(_klist, total);
		if (key[tmp->key])
			(tmp->callback)();	
	}
#endif /* old_method */
	while (total-- > 0)
	{
		tmp = Neuro_GiveEBuf(_klist, total);
		if (Lib_CheckKeyStatus(tmp->key))
			(tmp->callback)();
	}
}

static void
handle_mouse()
{
	u8 button;
	int x, y;
	u32 total;
	MOUSEEVENT *tmp;
	
	if (Neuro_EBufIsEmpty(_mlist))
		return;	
	
	button = Lib_GetMouseState(&x, &y);
	
	total = Neuro_GiveEBufCount(_mlist) + 1;

	/* printf("mouse (%d,%d) button %d \n", x, y, button); */

	while (total-- > 0)
	{
		tmp = Neuro_GiveEBuf(_mlist, total);
		
		if (button)
		{
			if (button == tmp->button)
			{
				if (!tmp->lastState)
				{
					if (tmp->callbackClick)
						(tmp->callbackClick)(x, y);
					tmp->lastState = 1;
				}
			}
		}
		else
		{
			if (tmp->lastState == 1)
			{
				if (tmp->callbackRelease)
					(tmp->callbackRelease)(x, y);
				tmp->lastState = 0;
					
			}
		}	
	}
}

static void
mouseListChange(u32 button, void (*callback)(), MOUSEEVENT *ptr, u8 click_release)
{
	ptr->button = button;
	if (click_release)
		ptr->callbackRelease = callback;
	else
		ptr->callbackClick = callback;
}

/*--- Global Functions ---*/

void 
Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)())
{
	KEYBEVENT *tmp;

	Neuro_AllocEBuf(_klist, sizeof(KEYBEVENT*), sizeof(KEYBEVENT));

	tmp = Neuro_GiveCurEBuf(_klist);

	tmp->key = keysym;
	tmp->callback = callback;
}

void
Neuro_AddPressedMouseEvent(u32 button, void (*callback)())
{
	MOUSEEVENT *tmp;
	u32 total = 0;
	
	if (Neuro_EBufIsEmpty(_mlist))
	{
		Neuro_AllocEBuf(_mlist, sizeof(MOUSEEVENT*), sizeof(MOUSEEVENT));
	}
	else
	{
		total = Neuro_GiveEBufCount(_mlist) + 1;
		/* try to find the corresponding button so we can 
		 * change its data -- if it exists.
		 */
		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(_mlist, total);
			if (tmp->button == button)
			{
				mouseListChange(button, callback, tmp, 0);
				/* we changed the data and now we bail out */
				return;
			}
		}
		/* the mouse button wasn't found so we will create a new one for it */
		Neuro_AllocEBuf(_mlist, sizeof(MOUSEEVENT*), sizeof(MOUSEEVENT));
	}
	mouseListChange(button, callback, Neuro_GiveCurEBuf(_mlist), 0);
}

void
Neuro_AddReleasedMouseEvent(u32 button, void (*callback)())
{
	MOUSEEVENT *tmp;
	u32 total = 0;
	
	if (Neuro_EBufIsEmpty(_mlist))
		Neuro_AllocEBuf(_mlist, sizeof(MOUSEEVENT*), sizeof(MOUSEEVENT));
	else
	{
		total = Neuro_GiveEBufCount(_mlist) + 1;
		/* try to find the corresponding button so we can 
		 * change its data -- if it exists.
		 */
		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(_mlist, total);
			if (tmp->button == button)
			{
				mouseListChange(button, callback, tmp, 1);
				/* we changed the data and now we bail out */
				return;
			}
		}
		/* the mouse button wasn't found so we will create a new one for it */
		Neuro_AllocEBuf(_mlist, sizeof(MOUSEEVENT*), sizeof(MOUSEEVENT));
	}
	
	mouseListChange(button, callback, Neuro_GiveCurEBuf(_mlist), 1);
}

void
Neuro_CleanKeyboard()
{
	Neuro_CleanEBuf(&_klist);
	Neuro_CreateEBuf(&_klist);
}

void
Neuro_CleanMouse()
{
	Neuro_CleanEBuf(&_mlist);
	Neuro_CreateEBuf(&_mlist);
}


void
Events_Poll()
{	
	Lib_EventPoll();

	handle_keys();
	handle_mouse();
}

int
Events_Init()
{
	Neuro_CreateEBuf(&_klist);
	Neuro_CreateEBuf(&_mlist);

	if (!_klist || !_mlist)
		return 1;
	
	return 0;
}

void
Events_Clean()
{
	Neuro_CleanEBuf(&_klist);
	Neuro_CleanEBuf(&_mlist);
}
