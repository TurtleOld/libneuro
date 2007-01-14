
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

/* events.c 
 * Module Events_
 * Handling of the Input events(keyboard, mouse, ...)
 *
 * This is an engine and it will require a function to be able to
 * set which keyboard events will go with which functions.
 */

#include <extlib.h>
#include <events.h>
#include <ebuf.h> /* EBUF memory functions */

/* define the structure to hold our keyboard key header.
 * This header will be used for a EBUF buffer.
 */
typedef struct KEYBEVENT
{
	u32 key;		/* the actual keyboard key keysym */
	u8 lastState;		/* used to know if its been previously clicked or released  0 is nothing, 1 is that it was clicked previously */
	void (*callback)(); 	/* a callback we will call when key is pressed*/
	void (*callbackReleased)(); /* a callback we will call when the key is no longer pressed */
	u8 send_key; 		/* werther or not we send to key value as argument 
       				 * to the callback.
				 */
}KEYBEVENT;

typedef struct Mouse_Callback
{
	void (*callback)(int x, int y);
}Mouse_Callback;

/* define the structure to hold our mouse button header. 
 * This header will be used for a EBUF buffer.
 */
typedef struct MOUSEEVENT
{
	u32 button;		/* the actual button */
	u8 lastState; 		/* used to know if its been previously clicked or released  0 is nothing, 1 is that it was clicked previously */
	EBUF *ClickEventCbk; /* contains Mouse_Callback elements */
	EBUF *ReleaseEventCbk; /* contains Mouse_Callback elements */

	void (*callbackClick)(int x, int y); 	/* callback to call when clicked */
	void (*callbackRelease)(int x, int y); 	/* callback to call when released */
}MOUSEEVENT;

/* keyboard list buffer.
 * to keep track of what callback to call when
 * a certain key is triggered.
 */
static EBUF *_klist;

/* mouse list buffer. 
 * to keep track of what callback to call when
 * a certain mouse button is triggered.
 */
static EBUF *_mlist;


/* those are the mouse coordinates so external 
 * programs can get them from us.
 */
static int mouse_x, mouse_y;


static void 
clean_mouse_list(void *src)
{
	MOUSEEVENT *tmp;

	tmp = (MOUSEEVENT*)src;

	if (tmp)
	{
		Neuro_CleanEBuf(&tmp->ClickEventCbk);
		Neuro_CleanEBuf(&tmp->ReleaseEventCbk);
	}
}

/* 
 * this function checks every elements in the _klist buffer, 
 * checks which keys are being pressed in it and calls the
 * corresponding callbacks.
 */
static void
handle_keys()
{
	u32 total;
	KEYBEVENT *tmp;
	
	
	/* we check if _klist is empty, if it is, we leave. */
	if (Neuro_EBufIsEmpty(_klist))
		return;
	
	/* we put the total amount of elements in _klist to total 
	 * and increment by 1 to support a while (somevar-- > 0) 
	 * method.
	 */
	total = Neuro_GiveEBufCount(_klist) + 1;
	
	while (total-- > 0)
	{
		/* pass the element total from the buffer _klist
		 * to the pointer tmp.
		 */
		tmp = Neuro_GiveEBuf(_klist, total);

		if (tmp->lastState == 0)
		{

			/* we check if the key is being pressed */
			if (Lib_CheckKeyStatus(tmp->key) == 1)
			{
				tmp->lastState = 1;
				/* check if the callback exists */
				if (tmp->callback)
				{
					/* the key is being pressed so we call the 
					 * corresponding callback function.
					 */
					if (tmp->send_key == 0)
						(tmp->callback)();
					else
						(tmp->callback)(tmp->key);
				}
			}
		}
		else
		{
			if (Lib_CheckKeyStatus(tmp->key) == 0)
			{
				tmp->lastState = 0;
				if (tmp->callbackReleased)
				{
					/* the key is not pressed anymore so we call the 
					 * corresponding callback function.
					 */
				
					if (tmp->send_key == 0)
						(tmp->callbackReleased)();
					else
						(tmp->callbackReleased)(tmp->key);
				
				}
			}
			else
			{
				if (tmp->callback)
				{
					/* the key is being pressed so we call the 
					 * corresponding callback function.
					 */
					if (tmp->send_key == 0)
						(tmp->callback)();
					else
						(tmp->callback)(tmp->key);
				}
			}
		}
	}
}

/* 
 * this function checks every elements in the _mlist buffer.
 * It checks to see if any corresponds to the current button on 
 * the mouse being pressed or released.
 *
 * instead of using a switch method and getting mouse button pushed
 * and released cases, we implement our own way to keep track of which
 * one is being triggered by keeping track of the state of the button.
 * 
 */
static void
handle_mouse()
{
	u8 button;
	int x, y;
	u32 total, total2;
	MOUSEEVENT *tmp;
	Mouse_Callback *cbk_buf; /* callback buffer */
	
	/* check to see if the buffer _mlist is empty and leave if it is */
	if (Neuro_EBufIsEmpty(_mlist))
		return;	
	
	
	/* put the current button state into button 
	 * and put the current x,y coordinates in x and y.
	 */
	button = Lib_GetMouseState(&x, &y);

	mouse_x = x;
	mouse_y = y;
	
	/* we put the total amount of elements in _mlist to total 
	 * and increment by 1 to support a while (somevar-- > 0) 
	 * method.
	 */
	total = Neuro_GiveEBufCount(_mlist) + 1;

	/* Debug_Val(0, "mouse (%d,%d) button %d \n", x, y, button); */

	while (total-- > 0)
	{
		/* pass the element total from the buffer 
		 * _mlist into tmp
		 */
		tmp = Neuro_GiveEBuf(_mlist, total);
		
		if (button)
		{
			if (button == tmp->button)
			{
				if (!tmp->lastState)
				{
					/* this elements last state now is set to 1 
					 * meaning it was last clicked.
					 */
					tmp->lastState = 1;

					if (Neuro_EBufIsEmpty(tmp->ClickEventCbk))
						return;

					total2 = Neuro_GiveEBufCount(tmp->ClickEventCbk) + 1;

					while (total2-- > 0)
					{
						cbk_buf = Neuro_GiveEBuf(tmp->ClickEventCbk, total2);

						if (cbk_buf->callback)
							(cbk_buf->callback)(x, y);
					}

					return;
				}
			}
		}
		else
		{
			if (tmp->lastState == 1)
			{
				/* we reset the state back to nothing */
				tmp->lastState = 0;
			
				if (Neuro_EBufIsEmpty(tmp->ReleaseEventCbk))
					return;

				total2 = Neuro_GiveEBufCount(tmp->ReleaseEventCbk) + 1;

				while (total2-- > 0)
				{
					cbk_buf = Neuro_GiveEBuf(tmp->ReleaseEventCbk, total2);

					if (cbk_buf->callback)
						(cbk_buf->callback)(x, y);
				}

				return;
			}
		}	
	}
}

/*
 * Used to pass data to the
 * MOUSEEVENT header.
 */
static void
mouseListChange(u32 button, void (*callback)(int x, int y), MOUSEEVENT *ptr, u8 click_release)
{
	Mouse_Callback *tmp;
	EBUF *current;
	
	
	ptr->button = button;

	if (click_release)
	{
		if (Neuro_EBufIsEmpty(ptr->ReleaseEventCbk))
			Neuro_CreateEBuf(&ptr->ReleaseEventCbk);

		current = ptr->ReleaseEventCbk;
	}
	else
	{
		if (Neuro_EBufIsEmpty(ptr->ClickEventCbk))
			Neuro_CreateEBuf(&ptr->ClickEventCbk);

		current = ptr->ClickEventCbk;
	}

	Neuro_AllocEBuf(current, sizeof(Mouse_Callback*), sizeof(Mouse_Callback));


	tmp = Neuro_GiveCurEBuf(current);
	
	tmp->callback = callback;
}

static void
addKeyEvent(u32 keysym, void (*pressedCallback)(), void (*releasedCallback)(), u8 send_key)
{
	KEYBEVENT *tmp = NULL;

	if (Neuro_EBufIsEmpty(_klist))
	{
		Neuro_AllocEBuf(_klist, sizeof(KEYBEVENT*), sizeof(KEYBEVENT));
		tmp = Neuro_GiveCurEBuf(_klist);
	}
	else
	{
		u32 total = 0;

		total = Neuro_GiveEBufCount(_klist) + 1;

		/* here we search to see if the keysym was already added so we can 
		 * update it directly instead of dumbly make a new one.
		 */
		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(_klist, total);

			if (tmp->key == keysym)
				break;
			
			tmp = NULL;
		}

		if (tmp == NULL)
		{
			Neuro_AllocEBuf(_klist, sizeof(KEYBEVENT*), sizeof(KEYBEVENT));
			tmp = Neuro_GiveCurEBuf(_klist);
		}
	}

	tmp->key = keysym;
	if (pressedCallback)
		tmp->callback = pressedCallback;
	if (releasedCallback)
		tmp->callbackReleased = releasedCallback;
	
	tmp->send_key = send_key;
	tmp->lastState = 0;
}

static void
addKeyPressEvent(u32 keysym, void (*callback)(), u8 send_key)
{
	addKeyEvent(keysym, callback, NULL, send_key);
}

static void
addKeyReleaseEvent(u32 keysym, void (*callback)(), u8 send_key)
{
	addKeyEvent(keysym, NULL, callback, send_key);
}

/*--- Global Functions ---*/

void
Neuro_GetMousePos(int *x, int *y)
{
	if (x)
		*x = mouse_x;
	if (y)
		*y = mouse_y;
}

void 
Neuro_AddPressedKeyEvent(u32 keysym, void (*callback)())
{
	addKeyPressEvent(keysym, callback, 0);
}

void 
Neuro_AddReleasedKeyEvent(u32 keysym, void (*callback)())
{
	addKeyReleaseEvent(keysym, callback, 0);
}

void
Neuro_AddPressedMultiKeyEvent(u32 keysym, void (*callback)())
{
	addKeyPressEvent(keysym, callback, 1);
}

void
Neuro_AddReleasedMultiKeyEvent(u32 keysym, void (*callback)())
{
	addKeyReleaseEvent(keysym, callback, 1);
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
	_klist = NULL;

	Neuro_CreateEBuf(&_klist);
}

void
Neuro_CleanMouse()
{
	Neuro_CleanEBuf(&_mlist);
	_mlist = NULL;
	
	Neuro_CreateEBuf(&_mlist);
}


void
Events_Poll()
{	
	handle_keys();
	handle_mouse();
}

int
Events_Init()
{
	Neuro_CreateEBuf(&_klist);
	Neuro_CreateEBuf(&_mlist);

	Neuro_SetcallbEBuf(_mlist, clean_mouse_list);

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
