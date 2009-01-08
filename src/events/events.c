
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

#include <global.h>
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
	/* second version of the above callbacks, used when a single KEYBEVENT is set to catch all the keysyms 
	 * and return them to the end program.
	 */ 
	void (*callback2)(u32 keysym);
	void (*callbackReleased2)(u32 keysym);
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


static void
trigger_key(KEYBEVENT *key, u8 keystatus)
{
	
	if (key->lastState == 0)
	{
		/* we check if the key is being pressed */
		if (keystatus == 1)
		{
			key->lastState = 1;
			/* check if the callback exists */
			if (key->callback)
			{
				/* the key is being pressed so we call the 
				 * corresponding callback function.
				 */
				if (key->send_key == 0)
					(key->callback)();
				else
					(key->callback2)(key->key);
			}
		}
	}
	else
	{
		if (keystatus == 0)
		{
			key->lastState = 0;
			if (key->callbackReleased)
			{
				/* the key is not pressed anymore so we call the 
				 * corresponding callback function.
				 */
			
				if (key->send_key == 0)
					(key->callbackReleased)();
				else
					(key->callbackReleased2)(key->key);
			
			}
		}
		else
		{
			if (key->callback)
			{
				/* the key is being pressed so we call the 
				 * corresponding callback function.
				 */
				if (key->send_key == 0)
					(key->callback)();
				else
					(key->callback2)(key->key);
			}
		}
	}
}

/* 
 * this function checks every elements in the _klist buffer, 
 * checks which keys are being pressed in it and calls the
 * corresponding callbacks.
 */
static void
handle_keys(void)
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

		trigger_key(tmp, Lib_CheckKeyStatus(tmp->key));
	}
}

static void
trigger_button(MOUSEEVENT *mouse, int x, int y, int status)
{
	u32 total = 0;
	Mouse_Callback *cbk_buf; /* callback buffer */

	if (mouse->lastState == 0)
	{
		/* this elements last state now is set to 1 
		 * meaning it was last clicked.
		 */
		mouse->lastState = 1;

		if (Neuro_EBufIsEmpty(mouse->ClickEventCbk))
			return;

		total = Neuro_GiveEBufCount(mouse->ClickEventCbk) + 1;

		while (total-- > 0)
		{
			cbk_buf = Neuro_GiveEBuf(mouse->ClickEventCbk, total);

			if (cbk_buf->callback)
				(cbk_buf->callback)(x, y);
		}
	}
	else if (mouse->lastState == 1)
	{
		/* we reset the state back to nothing */
		mouse->lastState = 0;
	
		if (Neuro_EBufIsEmpty(mouse->ReleaseEventCbk))
			return;

		total = Neuro_GiveEBufCount(mouse->ReleaseEventCbk) + 1;

		while (total-- > 0)
		{
			cbk_buf = Neuro_GiveEBuf(mouse->ReleaseEventCbk, total);

			if (cbk_buf->callback)
				(cbk_buf->callback)(x, y);
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
handle_mouse(void)
{
	u8 button;
	int x, y;
	u32 total;
	MOUSEEVENT *tmp;
	
	/* check to see if the buffer _mlist is empty and leave if it is */
	if (Neuro_EBufIsEmpty(_mlist))
		return;	
	
	
	/* put the current button state into button 
	 * and put the current x,y coordinates in x and y.
	 */
	button = Lib_GetMouseState(&x, &y);

	/* we pass those as variables which can be recovered
	 * by external programs at will.
	 */
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
				trigger_button(tmp, x, y, 1);
			else
				trigger_button(tmp, x, y, 0);
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
addKeyEvent(u32 keysym, void (*pressedCallback)(), void (*releasedCallback)(), 
		void (*pressedCallback2)(u32 keysym), void (*releasedCallback2)(u32 keysym), u8 send_key)
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
		/*
		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(_klist, total);

			if (tmp->key == keysym)
				break;
			
			tmp = NULL;
		}
		*/

		if (tmp == NULL)
		{
			Neuro_AllocEBuf(_klist, sizeof(KEYBEVENT*), sizeof(KEYBEVENT));
			tmp = Neuro_GiveCurEBuf(_klist);
		}
	}

	tmp->key = keysym;
	if (pressedCallback)
	{
		if (send_key)
			tmp->callback2 = pressedCallback2;
		else
			tmp->callback = pressedCallback;
	}
	if (releasedCallback)
	{
		if (send_key)
			tmp->callbackReleased2 = releasedCallback2;
		else
			tmp->callbackReleased = releasedCallback;
	}
	
	tmp->send_key = send_key;
	tmp->lastState = 0;
}

static void
addKeyPressEvent(u32 keysym, void (*callback)(), void (*callback2)(u32 keysym), u8 send_key)
{
	if (send_key)
		addKeyEvent(keysym, NULL, NULL, callback2, NULL, send_key);
	else
		addKeyEvent(keysym, callback, NULL, NULL, NULL, send_key);
}

static void
addKeyReleaseEvent(u32 keysym, void (*callback)(), void (*callback2)(u32 keysym), u8 send_key)
{
	if (send_key)
		addKeyEvent(keysym, NULL, NULL, NULL, callback2, send_key);
	else
		addKeyEvent(keysym, NULL, callback, NULL, NULL, send_key);
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
	addKeyPressEvent(keysym, callback, NULL, 0);
}

void 
Neuro_AddReleasedKeyEvent(u32 keysym, void (*callback)())
{
	addKeyReleaseEvent(keysym, callback, NULL, 0);
}

void
Neuro_AddPressedMultiKeyEvent(u32 keysym, void (*callback)(u32 keysym))
{
	addKeyPressEvent(keysym, NULL, callback, 1);
}

void
Neuro_AddReleasedMultiKeyEvent(u32 keysym, void (*callback)(u32 keysym))
{
	addKeyReleaseEvent(keysym, NULL, callback, 1);
}

void
Neuro_AddPressedMouseEvent(u32 button, void (*callback)(int x, int y))
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
Neuro_AddReleasedMouseEvent(u32 button, void (*callback)(int x, int y))
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
Neuro_CleanKeyboard(void)
{
	Neuro_CleanEBuf(&_klist);
	_klist = NULL;

	Neuro_CreateEBuf(&_klist);
}

void
Neuro_CleanMouse(void)
{
	Neuro_CleanEBuf(&_mlist);
	_mlist = NULL;
	
	Neuro_CreateEBuf(&_mlist);
}


void
Events_TriggerMotion(int x, int y)
{
	/* we pass those as variables which can be recovered
	 * by external programs at will.
	 */
	mouse_x = x;
	mouse_y = y;
}

void
Events_TriggerButton(u32 button, int x, int y, int status)
{
	u32 total;
	MOUSEEVENT *tmp;
	
	/* check to see if the buffer _mlist is empty and leave if it is */
	if (Neuro_EBufIsEmpty(_mlist))
		return;		
	
	/* we put the total amount of elements in _mlist to total 
	 * and increment by 1 to support a while (somevar-- > 0) 
	 * method.
	 */
	total = Neuro_GiveEBufCount(_mlist) + 1;

	/* we push the coordinate of the click */
	Events_TriggerMotion(x, y);

	/* Debug_Val(0, "mouse (%d,%d) button %d \n", x, y, button); */

	while (total-- > 0)
	{
		/* pass the element total from the buffer 
		 * _mlist into tmp
		 */
		tmp = Neuro_GiveEBuf(_mlist, total);
	
		/* if (button == tmp->button) */
		trigger_button(tmp, x, y, status);
	}
}

void
Events_TriggerKey(u32 keysym, int keystatus)
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

		if (tmp->key == keysym)
			trigger_key(tmp, keystatus);
	}

}



void
Events_Poll(void)
{	
	handle_keys();
	handle_mouse();
}

int
Events_Init(void)
{
	Neuro_CreateEBuf(&_klist);
	Neuro_CreateEBuf(&_mlist);

	Neuro_SetcallbEBuf(_mlist, clean_mouse_list);

	if (!_klist || !_mlist)
		return 1;
	
	return 0;
}

void
Events_Clean(void)
{
	Neuro_CleanEBuf(&_klist);
	Neuro_CleanEBuf(&_mlist);
}
