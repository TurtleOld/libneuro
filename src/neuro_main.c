
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

/* neuro.c
 * Main libNeuro source file.
 */

/*-------------------- Extern Headers Including --------------------*/


/*-------------------- Local Headers Including ---------------------*/
#include <config.h>
#include <global.h>
#include <graphics.h>
#include <events.h>
#include <extlib.h>
#include <debug.h>

/*-------------------- Main Module Header --------------------------*/
#include <neuro_main.h>


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

void
Neuro_SetNeuroDebugFilter(const char *filter)
{
	Neuro_SetDebugFilter(filter);
}

/*-------------------- Poll ----------------------------------------*/

int
Neuro_Poll(void)
{
	int _err = 0;
	
	/* this should now be moved to Lib_PollEvent 
	 * if it is still used.
	 */
	/* Events_Poll(); */

#if USE_VIDEO
	Graphics_Poll();
#endif /* USE_VIDEO */

	_err = Lib_PollEvent(NULL);

	return _err;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Neuro_Init(void)
{
	int _err = 0;
	
	_err = Debug_Init();
	if (_err)
		return _err;
#if USE_VIDEO
	_err = Graphics_Init();
	if (_err)
	{
		return _err;
	}
#endif /* USE_VIDEO */

	_err = Events_Init();
	if (_err)
	{
		return _err;
	}

	_err = Lib_FontsInit();
	if (_err)
	{
		return _err;
	}
	
	return _err;
}

void
Neuro_Quit(void)
{
	Lib_FontsExit();
	Events_Clean();
#if USE_VIDEO
	Graphics_Clean();
#endif /* USE_VIDEO */
	Debug_Clean();
}
