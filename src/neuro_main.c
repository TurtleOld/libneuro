
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
#include <graphics.h>
#include <events.h>
#include <extlib.h>

/*-------------------- Main Module Header --------------------------*/
#include <neuro_main.h>


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

/*-------------------- Poll ----------------------------------------*/

int
Neuro_Poll()
{
	int _err = 0;
	
	Events_Poll();
	Graphics_Poll();

	_err = Lib_PollEvent(NULL);

	return _err;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Neuro_Init()
{
	int _err = 0;
	
	_err = Graphics_Init();
	if (_err)
	{
		return _err;
	}

	_err = Events_Init();
	if (_err)
	{
		return _err;
	}
	
	return _err;
}

void
Neuro_Quit()
{
	Events_Clean();
	Graphics_Clean();
}
