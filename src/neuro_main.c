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
