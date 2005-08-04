/* sdl.c
 * contains all the "useful" or used functions by the engines.
 * those functions are called by calling a tertiarry function which will
 * be the same for all the graphics/sound/... libraries. The idea of this
 * way is to provide an easy way to implement other libraries.
 * functions which can be used for different libraries will be of the module
 * Low_. example : Low_Init(). Only one library can be used at a time.
 * Options also will have to be set expetially for the initialisation functions.
 * Im not sure how exactly we should do it yet.
 */

#if USE_SDL

#include <SDL.h>

int
Low_Init()
{
	
}

#endif /* USE_SDL */
