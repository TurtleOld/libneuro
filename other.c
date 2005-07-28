/* other.c 
 * contains miscellaneous functions 
 * useful not directly associated with 
 * xml handling but most of the time useful.
 * Especially when you want to have more than 1 data per nodes
 */

#include <SDL.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef WIN32
	#define WINDOWS_MEAN_AND_LEAN
	#include <windows.h>
#else /* NOT WIN32 */
	#include <unistd.h>
#endif /* NOT WIN32 */

#include "other.h"

#include "graphics.h"

/*
struct OBJBUF
{
	void **buffer;
	u32 total;
	void (*callback)(void *src);
};
*/

void
Other_Slp(u32 t)
{
#ifdef WIN32
	Sleep(t);
#else /* NOT WIN32 */
	usleep(t);
#endif /* NOT WIN32 */
}

void
OtherCallbackBuf(OBJBUF *eng, void (*callback)(void *src))
{
	eng->callback = callback;
}

void
OtherAllocBuf(OBJBUF *eng, size_t sptp)
{
	void ***buf = NULL;
	u32 total = 0;
		
	buf = &eng->buffer;
	total = eng->total;
			
	if (!*buf)
	{
		*buf = calloc(1, sptp);
		total = 0;
	}
	else
		*buf = realloc(*buf, sptp * (total + 1));	
	
	total++;
	eng->total = total;
}

void
OtherCleanBuf(OBJBUF *eng)
{
	void ***buf;
	u32 i;
	
	if (!eng)
		return;
		
	buf = &eng->buffer;
	i = eng->total;

	while (i-- > 0)
	{
		if ((*buf)[i])
			(eng->callback)((*buf)[i]);
	}

	if (*buf)
		free(*buf);
	*buf = NULL;
	
	eng->total = 0;
	eng->buffer = NULL;
}

/* this functions was made to separate a given character, 
 * like the character ':' of a string, 
 * example : foo:bar:test:test2  
 * the functions will have the pointers to each words in the string so
 * foo bar test and test2 will be accessable independently.
 */
char **
Other_SepChr(const unsigned char chr, char *source, int *items)
{/* separate characters of words */
	char **ending = NULL;
	u32 slen = strlen(source);
	if (slen == 0)
		return 0;
	u32 o_total = 0;
	char *object;
	u32 o_ptr = 0;
	u32 last_ptr = 0;
	while ((last_ptr + o_ptr) < slen)
	{
		if (o_ptr == 0)
			last_ptr = o_ptr;
		else
			last_ptr += o_ptr + 1;
		if ((object = memchr(&source[o_ptr], chr, slen)) != NULL)
			object[0] = '\0'; /* reset the ':' to \0 so we can point to it */
		o_ptr = strlen(&source[last_ptr]);
		if (o_total == 0)
			ending = (char**)calloc(1, sizeof(char*));	
		else
		{ 
			ending = (char **)realloc((char*)ending, (sizeof(char*) * (o_total + 1))); 
			ending[o_total] = (char*)calloc(1, sizeof(char*));
		}	
		ending[o_total] = (char*)&source[last_ptr];
		o_total++;
	}			
	*items = o_total;
	return ending;
}


u32
Other_gRGB(u8 R, u8 G, u8 B)
{
	u32 out;

	out = ((R * 63) / 255) << 11;
	out ^= ((G * 63) / 255) << 6;
	out ^= ((B * 63) / 255);
	
	return out;
}

/*
 *  * Return the pixel value at (x, y)
 *   * NOTE: The surface must be locked before calling this!
 *    */
u32 
Other_GetPixel(void *srf, int x, int y)
{
	SDL_Surface *surface = (SDL_Surface*)srf;

	SDL_LockSurface(surface);

	int bpp = surface->format->BytesPerPixel;
	/* Here p is the address to the pixel we want to retrieve */
	u8 *p = (u8 *)surface->pixels + y * surface->pitch + x * bpp;
	u32 err = 0;
	
	switch(bpp) 
	{
		case 1:
		{
			err = *p;
			/* err = *(u16 *)p; */
		}
		break;
		
		case 2:
		{
			err = *(u16 *)p;
		}
		break;
		
		case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
				err = p[0] << 16 | p[1] << 8 | p[2];
			else
				err = p[0] | p[1] << 8 | p[2] << 16;
		}
		break;
		
		case 4:
		{
			err = *(u32 *)p;
		}
		break;
		
		default:
		{
			err = 0;       /* shouldn't happen, but avoids warnings */
		}
	}

	SDL_UnlockSurface(surface);

	return err;
}


/*
 *  * Set the pixel at (x, y) to the given value
 *   * NOTE: The surface must be locked before calling this!
 *    */
void 
Other_PutPixel(void *srf, int x, int y, u32 pixel)
{
	SDL_Surface *surface = (SDL_Surface*)srf;
	SDL_LockSurface(surface);
	
	int bpp = surface->format->BytesPerPixel;
	/* Here p is the address to the pixel we want to set */
	u8 *p = (u8*)surface->pixels + y * surface->pitch + x * bpp;

	switch(bpp) 
	{
		case 1:
		{
			*p = pixel;
		}
		break;

		case 2:
		{
			*(u16*)p = pixel;
		}
		break;

		case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN) 
			{
				p[0] = (pixel >> 16) & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = pixel & 0xff;
			} 
			else 
			{
				p[0] = pixel & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = (pixel >> 16) & 0xff;
			}
		}
		break;
	
		case 4:
		{
			*(u32 *)p = pixel;
		}
		break;
		
	}
	SDL_UnlockSurface(surface);
}

void
Other_PrintFPS()
{
	t_tick fps;

	Graphics_GiveFPS(&fps);

	if (fps > 0)
	{
		printf("current fps : %d\n", fps);
	}
}



