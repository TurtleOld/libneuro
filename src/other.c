/* other.c 
 * contains miscellaneous functions 
 * useful not directly associated with 
 * xml handling but most of the time useful.
 * Especially when you want to have more than 1 data per nodes
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef WIN32
	#define WINDOWS_MEAN_AND_LEAN
	#include <windows.h>
#else /* NOT WIN32 */
	#define __USE_BSD
	#include <unistd.h>
#endif /* NOT WIN32 */

#include "other.h"
#include "extlib.h"

#include "graphics.h"

/*
struct OBJBUF
{
	void **buffer;
	u32 total;
	void (*callback)(void *src);
};
*/

u8
Chk_bound(Rectan *rec, u16 x, u16 y)
{
	if (rec->x <= x && rec->x + rec->w >= x)
	{
		if (rec->y <= y && rec->y + rec->h >= y)
			return 1;
	}
	return 0;
}

void
Neuro_Sleep(u32 t)
{
#ifdef WIN32
	Sleep(t);
#else /* NOT WIN32 */
	usleep(t);
#endif /* NOT WIN32 */
}

void
Neuro_CallbackBuf(OBJBUF *eng, void (*callback)(void *src))
{
	eng->callback = callback;
}

void
Neuro_AllocBuf(OBJBUF *eng, size_t sptp)
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
Neuro_CleanBuf(OBJBUF *eng)
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
Neuro_SepChr(const unsigned char chr, char *source, int *items)
{/* separate characters of words */
	char **ending = NULL;
	u32 slen = strlen(source);
	u32 o_total = 0;
	char *object;
	u32 o_ptr = 0;
	u32 last_ptr = 0;

	
	if (slen == 0)
		return 0;
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
Neuro_GiveRGB(u8 R, u8 G, u8 B)
{
	u32 out;
	
	out = ((R * 31) / 255) << 11;
	out ^= ((G * 31) / 255) << 6;
	out ^= ((B * 31) / 255);
	
	return out;
}

/*
 *  * Return the pixel value at (x, y)
 *   * NOTE: The surface must be locked before calling this!
 *    */
u32 
Neuro_RawGetPixel(v_object *srf, int x, int y)
{
	u8 bpp;
	u8 *p;
	void *pixels;
	u32 pitch;
	u32 err;
	
	
	Lib_LockVObject(srf);
	
	Lib_GetVObjectData(srf, NULL, NULL, NULL, &pitch, &pixels, NULL, &bpp,
			NULL, NULL, NULL, NULL);
	
	/* bpp = surface->format->BytesPerPixel; */
	/* Here p is the address to the pixel we want to retrieve */
	/* p = (u8 *)surface->pixels + y * surface->pitch + x * bpp; */
	p = (u8 *)pixels + y * pitch + x * bpp;
	err = 0;
	
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
		
		/*case 3:
		{
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
				err = p[0] << 16 | p[1] << 8 | p[2];
			else
				err = p[0] | p[1] << 8 | p[2] << 16;
		}
		break;
		*/
		
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

	Lib_UnlockVObject(srf);

	return err;
}


/*
 *  * Set the pixel at (x, y) to the given value
 *   * NOTE: The surface must be locked before calling this!
 *    */
void 
Neuro_RawPutPixel(v_object *srf, int x, int y, u32 pixel)
{
	/* SDL_Surface *surface = (SDL_Surface*)srf; */
	/* SDL_LockSurface(surface); */
	u8 bpp;
	u8 *p;
	void *pixels;
	u32 pitch;
	
	Lib_GetVObjectData(srf, NULL, NULL, NULL, &pitch, &pixels, NULL, &bpp,
			NULL, NULL, NULL, NULL);

	/* int bpp = surface->format->BytesPerPixel; */
	/* Here p is the address to the pixel we want to set */
	/* p = (u8*)surface->pixels + y * surface->pitch + x * bpp; */
	p = (u8 *)pixels + y * pitch + x * bpp;

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
		/*
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
		*/
		case 4:
		{
			*(u32 *)p = pixel;
		}
		break;
		
	}
	/* SDL_UnlockSurface(surface); */
}

void
Neuro_PrintFPS()
{
	t_tick fps;

	Neuro_GiveFPS(&fps);

	if (fps > 0)
	{
		printf("current fps : %d\n", fps);
	}
}

/* rectangle or square bounds check function.
 * return values :
 * 0 = depen is inside indep.
 * 1 = depen and indep are not touching each others(they are far away).
 * 2 = depen is overlaping indep. 
 * this function needs to be converted to a macro so its lightning quick
 */
u8
Neuro_BoundsCheck(Rectan *indep, Rectan *depen)
{		
	register u8 status = 0;

	status = Chk_bound(indep, depen->x, depen->y); /* up left */
	status += Chk_bound(indep, depen->x + depen->w, depen->y); /* up right */
	status += Chk_bound(indep, depen->x + depen->w, depen->y + depen->h); /* bottom right */
	status += Chk_bound(indep, depen->x, depen->y + depen->h); /* bottom left */

	if (status == 4)
		status = 0; /* obj is inside form */
	else
	{
		if (status > 0)
			status = 2; /* obj overlaps form */
		else
			status = 1; /* obj is not touching form */
	}
	return status;
}
