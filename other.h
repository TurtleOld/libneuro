/* other.h
 */

#ifndef __OTHER_H
#define __OTHER_H

#include "engine.h"


typedef struct OBJBUF OBJBUF;

struct OBJBUF
{
	void **buffer;
	u32 total;
	void (*callback)(void *src);
};

extern void OtherAllocBuf(OBJBUF *eng, size_t sptp);

extern void OtherCleanBuf(OBJBUF *eng);

extern void OtherCallbackBuf(OBJBUF *eng, void (*callback)(void *src));


extern char **Other_SepChr(const unsigned char chr, char *source, int *items);

/* returns a u32 that contains a 16bit color system. */
extern u32 Other_gRGB(u8 R, u8 G, u8 B);

extern u32 Other_GetPixel(void *srf, int x, int y);

extern void Other_PutPixel(void *srf, int x, int y, u32 pixel);

extern void Other_Slp(u32 t);

extern void Other_PrintFPS();


#endif /* __OTHER_H */
