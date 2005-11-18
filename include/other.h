/* other.h
 */

#ifndef __OTHER_H
#define __OTHER_H

#include <neuro_engine.h>

#include <stdlib.h>

typedef struct OBJBUF OBJBUF;

struct OBJBUF
{
	void **buffer;
	u32 total;
	void (*callback)(void *src);
};

extern void Neuro_AllocBuf(OBJBUF *eng, size_t sptp);

extern void Neuro_CleanBuf(OBJBUF *eng);

extern void Neuro_CallbackBuf(OBJBUF *eng, void (*callback)(void *src));


extern char **Neuro_SepChr(const unsigned char chr, char *source, int *items);

/* returns a u32 that contains a 16bit color system. */
extern u32 Neuro_GiveRGB(u8 R, u8 G, u8 B);

extern u32 Neuro_RawGetPixel(v_object *srf, int x, int y);

extern void Neuro_RawPutPixel(v_object *srf, int x, int y, u32 pixel);

extern void Neuro_Sleep(u32 t);

extern void Neuro_PrintFPS();

extern u8 Neuro_DumbBoundsCheck(Rectan *indep, Rectan *depen);

extern u8 Neuro_BoundsCheck(Rectan *indep, Rectan *depen);

extern void Neuro_VerticalBoundFix(Rectan *indep, Rectan *isrc, Rectan *idst);

extern void Neuro_HorizontalBoundFix(Rectan *indep, Rectan *isrc, Rectan *idst);

#endif /* __OTHER_H */
