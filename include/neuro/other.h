/* other.h
 */

#ifndef __OTHER_H
#define __OTHER_H

#include "neuro_engine.h"
#include "ebuf.h"

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

/* the EBUF data uses this struct */
typedef struct SepChr_Data
{
	char *string;
}SepChr_Data;

extern EBUF *Neuro_SepChr2(const u8 chr, char *source);

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

/* generate characters (source is chgen.c in src/misc TODO might need to have the Neuro_ prefix */
extern void Uchar(int amount, unsigned char **buf);

/* internal function (source is bitmap.c in src/misc) */
extern void readBitmapFileToPixmap(const char *bitmap, EBUF **output_pixmap);
/* internal function (source is bitmap.c in src/misc) 
 * pretty much useless, use Neuro_CleanEBuf() instead
 */
extern void cleanPixmapEbuf(EBUF **pixmap);

/* -------- Argument System ---------- */

enum
{
	OPTION_NORMAL	= 0x00000000, /* normal option which includes a callback or not */
	OPTION_ARGUMENT = 0x00000001, /* needs an argument */
	OPTION_REQUIRED = 0x00000010, /* is required to make the app run */
	OPTION_NESTED	= 0x00000100, /* can be nested with other */
	OPTION_MULTI	= 0x00001000, /* can have more than one option of this type */
	OPTION_VOID	= 0x00010000, /* when the command has no options, this option is executed */
	OPTION_QUIT	= 0x00100000  /* when this option is called, no more options r executed. */
};

extern int Neuro_ArgInit(int argc, char **argv);

extern void Neuro_ArgClean();

extern void Neuro_ArgOption(char *string, int options, void (*action)(char *data));

/* return 2 on error, 1 on normal exit requested and 0 on execution continue */
extern int Neuro_ArgProcess();

/* ---------- End of the Argument System ---------- */


#endif /* __OTHER_H */
