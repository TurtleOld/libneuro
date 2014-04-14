/* intern_bitmap.h */

#ifndef __INTERN_BITMAP_H
#define __INTERN_BITMAP_H

#include <config.h>

#include <bitmap.h>

#include <neuro_engine.h>
#include <ebuf.h>

#ifdef USE_ZLIB
/* this is used to open the bitmaps, 
 * the beauty of zlib is it works 
 * for compressed and uncompressed
 * transparently, meaning no extra code
 * for both!
 */
#include <zlib.h>
typedef gzFile nFILE;
#else /* NOT USE_ZLIB */
typedef FILE *nFILE;
#endif /* USE_ZLIB */

typedef struct BITMAP_HEADER
{
	u16 type __attribute__((packed));			/* Magic identifier            */
	u32 size __attribute__((packed));			/* File size in bytes          */
	u16 reserved1, reserved2;	/* reserved */
	u32 offset __attribute__((packed));			/* Offset to image data, bytes */
}BITMAP_HEADER;

typedef struct BITMAP_INFOHEADER
{
	u32 size;			/* Header size in bytes      */
	i32 width, height;		/* Width and height of image */
	u16 planes;			/* Number of colour planes   */
	u16 bits;			/* Bits per pixel            */
	u32 compression;		/* Compression type          */
	u32 imagesize;			/* Image size in bytes       */
	i32 xresolution, yresolution;	/* Pixels per meter          */
	u32 ncolors;			/* Number of colours         */
	u32 importantcolours;		/* Important colours         */
}BITMAP_INFOHEADER;

typedef struct BITMAP_HDATA
{
	BITMAP_HEADER header __attribute__((packed));
	BITMAP_INFOHEADER infoheader __attribute__((packed));
}BITMAP_HDATA;

struct BMP_CTX
{
	nFILE f_bitmap;

	/* major (buffers) */
	EBUF *bmap_colors; /* the colors */
	u8 *buf; /* the buffer that will contain the content of the file */
	
	/* minor (mostly pointers and temporary variables) */
	i32 i; /* incremental variable */
	u32 x, y;
	
	u32 psize; /* the full size of the pixels data */
	BITMAP_HDATA *bmap; /* this is how we will get informations about the bitmap */
	int aux_var; /* auxiliary variable that can be used by external functions */
	char *aux_buf; /* same as aux_var but a buffer */
	double wmult;
	u32 DATA;
	i32 advance;
	u32 row_amount; /* processing of the current row */
	v_object *output; /* the image into which we will load the bitmap */
	u32 cut_size; /* amount of bytes to load per cycles */
};

typedef struct BITMAP_COLOR
{
	u8 r, g, b /*, a*/ ; /* red green blue */
}BITMAP_COLOR;

typedef struct BITMAP_MAP
{
	BITMAP_COLOR *color;
}BITMAP_MAP;

/* io.c */

extern int fpdata8(nFILE input, u8 *output);
extern int fpdata16(nFILE input, u16 *output);
extern int fpdata32(nFILE input, u32 *output);


/* process.c */

extern i8 Bitmap_ProcessGradual(BMP_CTX *ctx, u32 loops);

/* core.c */

extern v_object *Bitmap_LoadBMP(const char *bitmap);

extern i8 Bitmap_Poll(BMP_CTX *ctx);

extern BMP_CTX *Bitmap_CreateCTX(const char *path);

extern v_object *Bitmap_DestroyCTX(BMP_CTX *ctx);

#endif /* NOT __INTERN_BITMAP_H */
