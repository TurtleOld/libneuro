/* bitmap.h */

#ifndef __BITMAP_H
#define __BITMAP_H

typedef struct BMP_CTX BMP_CTX;

extern u8 Bitmap_Poll(BMP_CTX *ctx);
extern BMP_CTX *Bitmap_CreateCTX(const char *path);

extern v_object *Bitmap_DestroyCTX(BMP_CTX *ctx);

#endif /* NOT __BITMAP_H */
