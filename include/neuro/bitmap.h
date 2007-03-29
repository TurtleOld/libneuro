/* bitmap.h */

#ifndef __BITMAP_H
#define __BITMAP_H

#include "neuro_engine.h"

typedef struct BMP_CTX BMP_CTX;

extern i8 Bitmap_Poll(BMP_CTX *ctx);
extern BMP_CTX *Bitmap_CreateCTX(const char *path);

extern v_object *Bitmap_DestroyCTX(BMP_CTX *ctx);

#endif /* NOT __BITMAP_H */
