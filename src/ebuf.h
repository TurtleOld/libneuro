#ifndef __EBUF_H
#define __EBUF_H

#include "engine.h"
#include <stdlib.h>

#define MEMORY_ALLOC_OVERH 10

typedef struct EBUF EBUF;

extern void Neuro_CreateEBuf(EBUF **eng);

extern void Neuro_SetcallbEBuf(EBUF *eng, void (*callback)(void *src));

extern void Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj);

extern void Neuro_CleanEBuf(EBUF **eng);

extern void Neuro_SCleanEBuf(EBUF *eng, void *object);

extern u32 Neuro_GiveEBufCount(EBUF *eng);

extern i32 Neuro_GiveEBufElem(EBUF *eng, void *object);

extern void **Neuro_GiveEBufAddr(EBUF *eng, u32 elem);

extern void *Neuro_GiveEBuf(EBUF *eng, u32 elem);

extern void Neuro_SetEBuf(EBUF *eng, void **to, void *from);

extern void Neuro_CopyEBuf(EBUF *to, EBUF *from);

extern void Neuro_ResetEBuf(EBUF *eng);

extern u8 Neuro_EBufIsEmpty(EBUF *eng);

#endif /* not __EBUF_H */
