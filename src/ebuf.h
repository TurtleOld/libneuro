#ifndef __EBUF_H
#define __EBUF_H

#include "engine.h"

#define MEMORY_ALLOC_OVERH 10

typedef struct EBUF EBUF;

extern EBUF *Neuro_CreateEBuf();

extern void Neuro_AllocEBuf(EBUF *eng, size_t sptp, size_t sobj);

extern void Neuro_CleanEBuf(EBUF **eng);

extern u32 Neuro_GiveEBufCount(EBUF *eng);

extern void **Neuro_GiveEBufAddr(EBUF *eng, u32 elem);

extern void *Neuro_GiveEBuf(EBUF *eng, u32 elem);

extern void Neuro_SetEBuf(EBUF *eng, void **to, void *from);


#endif /* not __EBUF_H */
