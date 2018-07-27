/* lbuf.h */

#ifndef __LBUF_H
#define __LBUF_H

#include <neuro/NEURO.h>

typedef struct LBUF LBUF;

extern void Neuro_SetcallbLBuf(LBUF *eng, void (*callback)(void *src));

extern void Neuro_AllocLBuf(LBUF *eng, size_t sobj);
/* places a new element at the beginning of the buffer instead of at the end */
extern void Neuro_AllocStartLBuf(LBUF *eng, size_t sobj);

/* gives the last LBuf element */
extern void *Neuro_GiveCurLBuf(LBUF *eng);

extern void *Neuro_GiveFirstLBuf(LBUF *eng);
extern void *Neuro_GiveLastLBuf(LBUF *eng);

extern u32 Neuro_GiveLBufCount(LBUF *eng);

extern void Neuro_SCleanLBuf(LBUF *eng, void *object);

/* This gives the first element and it resets the 
 * arrow pointer for Neuro_GiveNextLBuf
 */
extern void *Neuro_GiveLBuf(LBUF *eng);

/* sets the current LBuf to the next and returns the result */
extern void *Neuro_GiveNextLBuf(LBUF *eng);

extern void Neuro_ResetLBuf(LBUF *eng);

extern u32 Neuro_LBufIsEmpty(LBUF *eng);


extern LBUF *Neuro_CreateLBuf(void);
extern void Neuro_CleanLBuf(LBUF *eng);

#endif /* NOT __LBUF_H */
