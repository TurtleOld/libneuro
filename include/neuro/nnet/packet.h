/* packet.h */

#ifndef __PACKET_H
#define __PACKET_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Packet Packet;

extern int Packet_Push64(Packet *pkt, double num);
extern int Packet_Push32(Packet *pkt, int num);
extern int Packet_Push16(Packet *pkt, short num);
extern int Packet_Push8(Packet *pkt, char num);
extern int Packet_PushStruct(Packet *pkt, unsigned int len, const void *stru);
extern int Packet_PushString(Packet *pkt, unsigned int len, const char *string);

extern double Packet_Pop64(Packet *pkt);
extern int Packet_Pop32(Packet *pkt);
extern short Packet_Pop16(Packet *pkt);
extern char Packet_Pop8(Packet *pkt);
extern char *Packet_PopString(Packet *pkt);
extern char *Packet_PopData(Packet *pkt, unsigned int len);

extern int Packet_GetLen(const Packet *pkt);
extern char *Packet_GetBuffer(const Packet *pkt);

extern Packet *Packet_Set(char *buffer, unsigned int len);
extern void Packet_Reset(Packet *pkt);

extern Packet *Packet_Create();
extern void Packet_Destroy(Packet *pkt);

#ifdef __cplusplus
}
#endif

#endif /* NOT __PACKET_H */
