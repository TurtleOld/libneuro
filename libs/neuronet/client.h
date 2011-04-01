/* client.h */

#ifndef __CLIENT_H
#define __CLIENT_H

#include "common.h"

extern char *Client_GetIP(Slave *slv);

extern void Client_SetTimeOut(Slave *slv, t_tick ts);

extern int Client_Send(Slave *slv, const char *message, u32 len);

extern Slave *Client_Connect(Master *msr, const char *host, int port);

extern void Client_Disconnect(Slave *slv);

extern void Client_PopData(Slave *slv);

extern int Client_PollRead(Slave *slv);
extern int Client_PollSend(Slave *slv);

extern void Client_Init(Master *msr, Client *client);
extern Client *Client_Create(Master *msr);
extern void Client_Destroy(Client *clt);

extern int Client_Poll(Client *client);

#endif /* NOT __CLIENT_H */
