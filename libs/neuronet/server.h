/* server.h */

#ifndef __SERVER_H
#define __SERVER_H

#include "common.h"

extern int Server_Poll(Slave *slv);

extern void Server_DisconnectClient(Slave *slv);

extern Slave *Server_Create(Master *msr, int port);
extern void Server_Destroy(Server *srv);

#endif /* NOT __SERVER_H */
