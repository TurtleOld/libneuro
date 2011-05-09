/* server.h */

#ifndef __SERVER_H
#define __SERVER_H

#include "common.h"

extern int Server_Poll(Slave *slv);

extern void Server_DisconnectClient(Slave *slv);

extern Slave *Server_Create(Master *msr, const char *listen_ip, int port);
extern void Server_Destroy(Server *srv);

#endif /* NOT __SERVER_H */
