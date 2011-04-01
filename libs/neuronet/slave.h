/* slave.h */

#ifndef __SLAVE_H
#define __SLAVE_H

extern void *Slave_GetData(const Slave *slv);
extern void Slave_SetData(Slave *slv, const void *ptr);

extern int Slave_Init(Slave *slv, Master *master, int socket, int type, Client *client, Server *server);

extern Slave *Slave_Create(Master *msr, int socket, int type, Client *client, Server *server);

extern void Slave_Clean(Slave *slv);

extern void Slave_Destroy(Slave *slv);


#endif /* NOT __SLAVE_H */
