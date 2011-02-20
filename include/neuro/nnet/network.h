/* sv_core.h */

#ifndef __SVCORE_H
#define __SVCORE_H

#include <neuro/NEURO.h>

typedef struct LISTEN_DATA LISTEN_DATA;
typedef struct CONNECT_DATA CONNECT_DATA;

/* Create an instance of a client or server context (both needs this). 
 *
 * the callback is called for every packets recieved by the connection.
 * make the callback return 1 to disconnect the current connection.
 * Return 0 for normal continuous behavior.
 */
extern LISTEN_DATA *SVCore_Create(int (*callback)(CONNECT_DATA *conn, char *data, u32 len), u32 type);

/* destroy/clean a context created using SVCore_Create(3) */
extern void SVCore_Destroy(LISTEN_DATA *src);


/* establish connection with a server */
extern int SVCore_Connect(LISTEN_DATA *src, char *host, int port, CONNECT_DATA **result);

/* output the IP of a client/server connected */
extern char *SVCore_GetIP(CONNECT_DATA *src);

/* send a packet(message) to a client/server connected */
extern int SVCore_Send(CONNECT_DATA *src, char *message, u32 len);

/* listen to incomming connections on a certain port. 
 * This is the function to start a server.
 */
extern int SVCore_Listen(LISTEN_DATA *src, int port);

extern int SVCore_ClientExist(CONNECT_DATA *c);

extern int SVCore_ClientExist2(LISTEN_DATA *l, CONNECT_DATA *c);


extern int SVCore_Poll();
extern int SVCore_Init();
extern void SVCore_Clean();

#endif /* NOT __SVCORE_H */
