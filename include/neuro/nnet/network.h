/* sv_core.h */

#ifndef __SVCORE_H
#define __SVCORE_H

#include <neuro/NEURO.h>

enum CONNECT_TYPE
{
	TYPE_SERVER,
	TYPE_CLIENT
};

typedef struct LISTEN_DATA LISTEN_DATA;
typedef struct CONNECT_DATA CONNECT_DATA;

/* Create an instance of a client or server context (both needs this). 
 *
 * the callback is called for every packets recieved by the connection.
 * make the callback return 1 to disconnect the current connection.
 * Return 0 for normal continuous behavior.
 *
 * connection_type : put one of the CONNECT_TYPE values 
 * 	example : TYPE_CLIENT
 *
 * NOTE NOTE NOTE ******* For ease of use, on a listening server, 
 * EVERY NEW CLIENT CONNECTIONS ARE INTRODUCED TO THE CALLBACK WITH
 * AN EMPTY DATA AND 0 LEN PACKET!!!
 */
extern LISTEN_DATA *NNet_Create(int (*callback)(CONNECT_DATA *conn, const char *data, u32 len), u32 connection_type);

/* destroy/clean a context created using NNet_Create(3) */
extern void NNet_Destroy(LISTEN_DATA *src);


/* establish connection with a server */
extern int NNet_Connect(LISTEN_DATA *src, const char *host, int port, CONNECT_DATA **result);

/* output the IP of a client/server connected */
extern char *NNet_GetIP(CONNECT_DATA *src);

/* send a packet(message) to a client/server connected */
extern int NNet_Send(CONNECT_DATA *src, const char *message, u32 len);

/* listen to incomming connections on a certain port. 
 * This is the function to start a server.
 */
extern int NNet_Listen(LISTEN_DATA *src, int port);

extern int NNet_ClientExist(CONNECT_DATA *c);

extern int NNet_ClientExist2(LISTEN_DATA *l, CONNECT_DATA *c);

/* set timeout for an active connection */
extern int NNet_SetTimeout(CONNECT_DATA *src, t_tick ts);

extern void NNet_DisconnectClient(LISTEN_DATA *l, CONNECT_DATA *c);

/* to set the debugging filter for this library */
extern void NNet_SetDebugFilter(const char *filter);

/* this function actually toggles if the connection include a 32 bit
 * packet header for each of the packets sent containing the
 * size of the packet for parity check. This can be toggled
 * on or off with the appropriate function.
 *
 * NOTE - This is only to be used when libneuronet is used 
 * as a client AND server!!
 * never activate this for when libneuronet is used for other
 * kind of connections.
 *
 * This is for client connections, see NNet_ServerTogglePacketSize
 * for the listening server toggle.
 */
extern void NNet_ClientTogglePacketSize(CONNECT_DATA *client);

/* Exactly like NNet_ClientTogglePacketSize but for a listening 
 * server this time.
 */
extern void NNet_ServerTogglePacketSize(LISTEN_DATA *server);

/* for each loops, the value returned by this poll is significant 
 * a value of 1 means that the main loop has to be stopped as an
 * error or disconnection occured.
 *
 * value 0 is all is ok.
 */
extern int NNet_Poll();
extern int NNet_Init();
extern void NNet_Clean();

#endif /* NOT __SVCORE_H */
