/* sv_core.h */

#ifndef __SVCORE_H
#define __SVCORE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <neuro/config.h>
#include <neuro/NEURO.h>

enum CONNECT_TYPE
{
	TYPE_SERVER,
	TYPE_CLIENT
};

typedef struct Master NNET_MASTER;

typedef struct Slave NNET_SLAVE;

/* Status types :
 * NoData - No data is available to read
 * DataAvail - data is available to read
 * NewClient - for a server only
 * ClientDisconnect - for a server only
 */
enum
{
	State_Start = 0,

	State_NoData,
	State_DataAvail,
	State_Disconnect,
	State_NewClient,
	State_ClientDisconnect,
	
	State_End
};

typedef struct Status NNET_STATUS;

extern void NNet_Destroy(NNET_MASTER *msr);

extern NNET_MASTER *NNet_Create(u32 connection_type);

/* special feature 
 *
 * this function actually toggles if the connection include a 32 bit
 * packet header for each of the packets sent containing the
 * size of the packet for parity check. This can be toggled
 * on or off and the return value returns 0 or 1 if it's off and on
 * accordingly.
 *
 * NOTE - This is only to be used when libneuronet is used 
 * as a client AND server!!
 * never activate this for when libneuronet is used for other
 * kind of connections.
 */
extern int NNet_SetSendPacketSize(NNET_MASTER *msr);

extern void NNet_SetQuitFlag(NNET_MASTER *msr);

/* Sets a callback which will recieve all Status events;
 * If set using this function, the callback function will
 * receive all statuses voiding the NNet_Poll return of
 * statuses (it is not possible to use both at once. Anyway,
 * this would break the whole point).
 *
 * customData is simply a pointer which is sent to the callback.
 * The end program can use it as their main struct or variable
 * for keeping a state.
 */
extern void NNet_SetResponderCB(NNET_MASTER *msr, void *customData, int (*callback)(void *customData, NNET_STATUS *sta));

extern NNET_STATUS *NNet_Poll(NNET_MASTER *msr);

/* a client */
extern NNET_SLAVE *NNet_Connect(NNET_MASTER *msr, const char *host, int port);

/* a server */
extern NNET_SLAVE *NNet_Listen(NNET_MASTER *msr, char *listen_ip, int port);

extern void NNet_SetTimeOut(NNET_SLAVE *slv, t_tick ts);

extern void NNet_DisconnectClient(NNET_SLAVE *client);

extern int NNet_Send(NNET_SLAVE *src, const char *message, u32 len);
extern char *NNet_GetIP(NNET_SLAVE *slv);

extern u32 NNet_GetStatus(const NNET_STATUS *sta);
extern char *NNet_GetPacket(const NNET_STATUS *sta);
extern int NNet_GetPacketLen(const NNET_STATUS *sta);
extern NNET_SLAVE *NNet_GetSlave(const NNET_STATUS *sta);

/* set user defined data set with a NNET_SLAVE element */
extern void NNet_SetData(NNET_SLAVE *slv, void *ptr);
extern void *NNet_GetData(const NNET_SLAVE *slv);

/* to set the debugging filter for this library */
extern void NNet_SetDebugFilter(const char *filter);

#ifdef __cplusplus
}
#endif

#endif /* NOT __SVCORE_H */
