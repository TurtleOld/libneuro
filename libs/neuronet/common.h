/* common.h */

#ifndef __COMMON_H
#define __COMMON_H

#define use_epoll 0

#ifndef WIN32

#if use_epoll
#include <sys/epoll.h>
#endif /* use_epoll */

#include <arpa/inet.h> /* (struct sockaddr_in)  */

#else /* WIN32 */

#include <windows.h>
#define MSG_DONTWAIT 0

#endif /* WIN32 */


#include <neuro/NEURO.h>

#include "epoll.h"

enum CONNECT_TYPE
{
	TYPE_SERVER,
	TYPE_CLIENT
};


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

/* private version of the public NNET_MASTER */
typedef struct Master Master;

/* private version of the public NNET_SLAVE */
typedef struct Slave Slave;

/* private */
typedef struct Server Server;

/* private */
typedef struct Client Client;

/* private version of the public NNET_STATUS */
typedef struct Status Status;

struct Master
{
	u32 type; /* Connection type
		   * 0 : server
		   * 1 : client 
		   */

	/* this flag actually toggles if the connection include a 32 bit 
	 * packet header for each of the packets sent containing the
	 * size of the packet for parity check. This can be toggled
	 * on or off with the appropriate function.
	 */
	u32 inclpacket_size; /* includes the packet size before each of the packets */

	Slave *slave;

	EBUF *statuses; /* contains Status elements */

	/* used only for a client */
	Status *status;

	EPOLL *ep;

#if tmp
#if use_epoll
	int epoll_fd;
	struct epoll_event *epEvent;
#else /* not use_epoll */
	/* poll -- specific file descriptor buffers */
	struct pollfd *ufds;
	int nfds;
#endif /* not use_epoll */
#endif /* tmp */

	EBUF *cevents; /* contains Event elements */

#ifdef WIN32
	WSADATA wsaData;
#endif /* WIN32 */
};

struct Slave
{
	Master *master;
	int type; /* 0 server -- 1 client */

	/* connection core settings */
	u32 socket; /* specific socket for this connection */
	struct sockaddr_in c_address; /* client address */
	u32 addrlen;

	int sigmask; 	/* event mask
			 * 0 : no event
			 * 1 : read event 
			 * 2 : write event
			 * 4 : exception event
			 */

	/* pointer for a user defined data attached with the slave */
	const void *ptr;

	union
	{
		Server *server;
		Client *client;
	}cType;
};

/* old LISTEN_DATA */
struct Server
{
#if tmp
	u32 socket; /* server's socket */

	u32 port;

	int sigmask; 	/* event mask
			 * 0 : no event
			 * 1 : read event 
			 * 2 : write event
			 * 4 : exception event
			 */

	struct sockaddr_in saddress; /* server address */
	u32 addrlen;
#endif /* tmp */

	EBUF *connections; /* contains Slave elements -- it's those clients which are connected */
};

/* old CONNECT_DATA */
struct Client
{
#if tmp
	/* connection core settings */
	u32 socket; /* specific socket for this connection */
	struct sockaddr_in c_address; /* client address */
	u32 addrlen;

	int sigmask; 	/* event mask
			 * 0 : no event
			 * 1 : read event 
			 * 2 : write event
			 * 4 : exception event
			 */
#endif /* tmp */

	/* connection statistics */
	t_tick connection_start_time;
	t_tick idle_time; /* idle time... actually its the exact time we last received activity from the connection */
	t_tick timeout;

	/* connection buffers */
	EBUF *input; /* input buffer that contains FRAGMENT_MASTER elements */
	EBUF *output; /* output buffer that contains PACKET_BUFFER elements */

	/* extra buffer for incomplete packet */
	u32 incomplete_len;
	char *incomplete_data;
};

struct Status
{
	u32 status;

	Slave *connection; /* the connection that raised the status */

	char *packet;
	u32 packet_len;

	u32 moreStatus; /* if there's more Status ready in the buffer to be processed */
};

typedef struct Event Event;

struct Event
{
	Slave *slave;

	int sigmask; 	/* event mask
			 * 0 : no event
			 * 1 : read event 
			 * 2 : write event
			 * 4 : exception event
			 */
};

#define MAX_PACKET_SIZE 5120
#define INPUT_PACKET_BUFFERING 10

typedef struct PACKET_BUFFER PACKET_BUFFER;

/* this struct is actually used when a buffer (of any size) can't
 * be assumed to be sent in just one go. Sending the buffer might
 * require more than just one pass in order for the buffer to be
 * successfully sent. 
 *
 * The arrow pointer is used to point to the last position where
 * the data is currently as the data before it was already sent
 * to the pipe. The remaining integer is used to know how many
 * bytes are still remaining to be sent through the pipe in
 * order for the buffer to be sent as a whole.
 */
struct PACKET_BUFFER
{
	u32 len;
	u32 remaining;

	char *arrow;
	char *data;
};


typedef struct FRAGMENT_MASTER FRAGMENT_MASTER;

/* Inputed packets are known to sometimes be received in more than one chunk at once.
 * This means that a single recieve by recv() could return more than one packet at
 * once per cycles. 
 *
 * Because of that behavior, we have to buffer the input packets and fragment the allocated
 * buffer into their respective packet sizes. THIS STRUCT IS TO CONTAIN SUCH PACKETS!
 *
 * Every packets start with an integer that is the size of the data in a packet.
 * We check to see if that value is of a valid range then proceed to fill this struct.
 *
 * It is important to note that the initial data pointer is the buffer itself which
 * contains more than one packet (or just one could happen too). The EBUF fragmented
 * buffer is then filled with pointers to the areas in the initial data pointer.
 * This method is meant to saves memory by a lot.
 *
 *
 * EXAMPLE :
 *
 *	 buffer len(24) :
 *	 ---------------------------------
 *	 |000|000|000|000|000|000|000|000|
 *	 ---------------------------------
 *
 *	 single packet len(3) :
 *	 -----
 *	 |000|
 *	 -----
 *
 *	 so in that buffer we have 8 packets that are in the same buffer... which we need to
 *	 fragment up manually.
 */
struct FRAGMENT_MASTER
{
	/* the length of the initial data */
	u32 len;
	/* the data buffer which could contain one or more actual packets. */
	char *data;

	EBUF *fragmented; /* contains the fragmented PACKET_BUFFER elements */
};

typedef struct FRAGMENT_SLAVE FRAGMENT_SLAVE;

/* no need to clean this struct type, it should have nothing allocated inside it */
struct FRAGMENT_SLAVE
{
	u32 *len;
	char *data; /* this pointer actually points to data of the data 
		     * variable inside the FRAGMENT_MASTER structure.
		     * this is some kind of relationnal method which
		     * uses only a limited amount of memory to organise
		     * data.
		     */
};



#endif /* NOT __COMMON_H */
