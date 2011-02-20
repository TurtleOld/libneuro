/* netabs.c
 * Module : NetAbs_
 *
 * this module is to abstract the following
 * functions -- all network related.
 *
 * socket() -- creates a socket
 * accept() -- accepts a connection on a server
 * bind() -- bind a port for accepting connections on a server
 * connect() -- connects to a server on a client
 * inet_ntoa() -- transforms a sin_addr type into a string (the IP)
 * gethostbyname() -- dns interface to get the IP of a domain
 *
 * unix -- w32 -- description
 *
 * close() -- closesocket() -- closes a socket
 * inet_aton() -- inet_addr() -- transforms an IP string into the type sin_addr.s_addr
 * fcntl() -- ioctlsocket() -- to set options on the sockets
 *
 *
 *
 * and it also abstracts those structures/variables/macros
 *
 * struct sockaddr_in -- contains socket address details on a client to connect()
 * struct hostent -- gethostbyname() outputs this type of structure
 *
 * unix -- w32 -- description
 *
 * socklen_t -- int -- just an integer to the length of a socket
 * F_SETFL -- FIONBIO -- 2nd argument of fcntl() and ioctlsocket()
 * O_NONBLOCK -- &integer -- to set the socket to not block
 * EINPROGRESS -- WSAEINPROGRESS -- connection in progress
 * EALREADY -- WSAEALREADY -- already connected
 *  -- WSAStartup() -- some winsock init thingy
 *  -- WSACleanup() -- some winsock cleanup thingy
 *
 *
 *
 * please note that under the w32 platform, only the
 * header file windows.h is required.
 */

/*-------------------- Extern Headers Including --------------------*/


/*-------------------- Local Headers Including ---------------------*/

/*-------------------- Main Module Header --------------------------*/
#include "neuro/nnet/netabs.h"


/*--------------------      Other       ----------------------------*/

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

/*-------------------- Poll ----------------------------------------*/

void
NetAbs_Poll()
{
	
}

/*-------------------- Constructor Destructor ----------------------*/

int
NetAbs_Init()
{
	return 0;
}

void
NetAbs_Clean()
{
	
}
