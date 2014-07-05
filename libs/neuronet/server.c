/* server.c
 * Module : Server
 */

/*-------------------- Extern Headers Including --------------------*/
#ifndef WIN32
#include <sys/socket.h>
#include <netinet/in.h> /* htons */

#include <fcntl.h> /* fcntl (control of the sockets) */
#include <unistd.h> /* close */

#else /* WIN32 */

#include <windows.h> /* (winsock2 functions) */
#define MSG_DONTWAIT 0

#endif /* WIN32 */

#include <string.h> /* memcpy */

#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include "common.h"

#include "slave.h"
#include "master.h"
#include "client.h"
#include "status.h"
#include "util.h"

/*-------------------- Main Module Header --------------------------*/
#include "server.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("netserver");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

static void
clean_connection(void *src)
{
	Slave *tmp;

	tmp = src;

	if (!tmp)
		return;

	Slave_Clean(tmp);
}

/* 0 is the same, 1 is different */
static int
sockaddrCompare(struct sockaddr_in *a, struct sockaddr_in *b)
{
	int i = 0;
	int size = sizeof(struct sockaddr_in);
	char *bA, *bB;

	bA = (char *)a;
	bB = (char *)b;

	while( i < size)
	{
		if (bA[i] == bB[i])
		{
			i++;
			continue;
		}

		return 1;
	}
	return 0;
}

static Slave *
getSlaveByConnectAddr(EBUF *connections, struct sockaddr_in *connect_addr)
{
	int total = 0;
	Slave *buf = NULL;

	if (Neuro_EBufIsEmpty(connections))
		return NULL;

	total = Neuro_GiveEBufCount(connections) + 1;

	while (total-- > 0)
	{
		buf = Neuro_GiveEBuf(connections, total);

		if (!sockaddrCompare(connect_addr, &buf->c_address))
			return buf;
	}

	return NULL;
}

/*-------------------- Global Functions ----------------------------*/

void
Server_DisconnectClient(Slave *slv)
{
	EBUF *connections;
#if old
	EBUF *ce;
#endif /* old */

	if (slv)
		TRACE(Neuro_s("Attempting to disconnect the slave %x", (int)slv));

	if (!slv || !slv->master || !slv->master->slave)
	{
		ERROR("Invalid slave trying to be disconnected");
		return;
	}

	if (slv->master->slave->cType.server == NULL || slv->master->slave->cType.client == NULL)
	{
		ERROR("The client attempted to disconnect a client that was already disconnected!");
		return;
	}

	connections = slv->master->slave->cType.server->connections;
	
	if (Neuro_EBufIsEmpty(connections))
		return;

#if old
	ce = slv->master->cevents;

	/* delete all the remaining events for the slave client, if any */
	if (!Neuro_EBufIsEmpty(ce))
	{
		int total = 0;
		Event *tmp;

		total = Neuro_GiveEBufCount(ce) + 1;

		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(ce, total);

			if (tmp->slave == slv)
			{
				Util_SCleanEBuf(ce, tmp);
			}
		}
	}

	/* delete all the remaining statuses for the slave client, if any */
	if (!Neuro_EBufIsEmpty(slv->master->statuses))
	{
		int total = 0;
		Status *tmp;

		total = Neuro_GiveEBufCount(slv->master->statuses) + 1;

		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(slv->master->statuses, total);

			if (tmp->connection == slv)
			{
				Util_SCleanEBuf(slv->master->statuses, tmp);
			}
		}
	}
#endif /* old */

	TRACE(Neuro_s("Disconnected client %x", slv));

	Status_PurgeSlave(slv->master, slv);

	Neuro_SCleanEBuf(connections, slv);
}

/*-------------------- Poll ----------------------------------------*/

int
Server_Poll(Slave *slv)
{
	struct sockaddr_in connect_addr; /* new connection's address */
#ifndef WIN32
	u32 addrlen = 0;
#else /* WIN32 */
	i32 addrlen = 0;
#endif /* WIN32 */
	i32 _err = 0;
	int sigmask = 0;
	Server *server;

	if (!slv)
	{
		return 1;
	}

	server = slv->cType.server;
	sigmask = slv->sigmask;

#if old
	/* only read events are handled for now */
	if ((sigmask & 1) != 1)
	{
		return 0;
	}
#endif /* old */

	addrlen = sizeof(struct sockaddr_in);

	switch (slv->master->protocolType)
	{
		case SOCK_STREAM:
		{
			/* we check to see if theres new clients who want to join the party. */
			_err = accept(slv->socket, (struct sockaddr*)&connect_addr, &addrlen);


			/* if theres a new connection, we will add it to our buffer */
			if (_err != -1)
			{
				Slave *buf;
				Client *tmp;

				/* we have a new client that wishes to connect so we let it connect */
			
				Neuro_AllocEBuf(server->connections, sizeof(Slave*), sizeof(Slave));

				buf = Neuro_GiveCurEBuf(server->connections);

				tmp = Client_Create(slv->master);

				tmp->connection_start_time = Neuro_GetTickCount();
				tmp->idle_time = tmp->connection_start_time;
				tmp->timeout = 0;

				Slave_Init(buf, slv->master, _err, 1, tmp, NULL);

				memcpy(&buf->c_address, &connect_addr, sizeof(struct sockaddr_in));
				buf->addrlen = addrlen;

				Status_Add(slv->master, State_NewClient, NULL, 0, buf);

				TRACE(Neuro_s("New Client Connection %x on socket %d (master %x slave %x)", buf, slv->socket, (int)buf->master, (int)buf));
			}
		}
		break;

		case SOCK_DGRAM:
		{
			char *rbuffer = NULL;
#ifndef WIN32
			ssize_t rbuflen = 0;
#else /* WIN32 */
			int rbuflen = 0;
#endif /* WIN32 */
			int sock;
			Slave *clientSlv;

			rbuffer = calloc(1, MAX_PACKET_SIZE * INPUT_PACKET_BUFFERING);

			rbuflen = recvfrom(slv->socket, rbuffer, MAX_PACKET_SIZE * INPUT_PACKET_BUFFERING, 0, (struct sockaddr*)&connect_addr, &addrlen);

			TRACE(Neuro_s("Packet from IP %s", inet_ntoa(connect_addr.sin_addr)));

			clientSlv = getSlaveByConnectAddr(server->connections, &connect_addr);

			if (!clientSlv)
			{
				/* this is a new client connection, we create a new slave for this. */

				Slave *buf;
				Client *tmp;

				/* we have a new client that wishes to connect so we let it connect */
			
				Neuro_AllocEBuf(server->connections, sizeof(Slave*), sizeof(Slave));

				buf = Neuro_GiveCurEBuf(server->connections);

				tmp = Client_Create(slv->master);

				tmp->connection_start_time = Neuro_GetTickCount();
				tmp->idle_time = tmp->connection_start_time;
				tmp->timeout = 0;

				Slave_Init(buf, slv->master, slv->socket, 1, tmp, NULL);

				memcpy(&buf->c_address, &connect_addr, sizeof(struct sockaddr_in));
				buf->addrlen = addrlen;

				Status_Add(slv->master, State_NewClient, NULL, 0, buf);

				TRACE(Neuro_s("UDP-> New Client Connection %x on socket %d", buf, slv->socket));

				clientSlv = buf;
			}
			else
			{
				/* we are already connected to this slave, so we just handle its packet */
			}

			TRACE(Neuro_s("UDP-> Got a new packet of size %d", rbuflen));

			Util_Buffer_Recv_Data(clientSlv, rbuffer, rbuflen);

			Client_PopData(clientSlv);
		}
		break;
	}

	return 0;
}

/*-------------------- Constructor Destructor ----------------------*/

Slave *
Server_Create(Master *msr, const char *listen_ip, int port)
{
	Slave *output;
	struct sockaddr_in saddress; /* server address */
	int _err = 0;
	unsigned int addrlen = 0;
	Server *svr;
	int sock = 0;

	svr = calloc(1, sizeof(Server));

	addrlen = sizeof(struct sockaddr_in);

	sock = socket(AF_INET, msr->protocolType, 0);

	if (sock <= 0)
	{
		ERROR("socket creation failed\n");
		return NULL;
	}

#ifndef WIN32
	{
		int set = 1;

		/* this should fix an annoying issue happening between the execution 
		 * of the server. Bind fails for a while until the address is available
		 * again.
		 */
		setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (void *)&set, sizeof(int));
	}
#endif /* not WIN32 */

	saddress.sin_family = AF_INET;
	saddress.sin_addr.s_addr = inet_addr(listen_ip);
	saddress.sin_port = htons(port);

	_err = bind(sock, (struct sockaddr*)&saddress, sizeof(saddress));

#ifndef WIN32
	fcntl(sock, F_SETFL, O_NONBLOCK);
#else /* WIN32 */
	{
		unsigned long nb = 1;
		if (ioctlsocket(sock, FIONBIO, &nb) == SOCKET_ERROR)
			return NULL;
	}
#endif /* WIN32 */

	if (_err == -1)
	{
		ERROR("binding failed");

		free(svr);

#ifndef WIN32
		close(sock);
#else /* WIN32 */
		closesocket(sock);
#endif /* WIN32 */
		return NULL;
	}

	_err = 0;

	if (msr->protocolType == SOCK_STREAM)
	{
		_err = listen(sock, 2);
		if (_err == -1)
		{
			ERROR("flagging the master socket as listening failed");
#ifndef WIN32
			close(sock);
#else /* WIN32 */
			closesocket(sock);
#endif /* WIN32 */

			return NULL;
		}
	}

	Neuro_CreateEBuf(&svr->connections);
	Neuro_SetcallbEBuf(svr->connections, clean_connection);

	output = Slave_Create(msr, sock, 0, NULL, svr);

	msr->slave = output;
	msr->slave->c_address = saddress;
	msr->slave->addrlen = sizeof(saddress);

	TRACE(Neuro_s("Server is accepting connections on port %d", port));

	return output;
}

void
Server_Destroy(Server *srv)
{
	if (!srv)
		return;

	Neuro_CleanEBuf(&srv->connections);

	free(srv);
}
