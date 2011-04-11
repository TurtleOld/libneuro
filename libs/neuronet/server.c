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

#if old
static int
client_exist(LISTEN_DATA *src, CONNECT_DATA *c)
{
	CONNECT_DATA *buf;
	u32 total = 0;

	if (!src || !c)
	{
		NEURO_ERROR("Invalid argument", NULL);
		return -1;
	}

	if (!src->connections)
	{
		NEURO_ERROR("src->connections is NULL", NULL);
		return -1;
	}

	if (Neuro_EBufIsEmpty(src->connections))
	{
		NEURO_WARN("src->connections is empty", NULL);

		/* we return that the client don't exist */
		return 0;
	}

	total = Neuro_GiveEBufCount(src->connections) + 1;

	while (total-- > 0)
	{
		buf = Neuro_GiveEBuf(src->connections, total);

		if (buf == c)
			return 1;
	}

	NEURO_TRACE("No Active connection found for this connection on total %d", Neuro_GiveEBufCount(src->connections) + 1);
	return 0;
}
#endif /* old */

/*-------------------- Global Functions ----------------------------*/

void
Server_DisconnectClient(Slave *slv)
{
	EBUF *connections;
	EBUF *ce;

	if (!slv)
		return;

	connections = slv->master->slave->cType.server->connections;
	
	if (Neuro_EBufIsEmpty(connections))
		return;

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

	NEURO_TRACE("Disconnected client %x", slv);

	Neuro_SCleanEBuf(connections, slv);
}

/*-------------------- Poll ----------------------------------------*/

int
Server_Poll(Slave *slv)
{
	struct sockaddr_in connect_addr; /* server address */
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

	/* only read events are handled for now */
	if ((sigmask & 1) != 1)
	{
		return 0;
	}

	addrlen = sizeof(struct sockaddr_in);

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

#if old
		if (parent->type == TYPE_SERVER)
		{
			/* this sends a NULL packet to the server for a new client 
			 * connection.
			 */
			(parent->callback)(tmp, NULL, 0);
		}
#endif /* old */

		Slave_Init(buf, slv->master, _err, 1, tmp, NULL);

		memcpy(&slv->c_address, &connect_addr, sizeof(struct sockaddr_in));
		slv->addrlen = addrlen;

		Status_Add(slv->master, State_NewClient, NULL, 0, buf);

		NEURO_TRACE("%s", Neuro_s("New Client Connection %x on socket %d", tmp, slv->socket));
	}

	return 0;
}

/*-------------------- Constructor Destructor ----------------------*/

Slave *
Server_Create(Master *msr, int port)
{
	Slave *output;
	struct sockaddr_in saddress; /* server address */
	int _err = 0;
	unsigned int addrlen = 0;
	Server *svr;
	int sock = 0;

	svr = calloc(1, sizeof(Server));

	addrlen = sizeof(struct sockaddr_in);

	sock = socket(AF_INET, SOCK_STREAM, 0);

	if (sock <= 0)
	{
		NEURO_ERROR(0, "socket creation failed\n");
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
	saddress.sin_addr.s_addr = INADDR_ANY;
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
		NEURO_ERROR("binding failed", NULL);

		free(svr);

#ifndef WIN32
		close(sock);
#else /* WIN32 */
		closesocket(sock);
#endif /* WIN32 */
		return NULL;
	}

	_err = 0;

	_err = listen(sock, 2);
	if (_err == -1)
	{
		NEURO_ERROR("flagging the master socket as listening failed", NULL);
#ifndef WIN32
		close(sock);
#else /* WIN32 */
		closesocket(sock);
#endif /* WIN32 */

		return NULL;
	}

	Neuro_CreateEBuf(&svr->connections);
	Neuro_SetcallbEBuf(svr->connections, clean_connection);

	output = Slave_Create(msr, sock, 0, NULL, svr);

	msr->slave = output;

	NEURO_TRACE("Server is accepting connections on port %d", port);

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
