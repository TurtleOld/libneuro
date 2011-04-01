/* slave.c
 * Module : Slave
 */

/*-------------------- Extern Headers Including --------------------*/

#ifndef WIN32

#if use_epoll
#include <sys/epoll.h>
#endif /* use_epoll */

#include <unistd.h> /* close */

#else /* WIN32 */

#include <windows.h> /* closesocket */
#define MSG_DONTWAIT 0

#endif /* WIN32 */

#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

#include "common.h"

#include "client.h"
#include "server.h"
#include "master.h"

/*-------------------- Main Module Header --------------------------*/
#include "slave.h"


/*--------------------      Other       ----------------------------*/
NEURO_MODULE_CHANNEL("netslave");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

void *
Slave_GetData(const Slave *slv)
{
	if (!slv)
	{
		NEURO_ERROR("Status missing", NULL);
		return NULL;
	}

	return (void *)slv->ptr;
}

void
Slave_SetData(Slave *slv, const void *ptr)
{
	if (!slv)
	{
		NEURO_ERROR("Slave missing", NULL);
		return;
	}

	slv->ptr = ptr;
}

/*-------------------- Constructor Destructor ----------------------*/

int
Slave_Init(Slave *slv, Master *master, int socket, int type, Client *client, Server *server)
{
	if (!slv)
	{
		NEURO_ERROR("Empty argument", NULL);
		return 1;
	}

	slv->master = master;

	if (type == 0)
	{
		if (!server)
		{
			NEURO_ERROR("Empty server pointer", NULL);
			return 1;
		}
		slv->type = type;
		slv->cType.server = server;
	}
	else if (type == 1)
	{
		if (!client)
		{
			NEURO_ERROR("Empty client pointer", NULL);
			return 1;
		}
		slv->type = type;
		slv->cType.client = client;
	}
	else
	{
		NEURO_ERROR("Invalid type given %d", type);

		return 1;
	}

	slv->socket = socket;

	slv->sigmask = 0;

	Master_AddUfds(master, slv);

	return 0;
}

Slave *
Slave_Create(Master *msr, int socket, int type, Client *client, Server *server)
{
	Slave *output;

	output = malloc(sizeof(Slave));

	if (Slave_Init(output, msr, socket, type, client, server))
	{
		return NULL;
	}

	return output;
}

void
Slave_Clean(Slave *slv)
{
	if (!slv)
	{
		NEURO_ERROR("Empty argument", NULL);
		return;
	}

	Master_RmUfds(slv->master, slv);

#ifdef WIN32
	closesocket(slv->socket);
#else /* NOT WIN32 */
	close(slv->socket);
#endif /* NOT WIN32 */

	if (slv->type == 0)
	{
		Server_Destroy(slv->cType.server);
	}
	else
	{
		Client_Destroy(slv->cType.client);
	}
}

void
Slave_Destroy(Slave *slv)
{
	if (!slv)
	{
		NEURO_ERROR("Empty argument", NULL);
		return;
	}

	Slave_Clean(slv);

	free(slv);
}
