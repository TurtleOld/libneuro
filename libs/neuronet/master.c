/* master.c
 * Module : Master
 */

/*-------------------- Extern Headers Including --------------------*/
#ifndef WIN32

#else /* WIN32 */

#include <windows.h> /* WSAStartup WSACleanup */
#define MSG_DONTWAIT 0

#endif /* WIN32 */

#include <string.h> /* memset */
#include <errno.h> /* errno */

#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include "common.h"

#include "epoll.h"
#include "slave.h"
#include "client.h"
#include "server.h"
#include "status.h"
#include "util.h"
#include "lbuf.h"

/*-------------------- Main Module Header --------------------------*/
#include "master.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("netmaster");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

static void
clean_statuses_elem(void *src)
{
	Status *sta;

	sta = src;

	if (!sta)
		return;

	Status_Clear(sta);
}

static void
poll_disconnect_clients(Master *msr)
{
	Slave **tmp = NULL;

	if (Neuro_LBufIsEmpty(msr->disco_clients))
		return;

	Neuro_ResetLBuf(msr->disco_clients);

	while ((tmp = Neuro_GiveNextLBuf(msr->disco_clients)))
	{
		TRACE(Neuro_s("Disconnecting client %x from the buffer %x count %d", *tmp, tmp, Neuro_GiveLBufCount(msr->disco_clients)));

		Server_DisconnectClient(*tmp);

		Neuro_SCleanLBuf(msr->disco_clients, tmp);
	}
}

/* add a client to disconnect */
static void
add_client_disconnect(Slave *slv)
{
	Slave **tmp = NULL;

	if (!Neuro_LBufIsEmpty(slv->master->disco_clients))
	{
		TRACE("Checking the disconnection buffer for duplicate entries");

		Neuro_ResetLBuf(slv->master->disco_clients);

		while ((tmp = Neuro_GiveNextLBuf(slv->master->disco_clients)))
		{
			TRACE(Neuro_s("is %x the same as %x -- %s", *tmp, slv, *tmp == slv ? "yes" : "no"));

			if (*tmp == slv)
			{
				ERROR(Neuro_s("Slave %x already in the disconnection buffer!", slv));

				return;
			}
		}
	}

	Neuro_AllocLBuf(slv->master->disco_clients, sizeof(Slave**));

	tmp = Neuro_GiveCurLBuf(slv->master->disco_clients);

	*tmp = slv;

	TRACE(Neuro_s("Adding client %x for disconnection buffer %x -- buffer total %d", slv, tmp, Neuro_GiveLBufCount(slv->master->disco_clients)));

	/* so we no longer get any events for this client */
	Master_RmUfds(slv->master, slv);
}

/*-------------------- Global Functions ----------------------------*/

void
Master_AddUfds(Master *msr, Slave *slv)
{
	EPOLL_EVENT event;

	memset(&event, 0, sizeof(EPOLL_EVENT));

	event.events = EPOLLIN | EPOLLPRI | EPOLLERR;
	event.data.ptr = slv;

	Epoll_Ctl(msr->ep, EPOLL_CTL_ADD, slv->socket, &event);
}

void
Master_RmUfds(Master *msr, Slave *slv)
{
	EPOLL_EVENT event;

	memset(&event, 0, sizeof(EPOLL_EVENT));

	Epoll_Ctl(msr->ep, EPOLL_CTL_DEL, slv->socket, &event);
}

void
Master_EditEvent(Event *event, Slave *slave, int sigmask)
{
	event->slave = slave;
	event->sigmask = sigmask;
}

void
Master_PushEvent(Master *msr, Slave *slave, int sigmask)
{
	EBUF *ce;
	Event *tmp;

	if (!msr)
		return;

	ce = msr->cevents;

	Neuro_AllocEBuf(ce, sizeof(Event*), sizeof(Event));

	tmp = Neuro_GiveCurEBuf(ce);

	tmp->sigmask = sigmask;
	tmp->slave = slave;
}

int
Master_PollEvent(Master *msr)
{
	int i = 0;
	EPOLL_EVENT *events;

	events = Epoll_Wait(msr->ep, 0, &i);

	if (i == 0)
		return 0;

	if (i == -1)
	{
		ERROR(Neuro_s("Error raised by Epoll_Wait : returned the value %d", errno));

		return 1;
	}

	while (i-- > 0)
	{
		if ((events[i].events & EPOLLIN) == EPOLLIN || (events[i].events & EPOLLPRI) == EPOLLPRI)
		{
			Slave *slave = events[i].data.ptr;
			/* NEURO_TRACE("Input Event Catched", NULL); */

			if (slave->type == 0)
			{
				Server_Poll(slave);

				return 0;
			}
			else
			{
				int _err = 0;
				/* Status_Add(msr, State_DataAvail, NULL, 0, slave); */

				_err = Client_PollRead(slave);

				switch (_err)
				{
					case 0:
					{
						Client_PopData(slave);
					}
					break;

					case 1:
					{
						/* NEURO_WARN("TODO : Disconnect client -- Client_PollRead requested it", NULL); */
						Status_AddPriority(msr, State_ClientDisconnect, NULL, 0, slave);						
					}
					break;

					case 2:
					{
						return 1;
					}
					break;
				}
			}
		}
		else if ((events[i].events & EPOLLOUT) == EPOLLOUT)
		{
			/* NEURO_TRACE("Output event catched", NULL); */
		}
		else if ((events[i].events & EPOLLERR) == EPOLLERR || (events[i].events & EPOLLHUP) == EPOLLHUP)
		{
			/* NEURO_TRACE("Error Event Catched", NULL); */

			Status_Add(msr, State_ClientDisconnect, NULL, 0, events[i].data.ptr);
		}
	}
	return 0;
}

int
Master_SetSendPacketSize(Master *msr)
{
	if (!msr)
		return -1;

	if (msr->inclpacket_size == 0)
	{
		msr->inclpacket_size = 1;

		return 1;
	}
	else
		msr->inclpacket_size = 0;

	return 0;
}

void
Master_SetQuitFlag(Master *msr)
{
	if (!msr)
		return;

	msr->type = 2;
}

/*-------------------- Poll ----------------------------------------*/

Status *
Master_Poll(Master *msr)
{
	if (!msr)
		return NULL;

	if (msr->type == 2)
	{
		TRACE("End user application flagged the Master as quit, exiting");

		Status_Set(msr->status, State_Disconnect, NULL, 0, NULL);

		return msr->status;
	}

	if (!msr->slave)
	{
		ERROR("Attempted to poll without a properly initialized slave");
		return NULL;
	}

	if (msr->type == TYPE_SERVER)
	{
		if (!msr->slave->cType.server)
		{
			ERROR("Attempted to poll without a properly initialized server");
			return NULL;
		}
	}
	else if (msr->type == TYPE_CLIENT)
	{
		if (!msr->slave->cType.client)
		{
			ERROR("Attempted to poll without a properly initialized client");
			return NULL;
		}
	}
	else
	{
		ERROR("Invalid Master type, bailing out");

		return NULL;
	}

	if (!Neuro_LBufIsEmpty(msr->statuses))
	{
		/* delete the first entry of msr->statuses as it was used up */
	}


	/* disconnect all clients that are required to be */
	poll_disconnect_clients(msr);

	Master_PollEvent(msr);

	if (Neuro_LBufIsEmpty(msr->statuses))
		Status_Set(msr->status, State_NoData, NULL, 0, NULL);
	else
	{
		Status *buf;

		/* fetch the first element */
		buf = Neuro_GiveLBuf(msr->statuses);

		Status_Move(buf, msr->status);

		Neuro_SCleanLBuf(msr->statuses, buf);

		if (msr->status->status == State_ClientDisconnect)
		{
			TRACE("Sending the event to really disconnect the client");
			/* real disconnect event */
			/* Master_PushEvent(msr, msr->status->connection, 16); */
			add_client_disconnect(msr->status->connection);
		}
		else
		{
			TRACE(Neuro_s("Will send Status type %d -- data address %x", msr->status->status, msr->status->packet));
		}
	}

	/*
	if (Master_PollEvent(msr) > 0)
	{
		NEURO_WARN("Master_PollEvent raised an error", NULL);

		Status_Set(msr->status, State_Disconnect, NULL, 0, NULL);

		return msr->status;
	}
	*/

	return msr->status;
}

/*-------------------- Constructor Destructor ----------------------*/

/* see Master for the connection_type */
Master *
Master_Create(u32 connection_type)
{
	Master *msr;
	Slave *slv;

	msr = calloc(1, sizeof(Master));

	slv = NULL;

	msr->inclpacket_size = 0;

	msr->ep = Epoll_Create(sizeof(Slave));

	/* NEURO_TRACE("Creating the connect events buffer", NULL); */
	Neuro_CreateEBuf(&msr->cevents);

	if (connection_type >= TYPE_CLIENT)
		connection_type = TYPE_CLIENT;

	msr->type = connection_type;

#if WIN32
	{
		int _err = 0;

		_err = WSAStartup(MAKEWORD(1, 1), &msr->wsaData);

		if (_err < 0)
			return NULL;
	}
#endif /* WIN32 */

	msr->disco_clients = Neuro_CreateLBuf();

	msr->statuses = Neuro_CreateLBuf();
	Neuro_SetcallbLBuf(msr->statuses, clean_statuses_elem);

	msr->status = Status_Create();

	msr->callback = NULL;

	return msr;
}

void
Master_Destroy(Master *msr)
{
	if (!msr)
		return;

#ifdef WIN32
	WSACleanup();
#endif /* WIN32 */


	Neuro_CleanEBuf(&msr->cevents);


	Neuro_CleanLBuf(msr->disco_clients);


	Neuro_CleanLBuf(msr->statuses);

	Status_Destroy(msr->status);

	Slave_Destroy(msr->slave);

	Epoll_Destroy(msr->ep);

	free(msr);
}
