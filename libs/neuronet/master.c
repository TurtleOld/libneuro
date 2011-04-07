/* master.c
 * Module : Master
 */

/*-------------------- Extern Headers Including --------------------*/
#ifndef WIN32

#include <sys/epoll.h>

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

static int
handle_Events(Master *msr)
{
	int total = 0;
	EBUF *ce;
	Event *event;

	if (Neuro_EBufIsEmpty(msr->cevents))
		return 0;

	/* NEURO_TRACE("Handling events", NULL); */

	ce = msr->cevents;

	total = Neuro_GiveEBufCount(ce) + 1;

	while (total-- > 0)
	{
		event = Neuro_GiveEBuf(ce, total);

		NEURO_TRACE("%s", Neuro_s("type %d wanted sigmask %d", event->slave->type, event->sigmask));

		switch (event->slave->type)
		{
			/* Server type */
			case 0:
			{
				event->slave->sigmask = event->sigmask;

				Server_Poll(event->slave);

				event->slave->sigmask = 0;

				Util_SCleanEBuf(ce, event);
			}
			break;

			/* Client type */
			case 1:
			{
				int clean_elem = 0;

				event->slave->sigmask = event->sigmask;

				NEURO_TRACE("%s", Neuro_s("Client address %x current sigmask %d", event->slave->cType.client, event->slave->sigmask));

				if ((event->sigmask & 8) == 8 || ((event->sigmask & 4) == 4))
				{
					if (event->slave->master->type == TYPE_CLIENT)
					{
						Status_Add(event->slave->master, State_Disconnect, NULL, 0, NULL);
					}
					else
					{
						Status_Add(event->slave->master, State_ClientDisconnect, NULL, 0, event->slave);
						/* avoid any more packets coming from the client 
						 * creating chaos in the event queue.
						 */
						Master_RmUfds(msr, event->slave);

						event->slave->sigmask ^= 8;
						event->slave->sigmask ^= 4;

						clean_elem = 1;
					}
				}
				if ((event->sigmask & 1) == 1)
				{
					switch (Client_PollRead(event->slave))
					{
						case 0:
						{
							/* Status *state; */

							NEURO_TRACE("Data available for the server", NULL);

							Client_PopData(event->slave);
							/*state = Client_PopData(event->slave);
							if (state)
							{
								if (state->status != State_NoData)
									Status_Add(msr, state->status, state->packet, state->packet_len, state->connection);
							}
							*/

							event->slave->sigmask ^= 1;

							clean_elem = 1;
						}
						break;

						case 1:
						{
							NEURO_TRACE("Client disconnection required by Client_PollRead", NULL);
							Master_PushEvent(msr, event->slave, 8);


							event->slave->sigmask = 8;

							clean_elem = 1;
						}
						break;

						case 2:
						{
							NEURO_ERROR("Error raised by Client_PollRead", NULL);

							return 1;
						}
						break;
					}
				}
				if((event->sigmask & 2) == 2)
				{
					/* the buffer is not yet ready for writing */
					if ((event->slave->sigmask & 2) != 2)
						continue;

					switch (Client_PollSend(event->slave))
					{
						case 0:
						{
							clean_elem = 1;
						}
						break;

						case 1:
						{
							event->slave->sigmask ^= 2;
							clean_elem = 1;
						}
						break;

						case 2:
						{
							NEURO_TRACE("Client disconnection required by Client_PollSend", NULL);
							Master_PushEvent(msr, event->slave, 8);
							clean_elem = 1;
						}
						break;

						case 3:
						{
							NEURO_ERROR("Error raised by Client_PollSend", NULL);

							return 1;
						}
						break;
					}
				}
				else
				{
					clean_elem = 1;
				}

				if (clean_elem == 1)
					Util_SCleanEBuf(ce, event);
			}
			break;

			default:
			{
				NEURO_ERROR("Unhandled Event type %d", event->slave->type);

				return 1;
			}
			break;
		}
	}

	return 0;
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
	Epoll_Ctl(msr->ep, EPOLL_CTL_DEL, slv->socket, NULL);
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
	int sigmask = 0;
	EPOLL_EVENT *events;

	events = Epoll_Wait(msr->ep, 100, &i);

	if (i == 0)
		return handle_Events(msr);

	if (i == -1)
	{
		NEURO_ERROR("%s", Neuro_s("Error raised by Epoll_Wait : returned the value %d", errno));

		return 1;
	}

	while (i-- > 0)
	{
		sigmask = 0;

		if ((events[i].events & EPOLLIN) == EPOLLIN || (events[i].events & EPOLLPRI) == EPOLLPRI)
		{
			/* NEURO_TRACE("Input Event Catched -- revents %d", msr->ufds[i].revents); */
			sigmask += 1;
		}
		if ((events[i].events & EPOLLOUT) == EPOLLOUT)
		{
				sigmask += 2;
		}
		if ((events[i].events & EPOLLERR) == EPOLLERR || (events[i].events & EPOLLHUP) == EPOLLHUP)
		{
			/* NEURO_TRACE("Error Event Catched -- revents %d", msr->ufds[i].revents); */
			sigmask += 4;
		}


		if (sigmask > 0)
			Master_PushEvent(msr, (Slave*)events[i].data.ptr, sigmask);
	}

	return handle_Events(msr);
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

/*-------------------- Poll ----------------------------------------*/

Status *
Master_Poll(Master *msr)
{
	if (!msr)
		return NULL;

	if (!msr->slave)
	{
		NEURO_ERROR("Attempted to poll without a properly initialized slave", NULL);
		return NULL;
	}

	if (msr->type == TYPE_SERVER)
	{
		if (!msr->slave->cType.server)
		{
			NEURO_ERROR("Attempted to poll without a properly initialized server", NULL);
			return NULL;
		}
	}
	else
	{
		if (!msr->slave->cType.server)
		{
			NEURO_ERROR("Attempted to poll without a properly initialized client", NULL);
			return NULL;
		}
	}

	if (Neuro_EBufIsEmpty(msr->statuses))
		Status_Set(msr->status, State_NoData, NULL, 0, NULL);
	else
	{
		Status *buf;

		buf = Neuro_GiveEBuf(msr->statuses, 0);

		Status_Move(buf, msr->status);

		Util_SCleanEBuf(msr->statuses, buf);

		NEURO_TRACE("%s", Neuro_s("Will send Status type %d -- data address %x", msr->status->status, msr->status->packet));

		return (msr->status);
	}

	if (Master_PollEvent(msr) > 0)
	{
		NEURO_WARN("Master_PollEvent raised an error", NULL);

		Status_Set(msr->status, State_Disconnect, NULL, 0, NULL);

		return msr->status;
	}

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
			return 1;
	}
#endif /* WIN32 */

	Neuro_CreateEBuf(&msr->statuses);
	Neuro_SetcallbEBuf(msr->statuses, clean_statuses_elem);

	msr->status = Status_Create();

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

	Neuro_CleanEBuf(&msr->statuses);

	Status_Destroy(msr->status);

	Slave_Destroy(msr->slave);

	Epoll_Destroy(msr->ep);

	free(msr);
}
