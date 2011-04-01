/* master.c
 * Module : Master
 */

/*-------------------- Extern Headers Including --------------------*/
#ifndef WIN32

#if use_epoll
#include <sys/epoll.h>
#else /* not use_epoll */
#include <sys/poll.h> /* poll */
#endif /* not use_epoll */

#else /* WIN32 */

#include <windows.h> /* WSAStartup WSACleanup */
#define MSG_DONTWAIT 0

#endif /* WIN32 */

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

	Status_Destroy(sta);
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
						Server_DisconnectClient(event->slave);
					}

					return 0;
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

#if tmp
#if use_epoll
							event->slave->epollctx->events |= EPOLLOUT;
							epoll_ctl(msr->epoll_fd, EPOLL_CTL_MOD, event->slave->cType.client->socket, event->slave->epollctx);
#endif /* use_epoll */
#endif /* tmp */
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

#if tmp
#if use_epoll
#else /* not use_epoll */
static Slave *
lookup_SlaveSocket(Master *msr, int socket)
{
	if (msr->type == TYPE_CLIENT)
	{
		return msr->slave;
	}
	else
	{
		int total = 0;
		Slave *slave;
		EBUF *connections;

		connections = msr->slave->cType.server->connections;

		if (Neuro_EBufIsEmpty(connections))
			return NULL;

		total = Neuro_GiveEBufCount(connections) + 1;

		while (total-- > 0)
		{
			slave = Neuro_GiveEBuf(connections, total);

			if (slave->socket == socket)
			{
				return slave;
			}
		}
	}

	return NULL;
}

/* only required with poll */
static void
populate_ufds(Master *msr)
{
	if (msr->ufds)
	{
		free(msr->ufds);
		msr->ufds = NULL;
	}

	msr->nfds = 0;

	if (msr->type == TYPE_CLIENT)
	{
		Master_AddUfds(msr, msr->slave);
	}
	else
	{
		int total = 0;
		Slave *tmp;
		EBUF *connections;

		connections = msr->slave->cType.server->connections;

		Master_AddUfds(msr, msr->slave);
		
		if (Neuro_EBufIsEmpty(connections))
			return;

		total = Neuro_GiveEBufCount(connections) + 1;

		while (total-- > 0)
		{
			tmp = Neuro_GiveEBuf(connections, total);

			Master_AddUfds(msr, tmp);
		}
	}
}
#endif /* not use_epoll */
#endif /* tmp */

/*-------------------- Global Functions ----------------------------*/

void
Master_AddUfds(Master *msr, Slave *slv)
{
	EPOLL_EVENT event;

	event.events = EPOLLIN | EPOLLPRI | EPOLLERR;
	event.data.ptr = slv;

	Epoll_Ctl(msr->ep, EPOLL_CTL_ADD, slv->socket, &event);

#if tmp
#if use_epoll
	{
		if (!msr->epEvent)
			msr->epEvent = calloc(1, sizeof(struct epoll_event));
		else
			msr->epEvent = realloc(msr->epEvent, (msr->nfds + 1) * sizeof(struct epoll_event));
	}

	slv->epollctx = calloc(1, sizeof(struct epoll_event));

	slv->epollctx->events = EPOLLIN | EPOLLPRI | /*EPOLLOUT |*/ EPOLLERR;
	slv->epollctx->data.ptr = slv;

	NEURO_TRACE("Adding socket %d to epoll_ctl", slv->socket);
	if (epoll_ctl(msr->epoll_fd, EPOLL_CTL_ADD, slv->socket, slv->epollctx) == -1)
	{
		NEURO_ERROR("epoll_ctl raised the error %d while adding a socket", errno);
	}
#else /* not use_epoll */
	if (!msr->ufds)
		msr->ufds = calloc(1, sizeof(struct pollfd));
	else
		msr->ufds = realloc(msr->ufds, (msr->nfds + 1) * sizeof(struct pollfd));

	msr->ufds[msr->nfds].fd = slv->socket;

	msr->ufds[msr->nfds].events = POLLIN | POLLPRI;

	if (slv->type == 1)
	{
		if ((slv->sigmask & 2) == 0)
			msr->ufds[msr->nfds].events |= POLLOUT;
	}

#endif /* not use_epoll */
	msr->nfds++;
#endif /* tmp */
}

void
Master_RmUfds(Master *msr, Slave *slv)
{
	Epoll_Ctl(msr->ep, EPOLL_CTL_DEL, slv->socket, NULL);
#if tmp
#if use_epoll
	NEURO_TRACE("Removing socket %d from epoll", slv->socket);

	if (epoll_ctl(msr->epoll_fd, EPOLL_CTL_DEL, slv->socket, NULL) == -1)
	{
		NEURO_ERROR("%s", Neuro_s("Epoll gave an error : Couldn't remove socket %d errno %d", socket, errno));
	}
#else /* not use_epoll */
	int i = 0;

	if (!msr)
		return;

	if (!msr->ufds || msr->nfds <= 0)
		return;

	i = msr->nfds;

	while (i-- > 0)
	{
		if (msr->ufds[i].fd == slv->socket)
		{
			if ((msr->nfds - 1) == i)
			{
				free(msr->ufds);

				msr->ufds = NULL;
				msr->nfds = 0;
			}
			else /* place the last elem into the current one */
			{
				memcpy(&msr->ufds[i], &msr->ufds[msr->nfds - 1], sizeof(struct pollfd));
				msr->nfds -= 1;

				msr->ufds = realloc(msr->ufds, msr->nfds * sizeof(struct pollfd));
			}

			return;
		}
	}
#endif /* not use_epoll */
#endif /* tmp */
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
		NEURO_ERROR("%s", Neuro_s("Error raised by epoll : returned the value %d", errno));

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
#if tmp
#if use_epoll
	int i = 0;
	int sigmask = 0;
	struct epoll_event *events = NULL;

	i = epoll_wait(msr->epoll_fd, msr->epEvent, msr->nfds, 100);

	if (i == 0)
		return handle_Events(msr);

	if (i == -1)
	{
		NEURO_ERROR("%s", Neuro_s("Error raised by epoll : returned the value %d -- total was %d", errno, msr->nfds));

		return 1;
	}

	/* NEURO_TRACE("Epoll gave us some events", NULL); */

	events = msr->epEvent;

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
#if tmp
			int socket;
			Slave *tmp = events[i].data.ptr;
			int required = 0;

			if (tmp->type == 0)
			{
				socket = tmp->cType.server->socket;

				if (!tmp->cType.server->sigmask & 2)
					required = 1;
			}
			else
			{
				socket = tmp->cType.client->socket;

				if (!tmp->cType.client->sigmask & 2)
					required = 1;
			}

			NEURO_TRACE("events status %d", tmp->epollctx->events & EPOLLOUT);
			tmp->epollctx->events ^= EPOLLOUT;
			epoll_ctl(msr->epoll_fd, EPOLL_CTL_MOD, socket, tmp->epollctx);

			NEURO_TRACE("%s", Neuro_s ("Output Event Catched -- revents %d -- wanted %d", events[i].events, tmp->epollctx->events & EPOLLOUT));

			if (required)
#endif /* tmp */
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
#else /* not use_epoll */

	int _err = 0;
	int i = msr->nfds;
	int sigmask = 0;

	/* (re)populate ufds because poll only returns
	 * the ufds of the elements that raised an event
	 * freeing the others... hence the requirement of
	 * constantly repopulating the buffer.
	 */
	populate_ufds(msr);

	if (msr->ufds == NULL || msr->nfds == 0)
		return 0;

	_err = poll(msr->ufds, i, 100);

	/* no fd event happened, bailing out */
	if (_err == 0)
		return handle_Events(msr);;

	if (_err < 0)
	{
		NEURO_ERROR("Error raised by poll : returned the value %d", errno);

		return 1;
	}

	while (i-- > 0)
	{
		sigmask = 0;

		if ((msr->ufds[i].revents & POLLIN) == POLLIN || (msr->ufds[i].revents & POLLPRI) == POLLPRI)
		{
			/* NEURO_TRACE("Input Event Catched -- revents %d", msr->ufds[i].revents); */
			sigmask += 1;
		}
		if ((msr->ufds[i].revents & POLLOUT) == POLLOUT)
		{
			/* NEURO_TRACE("Output Event Catched -- revents %d", msr->ufds[i].revents); */
			sigmask += 2;
		}
		if ((msr->ufds[i].revents & POLLERR) == POLLERR || (msr->ufds[i].revents & POLLHUP) == POLLHUP)
		{
			/* NEURO_TRACE("Error Event Catched -- revents %d", msr->ufds[i].revents); */
			sigmask += 4;
		}

		if (sigmask > 0)
		{
			if (msr->type == TYPE_SERVER)
			{
				if (msr->slave->socket == msr->ufds[i].fd)
				{
					int sigmask1 = msr->slave->sigmask;

					msr->slave->sigmask |= sigmask;

					if (msr->slave->sigmask != sigmask1)
					{
						/* add element to Event */

						Master_PushEvent(msr, msr->slave, sigmask);

						continue;
					}
				}
			}

			/* for clients, in client mode and server mode */
			{
				Slave *slave;
				int sigmask1;

				slave = lookup_SlaveSocket(msr, msr->ufds[i].fd);

				if (!slave)
					return 1;

				sigmask1 = slave->sigmask;
				slave->sigmask |= sigmask;

				if (slave->sigmask != sigmask1)
				{
					/* add element to Event */
					Master_PushEvent(msr, slave, sigmask);
				}
			}
		}
	}

	return handle_Events(msr);
#endif /* not use_epoll */
#endif /* tmp */
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

#if tmp
	if (msr->type == TYPE_SERVER)
	{
		/* seems like nothing belongs here any longer */

		return (msr->status);
	}
	else
	{
		if (msr->slave->cType.client == NULL)
		{
			return NULL;
		}

		return Client_PopData(msr->slave);
	}
#endif /* tmp */

	/* Status_Set(msr->status, State_Disconnect, NULL, 0, NULL); */

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

#if tmp
#if use_epoll
	/* msr->epoll_fd = epoll_create(sizeof(Slave)); */
	/* msr->epEvent = NULL; */

#else /* not use_epoll */
	msr->ufds = NULL;
	msr->cevents = NULL;
#endif /* not use_epoll */
#endif /* tmp */

	/* NEURO_TRACE("Creating the connect events buffer", NULL); */
	Neuro_CreateEBuf(&msr->cevents);

	if (connection_type >= TYPE_CLIENT)
		connection_type = TYPE_CLIENT;

	msr->type = connection_type;

#if WIN32
	int _err = 0;

	_err = WSAStartup(MAKEWORD(1, 1), &msr->wsaData);

	if (_err < 0)
		return 1;
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


	Epoll_Destroy(msr->ep);

#if tmp
#if use_epoll
	/*close(msr->epoll_fd);

	free(msr->epEvent);*/
#else /* not use_epoll */
	if (msr->ufds)
	{
		free(msr->ufds);
		msr->ufds = NULL;
	}
#endif /* not use_epoll */

	msr->nfds = 0;
#endif /* tmp */

	Neuro_CleanEBuf(&msr->cevents);

	Neuro_CleanEBuf(&msr->statuses);

	Status_Destroy(msr->status);

	Slave_Destroy(msr->slave);

	free(msr);
}
