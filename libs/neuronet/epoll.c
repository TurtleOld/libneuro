/* epoll.c
 * Module : Epoll
 *
 * abstract the epoll interface to be useable on w32 
 * OS by using a wicked version with select() (cause that's
 * all w32 has).
 */

#include <global.h>

/*-------------------- Extern Headers Including --------------------*/

#ifndef WIN32

/* please note that only one of epoll, 
 * poll or select can be active at one time.
 */

#ifdef HAVE_EPOLL
#include <sys/epoll.h> /* epoll_ctl epoll_wait epoll_create */
#endif /* HAVE_EPOLL */

#ifdef HAVE_POLL
#include <sys/poll.h>
#endif /* HAVE_POLL */

#ifdef HAVE_SELECT
#include <sys/select.h>
#endif /* HAVE_SELECT */

#include <unistd.h> /* close */

#else /* WIN32 */

#include <windows.h> /* winsock (only supports select !!!) */
#define HAVE_SELECT

#define MSG_DONTWAIT 0

#endif /* WIN32 */

#include <errno.h> /* errno */
#include <string.h> /* memcpy */

#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include "common.h"

/*-------------------- Main Module Header --------------------------*/
#include <neuro/nnet/epoll.h>


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("netepoll");

struct EPOLL
{
#ifdef HAVE_SELECT
	EBUF *audit; /* contains ep_event elements */
#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
	int epoll_fd;
	int nfds;
#endif /* HAVE_EPOLL */

	int mem;

	EPOLL_EVENT *epEvents;
};

/* 
 * EPOLL_CTL_ADD
 * EPOLL_CTL_DEL
 * EPOLL_CTL_MOD
 *
 * EPOLLIN
 * EPOLLOUT
 * EPOLLPRI
 * EPOLLERR
 * EPOLLHUP
 * EPOLLET -- specific to epoll
 */

#define MEM_OVERHEAD 20
#define MEM_REALLOC_OVERHEAD 50

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/
#ifdef HAVE_SELECT
/*
 * returns 1 if pipe type [types]
 * is available else 0
 * types : 
 * 0 -- read
 * 1 -- write
 * 2 -- exception
 * 
 */
static int
CheckPipeAvail(int connection, int type, int timeout_sec, int timeout_usec)
{
	fd_set readfds, writefds, exceptfds;
	struct timeval timeout_write;
	int _err = 0;

	/* set how long we retry to see if we connected or not(used with select) 
	 * 
	 * 4 seconds wait/retry time
	 */
	timeout_write.tv_sec = timeout_sec;
	timeout_write.tv_usec = timeout_usec;

	if (type == 0)
	{
		FD_ZERO(&readfds);
		FD_SET(connection, &readfds);	
		_err = select(connection + 1, &readfds, NULL, NULL, &timeout_write);

		return (FD_ISSET(connection, &readfds) ? 1 : 0);
	}

	if (type == 1)
	{
		FD_ZERO(&writefds);
		FD_SET(connection, &writefds);	
		_err = select(connection + 1, NULL, &writefds, NULL, &timeout_write);

		return (FD_ISSET(connection, &writefds) ? 1 : 0);
	}

	if (type == 2)
	{
		FD_ZERO(&exceptfds);
		FD_SET(connection, &exceptfds);	
		_err = select(connection + 1, NULL, NULL, &exceptfds, &timeout_write);

		return (FD_ISSET(connection, &exceptfds) ? 1 : 0);
	}

	return 0;
}


static struct ep_event *
lookupFD(EPOLL *ep, int fd)
{
	struct ep_event *tmp;
	int total = 0;

	if (Neuro_EBufIsEmpty(ep->audit))
		return NULL;

	total = Neuro_GiveEBufCount(ep->audit) + 1;

	while (total-- > 0)
	{
		tmp = Neuro_GiveEBuf(ep->audit, total);

		if (tmp->sock == fd)
		{
			return tmp;
		}
	}

	return NULL;
}

static int
handle_events(EPOLL *ep, int timeout)
{
	int sigmask = 0;
	struct ep_event *event;
	int raised = 0; /* number of events raised */
	int total = 0;

	if (Neuro_EBufIsEmpty(ep->audit))
		return 0;

	total = Neuro_GiveEBufCount(ep->audit) + 1;

	while (total-- > 0)
	{
		event = Neuro_GiveEBuf(ep->audit, total);

		sigmask = 0;

		if (event->events & EPOLLIN || event->events & EPOLLPRI)
		{
			if (CheckPipeAvail(event->sock, 0, 0, timeout))
			{
				if (event->events & EPOLLIN)
					sigmask += EPOLLIN;
				if (event->events & EPOLLPRI)
					sigmask += EPOLLPRI;
			}
		}

		if (event->events & EPOLLOUT)
		{
			if (CheckPipeAvail(event->sock, 1, 0, timeout))
			{
				sigmask += EPOLLOUT;
			}
		}

		if (event->events & EPOLLERR || event->events & EPOLLHUP)
		{
			if (CheckPipeAvail(event->sock, 2, 0, timeout))
			{
				if (event->events & EPOLLERR)
					sigmask += EPOLLERR;
				if (event->events & EPOLLHUP)
					sigmask += EPOLLHUP;
			}
		}

		if (sigmask > 0)
		{
			memcpy(&ep->epEvents[raised], event, sizeof(struct ep_event));

			ep->epEvents[raised].events = sigmask;

			raised++;
		}
	}

	return raised;
}
#endif /* HAVE_SELECT */

/*-------------------- Global Functions ----------------------------*/

int
Epoll_Ctl(EPOLL *ep, int op, int fd, EPOLL_EVENT *event)
{
	int _err = 0;

	if (!ep)
	{
		ERROR("Invalid empty EPOLL argument");

		return -1;
	}

#ifdef HAVE_SELECT
	_err = 0;

#endif /* HAVE_SELECT */


#ifdef HAVE_EPOLL

	TRACE(Neuro_s("Start -- Elems in buffer %d op : %d", ep->nfds, op));

	_err = epoll_ctl(ep->epoll_fd, op, fd, event);

	if (_err == -1)
	{
		ERROR(Neuro_s("epoll_ctl Raised the error %d", errno));

		return _err;
	}

#endif /* HAVE_EPOLL */

	switch (op)
	{
		case EPOLL_CTL_ADD:
		{
#ifdef HAVE_SELECT
			struct ep_event *tmp;
			if (!event)
			{
				ERROR("EPOLL_EVENT is empty");
				return -1;
			}

			Neuro_AllocEBuf(ep->audit, sizeof(struct ep_event*), sizeof(struct ep_event));

			tmp = Neuro_GiveCurEBuf(ep->audit);

			tmp->events = event->events;
			tmp->data = event->data;
			tmp->sock = fd;

			if (!ep->epEvents)
				ep->epEvents = calloc(1, sizeof(struct ep_event));
			else
			{
				int total = 0;

				total = Neuro_GiveEBufCount(ep->audit) + 1;
				ep->epEvents = realloc(ep->epEvents, total * sizeof(struct ep_event));			
			}

#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
			ep->nfds++;
#endif /* HAVE_EPOLL */
		}
		break;

		case EPOLL_CTL_DEL:
		{
#ifdef HAVE_SELECT
			struct ep_event *tmp = NULL;

			tmp = lookupFD(ep, fd);

			if (!tmp)
			{
				WARN("Couldn't find the fd to delete");
				return 0;
			}

			Neuro_SCleanEBuf(ep->audit, tmp);

			if (Neuro_EBufIsEmpty(ep->audit))
			{
				free(ep->epEvents);
				return 0;
			}

			if (ep->epEvents)
			{
				int total = 0;

				total = Neuro_GiveEBufCount(ep->audit) + 1;
				ep->epEvents = realloc(ep->epEvents, total * sizeof(struct ep_event));
			}

#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
			ep->nfds--;
#endif /* HAVE_EPOLL */
		}
		break;

		case EPOLL_CTL_MOD:
		{
#ifdef HAVE_SELECT
			struct ep_event *tmp = NULL;
			if (!event)
			{
				ERROR("EPOLL_EVENT is empty");
				return -1;
			}


			tmp = lookupFD(ep, fd);

			if (!tmp)
			{
				WARN("Couldn't find the elem to modify");
				return 0;
			}

			tmp->events = event->events;
			tmp->data = event->data;
#endif /* HAVE_SELECT */
		}
		break;
	}



#ifdef HAVE_EPOLL
	TRACE(Neuro_s("End -- Elems in buffer %d epEvents 0x%x -- allocate size : %d", ep->nfds, ep->epEvents, ep->nfds * sizeof(EPOLL_EVENT)));

	if (!ep->epEvents)
	{
		ep->epEvents = calloc(MEM_OVERHEAD, sizeof(EPOLL_EVENT));
		ep->mem = MEM_OVERHEAD - 1;
	}
	else
	{
		if (ep->mem > 0)
		{
			ep->mem--;
		}
		else
		{
			ep->epEvents = realloc(ep->epEvents, ep->nfds * sizeof(EPOLL_EVENT) * MEM_REALLOC_OVERHEAD);
			ep->mem = MEM_REALLOC_OVERHEAD - 1;
		}
	}
#endif /* HAVE_EPOLL */

	return _err;
}

/*-------------------- Poll ----------------------------------------*/

/* timeout is in milliseconds */
EPOLL_EVENT *
Epoll_Wait(EPOLL *ep, int timeout, int *nfds)
{
	int _err = 0;

#ifdef HAVE_SELECT

	_err = handle_events(ep, timeout);

	*nfds = _err;

	return ep->epEvents;
#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
	_err = epoll_wait(ep->epoll_fd, ep->epEvents, ep->nfds, timeout);

	if (_err == -1)
	{
		ERROR(Neuro_s("epoll_wait Raised the error %d", errno));

		*nfds = -1;
		return NULL;
	}

	if (_err == 0)
	{
		*nfds = 0;
		return NULL;
	}

	*nfds = _err;
	return ep->epEvents;
#endif /* HAVE_EPOLL */
}

/*-------------------- Constructor Destructor ----------------------*/

EPOLL *
Epoll_Create(int size)
{
	int _err = 0;
	EPOLL *output = NULL;

	output = calloc(1, sizeof(EPOLL));

#ifdef HAVE_SELECT

	_err = 0;

	Neuro_CreateEBuf(&output->audit);

#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
	_err = epoll_create(size);

	if (_err == -1)
	{
		ERROR(Neuro_s("epoll_create raised the error %d", errno));
	}

	output->epoll_fd = _err;

#endif /* HAVE_EPOLL */

	return output;
}

void
Epoll_Destroy(EPOLL *ep)
{
	if (!ep)
		return;

#ifdef HAVE_SELECT
	Neuro_CleanEBuf(&ep->audit);
#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
	close(ep->epoll_fd);
#endif /* HAVE_EPOLL */

	if (ep->epEvents)
		free(ep->epEvents);

	free(ep);
}
