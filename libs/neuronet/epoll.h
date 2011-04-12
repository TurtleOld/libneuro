/* epoll.h */

#ifndef __EPOLL_H
#define __EPOLL_H

#include <global.h>

typedef struct EPOLL EPOLL;

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

#endif /* not WIN32 */

#ifdef HAVE_SELECT
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

typedef struct ep_event EPOLL_EVENT;

struct ep_event
{
	unsigned int events;
	int sock;
	
	union
	{
		void *ptr;
	}data;
};

enum
{
	EPOLL_CTL_ADD = 0,
	EPOLL_CTL_DEL,
	EPOLL_CTL_MOD
};

enum
{
	EPOLLIN = 0,
	EPOLLOUT,
	EPOLLPRI,
	EPOLLERR,
	EPOLLHUP,
	EPOLLET
};	
#endif /* HAVE_SELECT */

#ifdef HAVE_EPOLL
typedef struct epoll_event EPOLL_EVENT;

#endif /* HAVE_EPOLL */

extern int Epoll_Ctl(EPOLL *ep, int op, int fd, EPOLL_EVENT *event);

/* timeout is in milliseconds */
extern EPOLL_EVENT *Epoll_Wait(EPOLL *ep, int timeout, int *nfds);

extern EPOLL *Epoll_Create(int size);
extern void Epoll_Destroy(EPOLL *ep);

#endif /* NOT __EPOLL_H */
