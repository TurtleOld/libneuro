/* epoll.h */

#ifndef __EPOLL_H
#define __EPOLL_H

#include "common.h"

typedef struct EPOLL EPOLL;

#if WIN32
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
#else /* not WIN32 */
typedef struct epoll_event EPOLL_EVENT;

#endif /* not WIN32 */

extern int Epoll_Ctl(EPOLL *ep, int op, int fd, EPOLL_EVENT *event);

/* timeout is in milliseconds */
extern EPOLL_EVENT *Epoll_Wait(EPOLL *ep, int timeout, int *nfds);

extern EPOLL *Epoll_Create(int size);
extern void Epoll_Destroy(EPOLL *ep);

#endif /* NOT __EPOLL_H */
