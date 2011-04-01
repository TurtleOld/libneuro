/* epoll.h */

#ifndef __EPOLL_H
#define __EPOLL_H

#include "common.h"

#ifndef alt
#define alt 1
#endif /* not alt */

typedef struct EPOLL EPOLL;

#if alt
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

#if alt
struct ep_event
{
	unsigned int events;
	int sock;
	
	union
	{
		void *ptr;
	}data;
};
#endif /* alt */

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
#else /* not alt */
typedef struct epoll_event EPOLL_EVENT;

#endif /* not alt */

extern int Epoll_Ctl(EPOLL *ep, int op, int fd, EPOLL_EVENT *event);

/* timeout is in milliseconds */
extern EPOLL_EVENT *Epoll_Wait(EPOLL *ep, int timeout, int *nfds);

extern EPOLL *Epoll_Create(int size);
extern void Epoll_Destroy(EPOLL *ep);

#endif /* NOT __EPOLL_H */
