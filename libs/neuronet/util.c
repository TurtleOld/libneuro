/* util.c
 * Module : Util
 */

/*-------------------- Extern Headers Including --------------------*/
#include <errno.h> /* errno (variable) */

#include <string.h> /* memset */

#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

#include "common.h"
#include "epoll.h"

/*-------------------- Main Module Header --------------------------*/
#include "util.h"


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("netutil");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

/* complement to EBUF, same as Neuro_SCleanEBuf but without it's flaw
 * however, this is not an efficient solve for the problem.
 *
 * we delete the first element and reorder the elements back to their 
 * normal ordering.
 *
 * old clean_element_reorder
 */
void
Util_SCleanEBuf(EBUF *input, void *element)
{
	u32 total = 0;
	u32 i = 0;
	void *temp;
	void *buf;

	if (!input || !element)
	{
		NEURO_ERROR("Empty Argument detected", NULL);
		return;
	}

	if (Neuro_EBufIsEmpty(input))
		return;

	Neuro_SCleanEBuf(input, element);


	total = Neuro_GiveEBufCount(input);

	/* NEURO_TRACE("BEGIN LOOPING BUFFERED PACKETS", NULL); */
	while (i < total)
	{
		buf = Neuro_GiveEBuf(input, i);
		temp = Neuro_GiveEBuf(input, i + 1);

		/* NEURO_TRACE("BUFFERED PACKET LEN %d", buf->len); */
		if (temp == NULL)
			break;

		Neuro_SetEBuf(input, 
				Neuro_GiveEBufAddr(input, i + 1), buf);

		Neuro_SetEBuf(input, 
				Neuro_GiveEBufAddr(input, i), temp);
	
		i++;
	}

}

/*
 * returns 1 if pipe type [types]
 * is available else 0
 * types : 
 * 0 -- read
 * 1 -- write
 * 2 -- exception
 * 
 */
int
Util_CheckPipeAvail(int connection, int type, int timeout_sec, int timeout_usec)
{
	EPOLL *fd;
	EPOLL_EVENT ev;
	int _err = 0;
	EPOLL_EVENT *event;

	memset(&ev, 0, sizeof(EPOLL_EVENT));

	switch (type)
	{
		case 0:
			ev.events = EPOLLIN;
			break;

		case 1:
			ev.events = EPOLLOUT;
			break;

		case 2:
			ev.events = EPOLLERR;
			break;

		default:
			return -1;
			break;
	}

	fd = Epoll_Create(1);

	_err = Epoll_Ctl(fd, EPOLL_CTL_ADD, connection, &ev);

	if (_err == -1)
	{
		NEURO_ERROR("Epoll_Ctl raised an error %d\n", errno);
		return -1;
	}

	if (timeout_sec < 0)
		timeout_sec = 0;

	if (timeout_usec < 0)
		timeout_usec = 0;

	_err = 0;
	event = Epoll_Wait(fd, (timeout_sec * 1000) + timeout_usec, &_err);

	Epoll_Destroy(fd);

	if (_err > 0)
		return 1;

	return 0;
}
