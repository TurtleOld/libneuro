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
#include <neuro/nnet/epoll.h>

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
		ERROR("Empty Argument detected");
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
		ERROR(Neuro_s("Epoll_Ctl raised an error %d\n", errno));
		return -1;
	}

	if (timeout_sec < 0)
		timeout_sec = 0;

	if (timeout_usec < 0)
		timeout_usec = 0;

	_err = 0;
	event = Epoll_Wait(fd, (timeout_sec * 1000) + timeout_usec, &_err);

	Epoll_Destroy(fd);

	if (_err >= 0)
		return 1;

	return 0;
}

/* this function processes the data that was received in the input (read) buffer 
 * and calls the parent's callback with the first packet that was received.
 */
int
Util_Buffer_Recv_Data(Slave *slv, char *rbuffer, u32 len)
{
	Client *client;
	FRAGMENT_MASTER *cur = NULL;

	client = slv->cType.client;

	if (!rbuffer)
	{
		/* the packet is not valid */
		WARN("Invalid Packet was received. Suspected corrupt or not using the correct format for packets.");
		return 0;
	}
	else
	{
		if (slv->master->inclpacket_size == 0)
		{
			Neuro_AllocEBuf(client->input, sizeof(FRAGMENT_MASTER*), sizeof(FRAGMENT_MASTER));
			cur = Neuro_GiveCurEBuf(client->input);

			cur->data = rbuffer;
			cur->len = len;
		}
		else /* -information- client->inclpacket_size == 1 */
		{
			FRAGMENT_SLAVE *bufa;
			register u32 i = 0;
			u32 *plen = NULL;
			int amount = 0;
			
			TRACE(Neuro_s("Size header enabled, checking buffer for multiple packets", len));

			if (client->incomplete_data != NULL)
			{
				int oldlen = len;
				TRACE(Neuro_s("Found old incomplete packet -> len %d bytes of %d bytes", 
							client->incomplete_len, ((u32*)client->incomplete_data)[0]));

				/* reallocate memory of incomplte packet
				 * to fit the new buffer */
				client->incomplete_data = realloc(client->incomplete_data, client->incomplete_len + len);
				/* append new buffer to the incomplete packet */
				memcpy(client->incomplete_data + client->incomplete_len, rbuffer, len);
				/* correct len to fit with new data size */
				len += client->incomplete_len;
				
				/* Cleaning the unused buffer */
				free(rbuffer);

				TRACE(Neuro_s("New buffer size %d bytes was %d bytes", len, oldlen));

				/* Let rbuffer point to the new buffer */
				rbuffer = client->incomplete_data;
				client->incomplete_len  = 0;
				client->incomplete_data = NULL;
			}
			plen = (u32*)rbuffer;

			if (*plen == 0)
			{ 
				/* On the first case, the packet is empty, which is really odd.
				 *
				 * On the second case, the packet wasn't received in full so
				 * we just return to give the chance for the application to
				 * recieve it all.
				 */
				WARN("Invalid packet received of wrong length");
				return 0;
			}

			/* Buffer contains one or more full packets 
			 * maybe there is also an incomplete packet
			 */
			if(*plen <= len)
			{
				TRACE("Stream contains one or more full packets... processing... ");
				TRACE(Neuro_s("Buffer size: %d bytes | Packet size: %d bytes", len, *plen));

				Neuro_AllocEBuf(client->input, sizeof(FRAGMENT_MASTER*), sizeof(FRAGMENT_MASTER));

				cur = Neuro_GiveCurEBuf(client->input);
				cur->data = rbuffer;
				cur->len = len;
			}
			else
			{	/* No full packet in the buffer */
				TRACE("Stream contains no full packets... processing incomplete packet...");
				TRACE(Neuro_s("Buffer size: %d bytes | Packet size: %d bytes", len, *plen));
			}
			/* Splitting the buffer to get the single packets */
			while (i < len)
			{
				amount++;
				plen = (u32*)&rbuffer[i];
				i += *plen;
				if(i > len)
				{
					client->incomplete_len = (len + *plen) - i;
					/*NEURO_TRACE("%s", Neuro_s("(%d+%d)-%d = %d", 
								 *plen, len, i, client->incomplete_len)); */

					client->incomplete_data = (char*)calloc(1, sizeof(char) * client->incomplete_len);
					memcpy(client->incomplete_data, plen, client->incomplete_len);

					TRACE(Neuro_s("Detected incomplete Packet!! (%d bytes of %d bytes)", client->incomplete_len, *plen));
				}
				else
				{
					if(amount == 1) /* Create EBuf for Fragments */
						cur->fragmented = Neuro_CreateLBuf();

					TRACE(Neuro_s("Processing packet %d -> Packet size: %d | Buffer size: %d", 
								amount, *plen - sizeof(u32), *plen));

					Neuro_AllocLBuf(cur->fragmented, sizeof(FRAGMENT_SLAVE));

					bufa = Neuro_GiveCurLBuf(cur->fragmented);
					
					bufa->len = plen;
					*bufa->len -= sizeof(u32);

					bufa->data = (char*)plen;

					/* Maybe we want to clear the info in CONNECT_DATA
					 * to be sure that they are set to NULL
					 *
					 * client->incomplete_len  = 0;
					 * client->incomplete_data = NULL;
					 */
				}
			}
			if(client->incomplete_data == NULL)
				TRACE(Neuro_s("Buffer had %d full packets in it", amount));
			else
				TRACE(Neuro_s("Buffer had %d full packets and 1 incomplete packet in it", amount - 1));
		}
	}
	return 0;
}

