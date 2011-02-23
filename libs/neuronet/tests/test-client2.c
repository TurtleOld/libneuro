/* 
 *
 * this test will look send packets 2 bytes 
 * up to the DATA_SIZE in iteration to the 
 * power of 2 bytes for each of the loops.
 *
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* memcpy */

#include "../include/network.h"

#define ITERATIONS 2

/* more than 1MB of transfer total*/
#define DATA_SIZE 1048576

static int current_iteration;

static char databuf[DATA_SIZE];

static char recv_buf[DATA_SIZE];


static int lock_client = 0;

static int
catch_client(CONNECT_DATA *c, const char *data, u32 len)
{
	return 0;
}

static int
catch_server(CONNECT_DATA *c, const char *data, u32 len)
{
	char *buf = NULL;
	u32 used_len = len;
	static int saved_itr = 0;
	static int saved_pos = 0;
	int start_pos = 0; /* used to set which emplacement to start from the buffer */

	buf = (char *)data;

	if (buf == NULL && len == 0)
	{
		fprintf(stderr, "Client connection success!\n");
		return 0;
	}

	fprintf(stderr, "Server recieved a packet of size %d for %d or (if > 0) %d\n", len, current_iteration, saved_itr);

	if (len != current_iteration)
	{
		/* fprintf(stderr, "Error, the size of the packet is not valid : %d != %d\n", len, current_iteration); */

		if (saved_itr == 0)
		{
			if (len < current_iteration)
			{
				saved_itr = current_iteration;
				lock_client = 1;
			}
		}

		if (saved_itr > 0)
		{
			int ulen = len;

			if (saved_itr - saved_pos < len)
			{
				ulen = saved_itr - saved_pos;

				/* sets where the next packet starts */
				start_pos = ulen;
			}

			memcpy(&recv_buf[saved_pos], buf, ulen);
			saved_pos += ulen;


			if (saved_pos == saved_itr)
			{
				buf = &recv_buf[0];
				used_len = saved_itr;

				saved_pos = 0;

				lock_client = 0;
			}
			else
				return 0;
		}
	}

	if (!buf)
	{
		fprintf(stderr, "Error, The packet is empty -- size -> %d\n", used_len);
		return 1;
	}

	{
		u32 i = used_len;

		while (i-- > 0)
		{
			/*fprintf(stderr, "[%d] is %d the same as %d\n", 
					i, buf[i], databuf[i]);*/

			if (buf[i] != databuf[i])
			{
				fprintf(stderr, "Test failed, data sent to the server differ\n");
				return 1;
			}
		}

		if (saved_itr > 0)
		{
			fprintf(stderr, "Test Successfully passed - %d bytes sent %d recieved\n",
					saved_itr, used_len);

			saved_itr = 0;
		}
		else
			fprintf(stderr, "Test Successfully passed - %d bytes sent %d recieved\n",
					current_iteration, used_len);

		if (start_pos > 0)
		{
			int temp = start_pos;
			start_pos = 0;

			/* we throw the remaining data to a this exact function */
			return catch_server(c, &data[temp], used_len - temp);
		}
	}

	return 0;
}

static int
set_server(LISTEN_DATA **server, int port)
{
	*server = NNet_Create(catch_server, TYPE_SERVER);

	/* NNet_ServerTogglePacketSize(*server); */

	return NNet_Listen(*server, port);
}

static CONNECT_DATA *
set_client(LISTEN_DATA **client, int port)
{
	CONNECT_DATA *connection;

	*client = NNet_Create(catch_client, TYPE_CLIENT);

	if (NNet_Connect(*client, "localhost", port, &connection) == 1)
	{
		fprintf(stderr, "unable to connect to server\n");
		return NULL;
	}

	/* NNet_ClientTogglePacketSize(connection); */

	return connection;
}

static char *
fill_data(unsigned int i)
{
	while (i-- > 0)
	{
		databuf[i] = rand() % 254;
	}

	return databuf;
}

static int
send_packet(CONNECT_DATA *connection, unsigned int packet_size)
{
	char *data;
	
	data = fill_data(packet_size);

	if (NNet_Send(connection, data, packet_size) == 1)
	/* if (NNet_Send(connection, databuf, packet_size) == 1) */
	{
		fprintf(stderr, "unable to send packet\n");
		return 1;
	}

	return 0;
}

int main()
{
	LISTEN_DATA *client, *server;
	CONNECT_DATA *conn;
	int port = 90000;
	int port_itr = 10000;
	int _err = 0;
	int i = ITERATIONS;

	NNet_Init();


	/* fill_data(DATA_SIZE); */

	NNet_SetDebugFilter("+all");

	while (port_itr-- >= 0)
	{
		if ((_err = set_server(&server, port + port_itr)) == 0)
			break;
		else
			continue;
	}

	fprintf(stderr, "server is up on port %d\n", port + port_itr);

	if (_err == 1)
	{
		fprintf(stderr, "unable to bind to a port\n");
		return 1;
	}

	if ((conn = set_client(&client, port + port_itr)) == NULL)
	{
		return 1;
	}

	while (NNet_Poll() == 0)
	{

		if (i > DATA_SIZE && lock_client == 0)
			break;

		if (i <= DATA_SIZE && lock_client == 0)
		{
			current_iteration = i;
	
			fprintf(stderr, "Sending a packet of length %d to server\n", i);

			if (send_packet(conn, i))
			{
				fprintf(stderr, "Test failure\n");
				return 1;
			}

			i *= ITERATIONS;
		}

		Neuro_Sleep(5000);
	}



	NNet_Destroy(client);
	NNet_Destroy(server);

	NNet_Clean();

	return 0;
}
