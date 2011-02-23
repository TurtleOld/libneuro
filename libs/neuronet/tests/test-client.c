/* 
 */


#include <stdio.h>
#include <stdlib.h>

#include "../include/network.h"

#define DATA_SIZE 512

static char databuf[DATA_SIZE];

static int
catch_client(CONNECT_DATA *c, const char *data, u32 len)
{
	return 0;
}

static int
catch_server(CONNECT_DATA *c, const char *data, u32 len)
{
	if (data == NULL && len == 0)
	{
		fprintf(stderr, "Client connection success!\n");
		return 0;
	}

	if (len != DATA_SIZE)
	{
		fprintf(stderr, "Error, the size of the packet is not valid : %d != %d", len, DATA_SIZE);
		return 1;
	}

	if (!data)
	{
		fprintf(stderr, "Error, The packet is empty size -> %d\n", len);
		return 1;
	}

	{
		u32 i = len;

		while (i-- > 0)
		{
			/* fprintf(stderr, "[%d] is %d the same as %d\n", 
					i, data[i], databuf[i]);*/

			if (data[i] != databuf[i])
			{
				fprintf(stderr, "Test failed, data sent to server differ\n");
				return 1;
			}
		}

		fprintf(stderr, "Test Successfully passed - %d bytes sent %d recieved\n",
				DATA_SIZE, len);
	}

	return 0;
}

static int
set_server(LISTEN_DATA **server, int port)
{
	*server = NNet_Create(catch_server, TYPE_SERVER);

	return NNet_Listen(*server, port);
}

static int
set_client(LISTEN_DATA **client, int port)
{
	CONNECT_DATA *connection;

	*client = NNet_Create(catch_client, TYPE_CLIENT);

	if (NNet_Connect(*client, "localhost", port, &connection) == 1)
	{
		fprintf(stderr, "unable to connect to server\n");
		return 1;
	}

	if (NNet_Send(connection, &databuf[0], DATA_SIZE))
	{
		fprintf(stderr, "unable to send packet\n");
		return 1;
	}

	return 0;
}

static void
fill_data()
{
	unsigned int i = DATA_SIZE;

	while (i-- > 0)
	{
		databuf[i] = rand() % 254;
	}
}

int main()
{
	LISTEN_DATA *client, *server;
	int port = 90000;
	int port_itr = 10000;
	int _err = 0;

	fill_data();

	NNet_Init();

	NNet_SetDebugFilter("+all");

	while (port_itr-- >= 0)
	{
		if ((_err = set_server(&server, port + port_itr)) == 0)
			break;
		else
			continue;
	}

	if (_err == 1)
	{
		fprintf(stderr, "unable to bind to a port\n");
		return 1;
	}

	if (set_client(&client, port + port_itr) == 1)
	{
		return 1;
	}

	while (NNet_Poll() == 0)
	{
		Neuro_Sleep(5000);
	}

	NNet_Destroy(client);
	NNet_Destroy(server);

	NNet_Clean();

	return 0;
}
