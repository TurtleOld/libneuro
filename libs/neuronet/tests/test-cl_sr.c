/* 
 */


#include <stdio.h>
#include <stdlib.h>

#include "../include/network.h"

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
		return 1;

	return 0;
}

int main()
{
	LISTEN_DATA *client, *server;
	int port = 90000;
	int port_itr = 10000;
	int _err = 0;

	NNet_Init();

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
		fprintf(stderr, "unable to connect to server\n");
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
