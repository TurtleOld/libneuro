
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/network.h"
#include "../include/packet.h"

static Packet *pkt;
static char *message = "This seems to work";

void
printCore(char *buf, u32 len)
{
	return;

	printf("Printing buffer core : \"");

	while (len-- > 0)
	{
		printf(Neuro_s("%c", buf[0]));

		buf++;
	}

	printf("\"\n");
}

int test_push()
{
	pkt = Packet_Create();

	Packet_Push64(pkt, 800.20);

	Packet_Push32(pkt, 45);

	Packet_Push16(pkt, 99);

	Packet_Push8(pkt, 3);

	Packet_PushString(pkt, strlen(message) + 1, message);

	printCore(Packet_GetBuffer(pkt), Packet_GetLen(pkt));
	/* printf("packed buffer -> %s\n", Packet_GetBuffer(pkt)); */

	return 0;
}

int test_pop()
{
	int _err = 0;
	char *data = NULL;
	double a = 0;
	i32 b = 0;
	i16 c = 0;
	i8 d = 0;

	if ((a = Packet_Pop64(pkt)) != 800.20)
	{
		printf(Neuro_s("Double gave %f instead of 800.20\n", a));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt), Packet_GetLen(pkt));

	if ((b = Packet_Pop32(pkt)) != 45)
	{
		printf(Neuro_s("i32 gave %d instead of 45\n", b));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt), Packet_GetLen(pkt));

	if ((c = Packet_Pop16(pkt)) != 99)
	{
		printf(Neuro_s("i16 gave %d instead of 99\n", c));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt), Packet_GetLen(pkt));

	if ((d = Packet_Pop8(pkt)) != 3)
	{
		printf(Neuro_s("i8 gave %d instead of 3\n", d));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt), Packet_GetLen(pkt));

	/* data = Packet_PopData(pkt, strlen(message)); */
	data = Packet_PopString(pkt);
	if (data)
	{
		if (strcmp(message, data))
		{
			printf(Neuro_s("message is \"%s\" instead of \"%s\"", data, message));
			_err = 1;
		}
	}
	else
	{
		printf(Neuro_s("message is NULL instead of \"%s\"\n", message));
		_err = 1;
	}

	Packet_Destroy(pkt);

	if (_err == 0)
	{
		printf("Packet test 1 and 2 success!\n");
	}
	else
	{
		printf("Packet test 1 and 2 failure\n");
	}

	return _err;
}

int
test_pop2()
{
	struct 
	{
		int a;
		double b;
		short c;
		char d;
	} dta;
	int _err = 0;
	Packet *pkt2;
	char *marshall;
	double a = 0;
	i32 b = 0;
	i16 c = 0;
	i8 d = 0;

	dta.a = 55;
	dta.b = 5971.1234;
	dta.c = 6200;
	dta.d = 13;

	marshall = malloc(sizeof(dta));

	memcpy(marshall, &dta, sizeof(dta));
	pkt2 = Packet_Set(marshall, sizeof(dta));

	if ((b = Packet_Pop32(pkt2)) != 55)
	{
		printf(Neuro_s("i32 gave %d instead of 55\n", b));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt2), Packet_GetLen(pkt2));

	if ((a = Packet_Pop64(pkt2)) != 5971.1234)
	{
		printf(Neuro_s("Double gave %f instead of 5971.1234\n", a));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt2), Packet_GetLen(pkt2));

	if ((c = Packet_Pop16(pkt2)) != 6200)
	{
		printf(Neuro_s("i16 gave %d instead of 6200\n", c));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt2), Packet_GetLen(pkt2));

	if ((d = Packet_Pop8(pkt2)) != 13)
	{
		printf(Neuro_s("i8 gave %d instead of 13\n", d));
		_err = 1;
	}
	printCore(Packet_GetBuffer(pkt2), Packet_GetLen(pkt2));

	Packet_Destroy(pkt2);

	if (_err == 0)
	{
		printf("Packet test 3 success!\n");
	}
	else
	{
		printf("Packet test 3 failure\n");
	}

	return _err;
}

int main ()
{
	int _err = 0;

	NNet_SetDebugFilter("all+all");

	_err = test_push();

	if (! _err)
		_err = test_pop();

	if (! _err)
		_err = test_pop2();

	Neuro_Quit();

	return _err;
}

