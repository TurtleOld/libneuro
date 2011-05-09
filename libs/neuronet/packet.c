/* packet.c
 * Module : Packet_
 */

/*-------------------- Extern Headers Including --------------------*/

#include <neuro/NEURO.h>
#include <string.h> /* memcpy() */

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/

#include "neuro/nnet/packet.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("packet");

struct Packet
{
        u32 len; /* length of the memory used */
	u32 mem; /* available free memory */
	u32 buffer_pos; /* used for pop actions */
        char *buffer; /* the actual buffer */
};

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

/*-------------------- Static Functions ----------------------------*/

static int
boundStrlen(char *buffer, unsigned int len)
{
	int t = 0;
	char *tmp = buffer;

	while (t < len)
	{
		if (tmp[t] == '\0')
		{
			return (t + 1);
		}

		t++;
	}

	return 0;
}

/* return the data from the buffer
 * and remove the data from it.
 */
static char *
pop_data(Packet *pkt, unsigned int len, unsigned int stringType)
{
	char *output = NULL;

	if (!pkt)
	{
		NEURO_WARN("Packet type is NULL", NULL);
		return NULL;
	}

	if (!pkt->buffer)
	{
		NEURO_WARN("buffer pointer is NULL", NULL);
		return NULL;
	}

	if (pkt->len <= 0)
	{
		NEURO_WARN("buffer is empty", NULL);
		return NULL;
	}

	if (len <= 0 && stringType == 0)
	{
		NEURO_WARN("input lenght is invalid", NULL);
		return NULL;
	}

	if (stringType == 1)
	{
		len = boundStrlen(&pkt->buffer[pkt->buffer_pos], pkt->len);
	
		if (len == 0)
			return NULL;
	}

	output = &pkt->buffer[pkt->buffer_pos];

	pkt->mem += len;
	pkt->len -= len;
	pkt->buffer_pos += len;

	return output;
}

static int
push_data(Packet *pkt, unsigned int len, const char *data)
{
	if (!pkt)
	{
		NEURO_WARN("Packet type is NULL", NULL);
		return 1;
	}

	if (!data)
	{
		NEURO_WARN("Input data pointer is NULL", NULL);
		return 1;
	}

	if (len == 0)
	{
		NEURO_WARN("data lenght is 0", NULL);
		return 1;
	}

	if (len > pkt->mem)
	{
		/* overhead is set to 100 */
		pkt->buffer = realloc(pkt->buffer, pkt->len + pkt->mem + (len - pkt->mem) + 100);
		pkt->mem += (len - pkt->mem) + 100;
	}

	/*NEURO_TRACE("%s", Neuro_s("first byte %d - pkt->len %d, pkt->mem %d, data first byte %d, data len %d",
				pkt->buffer[0], pkt->len, pkt->mem, data[0], len));*/

	memcpy(&pkt->buffer[pkt->len], data, len);

	pkt->mem -= len;
	pkt->len += len;

	return 0;
}

/*-------------------- Global Functions ----------------------------*/

int
Packet_Push64(Packet *pkt, double num)
{
	return push_data(pkt, sizeof(double), (char *)&num);
}

int
Packet_Push32(Packet *pkt, int num)
{
	return push_data(pkt, sizeof(int), (char *)&num);
}

int
Packet_Push16(Packet *pkt, short num)
{
	return push_data(pkt, sizeof(short), (char *)&num);
}

int
Packet_Push8(Packet *pkt, char num)
{
	return push_data(pkt, sizeof(char), (char*)&num);
}

int
Packet_PushStruct(Packet *pkt, unsigned int len, const void *stru)
{
	return push_data(pkt, len, (const char*)stru);
}

int
Packet_PushString(Packet *pkt, unsigned int len, const char *string)
{
	return push_data(pkt, len, string);
}

char
Packet_Pop8(Packet *pkt)
{
	i8 output = 0;
	char *buf = NULL;

	buf = pop_data(pkt, sizeof(char), 0);

	memcpy(&output, buf, sizeof(char));

	return output;
}

short
Packet_Pop16(Packet *pkt)
{
	i16 output = 0;
	char *buf = NULL;

	buf = pop_data(pkt, sizeof(short), 0);

	memcpy(&output, buf, sizeof(short));

	return output;
}

int
Packet_Pop32(Packet *pkt)
{
	i32 output = 0;
	char *buf = NULL;

	buf = pop_data(pkt, sizeof(int), 0);

	memcpy(&output, buf, sizeof(int));

	return output;
}

double
Packet_Pop64(Packet *pkt)
{
	double output = 0;
	char *buf = NULL;

	buf = pop_data(pkt, sizeof(double), 0);

	memcpy(&output, buf, sizeof(double));

	return output;
}

char *
Packet_PopString(Packet *pkt)
{
	return pop_data(pkt, 0, 1);
}

char *
Packet_PopData(Packet *pkt, unsigned int len)
{
	return pop_data(pkt, len, 0);
}

int
Packet_GetLen(const Packet *pkt)
{
	if (!pkt)
	{
		NEURO_WARN("Packet type is NULL", NULL);
		return -1;
	}

	return pkt->len;
}

char *
Packet_GetBuffer(const Packet *pkt)
{
	if (!pkt)
	{
		NEURO_WARN("Packet type is NULL", NULL);
		return NULL;
	}

	return &pkt->buffer[pkt->buffer_pos];
}

/*-------------------- Constructor Destructor ----------------------*/

Packet *
Packet_Set(char *buffer, unsigned int len)
{
	Packet *new = NULL;

	if (!buffer || len <= 0)
		return NULL;

	new = malloc(sizeof(Packet));

	new->buffer = malloc(len);

	memcpy(new->buffer, buffer, len);

	new->buffer_pos = 0;
	new->len = len;
	new->mem = 0;

	return new;
}

int
Packet_Set2(Packet *pkt, char *buffer, unsigned int len)
{
	if (!pkt)
		return 1;

	Packet_Reset(pkt);

	return push_data(pkt, len, buffer);
}

void
Packet_Reset(Packet *pkt)
{
	if (!pkt)
	{
		NEURO_WARN("Packet type is NULL", NULL);
		return;
	}
	
	memset(pkt->buffer, 0, pkt->len);

	pkt->mem += pkt->len;
	pkt->buffer_pos = 0;
	pkt->len = 0;

}

Packet *
Packet_Create()
{
	Packet *output;

	output = calloc(1, sizeof(Packet));


	/* we allocate 50 bytes of overhead to accelerate the process */
	output->buffer = calloc(1, 50);
	/* we keep that amount of available memory in this variable */
	output->mem = 50;


	return output;
}

void
Packet_Destroy(Packet *pkt)
{
	if (pkt)
	{
		if (pkt->buffer)
			free(pkt->buffer);

		free(pkt);
	}
}
