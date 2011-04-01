/* status.c
 * Module : Status
 */

/*-------------------- Extern Headers Including --------------------*/
#include <neuro/NEURO.h>

#include <string.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include "common.h"

/*-------------------- Main Module Header --------------------------*/
#include "status.h"


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("netstatus");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

u32
Status_GetStatus(const Status *sta)
{
	if (!sta)
	{
		NEURO_ERROR("Status missing", NULL);
		return 0;
	}

	return sta->status;
}

char *
Status_GetPacket(const Status *sta)
{
	if (!sta)
	{
		NEURO_ERROR("Status missing", NULL);
		return NULL;
	}

	return sta->packet;
}

int
Status_GetPacketLen(const Status *sta)
{
	if (!sta)
	{
		NEURO_ERROR("Status missing", NULL);
		return 0;
	}

	return sta->packet_len;
}

Slave *
Status_GetSlave(const Status *sta)
{
	if (!sta)
	{
		NEURO_ERROR("Status missing", NULL);
		return 0;
	}

	return sta->connection;
}

void
Status_Move(Status *from, Status *to)
{
	if (!from || !to)
	{
		NEURO_ERROR("Invalid arguments are empty", NULL);

		return;
	}

	if (to->packet)
	{
		NEURO_TRACE("freeing packet %x", to->packet);
		free(to->packet);
	}

	to->status = from->status;
	to->packet = from->packet;
	to->packet_len = from->packet_len;
	to->connection = from->connection;

	from->packet = NULL;
}

/*-------------------- Constructor Destructor ----------------------*/

void
Status_Add(Master *msr, u32 state, char *data, int len, Slave *conn)
{
	Status *tmp;

	Neuro_AllocEBuf(msr->statuses, sizeof(Status*), sizeof(Status));

	tmp = Neuro_GiveCurEBuf(msr->statuses);

	Status_Set(tmp, state, data, len, conn);
}

void
Status_Set(Status *sta, u32 state, char *data, int len, Slave *conn)
{
	if (!sta)
	{
		NEURO_ERROR("Empty Status argument", NULL);
		return;
	}

	sta->status = state;

	if (data == NULL)
	{
		if (sta->packet != NULL)
		{
			NEURO_TRACE("neuting packet %x", sta->packet);

			sta->packet[0] = '\0';
			
			NEURO_TRACE("address after neuted packet %x", sta->packet);

			sta->packet_len = len;
		}
	}
	else
	{
		/*
		if (sta->packet == NULL)
			sta->packet = malloc(len);
		else
			sta->packet = realloc(sta->packet, len);

		memcpy(sta->packet, data, len);
		*/

		if (sta->packet)
		{
			NEURO_TRACE("freeing packet %x", sta->packet);
			free(sta->packet);
		}

		NEURO_TRACE("%s", Neuro_s("Status adding packet address %x previous address %x", data, sta->packet));
		sta->packet = data;

		sta->packet_len = len;
	}

	sta->connection = conn;
}

Status *
Status_Create()
{
	Status *output;

	output = calloc(1, sizeof(Status));

	return output;
}

void
Status_Destroy(Status *sta)
{
	if (!sta)
		return;

	if (sta->packet)
	{

		NEURO_TRACE("freeing packet %x", sta->packet);
		free(sta->packet);
	}

	free(sta);
}
