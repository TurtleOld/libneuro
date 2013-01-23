/* status.c
 * Module : Status
 */

/*-------------------- Extern Headers Including --------------------*/
#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include "common.h"

#include "lbuf.h"

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
		ERROR("Status missing");
		return 0;
	}

	return sta->status;
}

char *
Status_GetPacket(const Status *sta)
{
	if (!sta)
	{
		ERROR("Status missing");
		return NULL;
	}

	return sta->packet;
}

int
Status_GetPacketLen(const Status *sta)
{
	if (!sta)
	{
		ERROR("Status missing");
		return 0;
	}

	return sta->packet_len;
}

Slave *
Status_GetSlave(const Status *sta)
{
	if (!sta)
	{
		ERROR("Status missing");
		return NULL;
	}

	return sta->connection;
}

void
Status_Move(Status *from, Status *to)
{
	if (!from || !to)
	{
		ERROR("Invalid arguments are empty");

		return;
	}

	if (to->packet)
	{
		TRACE(Neuro_s("freeing packet %x", to->packet));
		free(to->packet);
	}

	to->status = from->status;
	to->packet = from->packet;
	to->packet_len = from->packet_len;
	to->connection = from->connection;

	from->packet = NULL;
}

/* purge all instances of the slave conn from the buffer msr */
void
Status_PurgeSlave(Master *msr, Slave *conn)
{
	Status *tmp;

	if (!msr || !conn)
	{
		ERROR("Invalid arguments are empty");

		return;
	}

	Neuro_ResetLBuf(msr->statuses);
	while  ((tmp = Neuro_GiveNextLBuf(msr->statuses)))
	{
		if (tmp->connection == conn)
		{
			Neuro_SCleanLBuf(msr->statuses, tmp);
		}
	}
}

/*-------------------- Constructor Destructor ----------------------*/

void
Status_AddPriority(Master *msr, u32 state, char *data, int len, Slave *conn)
{
	Status *tmp;
	
	if (!msr->callback)
	{
		Neuro_AllocStartLBuf(msr->statuses, sizeof(Status));

		TRACE(Neuro_s("Priority Status added for slave %x", conn));

		tmp = Neuro_GiveLBuf(msr->statuses);

		Status_Set(tmp, state, data, len, conn);
	}
	else
	{
		int _err = 0;

		Status_Set(msr->status, state, data, len, conn);

		_err = (msr->callback)(msr->status);

		if (_err >= 1)
			Master_SetQuitFlag(msr);
	}
}

void
Status_Add(Master *msr, u32 state, char *data, int len, Slave *conn)
{
	Status *tmp;

	if (!msr->callback)
	{
		Neuro_AllocLBuf(msr->statuses, sizeof(Status));

		tmp = Neuro_GiveCurLBuf(msr->statuses);

		Status_Set(tmp, state, data, len, conn);
	}
	else
	{ 
		int _err = 0;

		Status_Set(msr->status, state, data, len, conn);

		_err = (msr->callback)(msr->status);

		if (_err >= 1)
			Master_SetQuitFlag(msr);
	}
}

void
Status_Set(Status *sta, u32 state, char *data, int len, Slave *conn)
{
	if (!sta)
	{
		ERROR("Empty Status argument");
		return;
	}

	sta->status = state;

	if (data == NULL)
	{
		if (sta->packet != NULL)
		{
			TRACE(Neuro_s("neuting packet %x", sta->packet));

			sta->packet[0] = '\0';
			
			TRACE(Neuro_s("address after neuted packet %x", sta->packet));

			sta->packet_len = len;
		}
	}
	else
	{
		if (sta->packet)
		{
			TRACE(Neuro_s("freeing packet %x", sta->packet));
			free(sta->packet);
		}

		TRACE(Neuro_s("Status adding packet address %x previous address %x", data, sta->packet));
		sta->packet = data;

		sta->packet_len = len;
	}

	sta->connection = conn;
}

Status *
Status_Create(void)
{
	Status *output;

	output = calloc(1, sizeof(Status));

	return output;
}

void
Status_Clear(Status *sta)
{
	if (!sta)
		return;

	if (sta->packet)
	{

		TRACE(Neuro_s("freeing packet %x", sta->packet));
		free(sta->packet);
	}
}

void
Status_Destroy(Status *sta)
{
	if (!sta)
		return;

	Status_Clear(sta);

	free(sta);
}
