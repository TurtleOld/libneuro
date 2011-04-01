/* util.c
 * Module : Util
 */

/*-------------------- Extern Headers Including --------------------*/
#include <neuro/NEURO.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

#include "common.h"

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

