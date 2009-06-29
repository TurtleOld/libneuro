
/*    
 * libneuro, a light weight abstraction of high or lower libraries 
 * and toolkit for applications.
 * Copyright (C) 2005-2006  Nicholas Niro, Robert Lemay
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

/* io.c
 *
 * Basic input and output functions.
 */

/*-------------------- Extern Headers Including --------------------*/

#include <stdlib.h>
#include <stdio.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/
#include "intern_bitmap.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("bitmap");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

/*-------------------- Static Functions ----------------------------*/

static int
Ngetc(nFILE *input)
{
#ifdef USE_ZLIB
	i32 ret = 0;
	/*u8 data = 0;
	
	ret = gzread(input, &data, 1); */

	ret = gzgetc(input);

	if (ret < 0)
	{
		int errnum;
		const char *error;

		if (gzeof(input) == 0)
		{
			error = gzerror(input, &errnum);
			NEURO_ERROR("ZLIB returned an error -- %s", Neuro_s("%s - errnum %d (returned value %d)", error, errnum, ret));
			return 0;
		}
		else
		{
			NEURO_TRACE("file is eof", ret);
			return -1;
		}
	}

	/*
	u32 len = gzread(input, *output, sizeof(u32));
	if (len < 1)
		return 1;
	return 0;

	if (data)
		ret = (u8)data;*/

	return ret;


#else /* NOT USE_ZLIB */
	return fgetc(input);
#endif /* NOT USE_ZLIB */
}

/*-------------------- Global Functions ----------------------------*/

/* returns 0 on success and puts the data in *output
 * when 1 on error don't touch output
 */
int
fpdata8(nFILE *input, u8 *output)
{
	int ret = 0;

	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

	ret = Ngetc(input);


	if (ret < 0)
		return -1;

	*output = ret;

	return 0;
}

/* returns 0 on success and puts the data in *output
 * when 1 on error don't touch output
 */
int
fpdata16(nFILE *input, u16 *output)
{
	u8 feed[2];
	u16 *buf;
	int ret[2];

	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

	ret[0] = Ngetc(input);
	ret[1] = Ngetc(input);


	if (ret[0] < 0 || ret[1] < 0)
		return -1;

	if (IsLittleEndian())
	{
		feed[0] = ret[0];
		feed[1] = ret[1];
	}
	else
	{
		feed[1] = ret[0];
		feed[0] = ret[1];
	}

	buf = (u16*)&feed;
	
	*output = *buf;

	return 0;
}

/* returns 0 on success and puts the data in *output
 * when 1 on error don't touch output
 */
int
fpdata32(nFILE *input, u32 *output)
{
	/* register int feed; */
	u8 feed[4];
	u32 *buf;
	int ret[4];

	
	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

	ret[0] = Ngetc(input);
	ret[1] = Ngetc(input);
	ret[2] = Ngetc(input);
	ret[3] = Ngetc(input);


	if (ret[0] < 0 || ret[1] < 0 || ret[2] < 0 || ret[3] < 0)
		return -1;

	if (IsLittleEndian())
	{	
		feed[0] = ret[0];
		feed[1] = ret[1];
		feed[2] = ret[2];
		feed[3] = ret[3];
	}
	else
	{
		feed[3] = ret[0];
		feed[2] = ret[1];
		feed[1] = ret[2];
		feed[0] = ret[3];
	}

	buf = (u32*)&feed;
	
	*output = *buf;

	return 0;
}
