
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

NEURO_MODULE_CHANNEL("bitmap/io");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

/* returns 0 on success and puts the data in *output
 * when 1 on error don't touch output
 */
int
fpdata8(nFILE *input, u8 *output)
{
	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

#if USE_ZLIB 1
	*output = gzgetc(input);
#else /* NOT USE_ZLIB */
	*output = fgetc(input);
#endif /* NOT USE_ZLIB */


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
	
	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

	if (IsLittleEndian())
	{
#if USE_ZLIB 1
		feed[0] = gzgetc(input);
		feed[1] = gzgetc(input);
#else /* NOT USE_ZLIB */
		feed[0] = fgetc(input);
		feed[1] = fgetc(input);
#endif /* NOT USE_ZLIB */
	}
	else
	{
		feed[1] = gzgetc(input);
		feed[0] = gzgetc(input);
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
	
	
	if (input == NULL || output == NULL)
	{
		NEURO_WARN("Arguments are NULL", NULL);
		return 1;
	}

	if (IsLittleEndian())
	{
#if USE_ZLIB 1
		feed[0] = gzgetc(input);
		feed[1] = gzgetc(input);
		feed[2] = gzgetc(input);
		feed[3] = gzgetc(input);
#else /* NOT USE_ZLIB */
		feed[0] = fgetc(input);
		feed[1] = fgetc(input);
		feed[2] = fgetc(input);
		feed[3] = fgetc(input);
#endif /* NOT USE_ZLIB */
	}
	else
	{
		feed[3] = gzgetc(input);
		feed[2] = gzgetc(input);
		feed[1] = gzgetc(input);
		feed[0] = gzgetc(input);
	}

	buf = (u32*)&feed;
	
	*output = *buf;

	return 0;
}
