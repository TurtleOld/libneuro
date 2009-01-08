
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

/* core.c
 * Module : Bitmap_
 *
 * This module actually is a driver to load
 * bitmap file types to memory.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <ebuf.h>
#include <other.h>

#include <graphics.h> /* Neuro_PutPixel */

/*-------------------- Main Module Header --------------------------*/
#include "intern_bitmap.h"

/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("bitmap");

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/

/*-------------------- Static Functions ----------------------------*/

/*-------------------- Global Functions ----------------------------*/

v_object *
Bitmap_LoadBMP(const char *bitmap)
{
	BMP_CTX *ctx = NULL;
	int _err = 0;

	ctx = Bitmap_CreateCTX(bitmap);

	if (ctx == NULL)
	{
		NEURO_WARN("Context creation failed", NULL);
		return NULL;
	}
	
	while (1 != 2)
	{
		_err = Bitmap_Poll(ctx);

		if (_err == 100)
			break;

		if (_err < 0)
		{
			NEURO_WARN("Poll failed...", NULL);
			return NULL;
		}

		/* printf("loading progress : %d\n", _err); */
	}

	return Bitmap_DestroyCTX(ctx);
}

/*-------------------- Poll ----------------------------------------*/

/* returns a percentage of progress */
i8
Bitmap_Poll(BMP_CTX *ctx)
{
	return Bitmap_ProcessGradual(ctx, ctx->cut_size);
}

/*-------------------- Constructor Destructor ----------------------*/

BMP_CTX *
Bitmap_CreateCTX(const char *path)
{
	BMP_CTX *output;

	output = calloc(1, sizeof(BMP_CTX));

	if (output == NULL)
		return NULL;
#ifdef USE_ZLIB
	output->f_bitmap = gzopen(path, "rb"); /* can also be used for non compressed files */
#else /* NOT USE_ZLIB */
	output->f_bitmap = fopen(path, "rb");
#endif /* NOT USE_ZLIB */

	if (output->f_bitmap == NULL)
	{
		free(output);
		return NULL;
	}

	return output;
}

v_object *
Bitmap_DestroyCTX(BMP_CTX *ctx)
{
	v_object *output = NULL;

	if (ctx == NULL)
		return NULL;

	/* loading the image is done so we unlock */
	Neuro_UnlockVObject(ctx->output);

	output = ctx->output;

	free(ctx);

	return output;
}
