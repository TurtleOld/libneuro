/*    
 * libneuro, a light weight abstraction of high or lower libraries 
 * and toolkit for applications.
 * Copyright (C) 2005-2008 Nicholas Niro, Robert Lemay
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

/* fonts-none.c
 *
 * Abstracts the use of the fonts functionnalities
 * in the driver.
 *
 * This particular file implements empty interface functions.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <stdlib.h>

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>
#include <other.h>
#include <graphics.h>

/*-------------------- Main Module Header --------------------------*/
#include <extlib.h>

/*--------------------      Other       ----------------------------*/
/*-------------------- Global Variables ----------------------------*/
/*-------------------- Static Variables ----------------------------*/
/*-------------------- Static Prototypes ---------------------------*/
/*-------------------- Static Functions ----------------------------*/
/*-------------------- Global Functions ----------------------------*/

v_object *
Lib_RenderUnicode(font_object *ttf, u32 size, u32 character, i16 *x, i16 *y, u32 color, Rectan *src, Rectan *dst)
{
	return NULL;
}

font_object *
Lib_LoadFontFile(const char *fonts_file_path)
{
	return NULL;
}

void
Lib_CleanFont(font_object *font)
{

}

/*-------------------- Constructor Destructor ----------------------*/

int
Lib_FontsInit(void)
{
	return 0;
}

void
Lib_FontsExit(void)
{

}
