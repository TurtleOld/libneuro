
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


#ifndef __TYPEDEF_H
#define __TYPEDEF_H

/*! \file typedef.h
 * \brief typedefs main header 
 * 
 * this header file contains important
 * type definitions (or redefinition) to
 * support better portability and also to
 * make the use of variables easier.
 */

/*! \section signed 
 * the 1st bit to the left is a negative toggle, the number can be negative 
 */
/*! signed char which is 8 bit */
typedef signed char i8;
/*! signed short which is 16 bit */
typedef signed short i16;
/*! signed int which is 32 bit */
typedef signed int i32;
/*! TODO long long is not ansi compliant */
/* typedef signed long long i64; */

/*! \section unsigned 
 * the 1st bit from the left is also used to store data instead of a negative toggle 
 */
/*! TODO */
typedef unsigned char u8;
/*! TODO */
typedef unsigned short u16;
/*! TODO */
typedef unsigned int u32;
/*! TODO long long is not ansi compliant */
/* typedef unsigned long long u64; */

/* \section other */

/*! used to contain time data */
typedef signed int t_tick; 

/*! used to contain rectangle coordinates */
typedef struct Rectan 
{
	i16 x, y, w, h;
}Rectan;

/* video object */
typedef void v_object;

/* font "face" object */
typedef void font_object;

#endif /* __TYPEDEF_H */
