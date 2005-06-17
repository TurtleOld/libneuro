
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
/*! TODO */
typedef signed char i8;
/*! TODO */
typedef signed short i16;
/*! TODO */
typedef signed int i32;
/*! TODO */
typedef signed long long i64;

/*! \section unsigned 
 * the 1st bit from the left is also used to store data instead of a negative toggle 
 */
/*! TODO */
typedef unsigned char u8;
/*! TODO */
typedef unsigned short u16;
/*! TODO */
typedef unsigned int u32;
/*! TODO */
typedef unsigned long long u64;

/* \section other */

/*! used to contain time data */
typedef signed int t_tick; 


#endif /* __TYPEDEF_H */
