
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

/* nmap.h
 * node map for multiple node processing
 */

#ifndef __NMAP_H
#define __NMAP_H

/* this struct will be used to know 
 * how many and by what name 
 * the child nodes will be. 
 * an Example would be : 
 * <root> 
 *  	<parent>
 *  		<foo></foo>
 *  		<bar></bar> 
 *   	</parent>
 * <root>
 * this struct will be used to 
 * implement child nodes like 
 * <foo> and like <bar> to the parent node
 * <parent> and thus being able to duplicate
 * the node <parent> with his childs(create
 * an organised database). 
 * if we didnt use this then we 
 * would be with things like this 
 * <root> 
 * 	<parent>
 *  		<foo></foo>
 *  	</parent>
 *   	<parent>
 *   		<bar></bar> 
 *   	</parent>
 *  <root>
 * which is not really useful in xml
 */

#include "neuro_engine.h"


/*! 
 * the Nmap_ functions are made 
 * to deal with this struct. 
 * Do NOT deal with it directly. 
 */
typedef struct node_map
{
	char *root_name; /*!< the name of the xml root */
	char *parent_name; /*!< the name of the node parent */
	char **child_nodes; /*!< the pointer to a pointer of the childs */
	char **child_content; /*!< the pointer to a pointer to the childs's content */
	unsigned int total_child; /*!< the total child */
}NODE_MAP;



/* set of functions to deal with the struct node_map *n_map variable.
 *  they are used to configure the nodes to be repeated.
 */

/*! change the name of the root of the xml into which the nodes will all be put in*/
extern int Neuro_XMLAddRoot(char *rootname);

/*! add a new node to the repeated nodes and also set the content */
extern int Neuro_XMLAdd(char *parent_name, char *child_name, char *content);

/*! remove a node from the nodes */
extern int Neuro_XMLDel(char *parent_name, char *child_name);

/*! output the content of a node */
extern char *Neuro_XMLGetContent(char *child_name);

/*! clear the nodes */
extern void Neuro_XMLClear();

/*! debug : print the content of the nodes one by one */
extern void Neuro_XMLPrintData();

/*! clear only the content of all the nodes */
extern void Neuro_XMLClearContent();

/*! give the pointer to the struct containing the nodes, content and other */
extern NODE_MAP *Neuro_XMLGetData();


/* used internally */
extern void Nmap_Clean();

#endif /* NMAP_H */
