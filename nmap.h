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

#include "engine.h"


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
 *  they are used to configure the nodes to be repeated. todo
 */

/* FIXME */
extern int Nmap_AddRoot(char *rootname /* FIXME */);

/* FIXME */
extern int Nmap_Add(char *parent_name, /* FIXME */
		char *child_name, /* FIXME */
		char *content); /* FIXME */

/* FIXME */
extern int Nmap_Del(char *parent_name, /* FIXME */
	       	char *child_name); /* FIXME */

/* FIXME */
extern char *Nmap_GetContent(char *child_name /* FIXME */ );

/* FIXME */
extern void Nmap_Clear();

/* FIXME */
extern void Nmap_PrintData();

/* FIXME */
extern void Nmap_ClearContent();

/* FIXME */
extern NODE_MAP *Nmap_GetData();

/* FIXME */
extern void Nmap_Clean();

#endif /* NMAP_H */
