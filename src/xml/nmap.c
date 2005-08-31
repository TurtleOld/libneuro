/* nmap.c 
 *
 * this source file contains the functions
 * to deal with the specification file used 
 * with the functions to add more than one
 * node at a time (a set of nodes to be
 * duplicated with different content).
 *
 * TODO : convert all(almost) the integers returning to pointers and arguments
 * TODO : add checks to all the malloc, calloc and realloc function calls.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nmap.h"
#include "xmltool.h"

/*--- function static prototypes ---*/

static int give_parent_number(char *parent_name);
/* static char *give_parent_name(char *child_name); */
static int give_child_number(char *parent_name, char *child_name);
static int check_node_struct(char *parent_name, char *child_name);
static void clear_nmap();

/*--- static variables ---*/

/* this hold the current node 
 * map structure that was created
 * using the Nmap_Add() function,
 * it is altered using 
 * the Nmap_Edit(), Nmap_Del() and
 * Nmap_Add() functions. 
 */
static NODE_MAP *n_map;

/* carries the root name of n_map.
 * it is assigned by Nmap_AddRoot
 * and it is read by Nmap_Add()
 * when creating the node map
 */
static char *rootnam = NULL;

/* globals */


int 
Neuro_XMLAddRoot(char *rootname)
{
	/*printf("will add the root (%s)\n", rootname); */
	if (!rootname)
	{
		Neuro_XMLSetError(INCORRECT_INPUT);
		return INCORRECT_INPUT;
	}
	if (n_map)
	{
		
		if (!rootnam)
			n_map->root_name = (char*)calloc(1, strlen(rootname));
		else
			n_map->root_name = (char*)realloc(n_map->root_name, strlen(rootname));
		memcpy(n_map->root_name, rootname, strlen(rootname));
	}
	if (!n_map)
	{
		if (!rootnam)
			rootnam = calloc(1, strlen(rootname));
		else
			rootnam = realloc(rootnam, strlen(rootname));
		memcpy(rootnam, rootname, strlen(rootname));
	}
	return 0;
}

/* a debug printing of the current n_map
 * structure.
 */
void 
Neuro_XMLPrintData()
{
	unsigned int a = 0;
	while (a < n_map->total_child)
	{
		printf("child \"%s\" with content \"%s\"\n", n_map->child_nodes[a], n_map->child_content[a]);
		a++;
	}
	return;
}

/* returns the content of the child node . */ 
char *
Neuro_XMLGetContent(char *child_name)
{
	unsigned int a = 0;
	/* printf("getcontent childs total %d\n", n_map->total_child); */
	while (a < n_map->total_child)
	{
		if (n_map->child_nodes[a])
		{
			if (!strcmp(child_name, n_map->child_nodes[a]))
				return (char*)n_map->child_content[a];
		}
		else
		{
			/* if this happens theres something REAL wrong lol */
			printf("---WARNING--- !!// catched a memleak \\\\!! ---WARNING---\n");
		}
		a++;
	}
	return NULL;
}

/* the most important function of this
 * source file.
 * Its use is to render the making of a
 * structure of nodes that will have to be
 * repeated very easy to do. check out the
 * description of this file above for some
 * other informations.
 */
int 
Neuro_XMLAdd(char *parent_name, char *child_name, char *content)
{
	/* error nums : 
	 * 1 if the arguments r empty,
	 * 2 if the child_name is already there,
	 * 3 is that theres a big error internal to this alghorithm
	 */
	int o_checknode;	
	unsigned char new_pnode;
	char *p_name;


	new_pnode = 0;
	p_name = NULL;
	if (!parent_name || !child_name)
	{
		Neuro_XMLSetError(INCORRECT_INPUT);
		return INCORRECT_INPUT;
	}
#if debug == 0
	printf("Asking to Create the Parent (%s) and/or the child (%s)\n", parent_name, child_name);
#endif
	if (!n_map)
	{
		/* printf("creating the new struct n_map\n"); */
		o_checknode = 0;
		new_pnode = 1;
		n_map = (struct node_map*)calloc(1, sizeof(struct node_map));
		if (!rootnam)
			Neuro_XMLAddRoot("_xml_root_");
		else
			rootnam = realloc(rootnam, 0);
	}
	else 
	{
		/* printf("overwriting an existing n_map struct\n"); */
		if ((o_checknode = check_node_struct(parent_name, child_name)) == PARENT_UNEXIST)
		{
			return PARENT_UNEXIST;
		}
	}

	if (o_checknode != 1)
	{	
		/* printf("The child node doesnt exist, everything goes well\n"); */
		int cur_p_node = -1;
		int c_total;

		if (new_pnode == 1)
		{
			n_map->parent_name = (char*)calloc(1, strlen(parent_name));
			memcpy(n_map->parent_name, parent_name, strlen(parent_name));
			if (p_name)
				printf("Adding the node %s into the parent name %s\n", parent_name, n_map->parent_name);
			cur_p_node = 0;
		}
		
		if (cur_p_node == -1)
		{
			/*
			if ((cur_p_node = give_parent_number(parent_name)) == PARENT_UNEXIST)
			{
				printf("couldnt find parent\n");
				xmlt_errno = PARENT_UNEXIST;
				return PARENT_UNEXIST; // shouldnt happen, unless the above coding is broken.
			}
			*/
			cur_p_node = 0;
		}
		c_total = n_map[cur_p_node].total_child;
		if (!n_map[cur_p_node].child_nodes)
		{
			n_map[cur_p_node].child_nodes = (char**)calloc(1, sizeof(char*));
			n_map[cur_p_node].total_child = 0;
			c_total = 0;
		}
		else
			n_map[cur_p_node].child_nodes = (char**)realloc(n_map[cur_p_node].child_nodes, ((1 + c_total) * sizeof(char*)));
		
		n_map[cur_p_node].child_nodes[c_total] = (char*)calloc(1, strlen(child_name));
		strncpy(n_map[cur_p_node].child_nodes[c_total], child_name, strlen(child_name));
		n_map[cur_p_node].total_child++;
		
		

		/* now lets copy the content, alloc the mem the same as the previous ** pointer */
		
		if (!n_map[cur_p_node].child_content) /* virtually always true since the node wasnt there and was added above */
			n_map[cur_p_node].child_content = (char**)calloc(1, sizeof(char*));
		else
			n_map[cur_p_node].child_content = (char**)realloc(n_map[cur_p_node].child_content, ((1 + c_total) * sizeof(char*)));
		/* now lets copy the content, unless its NULL */
		if (content)
		{
			n_map[cur_p_node].child_content[c_total] = (char*)calloc(1, strlen(content));
			strncpy(n_map[cur_p_node].child_content[c_total], content, strlen(content));
		}
		else
		{
			n_map[cur_p_node].child_content[c_total] = NULL; /* to be sure that we dont get a bug with a pointer pointing to some void stuff and doing a seg fault */
		}

	}
	else
	{/* the node exists, so lets edit its content if the input isnt empty */
		/* printf("The child node exists, we will see if we can update its content\n"); */
		if (content)
		{
			int cur_p_node = -1;	
			int cur_c_node = -1;
			
			if ((cur_p_node = give_parent_number(parent_name)) == PARENT_UNEXIST)
			{
				Neuro_XMLSetError(PARENT_UNEXIST);
				return cur_p_node;
			}
			
			if ((cur_c_node = give_child_number(parent_name, child_name)) == CHILD_UNEXIST)
			{
				Neuro_XMLSetError(CHILD_UNEXIST);
				return cur_c_node;
			}
			if (!n_map[cur_p_node].child_content[cur_c_node])
				n_map[cur_p_node].child_content[cur_c_node] = (char*)calloc(1, strlen(content));
			else
			{
				
				if (!strcmp(n_map[cur_p_node].child_content[cur_c_node], content))
				{
					Neuro_XMLSetError(NODE_SAME_CONTENT);
					return NODE_SAME_CONTENT;
					
				}
				
				/* printf("childcont %s content %s\n", n_map[cur_p_node].child_content[cur_c_node], content); */
				memset(n_map[cur_p_node].child_content[cur_c_node], '\0', strlen(n_map[cur_p_node].child_content[cur_c_node]));
				n_map[cur_p_node].child_content[cur_c_node] = (char*)realloc(n_map[cur_p_node].child_content[cur_c_node], strlen(content));
				memset(n_map[cur_p_node].child_content[cur_c_node], '\0', strlen(content));

			}
			strncpy(n_map[cur_p_node].child_content[cur_c_node], content, strlen(content));
		}
		else
		{
			Neuro_XMLSetError(INCORRECT_INPUT);
			return INCORRECT_INPUT;
		}
	}
	return NO_ERROR;
}

/* deletes a single node, in n_map, completely, including
 * content.
 */
int 
Neuro_XMLDel(char *parent_name, char *child_name)
{ /* errors : 1 is that n_map doesnt exist, 2 if the parent doesnt exist, 3 if the child node doesnt exist (if the child node doesnt exist and theres no more nodes, it will delete it) */
	unsigned int todel = 0, loo = 0;
	/* unsigned int totalc; */
	int o_checknode = 0, pnum = 0, cnum = 0; 
	/* int nodes_total = 1; */

	if (!n_map)
		return N_MAP_UNEXIST;
	o_checknode = check_node_struct(parent_name, child_name);
	if ( o_checknode == PARENT_UNEXIST)
		return o_checknode;

	pnum = give_parent_number(parent_name);
	if (pnum == PARENT_UNEXIST)
		return pnum;
		
	if (o_checknode == 1 && n_map[pnum].total_child > 0)
	{
		cnum = give_child_number(parent_name, child_name);
		if (cnum == CHILD_UNEXIST)
			return cnum;
		loo = cnum;
#if old_del_method
		while (loo < n_map[pnum].total_child)
		{
			printf("nmap_del : loo %d\n", loo);
			unsigned int slen = strlen(n_map[pnum].child_nodes[loo + 1]) + 1;
			/* will have to assign the last 
			 * entry to the current pointer(pointer to delete) */	
			/* for the content */
			unsigned int c_slen = strlen(n_map[pnum].child_content[loo + 1]) + 1;

			if (slen != strlen(n_map->child_nodes[loo]))
				n_map[pnum].child_nodes[loo] = (char*)realloc(n_map->child_nodes[loo], slen);
			if (c_slen != strlen(n_map->child_content[loo]))
				n_map[pnum].child_content[loo] = (char*)realloc(n_map->child_content[loo], c_slen);

			/* this is stupid and newb... we only need to set the pointer heh
			 * the worse would be to have to set the pointers of all after the
			 * one to delete. */
			memcpy(n_map[pnum].child_nodes[loo], n_map[pnum].child_nodes[loo + 1], slen);
			memcpy(n_map[pnum].child_content[loo], n_map[pnum].child_content[loo + 1], c_slen);
			
			n_map->child_nodes[loo][slen] = '\0';
			n_map->child_content[loo][c_slen] = '\0';
			if (loo == n_map->total_child - 2)
			{
				printf("freeing last node\n");
				memset(n_map->child_nodes[loo + 1], '\0', slen);
				memset(n_map->child_content[loo + 1], '\0', c_slen);
				free(n_map->child_nodes[loo + 1]);
				free(n_map->child_content[loo + 1]);
				break;
			}
			else if (n_map->total_child == 1) /* freeing the last node in the struct */
			{
				printf("freeing last node and freeing the 2 variables\n");
				memset(n_map->child_nodes[loo], '\0', slen);
				memset(n_map->child_content[loo], '\0', c_slen);
				free(n_map->child_nodes[loo]);
				free(n_map->child_content[loo]);
				free(n_map->child_nodes);
				free(n_map->child_content);
				break;
			}
		
			//node_del_content(parent_name, child_name);

			
			loo++;
		}
#else /* not use_old_method */
		/* ok this is very simple yet it does the same and even better than the above
		 * coding. (using pointers) */
		while (1 != 2)
		{
			todel = give_child_number(parent_name, child_name);
			if (n_map->total_child == 1)
			{  /* we just delete the entry without doing anything else */
				printf("Nmap_del : only one left to del \n");
				free(n_map->child_nodes[todel]);
				free(n_map->child_content[todel]);
				free(n_map->child_nodes);
				free(n_map->child_content);
				n_map->total_child--;
			}
			else if (n_map->total_child == todel)
			{ /* we just delete the entry and realloc the node and content pointers */
				printf("Nmap_del : deleting the last one of the list\n");
				free(n_map->child_nodes[todel]);
				free(n_map->child_content[todel]);
				n_map->total_child--;
				/* realloc the node and content ptp */
				n_map->child_nodes = (char**)realloc(n_map->child_nodes, sizeof(char*) * (n_map->total_child));
				n_map->child_content = (char**)realloc(n_map->child_content, sizeof(char*) * (n_map->total_child));

			}
			else
			{
				printf("Nmap_del : normal deleting part\n");
				/* free the data of the current node and content */
				free(n_map->child_nodes[todel]);
				free(n_map->child_content[todel]);
				/* pass the end content and node to the freed pointers */
				n_map->child_nodes[todel] = NULL;
				n_map->child_content[todel] = NULL;
				n_map->child_nodes[todel] = n_map->child_nodes[n_map->total_child - 1];
				n_map->child_content[todel] = n_map->child_content[n_map->total_child - 1];
				/* substract the nodes and content total */
				n_map->total_child--;
				/* realloc the node and content ptp */
				n_map->child_nodes = (char**)realloc(n_map->child_nodes, sizeof(char*) * (n_map->total_child));
				n_map->child_content = (char**)realloc(n_map->child_content, sizeof(char*) * (n_map->total_child));
			}
			break;
		}
#endif /* not use_old_method */
		loo = 0;

		while (loo < n_map->total_child)
		{
			printf("nmap_del : list %s\n", n_map->child_nodes[loo]);
			loo++;
		}
		/* n_map->total_child--; */

	}
	
	
	/* destroy a parent only, if its empty only
	 TODO : if the child_name is NULL and if the total_child isnt, delete them all.
	 */
	if (child_name == NULL && n_map->total_child == 0)
	{
		/* printf("asked to free the struct n_map completely\n"); */
		free(n_map->root_name);
		free(n_map->parent_name);
		n_map->root_name = NULL;
		n_map->parent_name = NULL;
		/* now done above
		free(n_map->child_nodes);
		free(n_map->child_content);
		*/
		/* n_map = realloc(n_map, 0); */
		free(n_map);
		n_map = NULL;
#if multi_parents
		if (nodes_total > 0)
		{
			if (nodes_total > pnum)
			{
				int c = pnum;
				while (c < nodes_total)
				{
					n_map[c].parent_name = n_map[c + 1].parent_name;
					n_map[c].child_nodes = n_map[c + 1].child_nodes;
					n_map[c].total_child = n_map[c + 1].total_child;
					c++;
				}
			}
			n_map = (struct node_map*)realloc(n_map, ((nodes_total - 1) * sizeof(struct node_map)));
		}
		if (nodes_total == 1)
		{
			n_map = (struct node_map*)realloc(n_map, 0);
		}
		nodes_total--;
#endif /* multi_parents */
	}
	return NO_ERROR;
}

/* this function is to clear the content
 * for every nodes. char **child_content.
 */
void 
Neuro_XMLClearContent()
{
	int a = 0;
	unsigned int b = 0;

	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return;
	}
	
	while (a >= 0)
	{
		b = 0;
		while (b < n_map[a].total_child)
		{
			if (n_map[a].child_content[b])
			{
				memset(n_map[a].child_content[b], '\0', strlen(n_map[a].child_content[b]));
				n_map[a].child_content[b] = realloc(NULL, 0);
			}
			b++;
		}
		a--;
	}
}

/* this is to get the n_map struct
 * from the exterior of this source.
 */
NODE_MAP *
Neuro_XMLGetData()
{
	return (NODE_MAP*)n_map;
}



/*--- static functions ---*/

/* gives the name of the parent(not the root). */
/* commented so the compilation doesnt complain that
 * it isnt used. 
 */
/*
static char *
give_parent_name(char *child_name)
{
	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return NULL;
	}
	else
	{
		unsigned int a = 0;
		unsigned int b = 0;
		while (a < 1)
		{
			b = 0;
			while (b < n_map[a].total_child)
			{
				if (!strcmp(n_map[a].child_nodes[b], child_name))
					return n_map[a].parent_name;
				b++;
			}
			a++;
		}
	}
	return NULL;
}
*/

/* this function is used
 * to output the array number
 * of the parent name string 
 * TODO : return pointer
 */
static int 
give_parent_number(char *parent_name)
{
	unsigned int a = 0;

	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return N_MAP_UNEXIST; 
	}
	else
	{
#if debug == 1
		printf("Will now loop thru the n_map struct to find a certain parent name\n");
#endif
		while (a < 1)
		{
			if (!strcmp(n_map[a].parent_name, parent_name))
				return a;
			a++;
		}
	}
	Neuro_XMLSetError(PARENT_UNEXIST);
	return PARENT_UNEXIST;
}

/* TODO : return pointer */
static int 
give_child_number(char *parent_name, char *child_name)
{
	unsigned int a = 0;
	int pnum = give_parent_number(parent_name);
	unsigned int t_child = n_map[pnum].total_child;

	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return N_MAP_UNEXIST; 
	}
	else
	{
		if (pnum == PARENT_UNEXIST)
		{
			Neuro_XMLSetError(PARENT_UNEXIST);
			return pnum;
		}
		while (a < t_child)
		{
			if (!strcmp(n_map[pnum].child_nodes[a], child_name))
				return a;
			a++;
		}
	}
	Neuro_XMLSetError(CHILD_UNEXIST);
	return CHILD_UNEXIST;		
}

/* this functions is used to know
 * if the current parent_node already 
 * has a node with the name 
 * to put in it. will return 0 if 
 * its not existant and 1 if it is. 
 */
static int 
check_node_struct(char *parent_name, char *child_name)
{
	int p_num;
	unsigned int a = 0;

	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return N_MAP_UNEXIST; 
	}
	else
	{
		if ((p_num = give_parent_number(parent_name)) == PARENT_UNEXIST)
		{
			Neuro_XMLSetError(PARENT_UNEXIST);
			return p_num; /* happens if the parent_name doesnt exist */
		}
		while (a < n_map[p_num].total_child)
		{
			if (!strcmp(child_name, n_map[p_num].child_nodes[a]))
				return CHILD_EXISTS;
			a++;
		}
	}
	Neuro_XMLSetError(CHILD_UNEXIST);
	return CHILD_UNEXIST;
}

/* this function calls Nmap_Del() for
 * every elements of n_map until
 * it is completely empty.
 */
static void 
clear_nmap()
{
	int a = 1 - 1;

	if (!n_map)
	{
		Neuro_XMLSetError(N_MAP_UNEXIST);
		return;
	}
	
	while (a >= 0)
	{
		while (n_map->total_child != 0)
			Neuro_XMLDel(n_map->parent_name, n_map[a].child_nodes[0]);
		Neuro_XMLDel(n_map->parent_name, NULL);
		a--;
	}
}


/*--- allocators/deallocators ---*/

void 
Nmap_Clean()
{
	clear_nmap();
	return;
}


