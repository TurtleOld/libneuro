
/*! \file xmltool.h 
 * \brief the xmltools core main header.
 * 
 * This file contains the main functions used for xml handling.
 */

#ifndef __XMLTOOL_H
#define __XMLTOOL_H

#include "engine.h"


/*!
 * Sets the error types returned by the functions 
 * */
enum returns
{
	N_MAP_UNEXIST= -4,
	PARENT_UNEXIST= -3,
	CHILD_UNEXIST= -2,
	INCORRECT_INPUT=  -1,
	NO_ERROR= 0,
	CHILD_EXISTS= 1,
	PARENT_EXISTS= 2,
	FILE_UNEXIST= 3,
	NODE_SAME_CONTENT= 4
};

/*! prints the error description. 
 * This functions has to be used to 
 * be given a short description 
 * of an eventual error. 
 */
extern void Xmltool_Perror(
		/*! this is for the user to add extra 
		 * informations (like the function name it was called from).
		 */
		char *message );

/*! Internal, do not use. The name is self explanatory anyway.*/
void Xmltool_SetError(
		/*! the error number */
		int source);

/*! FIXME */
void Xmltool_GetError(
		/*! FIXME */
		int *destination);

/* set of functions to Input, 
 * Output, Edit and Delete to the xml file. 
 * They are used to add, edit, read 
 * and remove only 1 item at a time.
 */

/*! Write a single value to a node */
extern int Xmltool_WriteToXml(
			/*! xml file */
			char *filename, 	
			/*! parent node of the node */
			char *parent_name, 	
			/*! node to add */
			char *node_name, 
			/*! data to add to the node */ 
			char *node_info); 	

/*! Read a single value of a node */
extern char *Xmltool_ReadFromXml(
			   /*! the xml file */
			   char *filename, 	
			   /*! parent node of the node */
			   char *parent_name, 	
		   	   /*! node to read */
			   char *child_name); 		

/*! Modify a single value of a node */
extern int Xmltool_EditToXml(
		/*! the xml file */
		char *filename, 	
		/*! parent node of the node */ 
		char *parent_name, 		
		/*! node to edit */
		char *node_name, 		
		/*! New data to add to the node */
		char *node_info);

/*! Delete a single node */
extern int Xmltool_RemoveFromXml(
		/*! the xml file */
		char *filename, 	
		/*! parent node of the node */
		char *parent_name, 			
		/*! the node to delete */
		char *child_name);


/* 
 * set of functions to Input, 
 * Output, Edit and Delete to the xml file. 
 * They are used to add, edit, read and remove 
 * as many nodes as there is in the n_map struct.
 * 
 * FIXME : the Edit and Delete functions aren't done yet
 */

/*! This function actually checks and parse the DtD of an xml file.*/
extern int Xmltool_GetXmlDesc(
		/*! file to read from */
		char *filename );

/*! This function is to dump the data loaded, with Nmap_Add, in the xml file. */
extern int Xmltool_MAddXml(
		/*! file to write to */
		char *filename );

/*! Reads a file that has a repetitive number of parent nodes (as set in the DtD). */
extern struct node_map *Xmltool_MReadXml(
		/*! the xml file. */
		char *filename,				
		/*! parent node of the node. */
		char *parent_node, 			
		/*! node that has a unique data(compared to the other nodes) */
		char *node_name, 			
		/*! content that the child 
		 * must have to be recognised 
		 */
		char *content);

/*! clean up Xmltools. Call this before the program closes, at least. */
extern void Xmltool_Clean();

#endif /* __XMLTOOL_H */
