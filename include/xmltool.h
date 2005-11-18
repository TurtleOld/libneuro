
/*! \file xmltool.h 
 * \brief the xmltools core main header.
 * 
 * This file contains the main functions used for xml handling.
 */

#ifndef __XMLTOOL_H
#define __XMLTOOL_H

#include <engine.h>


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
 * @param [in] this is for the user to add extra informations (like the function name it was called from).
 */
extern void Neuro_XMLPerror(char *message);

/*! Internal, do not use. The name is self explanatory anyway.
 * @param [in] the error number
 */
void Neuro_XMLSetError(int source);

/*! Internal use only 
 * @param [in] destination
 */
void Neuro_XMLGetError(int *destination);

/* set of functions to Input, 
 * Output, Edit and Delete to the xml file. 
 * They are used to add, edit, read 
 * and remove only 1 item at a time.
 */

/*! Write a single value to a node 
 * @param [in] the xml file to use
 * @param [in] name of the parent node of the node to be read
 * @param [in] the name of the node to be created
 * @param [in] the data to add to the node
 */
extern int Neuro_WriteToXml(char *filename, char *parent_name, char *node_name, char *node_info); 	

/*! Read a single value of a node 
 * @param [in] the xml file to use
 * @param [in] the name of the parent node of the node to be read
 * @param [in] the name of the node to be read
 */
extern char *Neuro_ReadFromXml(char *filename, char *parent_name, char *child_name); 		

/*! Modify a single value of a node 
 * @param [in] the xml file to use
 * @param [in] the name of the parent node of the node to be edited
 * @param [in] the name of the node to be edited
 * @param [in] the data that will be newly put into the node
 */
extern int Neuro_EditToXml(char *filename, char *parent_name, char *node_name, char *node_info);

/*! Delete a single node 
 * @param [in] the xml file to use
 * @param [in] the name of the parent node of the node to be edited
 * @param [in] the name of the node to be deleted
 */
extern int Neuro_RemoveFromXml(char *filename, char *parent_name, char *child_name);


/* 
 * set of functions to Input, 
 * Output, Edit and Delete to the xml file. 
 * They are used to add, edit, read and remove 
 * as many nodes as there is in the n_map struct.
 * 
 * FIXME : the Edit and Delete functions aren't done yet
 */

/*! This function actually checks and parse the DtD of an xml file.
 * @param [in] the xml file to use
 */
extern int Neuro_GetXmlDesc(char *filename);

/*! This function is to dump the data loaded, with Nmap_Add, in the xml file. 
 * @param [in] the xml file to use
 */
extern int Neuro_MultiAddXml(char *filename);

/*! Reads a file that has a repetitive number of parent nodes (as set in the DtD). 
 * @param [in] the xml file to use
 * @param [in] the name of the parent node of the node to be read
 * @param [in] the name of the node to be read
 * @param [in] the content that the child must have to be recognised and read
 */
extern struct node_map *Neuro_MultiReadXml(char *filename, char *parent_node, char *node_name, char *content);

/*! clean up Xmltools. Internally used */
extern void Xxmltool_Clean();

#endif /* __XMLTOOL_H */
