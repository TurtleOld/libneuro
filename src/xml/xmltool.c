
/*!
 * xmltool.c
 * the basic xml functions for the project xmltools are here
 */

/* those are to set the errors.
 * to be used in the functions. 
 */
#define error_handle(error) 	xmlt_errno = error; return error
#define error_handle2(error) 	xmlt_errno = error; return NULL

/*--- extern ---*/

#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xmlwriter.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>

/*--- local ---*/

#include "xmltool.h"
#include "nmap.h"


/*--- static variables ---*/

/* Used by Xmltool_Perror to have 
 * a small sentence explaining what 
 * is the error. 
 */
static const char *xmltools_errors_string[] =
{
	"the struct n_map doesnt exist yet, first use node_add()", 
	"the parent node doesnt exist", 
	"the child node doesnt exist", 
	"the input argument isnt supported", 
	"success", 
	"the child exists",  /* the user will never get this error */
	"the parent exists",  /* the user will never get this error */
	"the file doesnt exist",
	"the corresponding node already has the given content"
};

/* keeps the current error signaled.
 * It is passed to the argument
 * in Xmltool_GetError or Xmltool_Perror.
 * To change the variable
 * from the exterior, use Xmltool_SetError. 
 */
static int xmlt_errno;


/*--- function static prototypes ---*/

static xmlBufferPtr createdtdheader(xmlDocPtr doc, char **buf1);
static void freedtdheader(xmlBufferPtr buf, char **buf1);
static xmlNodePtr *findNode(xmlDocPtr *doc, char *parent_name, char *child_name, char *content);



/*--- defines ---*/

/* this was to set which 
 * version of the allocations is to be
 * used in the dtd making process.
 */
#define usexmlbuffer 0


/*--- Externally accessable functions ---*/


/* prints the error description */
void 
Neuro_XMLGetError(int *destination)
{
	*destination = xmlt_errno;
}

void 
Neuro_XMLSetError(int source)
{
	xmlt_errno = source;
}

void 
Neuro_XMLPerror(char *message)
{
	int err = xmlt_errno;
	err += 4;
	printf("%s: %s\n", message, xmltools_errors_string[err]);
	return;
}

int 
Neuro_XMLWriteToXml(char *filename, char *parent_name, char *node_name, char *node_info)
{
	xmlDocPtr doc;
	xmlNodePtr cur;
	
	/* int pnum = 0;
	pnum = give_parent_number(parent_name); */
	if (open(filename, 0644) == -1) // check to see if the file exist
	{
		// the file doesnt exist, so we start it up
		doc = xmlNewDoc(NULL);
		cur = xmlNewDocRawNode(doc, NULL, (const xmlChar*)parent_name, NULL);
		xmlDocSetRootElement(doc, cur);
		xmlNewTextChild(cur, NULL, (const xmlChar*)node_name, (const xmlChar*)node_info);
		/*
		cur = xmlNewChild(cur, NULL, (const xmlChar*)parent_name, (const xmlChar*)NULL);
		
		if (pnum == -1)
		{
			//xmlAddChild(cur, xmlNewChild(cur, NULL, (const xmlChar*)node_name, NULL));
			xmlNewTextChild(cur, NULL, (const xmlChar*)node_name, (const xmlChar*)node_info);
		}
		else
			if (pnum >= 0)
			{		
				unsigned int a = 0;
				printf("debug 1 : %d\n", node_add(parent_name, node_name));
				while (a < n_map[pnum].total_child - 1)
				{
					xmlNewTextChild(cur, NULL, (const xmlChar*)n_map[pnum].child_nodes[a], (strcmp(node_name, n_map[pnum].child_nodes[a]) ? NULL  : (const xmlChar*)node_info));
					
					a++;
				}	
			}
		*/
		xmlSaveFormatFile(filename, doc, 1);
		xmlFreeDoc(doc);
	}
	else
	{
		xmlKeepBlanksDefault(0);
		doc = xmlParseFile(filename);
		/* cur = xmlDocGetRootElement(doc); */
	
		cur = (xmlNodePtr)findNode((xmlDocPtr*)&doc, parent_name, NULL, NULL);
		if (!cur)
			cur = xmlNewChild((xmlNodePtr)xmlDocGetRootElement(doc), NULL, (const xmlChar*)parent_name, NULL);
		xmlNewChild(cur, NULL, (const xmlChar*)node_name, (const xmlChar*)node_info);
		
		/* the pnum part is to the n_map thing. Will have to use it in the next set of function and remove it from this one, this one will only be used to put a single node at a time(or 2 if the parent doesnt exist). */
		/*
		if (pnum == -1)
		{
			xmlAddChild(cur, xmlNewChild(cur, NULL, (const xmlChar*)node_name, NULL));
			xmlNewTextChild(cur, NULL, (const xmlChar*)node_name, (const xmlChar*)node_info);
		}
		else
			if (pnum >= 0)
			{		
				unsigned int a = 0;
				printf("debug 1 : %d\n", node_add(parent_name, node_name));
				while (a < n_map[pnum].total_child - 1)
				{
					 xmlNewTextChild(cur, NULL, (const xmlChar*)n_map[pnum].child_nodes[a], (strcmp(node_name, n_map[pnum].child_nodes[a]) ? NULL  : (const xmlChar*)node_info));
					
					a++;
				}	
			}
		*/
		/* unlink(filename); */
		xmlSaveFormatFile(filename, doc, 1);
		xmlFreeDoc(doc);
	}
	return NO_ERROR;
}

char *
Neuro_ReadFromXml(char *filename, char *parent_name, char *child_name)
{
	/* might have to output the data 
	 * in a struct or a pointer to a pointer, 
	 * this way, gathering the data would be 
	 * relatively easy, instead of having to 
	 * eavedrop every nodes. <-- this idea will be used for the next set of function.
	 */ 
	xmlDocPtr doc;
	xmlNodePtr cur;
	doc = xmlParseFile(filename);
	xmlChar *output = NULL;

	cur = (xmlNodePtr)findNode((xmlDocPtr*)&doc, parent_name, child_name, NULL);
	if (cur)
		output = (xmlChar*)xmlNodeListGetString(doc, cur->children,1);
	
	if (!output)
		xmlt_errno = CHILD_UNEXIST;	
	
	xmlFreeDoc(doc);
	return (char*)output;
}

int 
Neuro_EditToXml(char *filename, char *parent_name, char *child_name, char *content)
{/* simply changes the content of the given node to the new one inputted. */
	xmlDocPtr doc;
	xmlNodePtr cur;
	if (open(filename, 0644) == -1) /* check to see if the file exist */
	{
		xmlt_errno = FILE_UNEXIST;
		return FILE_UNEXIST;
	}
	doc = xmlParseFile(filename);
	int _err_ = 0;
	
	cur = (xmlNodePtr)findNode((xmlDocPtr*)&doc, parent_name, child_name, NULL);
	if (cur)
		if (xmlStrcmp((xmlChar*)content, xmlNodeListGetString(doc, cur->children, 1)))
			xmlNodeSetContent(cur, (xmlChar*)content);
		else
			_err_ = NODE_SAME_CONTENT;
	else
		_err_ = CHILD_UNEXIST;
	
	xmlSaveFormatFile(filename, doc, 1);
	xmlFreeDoc(doc);
	
	xmlt_errno = _err_;
	return _err_;
}

int 
Neuro_RemoveFromXml(char *filename, char *parent_name, char *child_name)
{ /* remove the node of a parent, if the node is NULL or inexistant, remove the whole content of the parent or simply remove the empty parent node, in order. */
	
	xmlKeepBlanksDefault(0); /* to fix a libxml2 bug */
	xmlDocPtr doc;
	xmlNodePtr cur;
	
	if (open(filename, 0644) == -1) /* check to see if the file exist */
	{
		xmlt_errno = FILE_UNEXIST;
		return FILE_UNEXIST;
	}
	doc = xmlParseFile(filename);
	int _err_ = 0;
	cur = (xmlNodePtr)findNode((xmlDocPtr*)&doc, parent_name, child_name, NULL);
	if (cur)
		xmlUnlinkNode(cur);
	else
		_err_ = CHILD_UNEXIST;
	
	xmlSaveFormatFile(filename, doc, 1);
	xmlFreeDoc(doc);
	
	xmlt_errno = _err_;
	return _err_;
}

int 
Neuro_GetXmlDesc(char *filename)
{
	if (open(filename, 0644) == -1) /* check to see if the file exist */
	{
		return 1; /* it wont work if the file doesnt exist */
	}
	else
	{
		xmlDocPtr doc; /* the xml document */
		xmlDtdPtr dtd; /* the xml description */
	       	xmlValidCtxtPtr ctxt; /* the xml context for validation */
	       	xmlNodePtr cur; /* the xml temporary node pointer */
		
		doc = xmlParseFile(filename);
		ctxt = xmlNewValidCtxt();
		
		if (!xmlValidateDocument(ctxt, doc)) /* validating the document */
		{
			printf("this xml file is invalid\n");
			xmlFreeDoc(doc);
			return NO_ERROR;
		}

		Nmap_Clean(); /* clear our n_map so we can write in it */
		dtd = xmlGetIntSubset(doc); /* get the Xml Descriptor of the xml file which is located at the beginning of the file */
		cur = (xmlNodePtr)dtd->children;
		
		Neuro_XMLAddRoot((char*)cur->name);
		xmlNodePtr parent = cur->next;
		cur = cur->next->next;
		while (cur)
		{
			/* printf("getxmldesc %s\n", cur->name); */
			Neuro_XMLAdd((char*)parent->name, (char*)cur->name, NULL);
			if (cur->next)
				cur = cur->next;
			else 
				break;
		}
		xmlFreeValidCtxt(ctxt);
		xmlFreeDoc(doc);
	}	
	
	return NO_ERROR;
}

int 
Neuro_MultiAddXml(char *filename)
{
	xmlDocPtr doc;
	xmlNodePtr cur;
	xmlDtdPtr dtd;

	char *buf1 = NULL;
	unsigned int a = 0;

	/*
	xmlChar *elem = "<!ELEMENT ";
	xmlChar *pcdata = "(#PCDATA)";
	*/
	
	NODE_MAP *n_map = Neuro_XMLGetData();
	if (open(filename, 0644) == -1) /* check to see if the file exist */
	{
		/* the file doesnt exist, so we start it up */
		doc = xmlNewDoc(NULL);
		cur = xmlNewDocRawNode(doc, NULL, (const xmlChar*)n_map->root_name, NULL);
		
		xmlDocSetRootElement(doc, cur);		
		cur = xmlNewChild(cur, NULL, (const xmlChar*)n_map->parent_name, (const xmlChar*)NULL);	
		
		while (a < n_map->total_child)
		{
			xmlNewTextChild(cur, NULL, (const xmlChar*)n_map->child_nodes[a], (const xmlChar*)n_map->child_content[a]);				
			a++;
		}	
		
		xmlBufferPtr buf = createdtdheader(doc, &buf1);
		if (!buf1)
			printf("madd debug buf1 is empty, thats why it crashes\n");
		dtd = xmlCreateIntSubset(doc, (xmlChar*)buf1, NULL, NULL);
		printf("madd debug1\n");
		/* showMemStats(); */
		freedtdheader(buf, &buf1);
		/*showMemStats(); */
		printf("madd debug2\n");
		
		doc->intSubset = dtd;
		printf("madd debug3\n");	
		/* xmlNewChild((xmlNodePtr)dtd->children, NULL, "some stuff", NULL); */
				
		xmlSaveFormatFile(filename, doc, 1);
		printf("madd debug4\n");
		xmlFreeDoc(doc);
		printf("madd debug5\n");
	}
	else
	{
		
		xmlKeepBlanksDefault(0);
		doc = xmlParseFile(filename);
		xmlValidCtxtPtr ctxt = xmlNewValidCtxt();
		
		if (!xmlValidateDocument(ctxt, doc))
		{
			printf("this xml file is invalid\n");
			xmlFreeDoc(doc);
			return NO_ERROR;
		}
		xmlFreeValidCtxt(ctxt);

		cur = xmlDocGetRootElement(doc);
		
		cur = xmlNewTextChild(cur, NULL, (const xmlChar*)n_map->parent_name, (const xmlChar*)NULL);
		
		unsigned int a = 0;
		while (a < n_map->total_child)
		{
			 xmlNewTextChild(cur, NULL, (const xmlChar*)n_map->child_nodes[a], (const xmlChar*)n_map->child_content[a]);
			a++;
		}	
				
		xmlSaveFormatFile(filename, doc, 1);
		xmlFreeDoc(doc);
	}
	return NO_ERROR;
}

NODE_MAP *
Neuro_MultiReadXml(char *filename, char *parent_name, char *node_name, char *content)
{ 
	/* This function will check out the nodes in order until
	 * it finds a subnode in it (node_name) that has a set content.
	 * It will then take the whole node (the parent of the subnode) 
	 * and return it as node_map.
	 * The advantage of this way is its fairly easy to output the data
	 * but the disavantage is theres no easy ways to list all the nodes.
	 * A solution would be by returning a pointer to a pointer or to
	 * have a node in the parent_node have a numbering system.
	 */
	xmlDocPtr doc;
	xmlNodePtr cur;
	NODE_MAP *n_map = Neuro_XMLGetData();
	if (open(filename, 0644) == -1) /* check to see if the file exist */
	{
		error_handle2(FILE_UNEXIST);
	}
	
	if (!n_map)
	{
		xmlt_errno = N_MAP_UNEXIST;
		return NULL;
	}

	doc = xmlParseFile(filename);
	xmlValidCtxtPtr ctxt = xmlNewValidCtxt();
		
	if (!xmlValidateDocument(ctxt, doc))
	{
		printf("this xml file is invalid\n");
		xmlFreeDoc(doc);
		return NULL;
	}
	xmlFreeValidCtxt(ctxt);

	cur = (xmlNodePtr)findNode((xmlDocPtr*)&doc, parent_name, node_name, content);
	
	if (!cur)
	{
		xmlt_errno = CHILD_UNEXIST;
		return NULL;
	}
	
	if (cur)
	{
   		/* printf("found something in : %s\n", (xmlChar*)cur->name); */
		cur = cur->children;
		while (cur)
		{
			
			
			/* printf("going to the next node (%s)\n", cur->next->name); */
			if (cur->type == XML_ELEMENT_NODE)
			{
				xmlChar *buf1 = (xmlChar*)xmlNodeListGetString(doc, cur->children,1);
				/* printf("node name (%s) content (%s)\n", cur->name, buf1); */
				Neuro_XMLAdd(parent_name, (char*)cur->name, (char*)buf1);
			}
			if (cur->next)
			{
				cur = cur->next;
				continue;
			}
			else
				break;
		}

	}
	/* Debug printing of the struct n_map */
	/* Neuro_XMLPrintData(); */
	
	
	xmlFreeDoc(doc);
	return (struct node_map*)n_map;
}

/*--- static functions ---*/

static xmlBufferPtr 
createdtdheader(xmlDocPtr doc, char **buf1)
{
	char *elem = "<!ELEMENT ";
	char *pcdata = " (#PCDATA)";
	NODE_MAP *n_map = Neuro_XMLGetData();

#if usexmlbuffer
	
	
	xmlBufferPtr buf;
	buf = xmlBufferCreateSize(strlen(doc->children->name) + 3);
		

	xmlBufferCat(buf, doc->children->name);
	xmlBufferCat(buf, (xmlChar*)" [\n");
	unsigned int a = 0;
		
	int len = 0;
	xmlBufferGrow(buf, strlen(elem) + strlen(doc->children->name) + strlen(" ANY>"));
	xmlBufferCat(buf, elem);
	xmlBufferCat(buf, doc->children->name);
	xmlBufferCat(buf, " ANY>");
			
	xmlBufferGrow(buf, strlen(elem) + strlen(n_map->parent_name) + 2);
	xmlBufferCat(buf, elem);
	xmlBufferCat(buf, n_map->parent_name);
	xmlBufferCat(buf, " (");
	while (a < n_map->total_child - 1)
	{
		len = strlen(n_map->child_nodes[a]);
		xmlBufferGrow(buf, len + 2);
		xmlBufferCat(buf, n_map->child_nodes[a]);
		xmlBufferCat(buf, (xmlChar*)", ");

		a++;
	}
	xmlBufferGrow(buf, strlen(n_map->child_nodes[a]) + 2);
	xmlBufferCat(buf, n_map->child_nodes[a]);
	xmlBufferCat(buf, ")>");
		
	a = 0;
	while (a < n_map->total_child)
	{	
		len = strlen(n_map->child_nodes[a]);
		xmlBufferGrow(buf, len + strlen(elem) + strlen(pcdata) + 3);
		xmlBufferCat(buf, elem);
		xmlBufferCat(buf, n_map->child_nodes[a]);
		xmlBufferCat(buf, " ");
		xmlBufferCat(buf, pcdata);
		xmlBufferCat(buf, ">");	
		a++;
	}
	xmlBufferGrow(buf, 1);
	xmlBufferCat(buf, "]");
	/* printf("xmlBufferContent :\n%s\n", xmlBufferContent(buf)); */
	*buf1 = (xmlChar*)xmlBufferContent(buf);
	return buf;
#else /* not usexmlbuffer */
	unsigned int a = 0;
	
	/* the buffer is 512 until I make it dynamic
	 * FIXME : make it dynamic
	 */
	char *tmp1 = calloc(1, 512);
	if (tmp1 == NULL)
	{
		perror("createdtdheader():line#547:xmltool.c");
	}
	/* printf("will now use the 2nd way of doing the dtd header\n"); */
	sprintf(tmp1, "%s [\n%s%s ANY>", doc->children->name, elem, doc->children->name);
	/* printf("here1\n"); */
	sprintf(tmp1, "%s%s%s (", tmp1, elem, n_map->parent_name);
	/* printf("here2\n"); */
	while (a < n_map->total_child - 1)
	{
		sprintf(tmp1, "%s%s, ", tmp1, n_map->child_nodes[a]);
		a++;
	}
	sprintf(tmp1, "%s%s)>", tmp1, n_map->child_nodes[a]);
	a= 0;
	while (a < n_map->total_child)
	{
		sprintf(tmp1, "%s%s%s%s >", tmp1, elem, n_map->child_nodes[a], pcdata);
		a++;
	}
	sprintf(tmp1, "%s]", tmp1);
	/* printf("createdtdheader temp1 %s\n", tmp1); */
	*buf1 = tmp1;
	return NULL;
#endif /* not usexmlbuffer */
}

static void 
freedtdheader(xmlBufferPtr buf, char **buf1)
{
#if usexmlbuffer
	xmlBufferFree(buf);
#else /* not usexmlbuffer */
	free(buf);
	buf = NULL;
#endif /* not usexmlbuffer */
}

static xmlNodePtr *
findNode(xmlDocPtr *doc, char *parent_name, char *child_name, char *content)
{ 
	/* this functions will be smaller when 
	 * we will remove all the comments. 
	 * Dont remove them tho because debugging 
	 * of this function is highly probable. 
	 */
	xmlNodePtr cur;
	cur = xmlDocGetRootElement(*doc);
	while (cur)
	{
		/*if (cur->type == XML_ELEMENT_NODE)
		printf("%s  \"%s\" type %d\n", cur->name, xmlNodeListGetString(*doc, cur->children, 1), cur->type);*/	
		
		if (!child_name && !xmlStrcmp(cur->name, (xmlChar*)parent_name))
			return (xmlNodePtr*)cur;

		if (!xmlStrcmp(cur->name, (xmlChar*)child_name))	
		{
			if (!xmlStrcmp(cur->parent->name, (xmlChar*)parent_name))
			{
				if (content)
				{
					/* if content is not empty it will do the content check
					 * and will also return the parent node of
					 * the found node instead of just the node
					 * thats to be used with mult_read_xml()
					 * put content as NULL if not in this function.
					 */
					if (!xmlStrcmp((xmlChar*)content, (xmlChar*)xmlNodeListGetString(*doc, cur->children, 1)))
						return (xmlNodePtr*)cur->parent;
#if debug
					else
						printf("incorrect match (%s) (%s %s)\n", cur->name, content, xmlNodeListGetString(*doc, cur->children, 1));
#endif /* debug */
				}
				else 
					return (xmlNodePtr*)cur;
			}
		}
		
		if (!cur->children && cur->next)
		{
			/* printf("going to the next node (%s)\n", cur->next->name); */
			cur = cur->next;
			continue;
		}
		if (cur->children)
		{
			/* printf("going to the children (%s)\n", cur->children->name); */
			cur = cur->children;
			continue;
		}
		if (!cur->children && !cur->next && cur->parent)
		{
			if (cur->parent == xmlDocGetRootElement(*doc))
			{
				/* printf("bailing out 3\n"); */
				break;
			}
parent:
			/* printf("going to parent (%s)\n", cur->parent->name); */
			cur = cur->parent;
			if (cur->next)
			{
				/* printf("going to the next node 2(%s)\n", cur->next->name); */
				cur = cur->next;
			}
			else
			{	
				if (cur->parent == xmlDocGetRootElement(*doc))
				{
					/* printf("bailing out 4\n"); */
					break;
				}
				if (cur->parent)
				{
					goto parent;
					/* printf("going to the parent 2(%s)\n", cur->parent->name); */
					/* cur = cur->parent; */
				}
				else
				{
					/* printf("bailing out 2\n"); */
					break;
				}
			}
			continue;
		}
		else
			break;
	}
	return NULL;
}

/*--- destructor / creator ---*/

void 
Xmltool_Clean()
{
	Nmap_Clean();
	xmlCleanupParser();
	return;
}
