/* parseConf.c
 * Module : ParseConf
 *
 * This module parses configuration buffers or files that
 * has the format :
 *
 * ParameterName: ParameterValue;
 *
 * and # is for comments
 * any line or segment of lines that start or contains the # symbol
 * is considered as comment.
 *
 * As a plus, we could also make this module support the json format.
 */

/*-------------------- Extern Headers Including --------------------*/
#include <global.h> 
#include <debug.h>
#include <lbuf.h>

#include <stdlib.h> /* malloc free */
#include <stdio.h> /* fopen fread fclose */
#include <string.h> /* strlen strncpy strncmp */

/*-------------------- Local Headers Including ---------------------*/


/*-------------------- Main Module Header --------------------------*/
#include <parseConf.h>


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("parseConf");

#define INPUT_BUFFER 4096

struct ConfigData
{
	char *paramName;
	char *paramValue;
};

struct ConfigDatas
{
	LBUF *cData; /* contains ConfigData elements */
};

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

static void
cleanConfigData(void *src)
{
	ConfigData *temp;

	if (src)
	{
		temp = (ConfigData*)src;

		if (temp->paramName)
			free(temp->paramName);
		if (temp->paramValue)
			free(temp->paramValue);
	}
}

/* strip the current buffer until it hits a character other than a
 * space or a tab 
 */
void
stripIndentation(const char *buffer, size_t bufferSize, int *arrowIndex)
{
	int i = *arrowIndex;
	char current;

	while (i < bufferSize)
	{
		current = buffer[i];

		if (current != ' ' && current != '\t')
		{
			/* we are done */
			*arrowIndex = i;
			return;
		}
		i++;
	}
}

/* strip a backward indentation until it hits a character other than a space or a tab */
void
stripBackwardIndentation(const char *buffer, int *arrowIndex)
{
	int i = *arrowIndex - 1;
	char current;
	/* WARN("start"); */

	while (i >= 0)
	{
		current = buffer[i];
		/* WARN(Neuro_s("%d -> '%c`", i, current));*/

		if (current != ' ' && current != '\t')
		{
			/* we are done */
			*arrowIndex = i + 1;
			return;
		}
		i--;
	}

	*arrowIndex = 0;
}

/* parse a buffer and give one of three results : 
 * a fully allocated ConfigData structure,
 * an empty result but valid to mean a comment was parsed.
 * an error, where it was not possible to parse anything. This would
 * 	happen if say the file to parse has an invalid formatting.
 *
 * It is pretty simple to parse a ConfigData structure :
 * We simply parse up to a ':' character to get the parameter name
 * and then we parse up to either a newline or a '#' symbol for the
 * parameter value. If the line contains a '#' symbol, we simply parse
 * until the newline.
 *
 * (this function only parses the first parameter name and value or comment, to get the rest
 * this function has to be called again)
 *
 * result : 
 * 	0 no error, and we parsed a ConfigData struct
 * 	1 no error, no ConfigData struct, just the positionnal arrow was changed (comment)
 * 	2 error, no ConfigData or positionnal pointer
 */
static int
parseBuffer(const char *buffer, size_t bufferSize, int *arrowIndex, ConfigData *confD)
{
	int i = *arrowIndex;
	char buf[INPUT_BUFFER];
	int bufPos = 0;
	char current;
	int restIsComment = 0;

	char *paramName = NULL;
	char *paramValue = NULL;

	if (!buffer)
	{
		WARN("the buffer is empty");
		return 2;
	}

	if (bufferSize <= 0)
	{
		WARN("The bufferSize is below or equal to zero");
		return 2;
	}

	if (bufferSize <= i)
	{
		/* this is the end of the buffer */
		return 2;
	}

	if (!confD)
	{
		ERROR("confD must already be allocated/created");
		return 2;
	}

	if (buffer[i] == '\n')
	{
		/* this can be a normal way to make the configuration file prettier */
		/* WARN("the buffer's first caracter is a newline, bailing out"); */
		*arrowIndex = i + 1;
		return 1;
	}

	while (i < bufferSize)
	{
		current = buffer[i];

		if (restIsComment)
		{
			if (current == '\n')
			{
				i++;
				goto parseBufferWrapUp;
			}
			else
			{
				i++;
				continue;
			}
		}

		if (!paramName)
		{ /* we are in the param name phase */
			switch (current)
			{
				case ':':
				{
					/* we allocate paramName and copy what we have in buf to it 
					 * we also have to end what is in buf with an ending NUL '\0'
					 *
					 * we also reset bufPos to 0 so it can be used to parse the rest.
					 */

					stripBackwardIndentation(buf, &bufPos);
					paramName = malloc(bufPos + 1);
					strncpy(paramName, buf, bufPos);
					paramName[bufPos] = '\0';
					bufPos = 0;
				}
				break;

				case '#':
				{
					if (bufPos > 0)
					{
						/* this is an error, the current has an invalid format so we return 2 */
						ERROR("Incorrect format, an incomplete format was detected due to a misplaced comment");
						return 2;
					}
					else
					{
						/* the whole line is a comment */
						restIsComment = 1;
					}
				}
				break;

				default:
				{
					if (bufPos == 0)
					{
						if (current == ' ' || current == '\t')
						{ /* spacing */
							stripIndentation(buffer, bufferSize, &i);
							continue;
						}
					}

					if (!restIsComment)
					{
						if (bufPos >= INPUT_BUFFER)
						{
							/* we don't have enough memory to parse any further, let's bail out */
							ERROR("Not enough memory in the buffer to continue parsing the current line, bailing out");
							return 2;
						}

						buf[bufPos] = current;
						bufPos++;
					}
				}
				break;
			}
		}
		else
		{
			if (!paramValue)
			{ /* we are in the param value phase */
				switch (current)
				{
					case '\n':
					{
						/* we allocate paramValue and copy what we have in buf to it 
						 * we also have to end what is in buf with an ending NUL '\0'
						 */
						stripBackwardIndentation(buf, &bufPos);
						paramValue = malloc(bufPos + 1);
						strncpy(paramValue, buf, bufPos);
						paramValue[bufPos] = '\0';
						i++;
						goto parseBufferWrapUp;
					}
					break;

					case '#':
					{
						
						if (bufPos == 0)
						{
							/* this is an error, the current has an invalid format so we return 2 */
							ERROR("Incorrect format, an incomplete format was detected due to a misplaced comment");
							return 2;
						}
						else
						{
							/* the whole line is a comment */
							restIsComment = 1;

							if (bufPos > 0)
							{
								/* the previous stuff that was parsed is our param value so we wrap that up */
								stripBackwardIndentation(buf, &bufPos);
								paramValue = malloc(bufPos + 1);
								strncpy(paramValue, buf, bufPos);
								paramValue[bufPos] = '\0';
							}
							else
							{
								ERROR("Incorrect format, an incomplete format was detected due to a misplaced comment");
								return 2;
							}
						}
					}
					break;

					default:
					{
						if (bufPos == 0)
						{
							if (current == ' ' || current == '\t')
							{ /* spacing */
								stripIndentation(buffer, bufferSize, &i);
								continue;
							}
						}

						if (!restIsComment)
						{
							if (bufPos >= INPUT_BUFFER)
							{
								/* we don't have enough memory to parse any further, let's bail out */
								ERROR("Not enough memory in the buffer to continue parsing the current line, bailing out");
								return 2;
							}

							buf[bufPos] = current;
							bufPos++;
						}
					}
					break;
				}
			}
			else
			{
				/* if the rest is comment, we just continue with the loop until we hit the newline */
				if (!restIsComment)
				{
					ERROR("The function should have returned graciously by now, this is not supposed to happen");
					return 2;
				}
				else
				{
					if (current == '\n')
					{
						i++;
						goto parseBufferWrapUp;
					}
				}
			}
		}

		i++;
	}

parseBufferWrapUp:
	if ((paramName == NULL || paramValue == NULL) && !restIsComment)
	{
		if (paramName && bufPos > 0)
		{
			/* this seems to be valid data so we treat it like so (it was just not ended with a newline */

			stripBackwardIndentation(buf, &bufPos);
			paramValue = malloc(bufPos + 1);
			strncpy(paramValue, buf, bufPos);
			paramValue[bufPos] = '\0';
			i++;
			goto parseBufferWrapUp;
		}

		if (paramName)
			free(paramName);
		if (paramValue)
			free(paramValue);

		WARN("No valid data was parsed due to probable incorrect format of the input buffer");
		return 2;
	}
	else
	{
		if (paramName && paramValue)
		{
			confD->paramName = paramName;
			confD->paramValue = paramValue;
			*arrowIndex = i;

			return 0;
		}
		else
		{
			*arrowIndex = i;
			return 1;
		}
	}
}

/* 0 : no error 
 * 1 : error
 */ 
static int
fParseConf(FILE *input, LBUF *cData)
{
	size_t currentSize = 0;
	int arrowIndex = 0; /* current buffer position index (arrow) */
	int parsingArrowIndex = 0; /* this positionnal arrow is for the actual parsing */
	char *buffer = NULL;
	size_t bytesRead = 0;
	size_t totalBytesRead = 0;
	ConfigData *current = NULL;
	int _err = 0;

	if (!input)
		return 1;


	while (feof(input) == 0)
	{
		if (!buffer)
		{
			buffer = malloc(sizeof(char) * INPUT_BUFFER);
			arrowIndex = 0;
			currentSize += INPUT_BUFFER;
		}
		else
		{
			buffer = realloc(buffer, currentSize + sizeof(char) * INPUT_BUFFER);
			arrowIndex += INPUT_BUFFER;
			currentSize += INPUT_BUFFER;
		}

		bytesRead = fread(&buffer[arrowIndex], sizeof(char), INPUT_BUFFER, input);
		totalBytesRead += bytesRead;

		if (bytesRead < INPUT_BUFFER && feof(input) == 0)
		{
			/* this is very weird, fread is supposed to read as much as it can 
			 * into the buffer that we created. If it reads less than the amount
			 * allocated, then it *should* normally mean that the file at eof.
			 * We leave graciously regardless of the situation.
			 *
			 * I do not think this will ever happen though :)
			 */

			WARN("Bytes read by fread is less than the buffer but feof still does not flag the end of file, THIS IS WEIRD! Bailing out.");
			break;
		}
	}


	{
		while (parsingArrowIndex < totalBytesRead)
		{

			if (!current)
			{
				Neuro_AllocLBuf(cData, sizeof(ConfigData));
				current = Neuro_GiveCurLBuf(cData);
			}

			/* TRACE(Neuro_s("Parsing buffer from arrowIndex %d  at parsingArrowIndex %d bytes read %d current buffer size %d", arrowIndex, parsingArrowIndex, bytesRead, currentSize));*/
			_err = parseBuffer(buffer, currentSize, &parsingArrowIndex, current);

			switch (_err)
			{
				case 0:
				{
					/* current contains the correct values so we can now set the pointer to NULL 
					 * so it can contain the next element.
					 */
					current = NULL;
				}
				break;

				case 1:
				{
					/* parseBuffer parsed a comment so current should be left as is, it was not changed */
				}
				break;

				case 2:
				{
					/* an error happened so we bail out */
					if (buffer)
						free(buffer);
					fclose(input);
					return 1;
				}
				break;

				default:
				{
					ERROR("parseBuffer returned a result we do not support, bailing out");
					if (buffer)
						free(buffer);
					return 1;
				}
				break;
			}
		}
	}

	if (buffer)
		free(buffer);
	fclose(input);
	return 0;
}

/*-------------------- Global Functions ----------------------------*/

/* returns 1 on error and 0 if all is ok 
 * don't free paramName or paramValue and
 * don't change their values either
 */
int
Neuro_GiveNextConfig(ConfigDatas *c, char **paramName, char **paramValue)
{
	ConfigData *cur;
	if (!c)
		return 1;

	cur = Neuro_GiveNextLBuf(c->cData);

	if (!cur)
	{
		*paramName = NULL;
		*paramValue = NULL;
		return 1;
	}

	*paramName = cur->paramName;
	*paramValue = cur->paramValue;

	return 0;
}

char *
Neuro_ConfigSearchValueByName(ConfigDatas *c, const char *paramName)
{
	int len;
	ConfigData *cur = NULL;

	if (!c)
		return NULL;
	if (!paramName)
		return NULL;

	len = strlen(paramName);

	cur = Neuro_GiveLBuf(c->cData);

	while (cur)
	{
		if (!strncmp(paramName, cur->paramName, len))
		{
			return cur->paramValue;
		}

		cur = Neuro_GiveNextLBuf(c->cData);
	}

	return NULL;
}

void
Neuro_ResetConfig(ConfigDatas *c)
{
	if (!c)
		return;

	Neuro_ResetLBuf(c->cData);
}

ConfigDatas *
Neuro_ParseConf(const char *filePath)
{
	FILE *cFile = NULL;
	ConfigDatas *c;

	c = malloc(sizeof(ConfigDatas));

	c->cData = Neuro_CreateLBuf();
	Neuro_SetcallbLBuf(c->cData, cleanConfigData);

	cFile = fopen(filePath, "r");

	if (cFile)
	{
		/* this function takes care of closing the FILE pointer */
		if (fParseConf(cFile, c->cData) == 1)
		{
			return NULL;
		}

		return c;
	}
	else
	{
		WARN("Unable to open file");
		return NULL;
	}
}

void
Neuro_CleanConfigDatas(ConfigDatas *c)
{
	if (c)
	{
		Neuro_CleanLBuf(c->cData);
		free(c);
	}
}

/*-------------------- Constructor Destructor ----------------------*/

int
ParseConf_Init(void)
{
	return 0;
}

void
ParseConf_Clean(void)
{
	
}
