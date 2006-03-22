#include <string.h>

#include <other.h>

#include <ebuf.h>
#include <debug.h>

typedef struct DATA
{
	char *data;
}DATA;

/* the struct type that contains the list of options used  
 * by this application.
 * TODO find a better name
 */
typedef struct LOPTIONS
{
	char *string;
	int options;
	void (*action)(char *data);
	u32 present;
	char *data;
	EBUF *datas;
}LOPTIONS;

/* struct buffer to keep the input arguments */
typedef struct BUFINPUT
{
	char *string;
	u8 type; /* 0 is unknown, 1 is option, 2 is data and 3 is a nest of options */
	u8 used; /* if this is 0 it will do an error, with a nest of options, at least 
	one option needs to be invalid to make an error. */
}BUFINPUT;

/* input types */
enum
{
	TYPE_UNKNOWN, /* 0 */
	TYPE_OPTION,
	TYPE_DATA,
	TYPE_NEST /* 3 */
};

EBUF *bufinput; /* buffer of argv */
EBUF *loptions; /* Buffer of the list of options */

static void
clean_loptions(void *src)
{
	LOPTIONS *buf;

	buf = (LOPTIONS*)src;
	
	if (!Neuro_EBufIsEmpty(buf->datas))
		Neuro_CleanEBuf(&buf->datas);
	
	if (buf->string)
	{
		/* printf("cleaning %s\n", buf->string); */
		free(buf->string);
	}
}

/* finds a data option and diminish its presence by 1 */
/*
static void
Find_DATA_And_Revoke()
{
	LOPTIONS *option;
	u32 ototal;

	ototal = Neuro_GiveEBufCount(loptions) + 1;

	while (ototal-- > 0)
	{
		option = Neuro_GiveEBuf(loptions, ototal);
		
		if (option->string == NULL)
		{
			option->present--;
		}
	}
}
*/

static void
Push_Data(LOPTIONS *option, char *string)
{
	DATA *dta;
	
	if (!option->datas)
	{
		Neuro_CreateEBuf(&option->datas);
	}

	Neuro_AllocEBuf(option->datas, sizeof(DATA*), sizeof(DATA));

	dta = Neuro_GiveCurEBuf(option->datas);

	dta->data = string;
	
}

static int
Handle_Option_Argument(LOPTIONS *option, u32 i, char *input_string)
{
	BUFINPUT *input;
	char *temp;
	int _err = 0;
	
	/* completely useless because the caller of this function filters the strings 
	 * and only matches those in loptions (meaning none can in theory have a '='
	 * character in them)
	 */
	if ((temp = memchr(input_string, '=', strlen(input_string))))
	{
		if (temp[1] == '\0')
			_err = 1;
		else
		{
			/* option->data = &temp[1]; */
			Push_Data(option, &temp[1]);
		}
		
	}
	else
	{	
		input = Neuro_GiveEBuf(bufinput, i + 1);
		if (input)
		{
			if (input->type == TYPE_DATA)
			{
				/* option->data = input->string; */
				Push_Data(option, input->string);
				input->used = 1;
				/* Find_DATA_And_Revoke(); */
			}
			else
				_err = 1;
		}
		else
			_err = 1;

		input = Neuro_GiveEBuf(bufinput, i);
	}
	
	if (_err == 1)
	{
		Debug_Val(0, "Missing a DATA argument to the option %s\n", option->string);
	}
	/*else
	{
		Debug_Val(2, " includes data \"%s\" ", option->data);
	}*/

	return _err;
}

static int
Handle_Options_nest(BUFINPUT *input, LOPTIONS *option, char *input_string, 
		char *sep_string, u32 current)
{
	int _err = 0;
	
	
	Debug_Val(4, " N %s -%c", sep_string, input_string[0]);

	/* we drop any checks with long options (--) */
	if (strlen(sep_string) > 2)
		return 0;
	
	if (input_string[0] == sep_string[1])
	{
		option->present++;
		
		Debug_Val(4, " <- found %c ", input->used);
	
		if (option->options & OPTION_NESTED)
		{
			input->used++;
		
			_err = 3;
			
			if (option->options & OPTION_ARGUMENT)
				_err = Handle_Option_Argument(option, current, input->string);
		}
		else
		{
			_err = 1;
			Debug_Val(0, "Invalid option in a nest of options %d \n", input->used);
		}
		
	}

	return _err;
}

/* TODO find a better name eheh */
static int
Handle_Options(BUFINPUT *input, LOPTIONS *option, char *sep_string, u32 current)
{
	char *sep; /* will contain the - or -- prefix plus the option */
	int len; /* sep_string len */
	int _err = 0;

	len = strlen(sep_string);
	
	sep = calloc(1, len + 4);

	if (len > 1 /* && sep_string[0] != '-'*/)
	{
		/* could be either a long option (ie email) or a 
		 * short option ( ie -e ) but with extra options in
		 * it ( ie -eau ). An error would be that the
		 * extra option contains invalid nested options, a
		 * '=' character when a small option.
		 */
		sep[0] = '-';
		sep[1] = '-';
		strncpy(&sep[2], sep_string, len);
	}
	else
	{
		
		sep[0] = '-';
		sep[1] = sep_string[0];
		/*
		if (len == 1)
		{
			sep[2] = '\0';
			_err = Handle_Options(input, option, sep, current);
		}
		else
		{
			sep[2] = sep_string[1];
			sep[3] = '\0';
		}
		*/
	}
	
	Debug_Val(4, "%s %s", sep, input->string);

	/*
	tmp = strchr(input->string, '=');

	if (tmp)
	{
		tmp = '\0';
		printf(" <- data included ");
	}
	*/

	/* check if we have an element in bufinput that is a nest of options 
	 * ie -au 
	 */
	if (input->type == TYPE_NEST)
	{
		int i = 1;
		int ilen = strlen(input->string);
	
		Debug_Val(4, " <- nested options \n");
		
		/* input->used = 0; */

		while (i < ilen)
		{
		
			_err = Handle_Options_nest(input, option, &input->string[i], sep, current);
			if (i + 1 < ilen)
				Debug_Val(4, "\n");
			i++;
		}
		
	}
	else
	{
		if (!strncmp(input->string, sep, strlen(sep)))
		{
			option->present++;
			
			Debug_Val(4, " <- found ");

			input->used = 1;
		
			_err = 3;
			
			if (option->options & OPTION_ARGUMENT)
				_err = Handle_Option_Argument(option, current, input->string);
		}
		/*else
		{
			if (input->used != 1)
				input->used = 0;
		}
		*/
	}

	Debug_Val(4, "\n");
	free(sep);

	return _err;
}

int
Neuro_ArgInit(int argc, char **argv)
{
	int i = 0;
	BUFINPUT *buf;
	
	if (loptions)
		return 1;
	if (argc == 0)
		return 1;
	if (argv == NULL)
		return 1;

	Neuro_CreateEBuf(&bufinput);
	Neuro_CreateEBuf(&loptions);
	
	if (!bufinput || !loptions)
		return 1;

	Neuro_SetcallbEBuf(loptions, clean_loptions);
	
	/* since 0 is the name of the executed command */
	i = 1;
	while (i < argc)
	{
		Neuro_AllocEBuf(bufinput, sizeof(BUFINPUT*), sizeof(BUFINPUT));
		buf = Neuro_GiveCurEBuf(bufinput);

		buf->string = argv[i];
		i++;
	}

#if init_test	
	/* a test to see if everything works */
	i = Neuro_GiveEBufCount(bufinput) + 1;
	while (i-- > 0)
	{
		buf = Neuro_GiveEBuf(bufinput, i);
		Debug_Val(10, "arg output %s\n", buf->string);
	}
#endif /* init_test */
	
	return 0;
}

void
Neuro_ArgOption(char *string, int options, void (*action)(char *data))
{
	LOPTIONS *buf;
	
	if (!bufinput)
		return;
	if (!loptions)
		return;

	Neuro_AllocEBuf(loptions, sizeof(LOPTIONS*), sizeof(LOPTIONS));
	buf = Neuro_GiveCurEBuf(loptions);
	
	buf->string = NULL;
	if (string)
	{
		buf->string = calloc(1, strlen(string) + 1);
		strncpy(buf->string, string, strlen(string));
	}

	buf->options = options;

	if (action)
		buf->action = action;	
}

int
Neuro_ArgProcess()
{
	/* interesting notes : 
	 * - options can contain more than one option in it
	 * options only with the OPTION_NESTED flag can go in it.
	 * - options with the flag OPTION_ARGUMENT can't contain a
	 * = character in it, the next input argument is read to see
	 * if it is a DATA type, if it is not, it will do an error.
	 * 
	 * -- options can only contain a single option but the string
	 *  of the option can be of any lenght (unlike - ). In presence
	 *  of OPTION_ARGUMENT, the algorithm doesn't loop for the next
	 *  input argument like for -, it will look for the -- option and
	 *  try to find a '=' character which should follow a string that
	 *  is the argument or else it will do an error.
	 */

	BUFINPUT *input;
	u32 itotal;
	LOPTIONS *option;
	u32 ototal;
	u32 i = 0, i2 = 0, i3 = 0;
	SepChr_Data *sep;
	EBUF *sepchr;
	int _err = 0;
	
	ototal = Neuro_GiveEBufCount(loptions) + 1;
	
	/* in case there is no arguments inputed 
	 * we search the loptions to see if an 
	 * option contains the OPTION_VOID tag
	 * so we can run it. Normally, only one
	 * option should have this tag so we
	 * will only run the first we find.
	 */
	if (Neuro_EBufIsEmpty(bufinput))
	{
		while (i < ototal)
		{
			option = Neuro_GiveEBuf(loptions, i);

			if (option->options & OPTION_VOID)
			{
				if (option->action)
					(option->action)(NULL);
				return 1;
			}
			i++;
		}
		return 0;
	}
	
	
	itotal = Neuro_GiveEBufCount(bufinput) + 1;

	/* printf("TOTAL %d\n", itotal); */
	Debug_Val(3, "TOTAL %d\n", itotal);


	i = 0;
	/* flag the input arguments with their types */
	while (i < itotal)
	{
		input = Neuro_GiveEBuf(bufinput, i);
		
		Debug_Val(6, "input %s", input->string);

		if (input->string[0] == '-')
		{
			if (input->string[1] != '-' && strlen(input->string) > 2)
			{
				if (strchr(input->string, '='))
				{
					_err = 2;
					Debug_Val(0, "Invalid use of the option character \'-\', it may only be used alone or with other options, \n");
					Debug_Val(0, "to pass an argument to it, please use either the long version of the option (ie --foo=bar)\n"); 
					Debug_Val(0, "or have a space between the option and the argument (ie -f bar).\n");
				}
				else
				{
					input->type = TYPE_NEST;

					Debug_Val(6, " nest\n");
				}
			}
			else
			{
				input->type = TYPE_OPTION;

				Debug_Val(6, " option\n");
			}
		}
		else
		{
			input->type = TYPE_DATA;

			Debug_Val(6, " data\n");
		}
		i++;
	}
	
	i = 0;
	/* loop the options & arguments and process(change flags only except
	 * some exceptions like arguments to options which it will handle) the
	 * normal, <done>
	 * argument, <done> 	 
	 * nested(-au), <done> 
	 * multi <done>
	 * and required <done>
	 * type of options.
	 */
	while (i < itotal && !_err)
	{
		input = Neuro_GiveEBuf(bufinput, i);
		i2 = 0;
		while (i2 < ototal && !_err)
		{
			option = Neuro_GiveEBuf(loptions, i2);
			
			if (option->string)
			{
				sepchr = Neuro_SepChr2(',', option->string);
				
				i3 = Neuro_GiveEBufCount(sepchr) + 1;
				while (i3-- > 0 && !_err)
				{
					sep = Neuro_GiveEBuf(sepchr, i3);
				
					/* TODO find a better name eheh */
					_err = Handle_Options(input, option, sep->string, i);
					
					if (_err == 1)
					{
						/* an error occured */
						_err = 2;
					}
					else
					{
						/* 3 means the current option 
						 * and input concords
						 */
						if (_err == 3)
						{
							if (option->options & OPTION_QUIT)
								_err = 1;
							else
								_err = 0;
						}
					}
				}
				Neuro_CleanEBuf(&sepchr);
			}
			else
			{

				Debug_Val(4, "DATA %s", input->string);

				if (input->type == TYPE_DATA && input->used == 0)
				{
					input->used = 1;
					option->present++;
					/* option->data = input->string; */
					Push_Data(option, input->string);

					Debug_Val(4, " <- found ");
				}
				Debug_Val(4, "\n");
			}

			i2++;
		}
		i++;
	}

	if (!_err)
	{
		i = 0;
		/* loop the options to see if all the required 
		 * options are present and also check if options
		 * don't have more than one iteration of themselves
		 * unless they have the multi option.
		 */
		while (i < ototal)
		{
			option = Neuro_GiveEBuf(loptions, i);
	
			if (option->options & OPTION_REQUIRED && option->present == 0)
			{
				if (option->string)
					Debug_Val(0, "Required option %s is not present.\n", option->string);
				else
					Debug_Val(0, "Required option DATA is not present.\n");
			
				_err = 2;
			}

			if (!(option->options & OPTION_MULTI) && option->present > 1)
			{
				if (option->string)
					Debug_Val(0, "Invalid use of more than one iteration of the option %s.\n", option->string);
				else
					Debug_Val(0, "Invalid use of more than one iteration of the option DATA.\n");
				
				_err = 2;
	
			}
			i++;
		}
	}

	if (!_err)
	{
		i = 0;
		/* loop the bufinput elements and check to see if theres
		 * invalid arguments and print the according errors.
		 */
		while (i < itotal)
		{
			input = Neuro_GiveEBuf(bufinput, i);

			if (input->used == 0)
			{
				Debug_Val(0, "Invalid option used %s\n", input->string);
				_err = 2;
			}
			else
			{
				if (input->type == TYPE_NEST)
				{
					if (input->used < strlen(input->string) - 1)
					{
						Debug_Val(0, "One or more options in the option nest \"%s\" is invalid\n", input->string);
						_err = 2;
					}
				}
			}
		
			i++;
		}
	}

	if (_err <= 1)
	{
		i = 0;
		/* loop the options and call the callbacks if theres no errors */
		while (i < ototal)
		{
			option = Neuro_GiveEBuf(loptions, i);

			if (option->present)
			{
				if (option->action)
				{
					/*if (option->data)
						(option->action)(option->data);
					*/
					if (!Neuro_EBufIsEmpty(option->datas))
					{
						DATA *dta;
						int dtotal = 0;

						dtotal = Neuro_GiveEBufCount(option->datas);
						dtotal++;
						while (dtotal-- > 0)
						{
							dta = Neuro_GiveEBuf(option->datas, dtotal);
							(option->action)(dta->data);
						}
					}
					else
						(option->action)(NULL);

					if (option->options & OPTION_QUIT)
					{
						_err = 1;
						break;
					}
				}
			}
			i++;
		}
	}

	return _err;
}

void
Neuro_ArgClean()
{
	Neuro_CleanEBuf(&bufinput);
	Neuro_CleanEBuf(&loptions);
}
