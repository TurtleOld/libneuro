#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct DBBUFFER
{
	unsigned int total;
	void **buffer;
}DBBUFFER;

typedef struct Pdata
{
	unsigned int number;
	char *message;
}Pdata;

typedef struct Plist
{
	unsigned int number;
	char *name;
	unsigned short age;
	unsigned short level;
	unsigned short skill;
}Plist;


static DBBUFFER BufferData;
static DBBUFFER BufferList;

static void
allocBuffer(DBBUFFER *eng, unsigned int psize, unsigned int osize)
{
	void ***buf;
	int *current;

	buf = &eng->buffer;
	current = &eng->total;

	if (*buf == NULL)
	{
		*buf = calloc(1, psize);
		eng->total = 0;	
	}
	else
		*buf = realloc(*buf, psize * (eng->total + 1));

	(*buf)[*current] = calloc(1, osize);
	*current = *current + 1;
}

static void
cleanBuffer(DBBUFFER *eng)
{
	void ***buf;
	int i;
	
       	i = eng->total;
	buf = &eng->buffer;

	while (i-- > 0)
	{
		free((*buf)[i]);
		
	}

	free(*buf);
}

static void
cleanData(DBBUFFER *eng)
{
	Pdata ***buf;
	int i;

	i = eng->total;
	buf = (Pdata***)&eng->buffer;

	while (i-- > 0)
	{
		free((*buf)[i]->message);
		(*buf)[i]->message = NULL;
	}
	cleanBuffer(eng);
}

static void
cleanList(DBBUFFER *eng)
{
	Plist ***buf;
	int i;

	i = eng->total;
	buf = (Plist***)&eng->buffer;
	
	while (i-- > 0)
	{
		free((*buf)[i]->name);
		(*buf)[i]->name = NULL;
	}
	cleanBuffer(eng);
}

static void
fillData(DBBUFFER *eng, unsigned int number, char *message)
{
	int num;
	Pdata ***tmp;

	allocBuffer(eng, sizeof(Pdata*), sizeof(Pdata));
	num = eng->total - 1;
	tmp = (Pdata***)&eng->buffer;
	
	(*tmp)[num]->number = number;
	(*tmp)[num]->message = (char*)calloc(1, strlen(message) + 1);
	strncpy((*tmp)[num]->message, message, strlen(message));
}

static void
fillList(DBBUFFER *eng, unsigned int number, char *name, unsigned short age, unsigned short level, unsigned short skill)
{	
	int num;
	Plist ***tmp;
	
	allocBuffer(eng, sizeof(Plist*), sizeof(Plist));
	num = eng->total - 1;
	tmp = (Plist***)&eng->buffer;
	
	(*tmp)[num]->name = (char*)calloc(1, strlen(name) + 1);
	strncpy((*tmp)[num]->name, name, strlen(name));
	/* printf("added the elem %s to the List\n", (*tmp)[num]->name); */
	(*tmp)[num]->number = number;
	(*tmp)[num]->age = age;
	(*tmp)[num]->level = level;
	(*tmp)[num]->skill = skill;
}

static void
printData(Pdata ***tmp, unsigned int num)
{
	printf("--Pdata Dumping the %d elements content of the buffer Plist to the default output cannal\n", num);
	while (num-- > 0)
	{
		printf("%d : %d %s\n", 
				num, 
				(*tmp)[num]->number,
				(*tmp)[num]->message);
	}
}

static void
printList(Plist ***tmp, unsigned int num)
{
	printf("--Plist Dumping the %d elements content of the buffer Plist to the default output cannal\n", num);
	while (num-- > 0)
	{
		printf("%d : %d %s %d %d %d \n", 
				num, 
				(*tmp)[num]->number,
				(*tmp)[num]->name,
				(*tmp)[num]->age,
				(*tmp)[num]->level,
				(*tmp)[num]->skill);
	}
}

static void
action2()
{
	/*Pdata ***dta;
	Plist ***lst;*/
	DBBUFFER *tmp1;
	DBBUFFER *tmp2;
	/* int i; */

	tmp1 = &BufferData;
	tmp2 = &BufferList;

	/*
	dta = (Pdata***)&tmp1->buffer;
	lst = (Plist***)&tmp2->buffer;
	*/

	/*i = 10000;
	while (i-- > 0)*/
	{
		fillData(tmp1, 1, "some text");
		fillData(tmp1, 2, "Hello, World!");
		
		fillList(tmp2, 1, "foo", 20, 10, 15);
		fillList(tmp2, 2, "bar", 25, 15, 20);
		fillList(tmp2, 3, "foa", 30, 20, 25);
	}
	
	printData((Pdata***)&tmp1->buffer, tmp1->total);
	printList((Plist***)&tmp2->buffer, tmp2->total);
	
	cleanData(tmp1);
	cleanList(tmp2);
}

int main()
{
	action2();
	return 0;
}
