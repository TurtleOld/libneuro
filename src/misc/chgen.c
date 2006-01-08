/* character generator */

#include <stdlib.h>
#include <stdio.h>

/* min is 33 because characters lower are control which doesn't have a gfx.*/
#define START_NUMBER 45 /* normal ascii starts at 0 but theres alot of event characters 
			   that aren't graphical. To be safe we start at 45. 33 to 45 are
			   characters we prefer not to use(they are normally invalid anyway) */
/* max is 255 */
#define END_NUMBER 126 /* normal ascii finished at 126 */
/* obsolete */
#define SLEEP_TIME 5 /* in microseconds : 1 second == 10^6 microseconds */

static void
core_Uchar(unsigned char **buf, unsigned int *c_pass_count, unsigned int *cursor)
{
	register unsigned char *c_pass = NULL;
	
	if (*buf == NULL)
	{
		*buf = (unsigned char*)calloc(1, sizeof(unsigned char) + 1);
		c_pass = *buf;
		c_pass[0] = START_NUMBER;
		*c_pass_count = 0;
		c_pass[1] = '\0';
	}
	else
		c_pass = *buf;
		
	if (c_pass[*cursor] == END_NUMBER)
	{
		c_pass[*cursor] = START_NUMBER;
		
		*cursor = *cursor + 1;
		if (*c_pass_count < *cursor)
		{
			*buf = (unsigned char*)realloc(*buf, 
					sizeof(unsigned char) * (2 + *c_pass_count));
			*c_pass_count = *c_pass_count + 1;
			c_pass[*c_pass_count] = '\0';
			c_pass[*cursor] = START_NUMBER;
		}
			
		core_Uchar(buf, c_pass_count, cursor);
		
		*cursor = 0;
	}
	c_pass[*cursor] = c_pass[*cursor] + 1;
}

/*
static void
test_Uchar()
{
	int total = 83;
	unsigned char *buf = NULL;

	while (total-- > 1)
	{
		Uchar(total, &buf);
		printf("%s\n", buf);
		free(buf);
		buf = NULL;
	}
}
*/

void
Uchar(int amount, unsigned char **buf)
{
	/* register unsigned char *c_pass; */
	unsigned int c_pass_count = 0;
	unsigned int cursor = 0;
	register int total = amount;
		
	if (amount <= 0 || *buf) /* to avoid mem leaks we leave if buf is not NULL */
		return;
	
	while (total-- > 0)
	{
		core_Uchar(buf, &c_pass_count, &cursor);	
	}
}
