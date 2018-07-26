/* lbuf.c
 * Module : LBuf
 * a buffer implementation that
 * extends the normal EBUF to
 * make it care about the ordering
 * of the elements.
 */

/*-------------------- Extern Headers Including --------------------*/

/*-------------------- Local Headers Including ---------------------*/
#include <global.h>

/*-------------------- Main Module Header --------------------------*/
#include "neuro/lbuf.h"
#include "neuro/ebuf.h"


/*--------------------      Other       ----------------------------*/

NEURO_MODULE_CHANNEL("lbuf");

typedef struct linked_elem linked_elem;

struct linked_elem
{
	linked_elem *parent;
	linked_elem *child;

	void *content;

	void (*callback)(void *src);
};

struct LBUF
{
	linked_elem *start;
	linked_elem *end;

	linked_elem *current; /* remote access pointer, for the Next function */

	EBUF *buffer;

	void (*callback)(void *src);
};

/*-------------------- Global Variables ----------------------------*/

/*-------------------- Static Variables ----------------------------*/

/*-------------------- Static Prototypes ---------------------------*/



/*-------------------- Static Functions ----------------------------*/

static void
clean_linked_elem(void *src)
{
	linked_elem *tmp;

	tmp = src;

	if (!tmp)
		return;

	if (tmp->content)
	{
		if (tmp->callback)
			(tmp->callback)(tmp->content);

		free(tmp->content);
		tmp->content = NULL;
	}
}

/*-------------------- Global Functions ----------------------------*/

void
Neuro_SetcallbLBuf(LBUF *eng, void (*callback)(void *src))
{
	if (!eng || !callback)
		return;

	eng->callback = callback;
}

void
Neuro_AllocLBuf(LBUF *eng, size_t sobj)
{
	linked_elem *cur = NULL;

	if (!eng)
	{
		WARN("LBUF argument is empty");
		return;
	}

	cur = eng->end;

	TRACE(Neuro_s("Allocating new elem where ending elem is 0x%x", cur));
	if (!cur)
	{
		Neuro_AllocEBuf(eng->buffer, sizeof(linked_elem*), sizeof(linked_elem));

		cur = Neuro_GiveCurEBuf(eng->buffer);

		eng->start = cur;
		eng->end = cur;

		cur->parent = NULL;
		cur->child = NULL;

		cur->content = calloc(1, sobj);

		if (eng->callback)
			cur->callback = eng->callback;

		return;
	}
	else
	{
		linked_elem *parent = cur;

		Neuro_AllocEBuf(eng->buffer, sizeof(linked_elem*), sizeof(linked_elem));

		cur = Neuro_GiveCurEBuf(eng->buffer);

		parent->child = cur;

		eng->end = cur;
	
		cur->parent = parent;
		cur->child = NULL;
		cur->content = calloc(1, sobj);

		if (eng->callback)
			cur->callback = eng->callback;
	}
}

void
Neuro_AllocStartLBuf(LBUF *eng, size_t sobj)
{
	linked_elem *buf = NULL;
	linked_elem *temp = NULL;

	Neuro_AllocLBuf(eng, sobj);

	buf = eng->end;

	/* reorder the elements so the last element
	 * (the one we've just added) becomes the first.
	 *
	 * need to move eng->start to buf->child
	 * and eng->end to buf->parent
	 */

	if (buf->parent)
	{
		buf->parent->child = NULL;

		eng->end = buf->parent;
	}

	if (eng->start != buf)
	{
		temp = eng->start;

		temp->parent = buf;

		eng->start = buf;
	}

	buf->child = temp;
	buf->parent = NULL;
}

void *
Neuro_GiveCurLBuf(LBUF *eng)
{
	if (!eng)
		return NULL;

	if (eng->end)
		return eng->end->content;

	return NULL;
}

u32
Neuro_GiveLBufCount(LBUF *eng)
{
	if (!eng)
		return 0;

	return (Neuro_GiveEBufCount(eng->buffer));
}

void
Neuro_SCleanLBuf(LBUF *eng, void *object)
{
	linked_elem *cur;

	cur = eng->start;

	while (cur)
	{
		if (cur->content == object)
		{
			if (cur == eng->start)
			{
				eng->start = cur->child;
				/*
				if (cur->child)
					eng->start = cur->child;
				else
					eng->start = NULL;
				*/
			}
			if (cur == eng->end)
			{
				eng->end = cur->parent;
				/*
				if (cur->parent)
					eng->end = cur->parent;
				else
					eng->end = NULL;
				*/
			}

			if (cur == eng->current)
			{
				if (cur->child)
					eng->current = cur->parent;
				else
					eng->current = NULL;
			}

			if (cur->parent)
				cur->parent->child = cur->child;
			if (cur->child)
				cur->child->parent = cur->parent;

			Neuro_SCleanEBuf(eng->buffer, cur);
			return;
		}

		cur = cur->child;
	}
}

/* always gives the first elem and also resets
 * the current elem to the first so it can be used 
 * to access the other elems.
 */
void *
Neuro_GiveLBuf(LBUF *eng)
{
	if (!eng)
		return NULL;

	eng->current = eng->start;

	if (eng->current)
		return eng->current->content;

	return NULL;
}

void *
Neuro_GiveNextLBuf(LBUF *eng)
{
	if (!eng)
		return NULL;

	if (!eng->current)
		return Neuro_GiveLBuf(eng);

	eng->current = eng->current->child;

	if (eng->current)
		return eng->current->content;

	return NULL;
}

void
Neuro_ResetLBuf(LBUF *eng)
{
	if (!eng)
		return;

	/* eng->current = eng->start; */
	eng->current = NULL;
}

u32
Neuro_LBufIsEmpty(LBUF *eng)
{
	if (!eng)
		return 1;

	return Neuro_EBufIsEmpty(eng->buffer);
}

/*-------------------- Constructor Destructor ----------------------*/

LBUF *
Neuro_CreateLBuf(void)
{
	LBUF *output = NULL;

	output = (LBUF*)calloc(1, sizeof(LBUF));

	output->buffer = Neuro_CreateEBuf2();

	Neuro_SetcallbEBuf(output->buffer, clean_linked_elem);

	output->callback = NULL;

	output->start = NULL;
	output->end = NULL;
	output->current = NULL;

	return output;
}

void
Neuro_CleanLBuf(LBUF *eng)
{
	if (!eng)
		return;
	

	Neuro_CleanEBuf2(eng->buffer);

	free(eng);
}

