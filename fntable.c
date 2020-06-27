#include "fntable.h"
#include <stdlib.h>
#include <string.h>

/* 8.3utils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

unsigned int
fnthash(const char *name)
{
	unsigned int hash = 5381;
	int c;

	while (c = *name++)
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash % FNTABLE_SIZE;
}

struct fnnode *
fntlookup(const char *name)
{
	for (struct fnnode *n = fntable[fnthash(name)]; n != NULL;
			n = n->next) {
		if (!strcmp(name, n->name))
			return n;
	}
	return NULL;
}

struct fnnode *
fntregister(const char *name)
{
	struct fnnode **list = &fntable[fnthash(name)];
	struct fnnode *n = malloc(sizeof(struct fnnode));
	strncpy(n->name, name, 8);
	n->name[8] = '\0';
	n->num = 1;

	/* Add node to head of linked list */
	n->next = *list;
	*list = n;

	return n;
}

void
fntclear(void)
{
	for (int i = 0; i < FNTABLE_SIZE; ++i) {
		struct fnnode *n = fntable[i];
		while (n != NULL) {
			/* Store current head of list */
			struct fnnode *prev = n;
			/* Move head of list forward */
			n = n->next;
			/* Free previous head of list */
			free(prev);
		}
	}
}
