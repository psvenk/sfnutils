/* sfnutils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

#include "fntable.h"
#include <stdlib.h>
#include <string.h>

unsigned int
sfnutils_hash(const char name[], unsigned int max)
{
	unsigned int hash = 5381;
	int c;

	while ((c = *name++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash % max;
}

struct sfnutils_fnnode *
sfnutils_fntable_lookup(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size, const char name[])
{
	for (struct sfnutils_fnnode *n = fntable[sfnutils_hash(
				name, fntable_size)];
			n != NULL; n = n->next) {
		if (!strcmp(name, n->name))
			return n;
	}
	return NULL;
}

struct sfnutils_fnnode *
sfnutils_fntable_register(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size, const char name[])
{
	struct sfnutils_fnnode **list = &fntable[sfnutils_hash(
			name, fntable_size)];
	struct sfnutils_fnnode *n = malloc(sizeof(struct sfnutils_fnnode));
	strncpy(n->name, name, 8);
	n->name[8] = '\0';
	n->num = 1;

	/* Add node to head of linked list */
	n->next = *list;
	*list = n;

	return n;
}

void
sfnutils_fntable_destroy(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size)
{
	for (unsigned int i = 0; i < fntable_size; ++i) {
		struct sfnutils_fnnode *n = fntable[i];
		while (n != NULL) {
			/* Store current head of list */
			struct sfnutils_fnnode *prev = n;
			/* Move head of list forward */
			n = n->next;
			/* Free previous head of list */
			free(prev);
		}
	}
	free(fntable);
}
