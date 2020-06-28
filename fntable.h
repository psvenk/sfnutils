/* sfnutils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

#ifndef SFNUTILS_FNTABLE_H
#define SFNUTILS_FNTABLE_H

#include <stdint.h>

/* Linked list node in hash table mapping each base filename (before '~') to
 * the most recently used post-~ number
 * (a different file extension does not change the numbering) */
struct sfnutils_fnnode {
	char name[9];
	uint8_t num;
	struct sfnutils_fnnode *next;
};

/* djb2 algorithm */
unsigned int
sfnutils_hash(const char name[], unsigned int max);

struct sfnutils_fnnode *
sfnutils_fntable_lookup(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size, const char name[]);

struct sfnutils_fnnode *
sfnutils_fntable_register(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size, const char name[]);

void
sfnutils_fntable_finish(struct sfnutils_fnnode *fntable[],
		unsigned int fntable_size);

#endif
