#include <stdint.h>

enum {
	FNTABLE_SIZE = 100
};

/* Linked list node in hash table mapping each base filename (before '~') to
 * the most recently used post-~ number
 * (a different file extension does not change the numbering) */
struct fnnode {
	char name[9];
	uint8_t num;
	struct fnnode *next;
};

/* Hash function for fntable using djb2 algorithm
 * <http://www.cse.yorku.ca/~oz/hash.html> */
unsigned int
fnthash(const char *name);

/* Lookup filename in fntable */
struct fnnode *
fntlookup(const char *name);

/* Register filename in fntable */
struct fnnode *
fntregister(const char *name);

/* Clear fntable */
void
fntclear(void);
