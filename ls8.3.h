#ifndef _LS8_3_H
#define _LS8_3_H

#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <limits.h>

/* Represents an 8.3 filename */
struct filename {
	char name[9];
	char ext[4];
};

/* Get a list of files at path `path` and store it in `out`, an array which can
 * store `num_files` file names, and return the number of files stored, or -1
 * on error. */
size_t
getfiles(const char *path, struct filename names[], int max_files);

/*
 * Sanitize file name for 8.3, returning a pointer to the separator between
 * the file name and the extension. This function does not perform truncation.
 * NULL is returned if the file has no extension.
 */

/* Comparison function for struct filename * to be used with qsort */
int
cmpfilenamep(const void *p, const void *q);

void
make83(struct filename *dest, char *name);

#endif
