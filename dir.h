/* sfnutils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

#ifndef SFNUTILS_DIR_H
#define SFNUTILS_DIR_H

#define _POSIX_C_SOURCE 200809L

#include <dirent.h>
#include <limits.h>
#include <stdint.h>

/* Represents an 8.3 filename */
struct sfnutils_filename {
	char name[9];
	char ext[4];
};

/* Get a list of files at path `path` and store it in `names`, an array which
 * can store `max_files` 8.3 filenames, and return the number of files stored
 * or -1 on error. */
int8_t
sfnutils_getfiles(const char path[], struct sfnutils_filename names[],
		int max_files);

/* Comparison function for struct filename * to be used with qsort */
int
sfnutils_filename_compare(const void *p, const void *q);

#endif
