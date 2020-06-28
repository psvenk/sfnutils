/* sfnutils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

#include "dir.h"
#include "fntable.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 8.3utils is copyright 2020 psvenk and licensed under LGPL-2.1-or-later;
 * see files README and LICENSE for details. */

enum {
	MAX_FILES = 100,
	FNTABLE_SIZE = 100
};

/* Converts a long filename to an 8.3 filename */
static void
filename_make(struct sfnutils_filename *dest, const char orig_name[],
		struct sfnutils_fnnode *fntable[]);

int
main(int argc, char *argv[])
{
	struct sfnutils_filename names[MAX_FILES];
	int8_t num_files;

	if (argc > 1) {
		num_files = sfnutils_getfiles(argv[1], names, MAX_FILES);
	} else {
		num_files = sfnutils_getfiles(".", names, MAX_FILES);
	}
	if (num_files < 0) {
		perror(argv[0]);
		return 1;
	}

	qsort(names, num_files, sizeof(struct sfnutils_filename),
			sfnutils_filename_compare);
	for (int8_t i = 0; i < num_files; ++i) {
		if (strlen(names[i].ext) > 0) {
			printf("%-8s %-3s\n", names[i].name, names[i].ext);
		} else {
			printf("%s\n", names[i].name);
		}
	}
	return 0;
}

int8_t
sfnutils_getfiles(const char path[], struct sfnutils_filename names[],
		int max_files)
{
	DIR *dp = opendir(path);
	if (dp == NULL) {
		return -1;
	}

	struct sfnutils_fnnode *fntable[FNTABLE_SIZE];
	struct dirent *entry;
	uint8_t num_files = 0;
	char name[NAME_MAX];
	while ((entry = readdir(dp)) != NULL && num_files < max_files) {
		if (!strcmp(entry->d_name, ".") ||
				!strcmp(entry->d_name, "..")) {
			continue;
		}
		strcpy(name, entry->d_name);
		filename_make(&names[num_files], name, fntable);
		++num_files;
	}

	sfnutils_fntable_finish(fntable, FNTABLE_SIZE);
	closedir(dp);
	return num_files;
}

int
sfnutils_filename_compare(const void *p, const void *q)
{
	int result;
	struct sfnutils_filename fn1 = * (struct sfnutils_filename *) p;
	struct sfnutils_filename fn2 = * (struct sfnutils_filename *) q;
	if ((result = strcmp(fn1.name, fn2.name)))
		return result;
	return strcmp(fn1.ext, fn2.ext);
}

static void
filename_make(struct sfnutils_filename *dest, const char orig_name[],
		struct sfnutils_fnnode *fntable[])
{
	size_t orig_len = strlen(orig_name);

	/* Number of characters (including \0) for storing name */
	size_t name_size;
	if (orig_len >= 8) {
		name_size = orig_len + 1;
	} else {
		name_size = 9;
	}
	char *name = malloc(name_size * sizeof(char));
	strcpy(name, orig_name);

	char *ext = NULL;
	bool modified = false;
	for (char *p = name + orig_len; p >= name; --p) {
		if (*p >> 7 != 0 || *p == '+') {
			/* We don't care about Unicode here */
			*p = '_';
			modified = true;
		} else if (*p == '.' && p > name && ext == NULL) {
			ext = p + 1;
			*p = '\0';
		} else if (*p == ' ' || *p == '.') {
			memmove(p, p + 1, strlen(p + 1) + 1);
			++p;
			modified = true;
		} else {
			*p = toupper(*p);
		}
	}
	if (ext == NULL) {
		dest->ext[0] = '\0';
	} else {
		if (strlen(ext) > 3)
			modified = true;
		strncpy(dest->ext, ext, 3);
		dest->ext[3] = '\0';
	}
	if (strlen(name) > 8)
		modified = true;
	if (modified) {
		/* Length of base filename (portion before '~') */
		size_t base_len = strlen(name);
		if (base_len > 6)
			base_len = 6;

		/* Stop the string at base_len for now */
		name[base_len] = '\0';
		struct sfnutils_fnnode *n;
		uint8_t num;
		if ((n = sfnutils_fntable_lookup(fntable, FNTABLE_SIZE, name))
				!= NULL) {
			num = ++n->num;
		} else {
			sfnutils_fntable_register(fntable, FNTABLE_SIZE, name);
			num = 1;
		}

		if (num < 10) {
			name[base_len] = '~';
			name[base_len + 1] = '0' + num;
			name[base_len + 2] = '\0';
		}
		else {
			if (base_len == 6)
				--base_len;
			name[base_len] = '~';
			name[base_len + 1] = '0' + num / 10;
			name[base_len + 2] = '0' + num % 10;
			name[base_len + 3] = '\0';
		}
	}
	strncpy(dest->name, name, 8);
	dest->name[8] = '\0';

	free(name);
}
