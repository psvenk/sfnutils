sfnutils
========

Utilities for formatting [8.3 filenames][1] (**s**hort **f**ile**n**ames) --
mostly intended as a toy project.  Currently only `dir` (`ls`) is implemented.

Implementations in other languages are also included; mainly as testing grounds
for those languages.

[1]: https://en.wikipedia.org/wiki/8.3_filename


Tentative roadmap
-----------------

- Allow an 8.3 path as an argument
  + This would need a map or bidirectional map (implemented with hash tables)
    between 8.3 and long filenames
- Implement other tools like `cd` in terms of 8.3 file names
- Sort list of files before converting the files to 8.3 (this would solve
  discrepancies between C/Rust and Go because Go sorts files differently; also,
  C and Rust don't guarantee any particular order of files)


Usage
-----

- C: `make && ./dir test`
- Rust: `cd rust && cargo run ../test`
- Go: `cd go && go run dir.go ../test`
- Perl: `./perl/dir.pl test`
- Lingua::Romana::Perligata: `./perl/dir.perligata test`


Sample output
-------------

```console
$ ./dir test
ANDR__~1 TXT
ASDFGH~1
ASDFGH~2
ASDFGH~3
ASDFGH~4
ASDFGH~5
ASDFGH~6
ASDFGH~7
ASDFGH~8
ASDFGH~9
ASDFG~10
ASDFG~11 C  
ASDFG~12
ASDFG~13
ASDFG~14
A____~1
A~1      AB 
C_D~1    C_D
EIGHTCHR
HELLO    JPG
HELLO~1  JPE
THISIS~1 MAF
```


License
-------

SPDX-License-Identifier: LGPL-2.1-or-later

sfnutils: utilities for formatting 8.3 filenames (short filenames)

Copyright (c) 2020 psvenk

This library is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 2.1 of the License, or (at your option) any
later version.

This library is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with this library; if not, write to the Free Software Foundation, Inc., 51
Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

<!-- vim: set tw=80: -->
