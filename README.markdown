<!--
SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
SPDX-License-Identifier: MIT
-->

# Fanion

Exiguous command-line option parser.

## Description

Fanion is a Common Lisp library for parsing command-line options.
It parses them in the same way as OpenBSD [getopt(3)][], but
additionally supports long options, though currently with limitations
(see [Current issues](#current-issues)).

This library is intentionally spartan in its functionality.
It can parse options and nothing else; it cannot parse non-option
arguments or subcommands, and it does not automatically generate usage
of help messages.
However, Fanion's option parsing should be flexible enough to cater to
many use-cases thanks to its option-value reduction, which is modeled
after [Adopt][]'s.
See the [manual](fanion.markdown) for more information.

Fanion is currently under active development and its API is unstable,
but it follows [Semantic Versioning][] so breaking changes will be
indicated by an appropriate bump in version number.

[getopt(3)]: https://man.openbsd.org/getopt.3
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html

## Installing

Fanion is not available on [Quicklisp][], so you will need to clone it
to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/fanion
```

Fanion has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html

## Examples

The Common Lisp files located in Fanion's [`scripts`](scripts) directory
are good examples of Fanion being used in practice.
The following is an extract of
[`scripts/build-documentation.lisp`](scripts/build-documentation.lisp):

```cl
(defparameter +options+
  (list
   (fanion:make-option 'format #\f nil
                       :value #'parse-format)
   (fanion:make-option 'help nil "help")
   (fanion:make-option 'output #\o nil
                       :value #'parse-output
                       :initial-value uiop:*stdout*)))

(fanion:parse +options+ (uiop:command-line-arguments))
```

See the documentation of `FANION:MAKE-OPTION` for further examples of
its capabilities.

## Documentation

Fanion is documented in its manual, which consists of an embellished
version of this README supplemented with the API reference.
It is available in [Markdown form](fanion.markdown) and in PDF form, the
latter of which is distributed only alongside the release archive.

The manual is regenerated only at each release, so in order to consult
the latest available version, you will need to build it manually:

```sh
make markdown pdf
```

Building the manual requires [MGL-PAX][] and [Pathname-Utils][], and
building the PDF manual further requires [Pandoc][], LuaLaTeX and
suitable LaTeX packages.

[MGL-PAX]: https://melisgl.github.io/mgl-pax-world/pax-manual.html
[Pathname-Utils]: https://shinmera.github.io/pathname-utils/
[Pandoc]: https://pandoc.org/

## Current issues

Long options are supported, but only as booleans.
This will be corrected.

Also, no dedicated `FANION-ERROR` class exists for such errors as
unrecognized options and missing values.
An error class hierarchy may be added in the future.

## See also

More fully featured alternatives to Fanion include:

- [Adopt][] with optional [Adopt Subcommands][]
- [clingon][]
- [Clon][]

The manual contains a more exhaustive list of similar libraries.

[Adopt]: https://docs.stevelosh.com/adopt/
[Adopt Subcommands]: https://gitlab.com/daewok/adopt-subcommands
[clingon]: https://github.com/dnaeon/clingon
[Clon]: https://www.lrde.epita.fr/~didier/software/lisp/clon.php

## Acknowledgments

Fanion's option-value reduction, consisting of the `VALUE`, `REDUCE` and
`FINALLY` arguments to `FANION:MAKE-OPTION`, is modeled after Adopt's,
though in Adopt `VALUE` is named `KEY` and `REDUCE` is required.

## License

This project is licensed under the [MIT license (Expat)](LICENSE).

Unless you explicitly state otherwise, any contribution intentionally
submitted by you for inclusion in this project shall be licensed as
above, without any additional terms or conditions.
