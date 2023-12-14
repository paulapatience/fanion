;;;; documentation.lisp — Documentation for Fanion
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Package-specific documentation

(in-package #:fanion)

(pax:defsection @fanion-api (:title "API reference" :export nil)
  "PARSE is the main entrypoint to the library.
It takes a list of OPTIONs and the command-line arguments to be parsed,
and returns as multiple values a hashtable of parsed options and the
remaining non-option arguments.

Options are parsed in the same way as OpenBSD [getopt(3)][getopt(3)],
except that long options — though currently only boolean long options —
are also supported.
Short options start with one hyphen (`-a`), long options with
two (`--abc`).
Multiple short options may be joined or separate (`-ab -a -b`); their
arguments may be joined or separate (`-c1 -c 2`); and they may take
optional arguments, though these if provided must be joined (`-d -d1`).
Option parsing ends upon reaching the end of the argument list, the
argument ‘`\\-`’, an argument not starting with ‘`\\-`’, or the argument
‘`--`’, which is dropped.
In particular, options and non-option arguments cannot be mixed.

[getopt(3)]: https://man.openbsd.org/getopt.3"
  (parse function)
  (make-option function)
  (option class))

;;;* Definition of documentation package

(uiop:define-package #:fanion/documentation
  (:use #:common-lisp)
  (:import-from #:fanion #:@fanion-api)
  (:import-from #:mgl-pax))
(in-package #:fanion/documentation)

;;;* Manual

(pax:defsection @fanion-manual (:title "Fanion Manual")
  ("fanion" asdf:system)
  (@fanion-introduction pax:section)
  (@fanion-installing pax:section)
  (@fanion-examples pax:section)
  (@fanion-api pax:section)
  (@fanion-current-issues pax:section)
  (@fanion-similar-libraries pax:section)
  (@fanion-acknowledgments pax:section))

;;;** Introduction

(pax:defsection @fanion-introduction (:title "Introduction")
  "Fanion is a Common Lisp library for parsing command-line options.
It parses them in the same way as OpenBSD [getopt(3)][getopt(3)], but
additionally supports long options, though currently with
limitations (see @FANION-CURRENT-ISSUES).
In particular, short options may take optional arguments, and options
and non-option arguments cannot be mixed.
This makes parsing subcommands and their arguments trivial with the
existing API.

This library is intentionally spartan in its functionality.
It can parse options and nothing else; it cannot parse non-option
arguments or subcommands, because the comparative ease with which they
can be manually parsed, e.g., via [pattern matching][Trivia] or even
DESTRUCTURING-BIND, makes further complexifying the API unjustified,
especially when considering the myriad ways non-option arguments can be
arranged.

Also, Fanion does not automatically generate usage or help messages,
operating under the assumption that there will always be
someone (usually me) unhappy with the formatting.
Furthermore, I believe that a good help message, just like a good manual
page, is best manually written.

However, Fanion's option parsing should be flexible enough to cater to
many use-cases thanks to its option-value reduction (see the `\\REDUCE`
argument to FANION:MAKE-OPTION), which is modeled after
[Adopt][Adopt]'s.

Fanion is currently under active development and its API is unstable,
but it follows [Semantic Versioning][semver] so breaking changes will be
indicated by an appropriate bump in version number.

[getopt(3)]: https://man.openbsd.org/getopt.3
[Trivia]: https://github.com/guicho271828/trivia
[Adopt]: https://docs.stevelosh.com/adopt/
[semver]: https://semver.org/spec/v2.0.0.html"
  (@fanion-background pax:section))

(pax:defsection @fanion-background (:title "Background")
  "Why yet another command-line option parsing library, when there are
already twenty?
(See @FANION-SIMILAR-LIBRARIES.)
I first started with [unix-opts][unix-opts], then migrated to
[clingon][clingon] when I saw that it handled subcommands and seemed
well-designed, but I was unhappy with the automatic formatting of their
help messages and also that they allow options to follow non-option
arguments, like GNU [getopt_long(3)][getopt_long(3)].
The catalyst for my decision to write my own library was that I found no
way to make clingon options take optional arguments.

The name Fanion comes from the French term for [flag][flag] in this
context.

[unix-opts]: https://github.com/libre-man/unix-opts
[clingon]: https://github.com/dnaeon/clingon
[getopt_long(3)]: https://manpages.debian.org/getopt_long.3
[flag]: https://www.btb.termiumplus.gc.ca/tpv2alpha/alpha-eng.html?lang=eng&i=1&srchtxt=fanion&index=frt&codom2nd_wet=YB#resultrecs")

;;;** Installing

(pax:defsection @fanion-installing (:title "Installing")
  "Fanion is not available on [Quicklisp][Quicklisp], so you will need to
clone it to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/fanion
```

Fanion has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html")

;;;* Examples

(pax:defsection @fanion-examples (:title "Examples")
  "The Common Lisp files located in Fanion's `scripts` directory are good
examples of Fanion being used in practice.
The following is an extract of `scripts/build-documentation.lisp`:

```cl
(defparameter +options+
  (list
   (fanion:make-option 'format #\\f nil
                       :value #'parse-format)
   (fanion:make-option 'help nil \"help\")
   (fanion:make-option 'output #\\o nil
                       :value #'parse-output
                       :initial-value uiop:*stdout*)))

(fanion:parse +options+ (uiop:command-line-arguments))
```

See FANION:MAKE-OPTION for further examples of its capabilities.")

;;;** Current issues

(pax:defsection @fanion-current-issues (:title "Current issues")
  "Long options are supported, but only as booleans.
This will be corrected.

Also, no dedicated `FANION-ERROR` class exists for such errors as
unrecognized options and missing arguments.
An error class hierarchy may be added in the future.")

;;;** Similar libraries

(pax:defsection @fanion-similar-libraries (:title "Similar libraries")
  "In alphabetical order:

- [Ace Lisp Flag Library](https://github.com/qitab/ace.flag)
- [Adopt](https://docs.stevelosh.com/adopt/) with optional
  [Adopt Subcommands](https://gitlab.com/daewok/adopt-subcommands)
- [Apply-argv](https://github.com/pve1/apply-argv)
- [argparse](https://github.com/epuccini/argparse)
- [cl-argparse](https://github.com/simkoc/cl-argparse)
- [CL-CLI](https://github.com/renard/cl-cli)
- [cl-getopt](https://github.com/ghollisjr/cl-getopt/)
- [cli-parser](https://gitlab.common-lisp.net/cl-cli-parser/cl-cli-parser)
- [clingon](https://github.com/dnaeon/clingon)
- [Clon](https://www.lrde.epita.fr/~didier/software/lisp/clon.php)
- [command-line-arguments](https://gitlab.common-lisp.net/qitab/command-line-arguments)
- [DEFMAIN](https://40ants.com/defmain/)
- [getopt](http://git.kpe.io/?p=getopt.git;a=summary)
- [Just Getopt Parser](https://github.com/tlikonen/cl-just-getopt-parser)
- [lisp-gflags](https://github.com/brown/lisp-gflags)
- [opts](https://github.com/massung/opts)
- [parse-args](https://github.com/salvipeter/parse-args)
- [unix-options](https://github.com/astine/unix-options)
- [unix-opts](https://github.com/libre-man/unix-opts)
- [Utility Arguments](https://gitlab.com/fau/utility-arguments)

Other lists:

- [Awesome CL](https://github.com/CodyReichert/awesome-cl#command-line-options-parsers)
- [CLiki](https://www.cliki.net/command-line%20options%20parser)
- [Dark Chestnut blog](https://www.darkchestnut.com/2021/list-command-line-argument-parsers/)")

;;;** Acknowledgments

(pax:defsection @fanion-acknowledgments (:title "Acknowledgments")
  "Fanion's option-value reduction, consisting of the `\\VALUE`, `\\REDUCE`
and `\\FINALLY` arguments to FANION:MAKE-OPTION, is modeled after
Adopt's, though in Adopt `\\VALUE` is named `\\KEY` and `\\REDUCE` is
required.")
