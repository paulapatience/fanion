<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-MANUAL-20MGL-PAX-3ASECTION-29"></a>

# Fanion Manual

## Table of Contents

- [1 The fanion ASDF System][bb0a]
- [2 Introduction][66a4]
    - [2.1 Background][d61d]
- [3 Installing][0b7d]
- [4 Examples][61eb]
- [5 API reference][dc30]
- [6 Current issues][e410]
- [7 Similar libraries][78f7]
- [8 Acknowledgments][d52f]

###### \[in package FANION/DOCUMENTATION\]
<a id="x-28-22fanion-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>

## 1 The fanion ASDF System

- Version: 0.1.0
- Description: Exiguous command-line option parser.
- Licence: MIT (Expat)
- Author: Paul A. Patience
- Mailto: [paul@apatience.com](mailto:paul@apatience.com)
- Homepage: [https://git.sr.ht/~paulapatience/fanion](https://git.sr.ht/~paulapatience/fanion)
- Source control: [GIT](https://git.sr.ht/~paulapatience/fanion)

<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-INTRODUCTION-20MGL-PAX-3ASECTION-29"></a>

## 2 Introduction

Fanion is a Common Lisp library for parsing command-line options.
It parses them in the same way as OpenBSD [getopt(3)][getopt(3)], but
additionally supports long options, though currently with
limitations (see [Current issues][e410]).
In particular, short options may take optional arguments, and options
and non-option arguments cannot be mixed.
This makes parsing subcommands and their arguments trivial with the
existing API.

This library is intentionally spartan in its functionality.
It can parse options and nothing else; it cannot parse non-option
arguments or subcommands, because the comparative ease with which they
can be manually parsed, e.g., via [pattern matching][Trivia] or even
[`DESTRUCTURING-BIND`][b105], makes further complexifying the API unjustified,
especially when considering the myriad ways non-option arguments can be
arranged.

Also, Fanion does not automatically generate usage or help messages,
operating under the assumption that there will always be
someone (usually me) unhappy with the formatting.
Furthermore, I believe that a good help message, just like a good manual
page, is best manually written.

However, Fanion's option parsing should be flexible enough to cater to
many use-cases thanks to its option-value reduction (see the `REDUCE`
argument to [`FANION:MAKE-OPTION`][dfb1]), which is modeled after
[Adopt][Adopt]'s.

Fanion is currently under active development and its API is unstable,
but it follows [Semantic Versioning][semver] so breaking changes will be
indicated by an appropriate bump in version number.

[getopt(3)]: https://man.openbsd.org/getopt.3

[Trivia]: https://github.com/guicho271828/trivia

[Adopt]: https://docs.stevelosh.com/adopt/

[semver]: https://semver.org/spec/v2.0.0.html


<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>

### 2.1 Background

Why yet another command-line option parsing library, when there are
already twenty?
(See [Similar libraries][78f7].)
I first started with [unix-opts][unix-opts], then migrated to
[clingon][clingon] when I saw that it handled subcommands and seemed
well-designed, but I was unhappy with the automatic formatting of their
help messages and also that they allow options to follow non-option
arguments, like GNU [getopt\_long(3)][getopt\_long(3)].
The catalyst for my decision to write my own library was that I found no
way to make clingon options take optional arguments.

The name Fanion comes from the French term for [flag][flag] in this
context.

[unix-opts]: https://github.com/libre-man/unix-opts

[clingon]: https://github.com/dnaeon/clingon

[getopt\_long(3)]: https://manpages.debian.org/getopt_long.3

[flag]: https://www.btb.termiumplus.gc.ca/tpv2alpha/alpha-eng.html?lang=eng&i=1&srchtxt=fanion&index=frt&codom2nd_wet=YB#resultrecs


<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-INSTALLING-20MGL-PAX-3ASECTION-29"></a>

## 3 Installing

Fanion is not available on [Quicklisp][Quicklisp], so you will need to
clone it to a location known to ASDF or Quicklisp:

```sh
git clone https://git.sr.ht/~paulapatience/fanion
```

Fanion has no dependencies beyond ASDF/UIOP.
It is developed on SBCL, but should be compatible with any conventional
Common Lisp implementation.

[Quicklisp]: https://www.quicklisp.org/beta/releases.html


<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-EXAMPLES-20MGL-PAX-3ASECTION-29"></a>

## 4 Examples

The Common Lisp files located in Fanion's `scripts` directory are good
examples of Fanion being used in practice.
The following is an extract of `scripts/build-documentation.lisp`:

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

See [`FANION:MAKE-OPTION`][dfb1] for further examples of its capabilities.

<a id="x-28FANION-3A-40FANION-API-20MGL-PAX-3ASECTION-29"></a>

## 5 API reference

###### \[in package FANION\]
[`PARSE`][a91c] is the main entrypoint to the library.
It takes a list of [`OPTION`][c53e]s and the command-line arguments to be parsed,
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
argument ‘`-`’, an argument not starting with ‘`-`’, or the argument
‘`--`’, which is dropped.
In particular, options and non-option arguments cannot be mixed.

[getopt(3)]: https://man.openbsd.org/getopt.3


<a id="x-28FANION-3APARSE-20FUNCTION-29"></a>

- [function] **PARSE** *OPTIONS ARGUMENTS*

    Parse command-line `ARGUMENTS` according to `OPTIONS`.
    Return as multiple values the parsed options as a hashtable whose keys
    are the options' names, and the list of remaining non-option arguments.
    
    `OPTIONS` must be a list of [`OPTION`][c53e]s, `ARGUMENTS` a list of strings.
    The list of remaining non-option arguments may share structure with
    `ARGUMENTS`.

<a id="x-28FANION-3AMAKE-OPTION-20FUNCTION-29"></a>

- [function] **MAKE-OPTION** *NAME SHORT LONG &KEY (VALUE NIL VALUE-P) OPTIONAL-VALUE INITIAL-VALUE REDUCE FINALLY*

    Make an [`OPTION`][c53e] named `NAME` identified by at least one of `SHORT` and `LONG`.
    `SHORT` may not be `#\-`, and `LONG` may not contain `#\=`.
    
    If `VALUE` is non-`NIL`, the option takes an argument; if `VALUE` is a
    function, it is called with the option's argument to produce the
    option's value.
    If `OPTIONAL-VALUE` is non-`NIL`, the option takes an optional argument.
    
    The option's value is initially set to `INITIAL-VALUE`, and each time the
    option appears in the argument list, `REDUCE` is called with, and its
    result replaces, the option's current value.
    If the option takes an argument, it is passed as `REDUCE`'s second
    argument.
    The value of boolean options and missing optional arguments is `T`.
    When the options are exhausted, `FINALLY` is called with, and its result
    replaces, the option's value.
    
    If `REDUCE` and `VALUE` are `NIL`, `REDUCE` is initialized to `(CONSTANTLY T)`.
    If `REDUCE` is `NIL` and `VALUE` is not, `REDUCE` is initialized to a function
    which returns its second argument.
    If `FINALLY` is `NIL`, it is initialized to [`IDENTITY`][8ae0].
    
    Some examples of conventional option styles follow:
    
    ```cl
    (make-option 'verbose #\v nil)    ; Boolean short option
    (make-option 'help nil "help")    ; Boolean long option
    (make-option 'output #\o nil      ; Pathname option
                 :value #'uiop:parse-native-namestring
                 :initial-value uiop:*stdout*)
    (make-option 'formats #\f nil     ; List option (in order)
                 :value t
                 :reduce (lambda (a b) (cons b a))
                 :finally #'nreverse)
    ```


<a id="x-28FANION-3AOPTION-20CLASS-29"></a>

- [class] **OPTION** *[STRUCTURE-OBJECT][2038]*

    Command-line option.
    
    This type is exported only for use in type declarations.
    Its internal representation may change at any time.

<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-CURRENT-ISSUES-20MGL-PAX-3ASECTION-29"></a>

## 6 Current issues

Long options are supported, but only as booleans.
This will be corrected.

Also, no dedicated `FANION-ERROR` class exists for such errors as
unrecognized options and missing arguments.
An error class hierarchy may be added in the future.

<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-SIMILAR-LIBRARIES-20MGL-PAX-3ASECTION-29"></a>

## 7 Similar libraries

In alphabetical order:

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

- [Dark Chestnut blog](https://www.darkchestnut.com/2021/list-command-line-argument-parsers/)


<a id="x-28FANION-2FDOCUMENTATION-3A-40FANION-ACKNOWLEDGMENTS-20MGL-PAX-3ASECTION-29"></a>

## 8 Acknowledgments

Fanion's option-value reduction, consisting of the `VALUE`, `REDUCE`
and `FINALLY` arguments to [`FANION:MAKE-OPTION`][dfb1], is modeled after
Adopt's, though in Adopt `VALUE` is named `KEY` and `REDUCE` is
required.

  [0b7d]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-INSTALLING-20MGL-PAX-3ASECTION-29 "Installing"
  [2038]: http://www.lispworks.com/documentation/HyperSpec/Body/t_stu_ob.htm "STRUCTURE-OBJECT (MGL-PAX:CLHS CLASS)"
  [61eb]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-EXAMPLES-20MGL-PAX-3ASECTION-29 "Examples"
  [66a4]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-INTRODUCTION-20MGL-PAX-3ASECTION-29 "Introduction"
  [78f7]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-SIMILAR-LIBRARIES-20MGL-PAX-3ASECTION-29 "Similar libraries"
  [8ae0]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY (MGL-PAX:CLHS FUNCTION)"
  [a91c]: #x-28FANION-3APARSE-20FUNCTION-29 "FANION:PARSE FUNCTION"
  [b105]: http://www.lispworks.com/documentation/HyperSpec/Body/m_destru.htm "DESTRUCTURING-BIND (MGL-PAX:CLHS MGL-PAX:MACRO)"
  [bb0a]: #x-28-22fanion-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"fanion" ASDF/SYSTEM:SYSTEM'
  [c53e]: #x-28FANION-3AOPTION-20CLASS-29 "FANION:OPTION CLASS"
  [d52f]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-ACKNOWLEDGMENTS-20MGL-PAX-3ASECTION-29 "Acknowledgments"
  [d61d]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [dc30]: #x-28FANION-3A-40FANION-API-20MGL-PAX-3ASECTION-29 "API reference"
  [dfb1]: #x-28FANION-3AMAKE-OPTION-20FUNCTION-29 "FANION:MAKE-OPTION FUNCTION"
  [e410]: #x-28FANION-2FDOCUMENTATION-3A-40FANION-CURRENT-ISSUES-20MGL-PAX-3ASECTION-29 "Current issues"
