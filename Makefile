#* Makefile — Recipes for building Fanion artifacts
#
# SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
# SPDX-License-Identifier: MIT

#* Variables
#** Configuration variables

# Keep zero-indexed line number in sync in ASD file.
VERSION =\
0.1.0

INSTALL = install
INSTALL_DATA = ${INSTALL} -m 0644

SHA256 = sha256sum --tag

SBCL = sbcl
BUILD_DOCUMENTATION = ${SBCL} --script scripts/build-documentation.lisp

#** Internal variables

LISPSRCS =\
	documentation.lisp\
	fanion.asd\
	fanion.lisp

# Makefile added separately to dist.
DATAFILES =\
	LICENSE\
	NEWS.markdown\
	README.markdown\
	fanion.markdown\
	fanion.pdf

CONTRIBFILES =\
	contrib/generate-documentation.lisp\
	contrib/mgl-pax-filter.lua

SCRIPTFILES = scripts/build-documentation.lisp

DISTFILES = ${LISPSRCS} ${DATAFILES} ${CONTRIBFILES} ${SCRIPTFILES}

#* Targets
#** User targets

.PHONY: all
all:

.PHONY: check
check:

.PHONY: markdown
markdown: fanion.markdown

fanion.markdown: ${LISPSRCS} contrib/generate-documentation.lisp scripts/build-documentation.lisp
	${BUILD_DOCUMENTATION} -o $@

.PHONY: pdf
pdf: fanion.pdf

fanion.pdf: ${LISPSRCS} contrib/generate-documentation.lisp scripts/build-documentation.lisp
	${BUILD_DOCUMENTATION} -o $@

.PHONY: clean
clean:
	rm -f fanion-*.tar.gz fanion-*.sha256

.PHONY: distclean
distclean: clean

#** Maintainer targets

.PHONY: dist
dist: fanion-${VERSION}.sha256

# bmake recognizes $< only in suffix rules.
fanion-${VERSION}.sha256: fanion-${VERSION}.tar.gz
	${SHA256} fanion-${VERSION}.tar.gz >$@

# Intentionally fail if the directory already exists.
fanion-${VERSION}.tar.gz: ${DISTFILES}
	mkdir fanion-${VERSION}
	mkdir fanion-${VERSION}/contrib
	mkdir fanion-${VERSION}/scripts
	sed '/^VERSION/{n;s/.*/${VERSION}/;}' Makefile >fanion-${VERSION}/Makefile
	${INSTALL_DATA} ${LISPSRCS} ${DATAFILES} fanion-${VERSION}
	${INSTALL_DATA} ${CONTRIBFILES} fanion-${VERSION}/contrib
	${INSTALL_DATA} ${SCRIPTFILES} fanion-${VERSION}/scripts
	tar -czf $@ fanion-${VERSION}
	rm -rf fanion-${VERSION}

#* Suffix rules

.SUFFIXES:
