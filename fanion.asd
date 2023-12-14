;;;; fanion.asd — ASDF system definition for Fanion
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Principal systems

(defsystem "fanion"
  :version (:read-file-line "Makefile" :at 10)
  :description "Exiguous command-line option parser."
  :author "Paul A. Patience"
  :mailto "paul@apatience.com"
  :license "MIT (Expat)"
  :homepage "https://git.sr.ht/~paulapatience/fanion"
  :source-control (:git "https://git.sr.ht/~paulapatience/fanion")
  :class :package-inferred-system
  :depends-on ("fanion/fanion"))

(defsystem "fanion/documentation"
  :version (:read-file-line "Makefile" :at 10)
  :description "Documentation for Fanion."
  :author "Paul A. Patience"
  :mailto "paul@apatience.com"
  :license "MIT (Expat)"
  :depends-on ("fanion" "mgl-pax")
  :components ((:file "documentation")))
