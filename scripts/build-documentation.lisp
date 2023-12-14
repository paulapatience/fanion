;;;; build-documentation.lisp — Build Fanion documentation
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Script preamble

;;; Assume ASDF is missing when running as a script.
#-asdf
;;; Relying on the absence of :SWANK from *FEATURES* to run MAIN would
;;; be convenient, but MGL-PAX depends on Swank and thus loads it.
(pushnew :fanion/scripts/build-documentation-script *features*)

#+fanion/scripts/build-documentation-script
(require "asdf")
#+fanion/scripts/build-documentation-script
(asdf:load-systems "fanion"
                   "fanion/documentation"
                   "fanion/contrib/generate-documentation"
                   "pathname-utils")

;;;* Package definition

(uiop:define-package #:fanion/scripts/build-documentation
  (:use #:common-lisp)
  (:import-from #:fanion)
  (:import-from #:fanion/documentation)
  (:import-from #:fanion/contrib/generate-documentation)
  (:import-from #:pathname-utils))
(in-package #:fanion/scripts/build-documentation)

;;;* Implementation

(defparameter +usage+
  "Usage: sbcl --script scripts/build-documentation.lisp
            [-f FORMAT] [-o FILE] [--backtrace] [SECTION ...]")

(defparameter +help+
  "Build Fanion documentation.

Usage:
  sbcl --script scripts/build-documentation.lisp
       [-f FORMAT] [-o FILE] [--backtrace] [SECTION ...]
  sbcl --script scripts/build-documentation.lisp --help

Options:
  --backtrace  Print backtrace on error.
  -f FORMAT    Output format (\"plain\", \"markdown\", \"html\" or \"pdf\").
               Default from output extension or \"plain\" if stdout.
               PDF output requires Pandoc, LuaLaTeX and suitable LaTeX packages.
  --help       Print this help message and exit.
  -o FILE      Write to FILE rather than stdout unless \"-\".
               Known extensions: \".txt\", \".markdown\", \".md\", \".html\", \".pdf\".

Arguments:
  SECTION      Generate documentation for SECTION (default \"@fanion-manual\").")

(defun parse-format (string)
  (cond ((equal string "plain") :plain)
        ((equal string "markdown") :markdown)
        ((equal string "html") :html)
        ((equal string "pdf") :pdf)
        (t (error "~@<Format ~S is unrecognized.~:@>" string))))

(defun parse-output (string)
  (if (equal "-" string) uiop:*stdout*
      (pathname-utils:parse-native-namestring string)))

(defun parse-section (string)
  (let ((symbol (uiop:find-symbol* (string-upcase string) '#:fanion/documentation nil)))
    (when (null symbol)
      (error "~@<Section ~S does not exist.~:@>" string))
    (symbol-value symbol)))

(defparameter +options+
  (list
   (fanion:make-option 'backtrace nil "backtrace")
   (fanion:make-option 'format #\f nil :value #'parse-format)
   (fanion:make-option 'help nil "help")
   (fanion:make-option 'output #\o nil :value #'parse-output :initial-value uiop:*stdout*)))

(defun main ()
  (let* ((options (handler-case
                      (multiple-value-bind (options arguments)
                          (fanion:parse +options+ (uiop:command-line-arguments))
                        (when (gethash 'help options)
                          (ignore-errors
                           (write-string +help+ uiop:*stdout*)
                           (terpri uiop:*stdout*)
                           (finish-output uiop:*stdout*)
                           (finish-output uiop:*stderr*))
                          (uiop:quit 0))
                        (when (and (not (pathnamep (gethash 'output options)))
                                   (eq :pdf (gethash 'format options)))
                          ;; Catch this error early to present a better
                          ;; error message on the command-line, but let
                          ;; FANION/CONTRIB/GENERATE-DOCUMENTATION:GENERATE
                          ;; detect and possibly signal an error for the
                          ;; extension.
                          (error "~@<Cannot write PDF to standard output.~:@>"))
                        (setf (gethash 'sections options)
                              (if (endp arguments)
                                  (list fanion/documentation::@fanion-manual)
                                  (mapcar #'parse-section arguments)))
                        options)
                    (error (e)
                      (uiop:with-safe-io-syntax ()
                        (ignore-errors
                         (format uiop:*stderr* "build-documentation.lisp: ~A~%" e)
                         (write-string +usage+ uiop:*stderr*)
                         (terpri uiop:*stderr*)
                         (finish-output uiop:*stdout*)
                         (finish-output uiop:*stderr*)))
                      (uiop:quit 1))))
         (backtrace (gethash 'backtrace options)))
    (handler-bind ((error (lambda (e)
                            (uiop:with-safe-io-syntax ()
                              (ignore-errors
                               (cond (backtrace (uiop:print-condition-backtrace e))
                                     (t (format uiop:*stderr*
                                                "build-documentation.lisp: ~A~%" e)))
                               (finish-output uiop:*stdout*)
                               (finish-output uiop:*stderr*)))
                            (uiop:quit 1))))
      (fanion/contrib/generate-documentation:generate (gethash 'sections options)
                                                      (gethash 'output options)
                                                      :format (gethash 'format options))))
  (ignore-errors
   (finish-output uiop:*stdout*)
   (finish-output uiop:*stderr*))
  (uiop:quit 0))

#+fanion/scripts/build-documentation-script
(main)
