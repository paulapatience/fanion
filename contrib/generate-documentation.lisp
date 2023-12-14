;;;; generate-documentation.lisp — Generate documentation based on MGL-PAX
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Package definition

(uiop:define-package #:fanion/contrib/generate-documentation
  (:documentation "Generate documentation based on MGL-PAX.
See @FANION-CONTRIB-GENERATE-DOCUMENTATION.")
  (:use #:common-lisp)
  (:import-from #:pathname-utils)
  (:import-from #:mgl-pax))
(in-package #:fanion/contrib/generate-documentation)

;;;* Documentation

(pax:defsection @fanion-contrib-generate-documentation
    (:title "Generate MGL-PAX documentation")
  "This package makes more convenient the generation of MGL-PAX
documentation in various formats, overrides some defaults, and adds
support for PDF output.
For example, this package's manual can be generated in PDF format as
follows:

```cl
(fanion/contrib/generate-documentation:generate
 fanion/contrib/generate-documentation::@fanion-contrib-generate-documentation
 (pathname-utils:parse-unix-namestring \"gendoc-manual.pdf\")
```

PDF output depends on Pandoc and a TeX distribution which includes
LuaLaTeX and packages normally used by Pandoc, though the TeX engine and
LaTeX packages used can be adjusted in *PANDOC-PDF-OPTIONS* and
*PANDOC-PDF-METADATA*."
  (generate function)
  (*pandoc-pdf-options* variable)
  (*pandoc-pdf-metadata* variable))

;;;* Implementation

(declaim (list *pandoc-pdf-options*))
(defparameter *pandoc-pdf-options*
  (list "--pdf-engine=lualatex"
        (concatenate 'string "--lua-filter="
                     (pathname-utils:native-namestring
                      (asdf:system-relative-pathname
                       "fanion" "contrib/mgl-pax-filter.lua"))))
  "Options to pass to Pandoc when generating PDF output.")

(declaim (list *pandoc-pdf-metadata*))
(defparameter *pandoc-pdf-metadata*
  (list (list "documentclass" "memoir")
        (list "classoption" "article,oneside")
        (list "fontsize" "12pt")
        (list "mainfont" "XCharter")
        (list "sansfont" "Fira Sans")
        (list "monofont" "Fira Mono")
        (list "monofontoptions" "Scale=MatchLowercase")
        (list "csquotes" "true")
        (list "boxlinks" "true")
        (list "header-includes" "|
  \\setlrmarginsandblock{3.5cm}{*}{*}
  \\setulmarginsandblock{4cm}{4cm}{*}
  \\checkandfixthelayout
  \\reparticle
  \\maxsecnumdepth{subsection}
  \\maxtocdepth{subsection}")
        (list "include-before" "\\tableofcontents*"))
  "Metadata to add to Pandoc's leading YAML metadata block.")

(declaim (ftype (function (list stream) (values null &optional)) write-pandoc-metadata))
(defun write-pandoc-metadata (metadata stream)
  "Write the Pandoc METADATA to STREAM."
  (write-string "---" stream)
  (terpri stream)
  (loop for field in metadata do
    (write-string (first field) stream)
    (write-char #\: stream)
    (write-char #\  stream)
    (write-string (second field) stream)
    (terpri stream))
  (write-string "---" stream)
  (terpri stream))

(declaim (ftype (function (t (or stream pathname)
                             &key (:format (member nil :plain :markdown :html :pdf))
                             (:extra-pandoc-options list))
                          (values null &optional))
                generate))
(defun generate (documentable output &key format extra-pandoc-options)
  "Write DOCUMENTABLE to OUTPUT in FORMAT.
If FORMAT is NIL, detect it from OUTPUT's PATHNAME-TYPE; if OUTPUT has
no associated pathname, use :PLAIN.

The supported formats are :PLAIN, :MARKDOWN and :HTML via
MGL-PAX:DOCUMENT, and :PDF by calling Pandoc on the Markdown output.
Recognized extensions are `.txt` for plain, `.markdown` and `.md` for
Markdown, `.html` for HTML and `.pdf` for PDF.
The PDF output can be customized through EXTRA-PANDOC-OPTIONS,
*PANDOC-PDF-OPTIONS* and *PANDOC-PDF-METADATA*.

This function is a wrapper around MGL-PAX:DOCUMENT which overrides some
of its defaults, can write to files and supports PDF."
  (let ((format (or format
                    ;; Cannot check for CL:FILE-STREAM type because
                    ;; CL:*STANDARD-OUTPUT* may be an unnamed one.
                    (cond ((null (ignore-errors (pathname output))) :plain)
                          (t (let ((ext (pathname-type output)))
                               (cond ((equal ext "txt") :plain)
                                     ((equal ext "markdown") :markdown)
                                     ((equal ext "md") :markdown)
                                     ((equal ext "html") :html)
                                     ((equal ext "pdf") :pdf)
                                     (t (error "~@<Extension of ~S is unrecognized.~:@>"
                                               (pathname-utils:native-namestring output))))))))))
    (etypecase output
      (stream
       (ecase format
         (:plain
          (pax:document documentable :stream output :format format))
         (:markdown
          ;; GitHub-Flavored Markdown does not support URL version 2.
          (let ((pax:*document-url-versions* '(1)))
            (pax:document documentable :stream output :format format)))
         (:html
          (let ((pax:*document-fancy-html-navigation* nil)
                (pax:*document-url-versions* '(2)))
            (pax:document documentable :stream output :format format)))
         (:pdf
          (error "~@<Cannot write PDF to ~S.~:@>" (type-of output)))))
      (pathname
       (case format
         (:pdf
          (let ((pax:*document-max-numbering-level* 0)
                (pax:*document-max-table-of-contents-level* 0)
                (pax:*document-url-versions* '(1)))
            (uiop:with-temporary-file (:stream stream :pathname pathname :type "markdown")
              (write-pandoc-metadata *pandoc-pdf-metadata* stream)
              (pax:document documentable :stream stream :format :markdown)
              :close-stream
              (uiop:run-program (append (list "pandoc" "-f" "markdown" "-t" "pdf")
                                        *pandoc-pdf-options*
                                        extra-pandoc-options
                                        (list "-o"
                                              (pathname-utils:native-namestring output)
                                              (pathname-utils:native-namestring pathname)))
                                :error-output *error-output*))))
         (t
          (uiop:with-output-file (output output :element-type 'character
                                                :if-exists :supersede)
            (generate documentable output :format format)))))))
  nil)
