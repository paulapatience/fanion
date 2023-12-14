;;;; fanion.lisp — Exiguous command-line option parser
;;;;
;;;; SPDX-FileCopyrightText: Copyright (c) 2023 Paul A. Patience <paul@apatience.com>
;;;; SPDX-License-Identifier: MIT

;;;* Package definition

(uiop:define-package #:fanion
  (:use #:common-lisp)
  (:export #:option #:make-option #:parse))
(in-package #:fanion)

;;;* Implementation

(defstruct (option (:constructor %make-option
                       (name short long
                        &key value optional-value initial-value reduce finally))
                   (:predicate nil)
                   (:copier nil))
  "Command-line option.

This type is exported only for use in type declarations.
Its internal representation may change at any time."
  (name (error "NAME is required.") :type symbol :read-only t)
  (short nil :type (or null character) :read-only t)
  (long nil :type (or null string) :read-only t)
  (value nil :type (or boolean function) :read-only t)
  (optional-value nil :type t :read-only t)
  (initial-value nil :type t :read-only t)
  (reduce (error "REDUCE is required.") :type function :read-only t)
  (finally (error "FINALLY is required.") :type function :read-only t))

;;; SHORT and LONG are required arguments because at least one must be
;;; provided.
(declaim (ftype (function (symbol
                           (or null character)
                           (or null string)
                           &key (:value (or boolean function))
                           (:optional-value t)
                           (:initial-value t)
                           (:reduce (or null function))
                           (:finally (or null function)))
                          (values option &optional))
                make-option))
(defun make-option (name short long
                    &key (value nil value-p) optional-value initial-value reduce finally)
  "Make an OPTION named NAME identified by at least one of SHORT and LONG.
SHORT may not be `#\\-`, and LONG may not contain `#\\=`.

If VALUE is non-NIL, the option takes an argument; if VALUE is a
function, it is called with the option's argument to produce the
option's value.
If OPTIONAL-VALUE is non-NIL, the option takes an optional argument.

The option's value is initially set to INITIAL-VALUE, and each time the
option appears in the argument list, REDUCE is called with, and its
result replaces, the option's current value.
If the option takes an argument, it is passed as REDUCE's second
argument.
The value of boolean options and missing optional arguments is T.
When the options are exhausted, FINALLY is called with, and its result
replaces, the option's value.

If REDUCE and VALUE are NIL, REDUCE is initialized to `(CONSTANTLY T)`.
If REDUCE is NIL and VALUE is not, REDUCE is initialized to a function
which returns its second argument.
If FINALLY is NIL, it is initialized to IDENTITY.

Some examples of conventional option styles follow:

```cl
(make-option 'verbose #\\v nil)    ; Boolean short option
(make-option 'help nil \"help\")    ; Boolean long option
(make-option 'output #\\o nil      ; Pathname option
             :value #'uiop:parse-native-namestring
             :initial-value uiop:*stdout*)
(make-option 'formats #\\f nil     ; List option (in order)
             :value t
             :reduce (lambda (a b) (cons b a))
             :finally #'nreverse)
```"
  (cond ((and (null short) (null long))
         (error "~@<SHORT or LONG is required.~:@>"))
        ((eql short #\-)
         (error "~@<Short options may not be named ~S.~:@>" short))
        ((find #\= long)
         (error "~@<Long options may not contain ~S.~:@>" #\=))
        ((and value-p (null value) optional-value)
         (error "~@<Cannot specify no value and optional value.~:@>"))
        ((and (not (null long)) (or value optional-value))
         (error "~@<Long options cannot currently take values.~:@>")))
  (let* ((value (cond ((not value-p) (and optional-value #'identity))
                      ((eq value t) #'identity)
                      (t value)))
         (reduce (cond ((not (null reduce)) reduce)
                       ((null value) (constantly t))
                       (t (lambda (a b) (declare (ignore a)) b))))
         (finally (or finally #'identity)))
    (%make-option name short long
                  :value value
                  :optional-value optional-value
                  :initial-value initial-value
                  :reduce reduce
                  :finally finally)))

(declaim (ftype (function (list list) (values hash-table list &optional)) parse))
(defun parse (options arguments)
  "Parse command-line ARGUMENTS according to OPTIONS.
Return as multiple values the parsed options as a hashtable whose keys
are the options' names, and the list of remaining non-option arguments.

OPTIONS must be a list of OPTIONs, ARGUMENTS a list of strings.
The list of remaining non-option arguments may share structure with
ARGUMENTS."
  (flet ((reduce-boolean (option table)
           (let ((name (option-name option)))
             (setf (gethash name table)
                   (funcall (option-reduce option) (gethash name table)))))
         (reduce-value (option value table)
           (let ((name (option-name option)))
             (setf (gethash name table)
                   (funcall (option-reduce option) (gethash name table) value)))))
    (loop with lookup = (loop with lookup = (make-hash-table
                                             :test 'equal
                                             :size (loop for option in options
                                                         unless (null (option-short option))
                                                           sum 1
                                                         unless (null (option-long option))
                                                           sum 1))
                              for option in options
                              do (setf (gethash (option-short option) lookup) option
                                       (gethash (concatenate 'string "--" (option-long option))
                                                lookup)
                                       option)
                              finally (return lookup))
          with result = (loop with result = (make-hash-table :size (length options))
                              for option in options
                              do (setf (gethash (option-name option) result)
                                       (option-initial-value option))
                              finally (return result))
          for subargs = arguments then (rest subargs) until (endp subargs)
          for arg = (first subargs)
          while (and (eql #\- (char arg 0)) (> (length arg) 1))
          if (eql #\- (char arg 1)) do
            (when (eql (length arg) 2)
              (setf subargs (rest subargs))
              (loop-finish))
            (let ((option (gethash arg lookup)))
              (when (null option)
                (error "~@<Option ~S is unrecognized.~:@>" arg))
              (reduce-boolean option result))
          else do
            (loop with end = (length arg) for i = 1 then (1+ i) while (< i end) do
              (flet ((required-value ()
                       (incf i)
                       (cond ((< i end) (prog1 (subseq arg i) (setf i end)))
                             (t (setf subargs (rest subargs))
                                (cond ((endp subargs)
                                       (error "~@<Option \"-~A\" is missing its argument.~:@>"
                                              (char arg (1- i))))
                                      (t (first subargs))))))
                     (optional-value ()
                       (incf i)
                       (cond ((< i end) (prog1 (subseq arg i) (setf i end)))
                             (t t))))
                (declare (inline required-value optional-value))
                (let ((option (gethash (char arg i) lookup)))
                  (when (null option)
                    (error "~@<Option \"-~A\" is unrecognized.~:@>" (char arg i)))
                  (let ((value (option-value option)))
                    (cond ((null value) (reduce-boolean option result))
                          (t (reduce-value option
                                           (funcall value
                                                    (if (option-optional-value option)
                                                        (optional-value)
                                                        (required-value)))
                                           result)))))))
          finally
             (loop for option in options do
               (let ((name (option-name option)))
                 (setf (gethash name result) (funcall (option-finally option)
                                                      (gethash name result)))))
             (return (values result subargs)))))
