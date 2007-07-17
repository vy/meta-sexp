;;; Copyright (c) 2007, Volkan YAZICI <yazicivo@ttnet.net.tr>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;; - Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;; - Redistributions in binary form must reproduce the above
;;;   copyright notice, this list of conditions and the following
;;;   disclaimer in the documentation and/or other materials provided
;;;   with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(in-package :meta-sexp)

#-:lispworks
(defmacro with-unique-names ((&rest bindings) &body body)
  "Syntax: WITH-UNIQUE-NAMES ( { var | (var x) }* ) declaration* form*

Executes a series of forms with each VAR bound to a fresh,
uninterned symbol. The uninterned symbol is as if returned by a call
to GENSYM with the string denoted by X - or, if X is not supplied, the
string denoted by VAR - as argument.

The variable bindings created are lexical unless special declarations
are specified. The scopes of the name bindings and declarations do not
include the Xs.

The forms are evaluated in order, and the values of all but the last
are discarded \(that is, the body is an implicit PROGN)."
  ;; (Copied to meta-sexp from cl-ppcre project of Dr. Edmund Weitz.)
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3bshuf30f.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  `(let ,(mapcar #'(lambda (binding)
                     (check-type binding (or cons symbol))
                     (if (consp binding)
                       (destructuring-bind (var x) binding
                         (check-type var symbol)
                         `(,var (gensym ,(etypecase x
                                          (symbol (symbol-name x))
                                          (character (string x))
                                          (string x)))))
                       `(,binding (gensym ,(symbol-name binding)))))
                 bindings)
         ,@body))


;;; Accumulators

(declaim (inline make-char-accum))
(defun make-char-accum (&key (size 512))
  (make-array size :element-type 'character :adjustable t :fill-pointer 0))

(declaim (inline char-accum-push))
(defun char-accum-push (char accum)
  (if (typep char 'character)
      (vector-push-extend char accum)))

(declaim (inline reset-char-accum))
(defun reset-char-accum (accum)
  (setf (fill-pointer accum) 0))

(declaim (inline empty-char-accum-p))
(defun empty-char-accum-p (accum)
  (zerop (fill-pointer accum)))

(declaim (inline make-list-accum))
(defun make-list-accum ()
  nil)

(defmacro list-accum-push (item accum)
  `(push ,item ,accum))

(defmacro reset-list-accum (accum)
  `(setf ,accum nil))

(declaim (inline empty-list-accum-p))
(defun empty-list-accum-p (accum)
  (endp accum))