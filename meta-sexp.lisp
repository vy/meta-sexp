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


;;; Parser Context Structure & Routines

(defstruct parser-context
  (data nil :read-only t :type string)
  (size nil :read-only t :type unsigned-byte)
  (cursor 0 :type unsigned-byte)
  (checkpoints nil)
  (icases nil)
  attachment)

(defgeneric create-parser-context (input &rest args))

(defmethod create-parser-context ((input string) &key start end attachment)
  (make-parser-context :data input
                       :cursor (or start 0)
                       :size (or end (length input))
                       :attachment attachment))

(defmethod create-parser-context
    ((input string-stream) &key buffer-size start end attachment)
  (loop with out = (make-string-output-stream)
        with buffer-size = (or buffer-size 8192)
        with buf = (make-string buffer-size)
        for pos = (read-sequence buf input :end buffer-size)
        sum pos into size
        until (zerop pos)
        do (write-string buf out :end pos)
        finally (return
                  (create-parser-context
                   (get-output-stream-string out)
                   :start start
                   :end (or end size)
                   :attachment attachment))))

(declaim (inline peek-atom))
(defun peek-atom (ctx)
  (if (< (parser-context-cursor ctx) (parser-context-size ctx))
      (elt (parser-context-data ctx) (parser-context-cursor ctx))))

(declaim (inline read-atom))
(defun read-atom (ctx)
  (if (< (parser-context-cursor ctx) (parser-context-size ctx))
    (elt (parser-context-data ctx) (1- (incf (parser-context-cursor ctx))))))

(declaim (inline checkpoint))
(defun checkpoint (ctx)
  (push (parser-context-cursor ctx) (parser-context-checkpoints ctx)))

(declaim (inline rollback))
(defun rollback (ctx)
  (setf (parser-context-cursor ctx) (pop (parser-context-checkpoints ctx))))

(declaim (inline commit))
(defun commit (ctx)
  (pop (parser-context-checkpoints ctx)))


;;; Atom, Rule & Type Matching

(declaim (inline match-atom))
(defun match-atom (ctx atom &aux (c (peek-atom ctx)))
  (if (and c
           (if (first (parser-context-icases ctx))
               (char= (char-upcase atom) (char-upcase c))
               (char= atom c)))
      (read-atom ctx)))

(defmacro match-type (ctx type)
  `(if (typep (peek-atom ,ctx) ',type)
       (read-atom ,ctx)))

(defmacro match-rule (ctx rule args)
  `(,rule ,@(nconc (list ctx) args)))


;;; Grammar Compiler

(define-condition parser-return ()
  ((value :initarg :value :accessor parser-return-value)))

(defgeneric transform-grammar (ctx in-meta directive &optional args)
  (:documentation "META grammar transformation methods."))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive character) &optional args)
  "Transforms a character form."
  (declare (ignore args))
  `(match-atom ,ctx ,directive))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive string) &optional args)
  "Transforms a string form."
  (declare (ignore args))
  (transform-grammar
   ctx t :checkpoint
   `((and
      ,@(mapcar
         #'(lambda (form) `(match-atom ,ctx ,form))
         (coerce directive 'list))
      ,directive))))

(defmethod transform-grammar (ctx in-meta directive &optional args)
  "The most unspecific transformation method."
  (declare (ignore args))
  (cond
    ((and in-meta (consp directive) (keywordp (car directive)))
     (transform-grammar ctx t (car directive) (cdr directive)))
    ((and (not in-meta) (consp directive) (eql 'meta (car directive)))
     (transform-grammar ctx t :and (cdr directive)))
    ((consp directive)
     (mapcar #'(lambda (form) (transform-grammar ctx nil form)) directive))
    (t directive)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :icase)) &optional args)
  "\(:ICASE FORM FORM ...)

Make case-insensitive atom comparison in supplied FORMs."
  (with-unique-names (ret)
    `(progn
       (push t (parser-context-icases ,ctx))
       (let ((,ret
              (handler-case ,(transform-grammar ctx t :and args)
                (parser-return (data)
                  (pop (parser-context-icases ,ctx))
                  (signal 'parser-return
                          :value (parser-return-value data))))))
         (pop (parser-context-icases ,ctx))
         ,ret))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :checkpoint)) &optional args)
  "\(:CHECKPOINT FORM FORM ...)

Sequentially evaluates supplied forms and if any of them fails, moves cursor
back to its start position :CHECKPOINT began."
  (with-unique-names (ret)
    `(progn
       (checkpoint ,ctx)
       (let ((,ret
              (handler-case ,(transform-grammar ctx t :and args)
                (parser-return (data)
                  (let ((value (parser-return-value data)))
                    (if value
                        (commit ,ctx)
                        (rollback ,ctx))
                    (signal 'parser-return :value value))))))
         (if ,ret
             (commit ,ctx)
             (rollback ,ctx))
         ,ret))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :and)) &optional args)
  "\(:AND FORM FORM ...)

Sequentially evaluates FORMs identical to AND."
  `(and ,@(mapcar #'(lambda (form) (transform-grammar ctx t form)) args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :or)) &optional args)
  "\(:OR FORM FORM ...)

Sequentially evalutes FORMs identical to OR."
  `(or ,@(mapcar #'(lambda (form) (transform-grammar ctx t form)) args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :not)) &optional args)
  "\(:NOT FORM)

Identical to \(NOT FORM). \(FORM is encapsulated within a :CHECKPOINT before
getting evaluated.)"
  (transform-grammar
   ctx t :checkpoint
   `((not ,(transform-grammar ctx t (car args))))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :return)) &optional args)
  "\(:RETURN VALUE VALUE ...)

Returns from the rule with supplied VALUEs."
  `(signal 'parser-return :value (list ,@args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :render)) &optional args)
  "\(:RENDER RENDERER ARG ARG ...)

Calls specified renderer \(which is defined with DEFRENDERER) with the supplied
arguments."
  `(,(car args) ,@(nconc (list ctx) (cdr args))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :?)) &optional args)
  "\(:? FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope and regardless of the
return value of ANDed FORMs, block returns T. \(Similar to `?' in regular
expressions.)"
  `(prog1 t (and ,@(mapcar
                    #'(lambda (form) (transform-grammar ctx t form))
                    args))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :*)) &optional args)
  "\(:* FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope until it returns
NIL. Regardless of the return value of ANDed FORMs, block returns T. \(Similar
to `*' in regular expressions.)"
  `(not (do () ((not ,(transform-grammar ctx t :and args))))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :+)) &optional args)
  "\(:+ FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope, and repeats this
process till FORMs return NIL. Scope returns T if FORMs returned T once or more,
otherwise returns NIL. \(Similar to `{1,}' in regular expressions.)"
  (transform-grammar ctx t :and `(,@args (:* ,@args))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :type)) &optional args)
  "\(:TYPE TYPE-CHECKER)
\(:TYPE \(OR TYPE-CHECKER TYPE-CHECKER ...))

Checks type of the atom at the current position through supplied function(s)."
  `(match-type ,ctx ,(car args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :rule)) &optional args)
  "\(:RULE RULE ARG ARG ...)
\(:RULE (OR RULE RULE ...) ARG ARG ...)

Tests input in the current cursor position using specified type/form. If any,
supplied arguments will get passed to rule."
  (if (and (consp (car args)) (eql 'or (caar args)))
      (transform-grammar
       ctx t :or (mapcar #'(lambda (form) `(:rule ,form ,@(cdr args)))
                         (cdar args)))
      `(match-rule ,ctx ,(car args) ,(cdr args))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :assign)) &optional args)
  "\(:ASSIGN VAR FORM)
\(:ASSIGN \(VAR1 VAR2 ...) FORM)

Assigns returned value of FORM to VAR, and returns assigned value. \(Latter form
works similar to MULTIPLE-VALUE-SETQ.)"
  (if (consp (car args))
      `(multiple-value-setq ,(car args) ,(transform-grammar ctx t (cadr args)))
      `(setq ,(car args) ,(transform-grammar ctx t (cadr args)))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :list-push)) &optional args)
  "\(:LIST-PUSH ITEM-VAR LIST-ACCUM)

Pushes ITEM-VAR into the specified LIST-ACCUM. (See MAKE-LIST-ACCUM and
EMPTY-LIST-ACCUM-P.)"
  `(list-accum-push ,(car args) ,(cadr args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :list-reset)) &optional args)
  "\(:LIST-RESET LIST-ACCUM)

Resets supplied LIST-ACCUM."
  `(reset-list-accum ,(car args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :char-push)) &optional args)
  "\(:CHAR-PUSH CHAR-VAR CHAR-ACCUM)
\(:CHAR-PUSH CHAR-ACCUM)

Pushes supplied CHAR-VAR into specified CHAR-ACCUM. If called with
a single argument, current character gets read and pushed into supplied
accumulator. (See MAKE-CHAR-ACCUM and EMPTY-CHAR-ACCUM-P.)"
  (if (cdr args)
      `(char-accum-push ,(car args) ,(cadr args))
      `(char-accum-push (read-atom ,ctx) ,(car args))))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :char-reset)) &optional args)
  "\(:CHAR-RESET CHAR-ACCUM)

Resets supplied CHAR-ACCUM."
  `(reset-char-accum ,(car args)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :eof)) &optional args)
  "\(:EOF)

Returns T when reached to the end of supplied input data."
  (declare (ignore args))
  `(= (parser-context-cursor ,ctx) (parser-context-size ,ctx)))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :read-atom)) &optional args)
  "\(:READ-ATOM)

Reads current atom at the cursor position and returns read atom."
  (declare (ignore args))
  `(read-atom ,ctx))

(defmethod transform-grammar
    (ctx (in-meta (eql t)) (directive (eql :debug)) &optional args)
  "\(:DEBUG)
\(:DEBUG VAR)

Print current character and its position in the input data. If VAR is specified,
print the value of the VAR."
  `(prog1 t
     ,(if (car args)
          `(format t "DEBUG: ~s: ~a~%" ',(car args) ,(car args))
          `(format t "DEBUG: cursor: [~s] `~s'~%"
                   (parser-context-cursor ,ctx)
                   (elt (parser-context-data ,ctx)
                        (parser-context-cursor ,ctx))))))


;;; Atom, Rule & Renderer Definition Macros

(defmacro defatom (name (c) &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (,c) (when ,c ,@body))
     (deftype ,name () `(satisfies ,',name))))

(defmacro defrule (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       (handler-case
           ,(if attachment
                `(let ((,attachment (parser-context-attachment ,ctx)))
                   ,(transform-grammar ctx t :checkpoint body))
                (transform-grammar ctx t :checkpoint body))
         (parser-return (data)
           (return-from ,name (apply #'values (parser-return-value data))))))))

(defmacro defrenderer (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       ,(if attachment
            `(let ((,attachment (parser-context-attachment ,ctx)))
               ,@body)
            `(progn ,@body))
       t)))