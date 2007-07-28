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
  (when (< (parser-context-cursor ctx) (parser-context-size ctx))
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

(defmacro define-transformation (key (ctx forms) &body body)
  "Shortcut to register new transformer to *TRANSFORMATION-RULES* table."
  `(setf (gethash ,key *transformation-rules*)
        #'(lambda (,ctx ,forms)
            (declare (ignorable ,ctx ,forms))
            ,@body)))

(define-condition parser-return ()
  ((value :initarg :value :accessor parser-return-value)))

(defun transform-grammar (ctx form &optional (in-meta t))
  (if in-meta
      ;; In META scope.
      (cond
        ((and (consp form) (keywordp (car form)))
         (let ((transformer (gethash (car form) *transformation-rules*)))
           (if (null transformer)
               (transform-grammar ctx form nil)
               (funcall transformer ctx (cdr form)))))
        ((characterp form) `(match-atom ,ctx ,form))
        ((stringp form)
         (transform-grammar
          ctx
          `(:checkpoint
            (and
             ,@(mapcar
                #'(lambda (form) `(match-atom ,ctx ,form))
                (coerce form 'list))
             ,form))))
        (t (transform-grammar ctx form nil)))
      ;; Out of META scope.
      (cond
        ((and (consp form) (eql 'meta (car form)))
         (transform-grammar ctx `(:and ,@(cdr form))))
        ((consp form)
         (mapcar #'(lambda (form) (transform-grammar ctx form nil)) form))
        (t form))))

(define-transformation :icase (ctx forms)
  "\(:ICASE FORM FORM ...)

Make case-insensitive atom comparison in supplied FORMs."
  (with-unique-names (ret)
    `(progn
       (push t (parser-context-icases ,ctx))
       (let ((,ret
              (handler-case ,(transform-grammar ctx `(:and ,@forms))
                (parser-return (data)
                  (pop (parser-context-icases ,ctx))
                  (signal 'parser-return
                          :value (parser-return-value data))))))
         (pop (parser-context-icases ,ctx))
         ,ret))))

(define-transformation :checkpoint (ctx forms)
  "\(:CHECKPOINT FORM FORM ...)

Sequentially evaluates supplied forms and if any of them fails, moves cursor
back to its start position :CHECKPOINT began."
  (with-unique-names (ret)
    `(progn
       (checkpoint ,ctx)
       (let ((,ret
              (handler-case ,(transform-grammar ctx `(:and ,@forms))
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

(define-transformation :and (ctx forms)
  "\(:AND FORM FORM ...)

Sequentially evaluates FORMs identical to AND."
  `(and ,@(mapcar #'(lambda (form) (transform-grammar ctx form)) forms)))

(define-transformation :or (ctx forms)
  "\(:OR FORM FORM ...)

Sequentially evalutes FORMs identical to OR."
  `(or ,@(mapcar #'(lambda (form) (transform-grammar ctx form)) forms)))

(define-transformation :not (ctx forms)
  "\(:NOT FORM)

Identical to \(NOT FORM). \(FORM is encapsulated within a :CHECKPOINT before
getting evaluated.)"
  (transform-grammar
   ctx `(:checkpoint (not ,(transform-grammar ctx (car forms))))))

(define-transformation :return (ctx forms)
  "\(:RETURN VALUE VALUE ...)

Returns from the rule with supplied VALUEs."
  `(signal 'parser-return :value (list ,@forms)))

(define-transformation :render (ctx forms)
  "\(:RENDER RENDERER ARG ARG ...)

Calls specified renderer \(which is defined with DEFRENDERER) with the supplied
arguments."
  `(,(car forms) ,@(nconc (list ctx) (cdr forms))))

(define-transformation :? (ctx forms)
  "\(:? FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope and regardless of the
return value of ANDed FORMs, block returns T. \(Similar to `?' in regular
expressions.)"
  `(prog1 t (and ,@(mapcar #'(lambda (form) (transform-grammar ctx form)) forms))))

(define-transformation :* (ctx forms)
  "\(:* FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope until it returns
NIL. Regardless of the return value of ANDed FORMs, block returns T. \(Similar
to `*' in regular expressions.)"
  `(not (do () ((not ,(transform-grammar ctx `(:and ,@forms)))))))

(define-transformation :+ (ctx forms)
  "\(:+ FORM FORM ...)

Sequentially evaluates supplied FORMs within an AND scope, and repeats this
process till FORMs return NIL. Scope returns T if FORMs returned T once or more,
otherwise returns NIL. \(Similar to `{1,}' in regular expressions.)"
  (transform-grammar ctx `(:and ,@forms (:* ,@forms))))

(define-transformation :type (ctx forms)
  "\(:TYPE TYPE-CHECKER)
\(:TYPE \(OR TYPE-CHECKER TYPE-CHECKER ...))

Checks type of the atom at the current position through supplied function(s)."
  `(match-type ,ctx ,(car forms)))

(define-transformation :rule (ctx forms)
  "\(:RULE RULE ARG ARG ...)
\(:RULE (OR RULE RULE ...) ARG ARG ...)

Tests input in the current cursor position using specified type/form. If any,
supplied arguments will get passed to rule."
  (if (and (consp (car forms)) (eql 'or (caar forms)))
      (transform-grammar
       ctx `(:or ,@(mapcar #'(lambda (form) `(:rule ,form ,@(cdr forms)))
                           (cdar forms))))
      `(match-rule ,ctx ,(car forms) ,(cdr forms))))

(define-transformation :assign (ctx forms)
  "\(:ASSIGN VAR FORM)
\(:ASSIGN \(VAR1 VAR2 ...) FORM)

Assigns returned value of FORM to VAR, and returns assigned value. \(Latter form
  works similar to MULTIPLE-VALUE-SETQ.)"
  (if (consp (car forms))
      `(multiple-value-setq ,(car forms) ,(transform-grammar ctx (cadr forms)))
      `(setq ,(car forms) ,(transform-grammar ctx (cadr forms)))))

(define-transformation :list-push (ctx forms)
  "\(:LIST-PUSH ITEM-VAR LIST-ACCUM)

Pushes ITEM-VAR into the specified LIST-ACCUM. (See MAKE-LIST-ACCUM and
EMPTY-LIST-ACCUM-P.)"
  `(list-accum-push ,(car forms) ,(cadr forms)))

(define-transformation :list-reset (ctx forms)
  "\(:LIST-RESET LIST-ACCUM)

Resets supplied LIST-ACCUM."
  `(reset-list-accum ,(car forms)))

(define-transformation :char-push (ctx forms)
  "\(:CHAR-PUSH CHAR-VAR CHAR-ACCUM)
\(:CHAR-PUSH CHAR-ACCUM)

Pushes supplied CHAR-VAR into specified CHAR-ACCUM. If called with
a single argument, current character gets read and pushed into supplied
accumulator. (See MAKE-CHAR-ACCUM and EMPTY-CHAR-ACCUM-P.)"
  (if (cdr forms)
      `(char-accum-push ,(car forms) ,(cadr forms))
      `(char-accum-push (read-atom ,ctx) ,(car forms))))

(define-transformation :char-reset (ctx forms)
  "\(:CHAR-RESET CHAR-ACCUM)

Resets supplied CHAR-ACCUM."
  `(reset-char-accum ,(car forms)))

(define-transformation :eof (ctx forms)
  "\(:EOF)

Returns T when reached to the end of supplied input data."
  `(= (parser-context-cursor ,ctx) (parser-context-size ,ctx)))

(define-transformation :read-atom (ctx forms)
  "\(:READ-ATOM)

Reads current atom at the cursor position and returns read atom."
  `(read-atom ,ctx))

(define-transformation :debug (ctx forms)
  "\(:DEBUG)
\(:DEBUG VAR)

Print current character and its position in the input data. If VAR is specified,
print the value of the VAR."
  `(prog1 t
     ,(if (car forms)
          `(format t "DEBUG: ~s: ~a~%" ',(car forms) ,(car forms))
          `(format t "DEBUG: cursor: [~s] `~s'~%"
                   (parser-context-cursor ,ctx)
                   (elt (parser-context-data ,ctx)
                        (parser-context-cursor ,ctx))))))


;;; Atom, Rule & Renderer Definition Macros

(defmacro defatom (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (c) (when c ,@body))
     (deftype ,name () `(satisfies ,',name))))

(defmacro defrule (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       (handler-case
           ,(if attachment
                `(let ((,attachment (parser-context-attachment ,ctx)))
                   ,(transform-grammar ctx `(:checkpoint ,@body)))
                (transform-grammar ctx `(:checkpoint ,@body)))
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