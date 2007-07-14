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


;;; Parser Context Class & Routines

(defclass parser-context ()
  ((data
    :initarg :data
    :accessor parser-context-data
    :documentation "Input data getting parsed.")
   (size
    :initarg :size
    :initform nil
    :documentation "Size of the input data.")
   (cursor
    :initarg :cursor
    :initform 0
    :accessor parser-context-cursor
    :documentation "Current location on the input data.")
   (checkpoints
    :initform nil
    :accessor parser-context-checkpoints
    :documentation "Reversed list of declared checkpoints."))
  (:documentation "Information about current state of the parsing process."))

(defgeneric parser-context-size (ctx))

(defmethod parser-context-size ((ctx parser-context))
  (or (slot-value ctx 'size)
      (setf (slot-value ctx 'size)
	    (length (parser-context-data ctx)))))

(defgeneric peek-atom (ctx))
(defgeneric read-atom (ctx))
(defgeneric checkpoint (ctx))
(defgeneric rollback (ctx))
(defgeneric commit (ctx))

(define-condition parser-context-error ()
  ((operation :initarg :operation :accessor parser-context-error-operation)))

(defmethod peek-atom ((ctx parser-context))
  (if (< (parser-context-cursor ctx) (parser-context-size ctx))
      (elt (parser-context-data ctx) (parser-context-cursor ctx))))

(defmethod read-atom ((ctx parser-context))
  (when (< (parser-context-cursor ctx) (parser-context-size ctx))
    (incf (parser-context-cursor ctx))
    (elt (parser-context-data ctx) (1- (parser-context-cursor ctx)))))

(defmethod checkpoint ((ctx parser-context))
  (push (parser-context-cursor ctx) (parser-context-checkpoints ctx)))

(defmethod rollback ((ctx parser-context))
  (let ((prev-pos (pop (parser-context-checkpoints ctx))))
    (if prev-pos
	(setf (parser-context-cursor ctx) prev-pos)
	(error 'parser-context-error :operation 'rollback))))

(defmethod commit ((ctx parser-context))
  (if (not (pop (parser-context-checkpoints ctx)))
      (error 'parser-context-error :operation 'commit)))


;;; Atom, Rule & Type Matching

(defun match-atom (ctx atom &aux (c (peek-atom ctx)))
  (if (and c (char= atom c))
      (read-atom ctx)))

(defmacro match-type (ctx type)
  `(if (typep (peek-atom ,ctx) ',type)
       (read-atom ,ctx)))

(defmacro match-rule (ctx rule args)
  `(,rule ,@(nconc (list ctx) args)))


;;; Accumulators

(defun make-char-accum (&key (size 512))
  (make-array size :element-type 'character :adjustable t :fill-pointer 0))

(defun char-accum-push (char accum)
  (vector-push-extend char accum))

(defun reset-char-accum (accum)
  (setf (fill-pointer accum) 0))

(defun make-list-accum ()
  nil)

(defmacro list-accum-push (item accum)
  `(push ,item ,accum))

(defmacro reset-list-accum (accum)
  `(setf ,accum nil))


;;; Grammar Compiler

(defun compile-grammar (ctx form)
  (labels ((compile-exprs (form &optional (in-meta t))
	     (mapcar #'(lambda (form) (compile-expr form in-meta)) form))
	   (compile-expr (form &optional (in-meta t))
	     (if in-meta
		 (cond
		   ((and (consp form) (keywordp (car form)))
		    (ecase (car form)
		      (:checkpoint
		       (with-gensyms (ret)
			 `(progn
			    (checkpoint ,ctx)
			    (let ((,ret ,(compile-expr (cadr form))))
			      (if ,ret
				  (commit ,ctx)
				  (rollback ,ctx))
			      ,ret))))
		      (:and `(and ,@(compile-exprs (cdr form))))
		      (:or `(or ,@(compile-exprs (cdr form))))
		      (:not (compile-expr `(:checkpoint (not ,(compile-expr (cadr form))))))
		      (:return `(return-from rule-block (values ,@(cdr form))))
		      (:? `(prog1 t ,(compile-expr `(:and ,@(cdr form)))))
		      (:* `(not (do () ((not ,(compile-expr `(:and ,@(cdr form))))))))
		      (:+ (compile-expr `(:and ,@(cdr form) (:* ,@(cdr form)))))
		      (:type `(match-type ,ctx ,(cadr form)))
		      (:rule
		       (if (and (consp (cadr form))
				(eql 'or (caadr form)))
			   (compile-expr
			    `(:or ,@(mapcar #'(lambda (form) `(:rule ,form)) (cdadr form))))
			   `(match-rule ,ctx ,(cadr form) ,(cddr form))))
		      (:assign `(setq ,(cadr form) ,(compile-expr (caddr form))))
		      (:list-push `(list-accum-push ,(cadr form) ,(caddr form)))
		      (:list-reset `(reset-list-accum ,(cadr form)))
		      (:char-push
		       (if (cddr form)
			   `(char-accum-push ,(cadr form) ,(caddr form))
			   `(char-accum-push (read-atom ,ctx) ,(cadr form))))
		      (:char-reset `(reset-char-accum ,(cadr form)))
		      (:debug
		       `(prog1 t
			  ,(if (cadr form)
			       `(format t "DEBUG: ~a: ~a~%" ',(cadr form) ,(cadr form))
			       `(format t "DEBUG: cursor: [~a] `~a'~%"
					(parser-context-cursor ,ctx)
					(elt (parser-context-data ,ctx)
					     (parser-context-cursor ,ctx))))))))
		   ((characterp form) `(match-atom ,ctx ,form))
		   ((stringp form) (compile-expr `(:checkpoint (:and ,@(coerce form 'list)))))
		   (t (compile-expr form nil)))
		 (cond
		   ((and (consp form) (eql 'meta (car form)))
		    (format t "will get compiled: ~a~%" `(:and ,@(cdr form)))
		    (compile-expr `(:and ,@(cdr form))))
		   ((consp form) (compile-exprs form nil))
		   (t form)))))
    (compile-expr form)))


;;; Atom Definitions

(defmacro defatom (name &body body)
  `(progn
     (defun ,name (c) (when c ,@body))
     (deftype ,name () `(satisfies ,',name))))

(defatom ascii?
  (typep c 'standard-char))

(defatom extended?
  (typep c 'extended-char))

(defatom alpha?
  (alpha-char-p c))

(defatom alnum?
  (alphanumericp c))

(defatom graphic?
  (graphic-char-p c))

(defatom upper?
  (upper-case-p c))

(defatom lower?
  (lower-case-p c))

(defatom digit?
  (digit-char-p c))

(defatom bit?
  (or (char= c #\0)
      (char= c #\1)))

(defatom space?
  (char= c #\space))

(defatom newline?
  (char= c #\newline))

(defatom tab?
  (char= c #\tab))

(defatom white-space?
  (or (space? c)
      (tab? c)))


;;; Rule Definitions

(defmacro defrule (name (&rest args) &body body)
  (with-gensyms (ctx)
    `(defun ,name ,(nconc (list ctx) args)
       (block rule-block
	 ,(compile-grammar ctx `(:checkpoint (:and ,@body)))))))