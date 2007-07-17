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
  (checkpoints
   (make-array 8 :element-type 'unsigned-byte :adjustable t :fill-pointer 0)
   :type (vector unsigned-byte *))
  attachment)

(defgeneric create-parser-context (input &rest args))

(defmethod create-parser-context ((input string) &key start end attachment)
  (make-parser-context :data input
		       :cursor (or start 0)
		       :size (or end (length input))
		       :attachment attachment))

(defmethod create-parser-context
    ((input string-stream) &key buffer-size start end attachment)
  (assert (input-stream-p input))
  (let* (size
	 (string
	  (with-output-to-string (output)
	    (loop with buffer-size = (or buffer-size 8192)
		  with buf = (make-string buffer-size)
		  for pos = (read-sequence buf input :end buffer-size)
		  sum pos into size-acc
		  until (zerop pos)
		  do (write-string buf output :end pos)
		  finally (setq size size-acc)))))
    (create-parser-context
     string :start start :end (or end size) :attachment attachment)))

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
  (vector-push-extend (parser-context-cursor ctx) (parser-context-checkpoints ctx)))

(declaim (inline rollback))
(defun rollback (ctx)
  (setf (parser-context-cursor ctx) (vector-pop (parser-context-checkpoints ctx))))

(declaim (inline commit))
(defun commit (ctx)
  (vector-pop (parser-context-checkpoints ctx)))


;;; Atom, Rule & Type Matching

(declaim (inline match-atom))
(defun match-atom (ctx atom &aux (c (peek-atom ctx)))
  (if (and c (char= atom c))
      (read-atom ctx)))

(defmacro match-type (ctx type)
  `(if (typep (peek-atom ,ctx) ',type)
       (read-atom ,ctx)))

(defmacro match-rule (ctx rule args)
  `(,rule ,@(nconc (list ctx) args)))


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
		       (with-unique-names (ret)
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
		      (:render `(,(cadr form) ,@(nconc (list ctx) (cddr form))))
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
		      (:eof `(= (parser-context-cursor ,ctx) (parser-context-size ,ctx)))
		      (:read-atom `(read-atom ,ctx))
		      (:debug
		       `(prog1 t
			  ,(if (cadr form)
			       `(format t "DEBUG: ~a: ~a~%" ',(cadr form) ,(cadr form))
			       `(format t "DEBUG: cursor: [~s] `~s'~%"
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


;;; Atom, Rule & Renderer Definition Macros

(defmacro defatom (name &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name (c) (when c ,@body))
     (deftype ,name () `(satisfies ,',name))))

(defmacro defrule (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       ,(if attachment
	    `(let ((,attachment (parser-context-attachment ,ctx)))
	       (block rule-block
		 ,(compile-grammar ctx `(:checkpoint (:and ,@body)))))
	    `(block rule-block
	       ,(compile-grammar ctx `(:checkpoint (:and ,@body))))))))

(defmacro defrenderer (name (&rest args) (&optional attachment) &body body)
  (with-unique-names (ctx)
    `(defun ,name (,ctx ,@args)
       ,(if attachment
	    `(let ((,attachment (parser-context-attachment ,ctx)))
	       ,@body)
	    `(progn ,@body))
       t)))