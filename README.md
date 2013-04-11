# Overview

meta-sexp is a META parser generator using LL(1) grammars with s-expressions. meta-sexp uses in-memory string vectors, instead of commonly used streams, for efficiently stepping backward and forward through the input. It is tested on SBCL but should be portable to other implementations as well.

meta-sexp is implemented using sevaral transformation methods. Therefore, besides builtin grammar transformators coming with meta-sexp by default, you are allowed to add your own transformation methods too.

Inspired by `src/parser.lisp` of [core-stream project](http://core.gen.tr/).

Idea is based on the META language discussed in [Pragmatic Parsing in Common Lisp](http://home.pipeline.com/~hbaker1/Prag-Parse.html) paper of [Henry G. Baker](http://home.pipeline.com/~hbaker1/home.html).

# Quick Introduction

Most of the time, you'll need to define your own parsers using `CREATE-PARSER-CONTEXT` methods and `DEFRULE`, `DEFRENDERER` macros.

    create-parser-context ((input string) &key start end attachment)
    create-parser-context ((input string-stream) &key buffer-size start end attachment)

    defrule (name (&rest args) (&optional attachment) &body body)
    defrenderer (name (&rest args) (&optional attachment) &body body)

In a rule or renderer body, if supplied, `ATTACHMENT` argument will get bound to `ATTACHMENT` keyword given to `CREATE-PARSER-CONTEXT`.

In some certain situations, you may also need to use `DEFATOM` too. See `atoms.lisp` for `DEFATOM` examples.

Here is a tiny example:

    (defrule integer? (&aux (sign 1) d (num 0)) ()
      (:? (:or (:and "-" (:assign sign -1))
               "+"))
      (:+ (:assign d (:type digit?))
          (:assign num (+ (* num 10)
	                  (- (char-code d) #.(char-code #\0)))))
      (:return (* sign num)))

    (integer? (create-parser-context "+123")) ==> 123
    (integer? (create-parser-context "-123")) ==> -123

Here is another example demonstrating the usage of `META` symbol.

    (defrule in-wonderland? () ()
      "META-SEXP"
      (progn
        (format t "META-SEXP in Wonderland!")
        (meta (:type space?)
  	    "in Wonderland!"))
      (:return t))

    (in-wonderland?
     (create-parser-context "META-SEXP in Wonderland!"))
    META-SEXP in Wonderland!
    ==> T

    (in-wonderland?
     (create-parser-context "META-SEXP in Fooland!"))
    META-SEXP in Wonderland!
    ==> NIL

Here's a complete example with renderers and attachments.

    (defrenderer internal-link! (label &optional text) (attachment)
      (format attachment "<a href='~a'>~a</a>"
              label (if (empty-char-accum-p text) label text)))
    
    (defrule internal-link? (&aux (ref (make-char-accum)) (text (make-char-accum))) ()
      "[["
      (:+ (:not (:or "]]" (:type (or white-space? newline?))))
          (:char-push ref))
      (:? (:* (:type (or white-space? newline?)))
          (:+ (:not "]]")
  	    (:char-push text)))
      "]]"
      (:render internal-link! ref text))
    
    (defrule wiki-markup? (&aux c) (attachment)
      (:* (:or (:rule internal-link?)
               (:and (:assign c (:read-atom))
  	           (write-char c attachment))))
      (get-output-stream-string attachment))

    (wiki-markup?
     (create-parser-context
      "foo bar [[ref text]] and [[just-ref]] here."
      :attachment (make-string-output-stream)))
    ==> "foo bar <a href='ref'>text</a> and <a href='just-ref'>just-ref</a> here."

What's the role of `ATTACHMENT` slot given to `CREATE-PARSER-CONTEXT` (or specified as a keyword while making an instance of `PARSER-CONTEXT` class)? Think it as a state storage unit between passes to defined rules and renderers. (For instance, in our above example, `ATTACHMENT` used as a common output stream.) Yes, it is possible to let this problem get solved by the programmer via global variables. But this approach yields to another problem: thread safety. Anyway, that was the best that I can come up with; if you have any other ideas, I'd be happy to hear them.

Here is an example introducing a new transformation to the grammar:

    (defun callback (string cursor &rest args)
      (format t "Callback at char ~S at position ~D. (Args: ~S)~%"
              (elt string cursor) cursor args))

    (defmethod meta-sexp:transform-grammar
        (ret ctx (in-meta (eql t)) (directive (eql :callback)) &optional args)
      "\(:CALLBACK ARG ...)

    Calls CALLBACK function, where the first two arguments passed to the function
    are the input string and cursor position, and the rest is ARG."
      `(apply #'callback
              (meta-sexp::parser-context-data ,ctx)
              (meta-sexp::parser-context-cursor ,ctx)
              ,@args))

    (defrule integer-debug? () ()
      (:or (:rule integer?)
           (:callback (list :rule :integer-debug?))))

    ; TEST> (integer-debug? (create-parser-context "xy+123"))
    ; Callback at char #\x at position 0. (Args: (:RULE :INTEGER-DEBUG?))
    ; NIL

    ; TEST> (integer-debug? (create-parser-context "+123"))
    ; 123

# Available Type Checkers

These functions (and types) are routines introduced using `DEFATOM` and operates on character codes. In case of need, you can add your own type checkers. (See source for examples.)

    ALNUM? ALPHA? GRAPHIC? ASCII? BIT?
    DIGIT? EXTENDED? LOWER? NEWLINE?
    SPACE? TAB? UPPER? WHITE-SPACE?

# Built-In Transformations

`(:ICASE FORM FORM ...)`
> Make case-insensitive atom comparison in supplied `FORM`s.

`(:CHECKPOINT FORM)`
> Sequentially evaluates supplied forms and if any of them fails, moves cursor back to its start position `:CHECKPOINT` began.

`(:AND FORM FORM ...)<br/>
(:OR FORM FORM ...)<br/>
(:NOT FORM)`
> Identical to `(NOT FORM)`. (`FORM` is encapsulated within a `:CHECKPOINT` before getting evaluated.)

`(:RETURN VALUE VALUE ...)`
> Returns from the rule with supplied `VALUE`s.

`(:RENDER RENDERER ARG ARG ...)`
> Calls specified `RENDERER` (that is defined with `DEFRENDERER`) with supplied arguments.

`(:? FORM FORM ...)`
> Sequentially evaluates supplied `FORM`s within an `AND` scope and regardless of the return value of `AND`ed `FORM`s, block returns `T`. (Similar to `?` in regular expressions.)

`(:* FORM FORM ...)`
> Sequentially evaluates supplied `FORM`s within an `AND` scope until it returns `NIL`. Regardless of the return value of `AND`ed `FORM`s, block returns `T`. (Similar to `*` in regular expressions.)

`(:+ FORM FORM ...)`
> Sequentially evaluates supplied `FORM`s within an `AND` scope, and repeats this process till `FORM`s return `NIL`. Scope returns `T` if `FORM`s returned `T` once or more, otherwise returns `NIL`. (Similar to `{1,}` in regular expressions.)

`(:TYPE TYPE-CHECKER)<br/>
(:TYPE (OR TYPE-CHECKER TYPE-CHECKER ...))`
> Checks type of the atom at the current position through supplied function(s).

`(:RULE RULE ARG ARG ...)<br/>
(:RULE (OR RULE RULE ...) ARG ARG ...)`
> Tests input in the current cursor position using specified type/form. If any, supplied arguments will get passed to rule.

`(:ASSIGN VAR FORM)<br/>
(:ASSIGN (VAR1 VAR2 ...) FORM)`
> Assigns returned value of `FORM` to `VAR`, and returns assigned value. (Latter form works similar to `MULTIPLE-VALUE-SETQ`.)

`(:LIST-PUSH ITEM-VAR LIST-ACCUM)<br/>
(:CHAR-PUSH CHAR-VAR CHAR-ACCUM)<br/>
(:CHAR-PUSH CHAR-ACCUM)`
> Pushes supplied `ITEM-VAR`/`CHAR-VAR` into specified `LIST-ACCUM`/`CHAR-ACCUM`. If `:CHAR-PUSH` is called with only one argument, current character gets read and pushed into supplied accumulator. (You can use `MAKE-LIST-ACCUM` and `MAKE-CHAR-ACCUM` functions to initialize new accumulators. Moreover, you'll probably need `EMPTY-LIST-ACCUM-P` and `EMPTY-CHAR-ACCUM-P` predicates too.)

`(:LIST-RESET LIST-ACCUM)<br/>
(:CHAR-RESET CHAR-ACCUM)`
> Resets supplied accumulators.

`(:EOF)`
> Returns true when reached to the end of supplied input data.

`(:READ-ATOM)`
> Reads current atom at the cursor position and returns read atom.

`(:DEBUG)<br/>
(:DEBUG VAR)`
> Prints current character and its position in the input data. If `VAR` is specified, prints the value of the `VAR`.

If a form doesn't start with any of the above keywords, there're three possiblities remaining:

1. This can be a character.
2. This can be a string. (Will get expanded into an `AND`ed character list with an outermost `:CHECKPOINT`.)
3. Treat as a custom form. (Will get evaluated as is.)

When you're in the third situation, to be able to get your META s-expressions compiled again, use `META` keyword. (See the second example in the Quick Introduction.)

# Introducing New Transformations

Every transformation process issued by meta-sexp is controlled by `TRANSFORM-GRAMMAR` methods.

    (defgeneric transform-grammar (ctx in-meta directive &optional args)
      (:documentation "META grammar transformation methods."))

To introduce a new transformation directive, just create a new `TRANSFORM-GRAMMAR` method with related lambda list specializers. For
instance, consider how `:AND` and `:NOT` directive transformations are implemented:

    (defmethod transform-grammar
        (ctx (in-meta (eql t)) (directive (eql :and)) &optional args)
      `(and ,@(mapcar #'(lambda (form) (transform-grammar ctx t form))
                      args)))
    
    (defmethod transform-grammar
        (ctx (in-meta (eql t)) (directive (eql :not)) &optional args)
      (transform-grammar
       ctx t :checkpoint
       `((not ,(transform-grammar ctx t (car args))))))

Also pay attention how meta-sexp handles unrecognized transformation directives:

    (defmethod transform-grammar (ctx in-meta directive &optional args)
      "The most unspecific transformation method."
      (declare (ignore args))
      (cond
        ((and in-meta (consp directive) (keywordp (car directive)))
         (transform-grammar ctx t (car directive) (cdr directive)))
        ((and (not in-meta) (consp directive) (eql 'meta (car directive)))
         (transform-grammar ctx t :and (cdr directive)))
        ((consp directive)
         (mapcar #'(lambda (form) (transform-grammar ctx nil form))
                           directive))
        (t directive)))
     
With similar patterns, you can introduce new transformation directives to meta-sexp.
