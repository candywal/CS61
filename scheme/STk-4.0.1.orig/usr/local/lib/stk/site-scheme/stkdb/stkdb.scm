;; -*-Scheme-*-

;;;; UCB STk Debugger.
;;;;   Runtime support

;;;; Author P. N. Hilfinger

;;; This module provides support for debugging Scheme programs under STk.
;;; Specfically, this includes
;;;  a. Functions for annotating Scheme definitions with "hooks" that allow
;;;     setting breakpoints and examining stack frames.
;;;  b. A top-level command interpreter to allow users to enter debugging
;;;     commands.
;;;  c. Hooks into STk to intercept and report errors.
;;;  d. Provisions to output specially code strings that the stkdb.el 
;;;     Emacs package can interpret.

;;; The stk-debugger module contains the bulk of the implementation.  Your
;;; version of STk must use the SLIB Scheme library.  

;;; Before loading this module, the variable stkdb-vicinity must be 
;;; defined to be the prefix of all the paths of stkdb-related Scheme 
;;; files.  It defaults to a subdirectory "stkdb/" within the 
;;; SLIB library-vicinity.

(require "slib")
(require 'line-i/o)

(if (not (symbol-bound? 'stkdb-vicinity))
    (define stkdb-vicinity "/usr/local/lib/stk/site-scheme/stkdb/"))

(if (or (eq? stkdb-vicinity '())
	(string=? stkdb-vicinity ""))
    (set! stkdb-vicinity (string-append (library-vicinity) "stkdb/")))

(load (string-append stkdb-vicinity "pexpr.scm"))
(load (string-append stkdb-vicinity "read.scm"))

;; True iff the debugger is active.
(define stkdb:active? #f)

(define-module stk-debugger

  (export stkdb:bpchk stkdb:retchk stkdb:id stkdb:push 
	  stkdb:debug-file stkdb:instrument-file
	  stkdb:set-bp! stkdb:clear-bp stkdb:clear-all-bps! stkdb:reset
	  stkdb:set-bp-func! stkdb:set-bp-cond! stkdb-clear-bps-func!
	  stkdb:set-options!
	  stkdb:eval-loop)

  ;; Send message to error port.  Arguments as for 2nd and later 
  ;; arguments of format.
  (define (mesg . args) 
    (apply format (current-error-port) args))

  ;; Compensate for some berkeley Scheme weirdness.
  (define read-line (with-module Scheme read-line))

  ;; Maximum number of stack frames to show in traces.
  (define env-stack-limit 50)
  ;; Maximum number of local variables to display in an 'info locals'
  (define local-var-list-limit 20)

  ;; The Ctrl-Z character, used to communicate from Emacs to Scheme.
  (define OUTPUT-MARK (string (integer->char 26)))

  ;; The STk undefined value.
  (define UNDEFINED-VALUE (if #f #t))


  ;;;; SOURCE POSITIONS

  ;;; We denote source locations in instrumented code with integer indices 
  ;;; the vector all-positions. 
  (define all-positions (make-vector 1024))
  ;;; >= highest index of all-positions that has a valid entry.  Used for
  ;;; efficiency purposes only.
  (define high-water -1)
  
  ;;; Each slot in all-positions contains a "source position": a vector of 
  ;;; the form
  ;;;    #(start-line start-col end-line end-col filename breakpoint?)
  ;;; Indicating the range of text positions covered by a single expression.
  ;;; The column numbers take tabs into account (i.e, a tab may represent 1-8
  ;;; spaces, depending on position).  The breakpoint? field is set to indicate
  ;;; that the program is to break before evaluating this indicated expression.
  ;;; KNOWN WEAKNESS: What about expressions spread across >1 file?  Probably
  ;;;    not an important case.

  ;;; Unless otherwise indicated, an argument that is a "source position" 
  ;;; may either be an index or a source-position vector.


  ;; "Nowhere"
  (define null-posn (vector 0 0 0 0 "<none>" #f #f #t))

  ;; If POSN is an integer index, convert to the corresponding source
  ;; position.  Idempotent.
  (define (force-posn posn)
    (cond ((vector? posn) posn)
	  ((number? posn) 
	   (let ((vposn (vector-ref all-positions posn)))
	     (if (vector? vposn) vposn null-posn)))
	  (else null-posn)))

  ;;;; Accessors for source positions:

  (define (start-line posn)
    (vector-ref (force-posn posn) 0))

  (define (start-col posn)
    (vector-ref (force-posn posn) 1))

  (define (end-line posn)
    (vector-ref (force-posn posn) 2))

  (define (end-col posn)
    (vector-ref (force-posn posn) 3))

  (define (src-file posn)
    (vector-ref (force-posn posn) 4))

  (define (breakpointed? posn)
    (vector-ref (force-posn posn) 5))

  (define (set-break! posn val)
    (let ((p (force-posn posn)))
      (cond ((eq? (breakpointed? p) val) #f)
	    (else
	     (set-breakpoint-condition! p #t)
	     (vector-set! p 5 val)))))

  (define (breakpoint-condition posn)
    (vector-ref (force-posn posn) 7))

  (define (set-breakpoint-condition! posn condition)
    (vector-set! (force-posn posn) 7 condition))

  (define (src-func posn)
    (vector-ref (force-posn posn) 6))

  ;; A source-position index for file FILE from line and column LINE0, COL0
  ;; to LINE1, COL1, with function name FUNC (or #f).
  (define (get-posn file line0 col0 line1 col1 func)
    (let ((p (let loop ((k 0))
	       (cond ((> k high-water)
		      (begin (set! high-water (+ high-water 1))
			     (if (>= high-water (vector-length all-positions))
				 (vector-resize all-positions 
						(* 2 (vector-length all-positions))))
			     high-water))
		     ((not (vector-ref all-positions k)) k)
		     (else (loop (+ k 1)))))))
      (vector-set! all-positions p 
		   (vector line0 col0 line1 col1 file #f func #t))
      p))

  ;; A source position consisting of the same lines as indicated by POSN,
  ;; but covering all columns of those lines
  (define (widen-to-line posn) 
    (let ((fposn (force-posn posn)))
      (vector (start-line fposn) -1 
	      (end-line fposn) (max 4096 (end-col fposn))
	      (src-file fposn) #f (src-func fposn) #t)))

  ;; Invalidate all source position indices for file FILE.
  (define (clear-file-posns! file start end) 
    (let loop ((k 0))
      (if (<= k high-water)
	  (let
	      ((posn (force-posn k)))
	    (if (and (not (eq? posn null-posn))
		     (string=? (src-file posn) file)
		     (or (not start)
			 (not (start-line posn))
			 (<= start (start-line posn)))
		     (or (not end)
			 (not (end-line posn))
			 (>= end (end-line posn))))
		(vector-set! all-positions k #f))
	    (loop (+ k 1))))))

  ;; True if the text denoted by POSN0 does not overlap POSN1.
  (define (outside? posn0 posn1)
    (or (not (string=? (src-file posn0) (src-file posn1)))
	(> (start-line posn0) (end-line posn1))
	(and (= (start-line posn0) (end-line posn1))
	     (> (start-col posn0) (end-col posn1)))
	(< (end-line posn0) (start-line posn1))
	(and (= (end-line posn0) (start-line posn1))
	     (< (end-col posn0) (start-col posn1)))))


  ;;;; INSTRUMENTATION
  
  ;;; In the following, a "pexpr" is an s-expression 
  ;;; augmented with source position information (see pexpr.scm).

  ;;; At the moment, we instrument only expressions inside of (implicit
  ;;; or explicit) lambdas and define-classes.  Outer-level non-definitional
  ;;; expressions (including load, require, provide, etc.) are not modified.

  ;;; Our strategy is to convert each non-tail-recursive expression E into
  ;;;    (let ((stkdb:posn WHERE))
  ;;;        (stkdb:bpchk stkdb:sp WHERE)
  ;;;        (stkdb:id (stkdb:retchk stkdb:sp WHERE E')))
  ;;; and each tail-recursive expression E into
  ;;;    (let ((stkdb:posn WHERE))
  ;;;        (stkdb:bpchk stkdb:sp WHERE)
  ;;;        E')
  ;;; where E' is the translation of E, and WHERE is the source-position index
  ;;; of E.  Each lambda expression (lambda ARGS BODY) then becomes
  ;;;    (lambda ARGS 
  ;;;       (let ((stkdb:sp (stkdb:push))
  ;;;	          (stkdb:posn #f))
  ;;;         BODY'))
  ;;; where BODY' is the translation of BODY, and NAME is the name 
  ;;; of this function (if there is one, else <anonymous-lambda>).
  ;;; 
  ;;; The %get-environment-stack function in STk allows us to retrieve the 
  ;;; stack of environments correspoinding to the active evaluation stack 
  ;;; at a breakpoint or error.  We can then look for bindings of
  ;;; stkdb:sp and stkdb:posn to find our way around.  

  ;; The instrumented version of PEXPR.  Unless REMOVE-TAIL-RECURSION?, 
  ;; add code to catch return values from tail-recursive calls.  This
  ;; changes the behavior of the program, in the sense that tail-
  ;; recursive loops will use up additional stack space on each
  ;; iteration.
  (define (instrument pexpr remove-tail-recursion?)
    (let ((current-function-name #f) ;; Set with fluid-let.
	  (current-class-name #f))   ;; Set with fluid-let.

      ;; The instrumented version of PEXPR.  TAIL? should be true iff 
      ;; PEXPR appears in a tail-recursive position.
      (define (instrument-expr pexpr tail?) 
	(cond ((psd-pair? pexpr)
	       (case (pexp->sexp (psd-car pexpr))
		 ((lambda) (instrument-lambda (psd-car (psd-cdr pexpr))
					      (psd-cdr (psd-cdr pexpr))
					      '<anonymous-lambda>))
		 ((define) (instrument-define pexpr))
		 ((begin) (instrument-begin pexpr tail?))
		 ((cond) (instrument-cond pexpr tail?))
		 ((if) (instrument-if pexpr tail?))
		 ((or and) (instrument-logical pexpr tail?))
		 ((case) (instrument-case pexpr tail?))
		 ((do) (instrument-do pexpr tail?))
		 ((quasiquote) (instrument-quasiquote pexpr tail?))
		 ((quote) (pexp->sexp pexpr))
		 ((let letrec let* letrec*) (instrument-let pexpr tail?))
		 ((define-class) (instrument-define-class pexpr))
		 (else 
		  (let ((texpr (cons (instrument-expr (psd-car pexpr) #f) 
				     (instrument-expr-list (psd-cdr pexpr) 
							   #f))))
		    (if current-function-name
			(if (and tail? remove-tail-recursion?)
			    (marked-expr-tail pexpr texpr)
			    (marked-expr-non-tail pexpr texpr))
			texpr)))))
	      (else (pexp->sexp pexpr))))

      ;; Assuming TEXPR is the result of converting all subexpressions of
      ;; PEXPR, returns the instrumented version of PEXPR when it appears in 
      ;; a non-tail-recursive position.
      (define (marked-expr-non-tail pexpr texpr)
	(let ((where (get-posn (psd-expr-start-file pexpr) 
			       (psd-expr-start-line pexpr)
			       (psd-expr-start-column pexpr)
			       (psd-expr-end-line pexpr)
			       (psd-expr-end-column pexpr)
			       current-function-name)))
	  `(let ((stkdb:posn ,where))
	     (stkdb:bpchk stkdb:sp ,where)
	     (stkdb:id (stkdb:retchk stkdb:sp ,where ,texpr)))))

      ;; Assuming TEXPR is the result of converting all subexpressions of
      ;; PEXPR, returns the instrumented version of PEXPR when it appears in 
      ;; a tail-recursive position.
      (define (marked-expr-tail pexpr texpr)
	(let ((where (get-posn (psd-expr-start-file pexpr) 
			       (psd-expr-start-line pexpr)
			       (psd-expr-start-column pexpr)
			       (psd-expr-end-line pexpr)
			       (psd-expr-end-column pexpr)
			       current-function-name)))
	  `(let ((stkdb:posn ,where))
	     (stkdb:bpchk stkdb:sp ,where) 
	     ,texpr)))

      ;; Given PEXPR, a list (E1 E2 ... En) of expressions (with source 
      ;; positions), the list (E1' E2' ... En') of instrumented equivalents.
      ;; En is treated as a tail-recursive position iff TAIL?
      (define (instrument-expr-list pexpr tail?) 
	(cond ((psd-null? pexpr) '())
	      ((psd-null? (psd-cdr pexpr))
	       (list (instrument-expr (psd-car pexpr) tail?)))
	      (else (cons (instrument-expr (psd-car pexpr) #f)
			  (instrument-expr-list (psd-cdr pexpr) tail?)))))

      ;; The instrumented version of (lambda ARGS BODY), where ARGS and BODY 
      ;; are pexprs.  NAME is used as the name of the function (used by the 
      ;; debugger, and otherwise irrelevant to execution).
      (define (instrument-lambda args body name)
	(fluid-let ((current-function-name name))
	  `(lambda ,(pexp->sexp args)
	     (let ((stkdb:sp (stkdb:push))
		   (stkdb:posn #f))
	       ,@(instrument-expr-list body #t)))))

      ;; The instrumented version of PEXPR, assuming it is a 'define' special 
      ;; form.
      (define (instrument-define pexpr)
	(let ((defined (psd-cadr pexpr))
	      (definition (psd-cddr pexpr)))
	  (cond ((psd-pair? defined)  
					; (define (foo ...) ...)
		 (let ((name (pexp->sexp (psd-car defined))))
		   `(define ,name
		      ,(instrument-lambda (psd-cdr defined) definition name))))
		((and (psd-pair? definition)  
		      (psd-pair? (psd-car definition))
		      (psd-null? (psd-cdr definition))
		      (eqv? (pexp->sexp (psd-caar definition)) 'lambda))
					; (define foo (lambda...))
		 (let ((name (pexp->sexp defined))
		       (args (psd-cadar definition))
		       (body (psd-cddar definition)))
		   `(define ,name 
		      ,(instrument-lambda args body name))))
		(else `(define ,(pexp->sexp defined)
			 ,@(instrument-expr-list definition #t))))))

      ;; The instrumented version of PEXPR, assuming it is a 'begin' special 
      ;; form.
      (define (instrument-begin pexpr tail?)
	`(begin ,@(instrument-expr-list (psd-cdr pexpr) tail?)))

      ;; The instrumented version of PEXPR, assuming it is a 'cond' special 
      ;; form.
      (define (instrument-cond pexpr tail?) 
	`(cond ,@(psd-map 
		  (lambda (x) (instrument-expr-list x tail?)) 
		  (psd-cdr pexpr))))

      ;; The instrumented version of PEXPR, assuming it is an 'if' special 
      ;; form.
      (define (instrument-if pexpr tail?)
	(let ((test (psd-car (psd-cdr pexpr)))
	      (then (psd-car (psd-cdr (psd-cdr pexpr))))
	      (opt-else (psd-cdr (psd-cdr (psd-cdr pexpr)))))
	  (marked-expr-tail pexpr 
	       (if (psd-null? opt-else)
		   `(if ,(instrument-expr test #f)
			,(instrument-expr then tail?))
		   `(if ,(instrument-expr test #f)
			,(instrument-expr then tail?)
			,(instrument-expr (psd-car opt-else) tail?))))))
      
      ;; The instrumented version of PEXPR, assuming it is an 'and' or
      ;; 'or' special form.
      (define (instrument-logical pexpr tail?)
	(marked-expr-tail pexpr
			  (cons (pexp->sexp (psd-car pexpr))
				(psd-map (lambda (x) (instrument-expr x tail?))
					 (psd-cdr pexpr)))))

      ;; The instrumented version of PEXPR, assuming it is a 'case' special 
      ;; form.
      (define (instrument-case pexpr tail?)
	(let ((expr (psd-car (psd-cdr pexpr)))
	      (clauses (psd-cdr (psd-cdr pexpr))))
	  (marked-expr-tail 
	   pexpr
	   `(case ,(instrument-expr expr #f)
	      ,@(psd-map (lambda (x) (cons (pexp->sexp (psd-car x))
					   (instrument-expr-list (psd-cdr x) 
								 tail?)))
			 clauses)))))

      ;; The instrumented version of PEXPR, assuming it is a 'let', 'letrec'
      ;; or 'let*' special form.
      (define (instrument-let pexpr tail?)
	(define (instrument-binding binding)
	  (if (psd-pair? binding)
	      (cons (pexp->sexp (psd-car binding))
		    (instrument-expr-list (psd-cdr binding) #f))
	      (pexp->sexp binding)))
	(define (instrument-parts prefix bindings body)
	  `(,@prefix
	    ,(psd-map instrument-binding bindings)
	    ,@(instrument-expr-list body tail?)))

	
	;; Body of instrument-let

	(if (psd-pair? (psd-car (psd-cdr pexpr)))
	    (instrument-parts (list (pexp->sexp (psd-car pexpr)))
			      (psd-car (psd-cdr pexpr))
			      (psd-cdr (psd-cdr pexpr)))
	    (instrument-parts (list (pexp->sexp (psd-car pexpr))
				    (pexp->sexp (psd-car (psd-cdr pexpr))))
			      (psd-car (psd-cdr (psd-cdr pexpr)))
			      (psd-cdr (psd-cdr (psd-cdr pexpr))))))

      ;; The instrumented version of PEXPR, assuming it is a 'do' special 
      ;; form.
      (define (instrument-do pexpr tail?)
	(define (do-binding x)
	  `(,(pexp->sexp (psd-car x)) ,@(instrument-expr-list (psd-cdr x) #f)))

	(let ((bindings (psd-car (psd-cdr pexpr)))
	      (test (psd-car (psd-cdr (psd-cdr pexpr))))
	      (body (psd-cdr (psd-cdr (psd-cdr pexpr)))))
	  `(do ,(psd-map do-binding bindings)
	       ,(instrument-expr-list test tail?)
	     ,@(instrument-expr-list body #f))))

      ;; The instrumented version of PEXPR, assuming it is a 'quasiquote' 
      ;; special form (`...).  At the moment, this treats the entire 
      ;; expression as one subexpression, with no internal breakpoints.
      (define (instrument-quasiquote pexpr tail?)
	(if (and tail? remove-tail-recursion?)
	    (marked-expr-tail pexpr (pexp->sexp pexpr) )
	    (marked-expr-non-tail pexpr (pexp->sexp pexpr))))

      (define (instrument-define-class pexpr)
	(let ((header (pexp->sexp (psd-cadr pexpr))))
	  (fluid-let ((current-class-name (symbol->string (car header))))
	  `(define-class ,header
	   ,@(psd-map instrument-class-member (psd-cddr pexpr))))))

      (define (instrument-class-member pexpr) 
	(if (psd-pair? pexpr)
	    (let ((args (psd-cdr pexpr))
		  (op (pexp->sexp (psd-car pexpr))))
	      (cons 
	       op
	       (case op
		 ((instance-vars class-vars parent) 
		  ;; Not really handled yet
		  (pexp->sexp args))
		 ((default-method initialize)
		  (fluid-let ((current-function-name 
			       (string->symbol 
				(string-append current-class-name "." 
					       (symbol->string op)))))
		    `((let ((stkdb:sp (stkdb:push))
			    (stkdb:posn #f))
			,@(instrument-expr-list args #t)))))
		 ((method)
		  (let ((header (pexp->sexp (psd-car args)))
			(body (psd-cdr args)))
		    (fluid-let ((current-function-name 
				 (string->symbol 
				  (string-append 
				   current-class-name "."
				   (symbol->string (car header))))))
		      `(,header 
			(let ((stkdb:sp (stkdb:push))
			      (stkdb:posn #f))
			  ,@(instrument-expr-list body #t)))))))))
	    (pexp->sexp pexpr)))
			
      ;;; Body of instrument
      (instrument-expr pexpr #f)))

  ;; Instrument the contents of the file name SOURCE-FILE,
  ;; outputting the result into a file named INSTRUMENTED-FILE.
  ;; OPTIONS is a list flags that modify compilation.  The only
  ;; recognized flag is the symbol no-tail-recursion, which causes
  ;; the instrumented code to intercept all expression results, even
  ;; those in tail-recursive positions. Unknown options are ignored.
  ;; The contents of an instrumented file are only valid for a
  ;; single session, and then only until the next execution of
  ;; stkdb:instrument-file on the same source file.
  (define (stkdb:instrument-file source-file instrumented-file options)
    (let ((infile (open-input-file source-file))
	  (outfile (open-output-file instrumented-file))
	  (remove-tail-recursion? 
	   (not (get-keyword :keep-tail-recursion options #f)))
	  (start-line (get-keyword :start-line options #f))
	  (end-line (get-keyword :end-line options #f)))
      (clear-file-breakpoints! source-file start-line end-line)
      (dynamic-wind
	  (lambda () #f)
	  (lambda () 
	    (psd-reset-read)
	    (let loop ((expr (psd-read infile source-file)))
	      (if (eof-object? expr)
		  (newline outfile)
		  (begin
		    (write (instrument expr remove-tail-recursion?) outfile)
		    (newline outfile)
		    (loop (psd-read infile source-file))))))
	  (lambda ()
	    (close-input-port infile)
	    (close-output-port outfile)))))


  ;; Instrument and load the file named FILENAME.   Temporarily
  ;; creates a temporary file for the instrumented code.  Trailing
  ;; arguments are treated as compiler flags; see stkdb:instrument-file.
  (define (stkdb:debug-file filename . options)
    (let ((tmp-file (temporary-file-name)))
      (dynamic-wind
	  (lambda () #f)
	  (lambda () 
	    (stkdb:instrument-file filename tmp-file options)
	    (load tmp-file))
	  (lambda () (catch (remove-file tmp-file))))))

  ;;;; EXPORTED FUNCTIONS USED IN INSTRUMENTED SOURCE CODE.

  ;; Allocate and return an sp.
  (define (stkdb:push)
    (set! next-free-sp (+ next-free-sp 1))
    next-free-sp)

  ;; Annouces to stkdb that execution is at source position POSN with 'stack
  ;; pointer' SP.  Causes a breakpoint if appropriate; otherwise does nothing.
  (define (stkdb:bpchk sp posn)
    (top-level (should-stop? sp posn)))

  ;; Annouces to stkdb that execution has just finished for source 
  ;; position POSN with 'stack pointer' SP.  Causes a breakpoint 
  ;; if appropriate; otherwise does nothing.
  (define (stkdb:retchk sp posn val)
    (if (should-stop-when-returned-to? sp posn)
	(top-level-val val))
    val)

  ;; An identity function. This is used around stkdb:retchk calls to prevent
  ;; the latter from being evaluated tail-recursively.
  (define (stkdb:id x) x)


  ;;;; DEBUGGING EXECUTION STATE

  ;; True while executing a 'finish' command
  (define running-until-finish? #f)
  ;; True while executing a 'step' command
  (define doing-step? #f)
  ;; True while executing a 'next' command
  (define doing-next? #f)

  ;; Set debugger state to indicate stepping if STEP?, step-to-next if
  ;; NEXT?, and run-to-finish if FINISH?.
  (define (set-stepping! step? next? finish?)
    (set! running-until-finish? finish?)
    (set! doing-step? step?)
    (set! doing-next? next?))

  ;;; Each call to an instrumented lambda allocates an integer that serves
  ;;; to mark that frame.  We call these ``stack pointers'' (sp), for short.
  ;;; The markers increase montonically, though they may be reset to 0 when
  ;;; we know a user expression evaluation is complete.  

  ;; The next available sp value.
  (define next-free-sp 0)

  ;; The source position index of an expression within which we should not
  ;; break while in the frame whose sp is last-sp.
  (define step-over-posn #f)
  ;; The sp of the last frame in which we stopped for any reason.
  (define last-sp #f)
  ;; The sp of the frame from which we last executed a 'finish' command.
  (define finish-level #f)

  ;; The continuation to use to exit from the current debugging level.
  (define exit-continuation #f)

  ;; True iff we should stop at source position POSN, given that the current
  ;; frame's 'stack pointer' is SP.  Returns a reason: step for stepping, 
  ;; next for end of a next or finish, bpt for breakpoint.
  (define (should-stop? sp posn) 
    (cond ((not stkdb:active?) #f)
	  ((and running-until-finish? (< sp finish-level))
	   'next)
	  (doing-step? 'step)
	  ((and doing-next? 
		(or (< sp last-sp)
		    (and (eqv? sp last-sp) 
			 (outside? posn step-over-posn))))
	   'next)
	  ((and (breakpointed? posn)
		(or (outside? posn step-over-posn)
		    (not (eqv? sp last-sp))))
	   'bpt)
	  (else #f)))

  ;; True iff we should stop after finishing execution of the expression 
  ;; at source position POSN, in the frame whose 'stack pointer' is SP.
  ;; Returns the "reason" 'ret for true, indicating value returned.
  (define (should-stop-when-returned-to? sp posn) 
    (cond ((not stkdb:active?) #f)
	  ((and running-until-finish? (< sp finish-level))
	   'ret)
	  (doing-step? 'ret)
	  ((and doing-next? 
		(or (< sp last-sp)
		    (and (eqv? sp last-sp) 
			 (outside? posn step-over-posn))))
	   'ret)
	  (else #f)))


  ;;;; COMMUNICATION WITH EMACS 

  ;;; Emacs highlights up to one "current expression" at a time and may keep
  ;;; a window with a stack trace and some local variables (from 'bt' and
  ;;; 'info locals'.

  ;; Set to true if being run under Emacs.  Set with :emacs option of
  ;; stkdb:set-options!
  (define emacs-mode? #f)

  ;; Unhighlight current expression.
  (define (undisplay-posn)
    (if emacs-mode?
	(begin
	  (format (current-output-port) "~a~aR::::::" 
		  OUTPUT-MARK OUTPUT-MARK)
	  (flush (current-output-port)))))

  ;; Highlight the expression at source position PC.  The symbol
  ;; TYPE indicates why:
  ;;     bpt:   Breakpoint hit
  ;;     next:  End of 'next'
  ;;     step:  End of 'step'
  ;;     ret:   Return from function.
  ;;     frm:   Non-innermost frame displayed by 'up', 'down', or 'frame'
  ;;     intr:  Interrupt (^C, SIGINT).
  ;;     err:   Error.
  (define (display-posn pc type)
    (if emacs-mode? 
	(begin
	  (format (current-output-port)
		  "~a~a~a:~a:~a:~a:~a:~a:" 
		  OUTPUT-MARK OUTPUT-MARK 
		  (case type
		    ((bpt next step intr) 'b)
		    ((ret) 'r)
		    ((frm) 'f)
		    ((err) 'e))
		  (src-file pc) (start-line pc) (start-col pc)
		  (end-line pc) (+ (end-col pc) 1))
	  (flush (current-output-port)))))

  ;; Display stack-trace information for frame number FRAME-NUM (0 innermost),
  ;; source position PC, in function NAME, given that CURRENT-FRAME is 
  ;; the current frame (the one being highlighted).  Trace lines are to be
  ;; sent in increasing order by FRAME-NUM.
  (define (display-trace pc name frame-num current-frame)
    (if emacs-mode?
	(begin
	  (format (current-output-port) 
		  "~a~at:~a:~a:\"~a\":~a:~a:"
		  OUTPUT-MARK OUTPUT-MARK 
		  (src-file pc) (start-line pc) name frame-num current-frame)
	  (flush (current-output-port)))))

  ;; Clear the stack-frame backtrace data.
  (define (display-frame-mark current-frame)
    (if emacs-mode?
	(begin
	  (format (current-output-port) 
		  "~a~at:::\"\"::~a:"
		  OUTPUT-MARK OUTPUT-MARK current-frame)
	  (flush (current-output-port)))))

  ;; Display the local variables LOCALS for frame number FRAME-NUM.  LOCALS
  ;; is an alist of variable-name/value pairs.
  (define (display-vars locals frame-num)
    (if emacs-mode?
	(begin
	  (format (current-output-port) "~a~aV:~a:~a~a" 
		  OUTPUT-MARK OUTPUT-MARK frame-num 
		  OUTPUT-MARK OUTPUT-MARK)
	  (let loop ((L locals))
	    (cond ((null? L)
		   (flush (current-output-port)))
		  (else
		   (format (current-output-port)
			   "~a~aV:~a:~a~a~a~a"
			   OUTPUT-MARK OUTPUT-MARK frame-num
			   (caar L) OUTPUT-MARK (cdar L) OUTPUT-MARK)
		   (loop (cdr L))))))))

  ;;;; DEBUGGER TOP LEVEL 

  ;; Values may be 'bt (display backtrace on stops), 'bt-and-top (display
  ;; backtrace and innermost locals on stop), 'bt-and-locals (display
  ;; backtrace and all locals on stop), or #f (no auto-display). "Stops" 
  ;; here are breakpoints or errors, or after step, next, up, down, or frame.
  (define display-on-stop #f)
  ;; Number of prompts to suppress. Used in emacs-mode to prevent 
  ;; useless prompts.
  (define suppress-prompt-count 0)
  ;; The name of the module at the time the debugger was started.
  (define outer-module-name "")
  ;; When stepping, show all values returned by expressions (not just 
  ;; expressions exited by "finish")
  (define show-all-returns? #f)

  ;; Decrement suppress-prompt-count and return #t iff it was originally
  ;; a positive number.
  (define (decr-suppress-prompts!) 
    (let ((val (<= suppress-prompt-count 0)))
      (set! suppress-prompt-count (max 0 (- suppress-prompt-count 1)))
      val))

  ;; True iff next prompt is to be suppressed.
  (define (suppress-prompt?)
    (> suppress-prompt-count 0))

  ;; Suppress prompt an additional N times.
  (define (suppress-prompts! n)
    (set! suppress-prompt-count (+ n suppress-prompt-count)))

  ;; Clear prompt suppression.
  (define (unsuppress-prompts! n)
    (set! suppress-prompt-count 
	  (max 0 (- suppress-prompt-count (or n suppress-prompt-count)))))

  ;; Print a prompt (if not suppressed) and return the next s-expression from
  ;; the current input.
  (define (prompt)
    (if (decr-suppress-prompts!)
	(mesg "~A[~A] " outer-module-name (or debugging-level "-")))
    (flush (current-error-port))
    (read normal-input))

  ;; Given an enviroment stack ENV-STACK as created by 
  ;;   (%get-environment-stack),
  ;; return a vector of up to env-stack-limit environments, indexed by
  ;; frame number (as it appears in back traces).
  (define (env-stack-list->vector env-stack)
    (let ((result (make-vector env-stack-limit #f)))
      (let loop ((posn 0)
		 (prev-sp #f)
		 (L env-stack))
	(cond ((>= posn env-stack-limit) result)
	      ((null? L) (vector-resize result posn))
	      ((and (symbol-bound? 'stkdb:sp (car L))
		    (not (eqv? prev-sp (eval 'stkdb:sp (car L)))))
	       (vector-set! result posn (car L))
	       (loop (+ posn 1) (eval 'stkdb:sp (car L)) (cdr L)))
	      (else (loop posn prev-sp (cdr L)))))))
  
  ;; Given an environment FRAME that corresponds to "stack pointer" SP,
  ;; return an alist associating up to LIM variables with their values,
  ;; excluding those used by stkdb for special purposes and including only
  ;; those in frames with the same value of SP.  
  ;; [NOTE: STk's API here is extremely inefficient, as it does not allow
  ;; us to convert a single local environment to an alist.] 
  (define (frame->local-alist frame sp lim)
    (define (framelet->local-alist framelet lim rest)
      (let loop ((result rest)
		 (L framelet)
		 (n 0))
	(cond ((null? L) result)
	      ((> n lim) (cons '(... . "") result))
	      ((memq (caar L) '(stkdb:sp stkdb:posn))
	       (loop result (cdr L) n))
	      (else (loop (cons (car L) result) (cdr L) (- n 1))))))
    (let loop ((L (environment->list frame))
	       (env frame)
	       (result '())
	       (max-left lim))
      (cond ((or (< max-left 0) (null? L)) (reverse result))
	    ((or (not (symbol-bound? 'stkdb:sp env))
		 (not (eqv? sp (eval 'stkdb:sp env))))
	     (reverse (framelet->local-alist (car L) max-left result)))
	    (else
	     (let ((oneframe 
		    (framelet->local-alist (car L) max-left result)))
	       (loop (cdr L) (parent-environment env)
		     oneframe (- lim (length oneframe))))))))

  ;; Enter the debugger read-eval-print loop for reason WHY---symbol
  ;; init, step, next, bpt, err, or ret.  init is used for starting the 
  ;; debugger initially.  For the rest, see should-stop? and 
  ;; should-stop-when-returned-to?, above.
  (define (top-level why)
    (if why
	(let ((env-stack (if debugging-level 
			     (env-stack-list->vector (%get-environment-stack))
			     #f))
	      (do-stop? #f)
	      (current-frame #f)
	      (current-sp #f)
	      (current-pc #f))
	  (define (frame num)
	    (cond ((or (not num) (< num 0) (>= num (vector-length env-stack)))
		   #f)
		  (else (vector-ref env-stack num))))
	  (define (get-sp num)
	    (let ((f (frame num)))
	      (and f (eval 'stkdb:sp f))))
	  (define (get-pc num)
	    (let ((f (frame num)))
	      (and f (eval 'stkdb:posn f))))
	  (define (get-function-name num)
	    (let ((f (frame num)))
	      (and f (src-func (eval 'stkdb:posn f)))))
	  (define (set-frame! n)
	    (let ((f (frame n)))
	      (if f
		  (begin
		    (set! current-frame n)
		    (set! current-sp (eval 'stkdb:sp f))
		    (set! current-pc (eval 'stkdb:posn f))
		    #t)
		  #f)))
	  (define (bp-cond-true? posn)
	    (let ((pred (breakpoint-condition posn)))
	      (cond ((eq? pred #t) #t)
		    ((not pred) #f)
		    (else 
		     (let ((val #t))
		       (catch (set! val (eval pred (frame current-frame))))
		       val)))))
	  ;; Given that BP is an argument that is a number, symbol, or string
	  ;; whose print representation is one of
	  ;;    NAME:NUMBER
	  ;;    NAME
	  ;;    NUMBER
	  ;; Return (NAME . NUMBER), with #f for any missing element.  Something
	  ;; not of this form returns (#f . #f).
	  (define (bp-designator-parse bp)
	    (let* ((bp-as-num (cond ((string? bp) (string->number bp))
				    ((number? bp) bp)
				    (else #f)))
		   (bp-as-string (cond ((string? bp) bp)
				       ((symbol? bp) (symbol->string bp))
				       (else #f)))
		   (len (if bp-as-string (string-length bp-as-string) 0)))
	      (cond (bp-as-num (cons #f bp-as-num))
		    ((not bp-as-string) '(#f #f))
		    (else (let loop ((k 0))
			    (cond ((>= k len) (cons bp-as-string #f))
				  ((eqv? (string-ref bp-as-string k) #\:)
				   (let ((line (string->number 
						(substring bp-as-string (+ k 1) 
							   len))))
				     (if line 
					 (cons (substring bp-as-string 0 k) 
					       line)
					 (cons #f #f))))
				  (else (loop (+ k 1)))))))))
	  ;; The breakpoint number corresponding to BP, where BP has 
	  ;; external representation FILENAME:LINE, FUNCTIONNAME, or BPNUM.
	  ;; Returns -1 for a file position or function name that does 
	  ;; not correspond to an existing breakpoint, and #f for 
	  ;; invalid format.
	  (define (bp-designator->bpnum bp)
	    (let ((bp-info (bp-designator-parse bp)))
	      (if (car bp-info)
		  (let ((bp-record (if (cdr bp-info)
				       (find-bp-record 
					(car bp-info) (cdr bp-info) #f #f)
				       (find-bp-record 
					#f #f (car bp-info) #f))))
		    (if bp-record (bp-record-num bp-record) -1))
		  (cdr bp-info))))
			       
	  (define (up)
	    (set-frame! (+ 1 current-frame)))
	  (define (down)
	    (set-frame! (- current-frame 1)))
	  (define (back-trace print? with-locals?)
	    (let loop ((user-frame 0))
	      (if (< user-frame (vector-length env-stack))
		  (let ((pc (get-pc user-frame))
			(name (get-function-name user-frame)))
		    (display-trace pc name user-frame current-frame)
		    (if print?
			(format (current-output-port)
				"~a[~a] ~a:~a ~a~%" 
				(if (eqv? user-frame current-frame) "*" " ")
				user-frame
				(src-file pc) (start-line pc) 
				(or name "")))
		    (if with-locals? (info-locals user-frame print?))
		    (loop (+ user-frame 1))))))
	  (define (info-locals frame-num print?)
	    (let* ((f (frame frame-num))
		   (sp (get-sp frame-num))
		   (local-env-list (frame->local-alist f sp local-var-list-limit)))
	      (display-vars local-env-list frame-num)
	      (if print?
		  (let loop ((L local-env-list))
		    (if (not (null? L))
			(begin 
			  (format (current-output-port) "  ~a: ~a~%"
				  (caar L) (cdar L))
			  (loop (cdr L))))))))
	  (define (do-break-command arg) 
	    (if (eof-object? arg)
		(mesg "Break command needs an argument~%")
		(let ((bp-info (bp-designator-parse arg)))
		  (cond ((and (car bp-info) (cdr bp-info))
			 (stkdb:set-bp! (car bp-info) (cdr bp-info)))
			((car bp-info) (stkdb:set-bp-func! 
					(string->symbol (car bp-info))))
			(else
			 (mesg "Bad breakpoint designation: ~a~%" arg))))))
	  (define (do-condition-command bp-arg cond-arg)
	    (if (eof-object? bp-arg)
		(mesg "Missing arguments to condition~%")
		(let ((bpnum (bp-designator->bpnum bp-arg)))
		  (cond ((eqv? bpnum -1)
			 (mesg "No breakpoint for ~a~%" bp-arg))
			(bpnum
			 (stkdb:set-bp-cond! 
			  bpnum (if (eof-object? cond-arg) #t cond-arg)))
			(else (mesg "Bad breakpoint designator: ~a~%" 
				    bp-arg))))))

	  (define (auto-display-stack)
	    (back-trace (not emacs-mode?) #f))
	  (define (auto-display-stack-and-top-locals)
	    (back-trace (not emacs-mode?) #f)
	    (info-locals 0 (not emacs-mode?)))
	  (define (auto-display-stack-and-all-locals)
	    (back-trace (not emacs-mode?) #t))
	  ;; Display VAL.  Suppress printing if VAL is the undefined value
	  ;; and the next prompt is suppressed.  Note: This has an effect
	  ;; only when using the Berkeley revisions to stk, which cause
	  ;; #[undefined] to print as "okay".
	  (define (display-result val)
	    (if (not (and (suppress-prompt?) (eqv? val undefined-value)))
		(repl-display-result val)))

	  (define (safe-eval-print expr)
	    (fluid-let ((debugging-level (if debugging-level 
					     (+ 1 debugging-level)
					     1))
			(exit-continuation exit-continuation))
	      (let ((env (if (= debugging-level 1)
			     outer-environ
			     (frame current-frame))))
		(cond ((and (symbol? expr) 
			   (not (symbol-bound? expr env)))
		       (mesg "Symbol ~A not defined~%" expr))
		      ((> debugging-level 5) 
		       (if (catch (display-result (eval expr env)))
			   (mesg "Expression evaluation caused error.\n")))
		      (else 
		       (%clear-recursive-error)
		       (call/cc 
			(lambda (cont)
			  (set! exit-continuation cont)
			  (display-result (eval expr env))))))))
	    (if (not debugging-level) (unsuppress-prompts! #f)))

	  (set-stepping! #f #f #f)
	  (set! step-over-posn #f)
	  (set! last-sp #f)
	  (set! finish-level #f)
	  (if debugging-level 
	      (begin 
		(set-frame! 0)
		(set! last-sp current-sp)
		(set! step-over-posn current-pc))
	      (set! next-free-sp 0))
	      
	  (set! do-stop? (or (not (eq? why 'bpt))
			     (bp-cond-true? current-pc)))
	  
	  (if (and debugging-level do-stop?)
	      (begin
		(display-posn current-pc why)
		(display-frame-mark 0)
		(case display-on-stop 
		  ((bt) (auto-display-stack))
		  ((bt-and-top) (auto-display-stack-and-top-locals))
		  ((bt-and-locals) (auto-display-stack-and-all-locals)))
		(if (eq? why 'bpt)
		    (begin
		      (mesg "Hit breakpoint.~%")
		      (unsuppress-prompts! #f)))))
	  
	  (let loop ((command (if do-stop? (prompt) '*no-stop*)))
	    (case command
	      ((*no-stop*) (set-stepping! #f #f #f))
	      ((*quiet*) (suppress-prompts! 2) (loop (prompt)))
	      ((c cont continue) 
	       (set-stepping! #f #f #f)
	       (set! step-over-posn (widen-to-line step-over-posn)))
	      ((s step) (set-stepping! #t #f #f))
	      ((n next) (set-stepping! #f #t #f))
	      ((f finish) 
	       (set-stepping! #f #f #t)
	       (set! step-over-posn (widen-to-line step-over-posn))
	       (set! finish-level last-sp))
	      ((break br b) 
	       (call-with-input-string 
		(read-line normal-input)
		(lambda (port) (do-break-command (read port))))
	       (loop (prompt)))
	      ((cond condition)
	       (call-with-input-string 
		(read-line normal-input)
		(lambda (port) 
		  (let* ((arg1 (read port))
			 (arg2 (read port)))
		    (do-condition-command arg1 arg2))))
	       (loop (prompt)))
	      ((delete) 
	       (call-with-input-string 
		(read-line normal-input) 
		(lambda (port)
		  (let loop ((bp (read port)) (at-least-one? #f))
		    (cond ((eof-object? bp) 
			   (if (and (not at-least-one?)
				    (yorn? "Remove all breakpoints?" ))
			       (stkdb:clear-all-bps!)))
			  ((number? bp) (clear-numbered-bp! bp)
			   (loop (read port) #t))
			  (else
			   (mesg "Bad breakpoint number: ~a~%" bp))))))
	       (loop (prompt)))
	      ((up u) 
	       (or (up)
		   (display "At top level\n"))
	       (display-posn current-pc 
			     (if (eqv? current-frame 0) why 'frm))
	       (display-frame-mark current-frame)
	       (loop (prompt)))
	      ((display)
	       (with-input-from-string (read-line normal-input)
		 (lambda ()
		   (let ((subcommand (read)))
		     (case subcommand
		       ((vals values) 
			(mesg "Showing returned values when stepping.~%")
			(stkdb:set-options! :show-returns #t))
		       ((novals novalues)
			(mesg "No values shown when stepping.~%")
			(stkdb:set-options! :show-returns #f))
		       ((backtrace where bt)
			(mesg "Auto-display backtrace.~%")
			(stkdb:set-options! :auto-backtrace 'bt))
		       ((locals)
			(mesg "Auto-display backtrace with locals.~%")
			(stkdb:set-options! :auto-backtrace 'bt-and-locals))
		       ((top)
			(mesg "Auto-display backtrace and innermost locals.~%")
			(stkdb:set-options! :auto-backtrace 'bt-and-top))
		       ((off no n) 
			(mesg "Auto-display off.~%")
			(stkdb:set-options! :auto-backtrace #f))
		       (else (mesg "Bad display command: ~a~%" 
				   (if (eof-object? subcommand) 
				       "missing operand"
				       subcommand)))))))
	       (loop (prompt)))
	      ((down d) 
	       (or (down)
		   (display "At outermost level\n"))
	       (display-posn current-pc 
			     (if (eqv? current-frame 0) why 'frm))
	       (display-frame-mark current-frame)
	       (loop (prompt)))
	      ((info)
	       (with-input-from-string (read-line normal-input)
		 (lambda ()
		   (let ((subcommand (read)))
		     (case subcommand
		       ((locals loc l)
			(info-locals current-frame #t))
		       ((break breakpoints bp bps b)
			(info-breakpoints))
		       (else (mesg "Bad info command: ~a~%" 
				   (if (eof-object? subcommand) 
				       "missing operand"
				       subcommand)))))))
	       (loop (prompt)))
	      ((fr frame)
	       (let ((user-frame (read normal-input)))
		 (if (and (number? user-frame) (set-frame! user-frame))
		     (begin
		       (set-frame! user-frame)
		       (display-posn 
			current-pc (if (zero? current-frame) why 'frm))
		       (display-frame-mark current-frame))
		     (display "Illegal frame number")))
	       (loop (prompt)))
	      ((w bt where) (back-trace #t #f) (loop (prompt)))
	      ((p pr print) 
	       (safe-eval-print (read normal-input))
	       (loop (prompt)))
	      ((reset quit) (stkdb:reset #f) (loop (prompt)))
	      ((exit) (stkdb:reset #t))
	      (else 
	       (if (not (eof-object? command))
		   (begin
		     (safe-eval-print command)
		     (loop (prompt)))))))
	  (if do-stop? (undisplay-posn)))))

  ;; Called when breaking after an expression evaluation.
  (define (top-level-val val)
    (if (or running-until-finish? show-all-returns?)
	(begin
	  (if running-until-finish? (unsuppress-prompts! #f))
	  (display "Returned value: ")
	  (write val)
	  (newline)))
    (top-level 'ret))

  ;; Unwind one level. Return to plain stk if ALL-THE-WAY?
  (define (stkdb:reset all-the-way?)
    (cond ((and (not all-the-way?) (not debugging-level)) 'ok)
	  ((and (not all-the-way?) exit-continuation)
	   (undisplay-posn)
	   (exit-continuation #f))
	  (else (fluid-let ((suppress-error? #t))
		  (undisplay-posn)
		  (error "Back to top level")))))

  ;; Set a breakpoint at line LINE in FILENAME.
  (define (stkdb:set-bp! filename line)
    (let ((real-line (find-reasonable-line filename line 3)))
      (if (not real-line)
	  (mesg "There appears to be no code on line ~a.~%" line)
	  (begin
	    (record-bp! filename real-line #f)
	    (set-bps! filename real-line #t)))))

  ;; Set a breakpoint at the beginning of NAME.
  (define (stkdb:set-bp-func! name)
    (let loop ((posn 0)
	       (best-posn #f))
      (cond ((> posn high-water) 
	     (if best-posn
		 (begin
		   (set-bps! (src-file best-posn) (start-line best-posn) #t)
		   (record-bp! (src-file best-posn) 
			       (start-line best-posn) #f))))
	    ((not (and (force-posn posn)
		       (eqv? name (src-func posn))
		       (or (not best-posn)
			   (< (start-line posn) (start-line best-posn)))))
	     (loop (+ posn 1) best-posn))
	    (else 
	     (loop (+ posn 1) posn)))))

  ;; Attach condition PRED to breakpoint #BPNUM.
  (define (stkdb:set-bp-cond! bpnum pred)
    (let ((bp (find-bp-record #f #f #f bpnum)))
      (if (not bp)
	  (mesg "No breakpoint #~a.~%" bpnum)
	  (begin
	    (set-bp-conds! (bp-record-filename bp) (bp-record-line bp) pred)
	    (set-bp-record-cond! bp pred)
	    (if (eq? pred #t)
		(mesg "Breakpoint ~a is now unconditional.~%" bpnum)
		(mesg "Stop on breakpoint ~a if and only if ~a.~%" 
		      bpnum pred))))))

  ;; Remove all breakpoints in function NAME.
  (define (stkdb:clear-bps-func! name)
    (let loop ((posn 0))
      (cond ((> posn high-water) 
	     #f)
	    ((not (and (force-posn posn)
		       (eqv? name (src-func posn))))
	     (loop (+ posn 1)))
	    (else 
	     (set-bps! (src-file posn) (start-line posn) #f)
	     (loop (+ posn 1))))))

  ;; Set a breakpoint at line LINE in FILENAME.
  (define (stkdb:clear-bp filename line)
    (unrecord-bp! filename line #f #f)
    (set-bps! filename line #f))

  ;; Remove all breakpoints in file FILENAME.
  (define (clear-file-breakpoints! filename start end)
    (clear-file-posns! filename start end)
    (set! all-bps
	  (let loop ((L all-bps))
	    (cond ((null? L) '())
		  ((string=? filename (bp-record-filename (car L)))
		   (loop (cdr L)))
		  (else (cons (car L) (loop (cdr L))))))))

  ;; Remove all breakpoints.
  (define (stkdb:clear-all-bps!)
    (mesg "Removing all breakpoints.~%")
    (unrecord-all-bps!)
    (set-bps! #f #f #f))

  ;; True if FILE0 and FILE1 are the same file, or FILE0 is the wildcard, #f.
  (define (filename-match? file0 file1)
    (or (not file0) (not file1) (string=? file0 file1)))

  ;; True if LINE0 and LINE1 are the same line number, or LINE0 is 
  ;; the wildcard, #f.
  (define (line-match? line0 line1)
    (or (not line0) (not line1) (= line0 line1)))

  ;; If VAL, set a breakpoint at all expressions on line LINE of FILENAME,
  ;; otherwise clear all breakpoints on that line.
  (define (set-bps! filename line val)
    (let loop ((k 0))
      (if (<= k high-water)
	  (let ((posn (force-posn k)))
	    (if (and (filename-match? filename (src-file posn))
		     (line-match? line (start-line posn)))
		(set-break! posn val))
	    (loop (+ k 1))))))

  ;; Set the condition for all breakpoints on line LINE of FILENAME to
  ;; PRED.
  (define (set-bp-conds! filename line pred)
    (let loop ((k 0))
      (if (<= k high-water)
	  (let ((posn (force-posn k)))
	    (if (and (filename-match? filename (src-file posn))
		     (line-match? line (start-line posn)))
		(set-breakpoint-condition! posn pred))
	    (loop (+ k 1))))))

  ;; Clears the breakpoint numbered NUM, if any.
  (define (clear-numbered-bp! num)
    (let ((bp (find-bp-record #f #f #f num)))
      (if bp
	  (begin
	   (unrecord-bp! #f #f #f num)
	   (stkdb:clear-bp (bp-record-filename bp)
			   (bp-record-line bp))))))
  

  ;; The list of all breakpoints.  Each has the form
  ;;   (FILENAME LINE FUNCTION-NAME NUMBER)
  (define all-bps '())
  ;; Number to assign to next breakpoint that's set.  Increase monotonically
  ;; forever.
  (define next-bp-number 1)

  ;; Accessors for breakpoint records.
  (define (bp-record-filename bpr) (vector-ref bpr 0))
  (define (bp-record-line bpr) (vector-ref bpr 1))
  (define (bp-record-func bpr) (vector-ref bpr 2))
  (define (bp-record-num bpr) (vector-ref bpr 3))
  (define (bp-record-cond bpr) (vector-ref bpr 4))
  (define (set-bp-record-cond! bpr pred) (vector-set! bpr 4 pred))

  ;; The first line that appears to contain code at or after line LINE
  ;; in file FILENAME and before line LINE+LIMIT, or #f if none 
  ;; such found.
  (define (find-reasonable-line filename line limit)
    (let loop ((posn 0)
	       (best-posn #f))
      (cond ((> posn high-water) 
	     (and best-posn (start-line best-posn)))
	    ((and (force-posn posn)
		  (equal? (src-file posn) filename)
		  (<= line (start-line posn) (+ line limit -1)) 
		  (or (not best-posn)
		      (< (start-line posn) (start-line best-posn))))
	     (if (eqv? (start-line posn) line) line
		 (loop (+ posn 1) posn)))
	    (else (loop (+ posn 1) best-posn)))))

  ;; Record the setting of a new breakpoint at line LINE of FILENAME, or
  ;; in function FUNC (FUNC or both FILENAME and LINE may be #f).
  (define (record-bp! filename line func)
    (let ((old-record (find-bp-record filename line func #f)))
      (if old-record
	  (mesg "Breakpoint ~a already set.~%" (bp-record-num old-record))
	  (begin
	    (set! all-bps (cons (vector filename line func next-bp-number #t)
				all-bps))
	    (if func
		(mesg "Breakpoint ~a on ~a~%" next-bp-number func)
		(mesg "Breakpoint ~a at ~a:~a~%" next-bp-number filename line))
	    (set! next-bp-number (+ 1 next-bp-number))))))
  
  ;; Remove all breakpoints matching FILENAME, LINE, FUNC, and NUM (breakpoint
  ;; number).  #f for any argument is a wild card.
  (define (unrecord-bp! filename line func num)
    (let ((old-record (find-bp-record filename line func num)))
      (if old-record
	  (mesg "Removing breakpoint ~a~%" (bp-record-num old-record)))
      (set! all-bps (remq old-record all-bps))))

  ;; Remove all breakpoint records.  (Does not actually remove the breakpoints
  ;; themselves, however).
  (define (unrecord-all-bps!)
    (set! all-bps '()))

  ;; A breakpoint matching FILENAME, LINE, FUNC, and NUM (breakpoint number).
  ;; #f for any argument is a wild card.
  (define (find-bp-record filename line func num)
    (let loop ((L all-bps))
      (cond ((null? L) #f)
	    ((or (and filename 
		      (not (string=? filename (bp-record-filename (car L)))))
		 (and line
		      (not (= line (bp-record-line (car L)))))
		 (and func
		      (not (string=? func (bp-record-func (car L)))))
		 (and num
		      (not (= num (bp-record-num (car L))))))
	     (loop (cdr L)))
	    (else (car L)))))

  ;; Implementation of 'info break'
  (define (info-breakpoints)
    (let loop ((L (reverse all-bps)))
      (if (not (null? L))
	  (begin
	    (if (bp-record-func (car L))
		(format (current-output-port)
			"[~a] Function ~a~%" 
			(bp-record-num (car L)) (bp-record-func (car L)))
		(format (current-output-port)
			"[~a] File ~a, line ~a~%"
			(bp-record-num (car L)) (bp-record-filename (car L))
			(bp-record-line (car L))))
	    (if (not (eq? (bp-record-cond (car L)) #t))
		(format (current-output-port)
			"    stops if and only if ~a~%"
			(bp-record-cond (car L))))
	    (loop (cdr L))))))

  ;; Utility that returns string S truncated (if needed) to LEN characters.
  (define (truncate s len)
    (if (> (string-length s) len)
	(string-append (substring s 0 (- len 1)) " ...")
	s))

  ;; True if error message sent to (error) is to be suppressed.
  (define suppress-error? #f)

  ;; stkdb's version of report-error: Prints message, but then enters
  ;; top-level, unless suppress-error? is on.
  (define (stkdb:report-error . args)
    (unsuppress-prompts! #f)
    (if (not suppress-error?)
	(begin
	  (let ((msg (truncate 
		      (format #f "~%~A~A~A~%"
			      (car args) (cadr args)
			      (if (null? (caddr args)) ""
				  (format #f ": ~S" (caddr args))))
		      200)))
	    (display msg (current-error-port)))
	  (top-level 'err))))

  ;; True iff user responds affirmatively to message PROMPT.
  (define (yorn? prompt)
    (let loop ()
      (mesg "~a [yn] " prompt)
      (flush (current-error-port))
      (case (read normal-input)
	((y yes ok) #t)
	((n no) #f)
	(else
	 (format (current-error-port)
		 "Please answer y(es) or n(o).~%")
	 (loop)))))

  ;; #f when expression is not evaluating, and otherwise a number of
  ;; levels of top-level.
  (define debugging-level #f)
  ;; The environment on entry to the debugger.
  (define outer-environ (global-environment))

  ;; The input port on entry to the debugger.
  (define normal-input #f)

  ;; Entry point from outside to start debugging
  (define (stkdb:eval-loop module environ)
    (fluid-let ((stkdb:active? #t)
		(debugging-level #f)
		(outer-module-name (module-name module))
		(outer-environ environ)
		(normal-input (current-input-port))
		(report-error stkdb:report-error))
      (let ((signal-state (get-signal-handlers |SIGINT|)))
	(unsuppress-prompts! #f)
	(dynamic-wind
	    (lambda () 
	      (set-signal-handler! |SIGINT| (lambda (sig) (top-level 'intr))))
	    (lambda () (call/cc (lambda (cont) 
				  (fluid-let ((exit-continuation cont))
				    (top-level 'init)))))
	    (lambda () 
	      (cond ((pair? signal-state)
		     (set-signal-handler! |SIGINT| #f)
		     (let loop ((L (reverse signal-state)))
		       (if (not (null? L))
			   (begin
			     (add-signal-handler! |SIGINT| (car L))
			     (loop (cdr L))))))
		    ((not (null? signal-state))
		     (set-signal-handler! |SIGINT| signal-state))))))))

  ;;;; SETTING DEBUGGER STATE

  ;; The arguments are option keywords and (where needed) their values.
  (define (stkdb:set-options! . args) 
    (let loop ((L args))
      (if (not (null? L))
	  (case (car L)
	    ((:auto-backtrace)
	     (set! display-on-stop (cadr L))
	     (loop (cddr L)))
	    ((:emacs)
	     (set! emacs-mode? (cadr L))
	     (loop (cddr L)))
	    ((:show-returns)
	     (set! show-all-returns? (cadr L))
	     (loop (cddr L)))
	    (else (loop (cdr L)))))))
  )

;; Start the debugger.
(define stkdb 
  (macro dummy '(if stkdb:active?
		    (with-module stk-debugger (suppress-prompts! 1))
		    (stkdb:eval-loop (current-module) (the-environment)))))

