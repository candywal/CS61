;;;; Implementation of VICINITY and MODULES for Scheme
;Copyright (C) 1991, 1992, 1993, 1994, 1997, 2002 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define *SLIB-VERSION* "2d5")

(define (user-vicinity)
  (case (software-type)
    ((VMS)	"[.]")
    (else	"")))

(define *load-pathname* #f)
(define vicinity:suffix?
  (let ((suffi
	 (case (software-type)
	   ((AMIGA)				'(#\: #\/))
	   ((MACOS THINKC)			'(#\:))
	   ((MS-DOS WINDOWS ATARIST OS/2)	'(#\\ #\/))
	   ((NOSVE)				'(#\: #\.))
	   ((UNIX COHERENT PLAN9)		'(#\/))
	   ((VMS)				'(#\: #\])))))
    (lambda (chr) (memv chr suffi))))
(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
	  ((vicinity:suffix? (string-ref pathname i))
	   (substring pathname 0 (+ i 1)))
	  (else (loop (- i 1))))))
(define (program-vicinity)
  (if *load-pathname*
      (pathname->vicinity *load-pathname*)
      (slib:error 'program-vicinity " called; use slib:load to load")))

(define sub-vicinity
  (case (software-type)
    ((VMS) (lambda
	       (vic name)
	     (let ((l (string-length vic)))
	       (if (or (zero? (string-length vic))
		       (not (char=? #\] (string-ref vic (- l 1)))))
		   (string-append vic "[" name "]")
		   (string-append (substring vic 0 (- l 1))
				  "." name "]")))))
    (else (let ((*vicinity-suffix*
		 (case (software-type)
		   ((NOSVE) ".")
		   ((MACOS THINKC) ":")
		   ((MS-DOS WINDOWS ATARIST OS/2) "\\")
		   ((UNIX COHERENT PLAN9 AMIGA) "/"))))
	    (lambda (vic name)
	      (string-append vic name *vicinity-suffix*))))))

(define (make-vicinity <pathname>) <pathname>)

(define (slib:pathnameize-load *old-load*)
  (lambda (<pathname> . extra)
    (let ((old-load-pathname *load-pathname*))
      (set! *load-pathname* <pathname>)
      (apply *old-load* (cons <pathname> extra))
      (require:provide <pathname>)
      (set! *load-pathname* old-load-pathname))))

(set! slib:load-source
      (slib:pathnameize-load slib:load-source))
(set! slib:load
      (slib:pathnameize-load slib:load))

;;;; MODULES

(define *catalog* #f)
(define *modules* '())
(define *base-table-implementations* '())

(define (require:version path)
  (let ((expr (and (file-exists? path)
		   (call-with-input-file path (lambda (port) (read port))))))
    (and (list? expr) (= 3 (length expr))
	 (eq? (car expr) 'define) (eq? (cadr expr) '*SLIB-VERSION*)
	 (string? (caddr expr)) (caddr expr))))

(define (catalog/require-version-match? slibcat)
  (let* ((apair (assq '*SLIB-VERSION* slibcat))
	 (req (in-vicinity (library-vicinity)
			   (string-append "require" (scheme-file-suffix))))
	 (reqvers (require:version req)))
    (cond ((not (file-exists? req))
	   (slib:warn "can't find " req) #f)
	  ((not apair) #f)
	  ((not (equal? reqvers (cdr apair))) #f)
	  ((not (equal? reqvers *SLIB-VERSION*))
	   (slib:warn "The loaded " req " is stale.")
	   #t)
	  (else #t))))

(define (catalog:try-read vicinity name)
  (or (and vicinity name
	   (let ((path (in-vicinity vicinity name)))
	     (and (file-exists? path)
		  (call-with-input-file path
		    (lambda (port)
		      (do ((expr (read port) (read port))
			   (lst '() (cons expr lst)))
			  ((eof-object? expr)
			   (apply append lst))))))))
      '()))

(define (catalog:get feature)
  (if (not *catalog*)
      (let ((slibcat (catalog:try-read (implementation-vicinity) "slibcat")))
	(cond ((not (catalog/require-version-match? slibcat))
	       (slib:load (in-vicinity (library-vicinity) "mklibcat"))
	       (set! slibcat
		     (catalog:try-read (implementation-vicinity) "slibcat"))))
	(cond (slibcat
	       (set! *catalog* ((slib:eval
				 (cadr (or (assq 'catalog:filter slibcat)
					   '(#f identity))))
				slibcat))))
	(set! *catalog*
	      (append (catalog:try-read (home-vicinity) "homecat") *catalog*))
	(set! *catalog*
	      (append (catalog:try-read (user-vicinity) "usercat") *catalog*))))
  (and feature *catalog* (cdr (or (assq feature *catalog*) '(#f . #f)))))

(define (require:provided? feature)
  (if (symbol? feature)
      (if (memq feature *features*) #t
	  (and *catalog*
	       (let ((path (catalog:get feature)))
		 (cond ((symbol? path) (require:provided? path))
		       ((member (if (pair? path) (cdr path) path) *modules*)
			#t)
		       (else #f)))))
      (and (member feature *modules*) #t)))

(define (require:feature->path feature)
  (and (symbol? feature)
       (let ((path (catalog:get feature)))
	 (if (symbol? path) (require:feature->path path) path))))

(define (require:require feature)
  (or (require:provided? feature)
      (let ((path (catalog:get feature)))
	(cond ((and (not path) (string? feature) (file-exists? feature))
	       (set! path feature)))
	(cond ((not feature) (set! *catalog* #f))
	      ((not path) 
	       (slib:error ";required feature not supported: " feature))
	      ((symbol? path) (require:require path) (require:provide feature))
	      ((not (pair? path))	;simple name
	       (slib:load path)
	       (and (not (eq? 'new-catalog feature)) (require:provide feature)))
	      (else			;special loads
	       (require:require (car path))
	       (apply (case (car path)
			((macro) macro:load)
			((syntactic-closures) synclo:load)
			((syntax-case) syncase:load)
			((macros-that-work) macwork:load)
			((macro-by-example) defmacro:load)
			((defmacro) defmacro:load)
			((source) slib:load-source)
			((compiled) slib:load-compiled)
			((aggregate)
			 (lambda feature (for-each require:require feature)))
			((spectral-tristimulus-values) load-ciexyz)
			((color-names)
			 (lambda (filename)
			   (load-color-dictionary feature filename)))
			(else (slib:error "unknown package loader" path)))
		      (if (list? path) (cdr path) (list (cdr path))))
	       (require:provide feature))))))

(define (require:provide feature)
  (if (symbol? feature)
      (if (not (memq feature *features*))
	  (set! *features* (cons feature *features*)))
      (if (not (member feature *modules*))
	  (set! *modules* (cons feature *modules*)))))

(require:provide 'vicinity)

(define provide require:provide)
(define provided? require:provided?)
(define require require:require)

(if (and (string->number "0.0") (inexact? (string->number "0.0")))
    (require:provide 'inexact))
(if (rational? (string->number "1/19")) (require:provide 'rational))
(if (real? (string->number "0.0")) (require:provide 'real))
(if (complex? (string->number "1+i")) (require:provide 'complex))
(let ((n (string->number "9999999999999999999999999999999")))
  (if (and n (exact? n)) (require:provide 'bignum)))

(cond
 ((provided? 'srfi)
  (cond-expand (srfi-0 (provide 'srfi-0)) (else #f))
  (cond-expand (srfi-1 (provide 'srfi-1)) (else #f))
  (cond-expand (srfi-2 (provide 'srfi-2)) (else #f))
  (cond-expand (srfi-3 (provide 'srfi-3)) (else #f))
  (cond-expand (srfi-4 (provide 'srfi-4)) (else #f))
  (cond-expand (srfi-5 (provide 'srfi-5)) (else #f))
  (cond-expand (srfi-6 (provide 'srfi-6)) (else #f))
  (cond-expand (srfi-7 (provide 'srfi-7)) (else #f))
  (cond-expand (srfi-8 (provide 'srfi-8)) (else #f))
  (cond-expand (srfi-9 (provide 'srfi-9)) (else #f))
  (cond-expand (srfi-10 (provide 'srfi-10)) (else #f))
  (cond-expand (srfi-11 (provide 'srfi-11)) (else #f))
  (cond-expand (srfi-12 (provide 'srfi-12)) (else #f))
  (cond-expand (srfi-13 (provide 'srfi-13)) (else #f))
  (cond-expand (srfi-14 (provide 'srfi-14)) (else #f))
  (cond-expand (srfi-15 (provide 'srfi-15)) (else #f))
  (cond-expand (srfi-16 (provide 'srfi-16)) (else #f))
  (cond-expand (srfi-17 (provide 'srfi-17)) (else #f))
  (cond-expand (srfi-18 (provide 'srfi-18)) (else #f))
  (cond-expand (srfi-19 (provide 'srfi-19)) (else #f))
  (cond-expand (srfi-20 (provide 'srfi-20)) (else #f))
  (cond-expand (srfi-21 (provide 'srfi-21)) (else #f))
  (cond-expand (srfi-22 (provide 'srfi-22)) (else #f))
  (cond-expand (srfi-23 (provide 'srfi-23)) (else #f))
  (cond-expand (srfi-24 (provide 'srfi-24)) (else #f))
  (cond-expand (srfi-25 (provide 'srfi-25)) (else #f))
  (cond-expand (srfi-26 (provide 'srfi-26)) (else #f))
  (cond-expand (srfi-27 (provide 'srfi-27)) (else #f))
  (cond-expand (srfi-28 (provide 'srfi-28)) (else #f))
  (cond-expand (srfi-29 (provide 'srfi-29)) (else #f))
  (cond-expand (srfi-30 (provide 'srfi-30)) (else #f))))

(define report:print
  (lambda args
    (for-each (lambda (x) (write x) (display #\ )) args)
    (newline)))

(define slib:report
  (let ((slib:report (lambda () (slib:report-version) (slib:report-locations))))
    (lambda args
      (cond ((null? args) (slib:report))
	    ((not (string? (car args)))
	     (slib:report-version) (slib:report-locations #t))
	    ((require:provided? 'transcript)
	     (transcript-on (car args))
	     (slib:report)
	     (transcript-off))
	    ((require:provided? 'with-file)
	     (with-output-to-file (car args) slib:report))
	    (else (slib:report))))))

(define slib:report-version
  (lambda ()
    (report:print
     'SLIB *SLIB-VERSION* 'on (scheme-implementation-type)
     (scheme-implementation-version) 'on (software-type))))

(define slib:report-locations
  (let ((features *features*))
    (lambda args
      (report:print '(IMPLEMENTATION-VICINITY) 'is (implementation-vicinity))
      (report:print '(LIBRARY-VICINITY) 'is (library-vicinity))
      (report:print '(SCHEME-FILE-SUFFIX) 'is (scheme-file-suffix))
      (cond (*load-pathname*
	     (report:print '*LOAD-PATHNAME* 'is *load-pathname*)))
      (cond ((not (null? *modules*))
	     (report:print 'Loaded '*MODULES* 'are: *modules*)))
      (let* ((i (+ -1 5)))
	(cond ((eq? (car features) (car *features*)))
	      (else (report:print 'loaded '*FEATURES* ':) (display slib:tab)))
	(for-each
	 (lambda (x)
	   (cond ((eq? (car features) x)
		  (if (not (eq? (car features) (car *features*))) (newline))
		  (report:print 'Implementation '*FEATURES* ':)
		  (display slib:tab) (set! i (+ -1 5)))
		 ((zero? i) (newline) (display slib:tab) (set! i (+ -1 5)))
		 ((not (= (+ -1 5) i)) (display #\ )))
	   (write x) (set! i (+ -1 i)))
	 *features*))
      (newline)
      (report:print 'Implementation '*CATALOG* ':)
      (catalog:get #f)
      (cond ((pair? args)
	     (for-each (lambda (x) (display slib:tab) (report:print x))
		       *catalog*))
	    (else (display slib:tab) (report:print (car *catalog*))
		  (display slib:tab) (report:print '...)))
      (newline))))

(let ((sit (scheme-implementation-version)))
  (cond ((zero? (string-length sit)))
	((or (not (string? sit)) (char=? #\? (string-ref sit 0)))
	 (newline)
	 (slib:report-version)
	 (report:print 'edit (scheme-implementation-type) ".init"
		       'to 'set '(scheme-implementation-version) 'string)
	 (report:print '(IMPLEMENTATION-VICINITY) 'is (implementation-vicinity))
	 (report:print 'type '(slib:report) 'for 'configuration)
	 (newline))))
