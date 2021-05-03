;;; "dirs.scm" Directories.
; Copyright 1998, 2002 Aubrey Jaffer
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

(require 'transact)
(require 'line-i/o)
(require 'system)
(require 'glob)

(define current-directory
  (case (software-type)
    ;;((AMIGA)				)
    ;;((MACOS THINKC)			)
    ((MS-DOS WINDOWS ATARIST OS/2) (lambda () (system->line "cd")))
    ;;((NOSVE)				)
    ((UNIX COHERENT PLAN9) (lambda () (system->line "pwd")))
    ;;((VMS)				)
    (else #f)))

(define (make-directory name)
  (eqv? 0 (system (sprintf #f "mkdir %#a" name))))

(define (dir:lister dirname tmp)
  (case (software-type)
    ((UNIX COHERENT PLAN9)
     (zero? (system (string-append "ls '" dirname "' > " tmp))))
    ((MS-DOS WINDOWS OS/2 ATARIST)
     (zero? (system (string-append "DIR /B \"" dirname "\" > " tmp))))
    (else (slib:error (software-type) 'list?))))

(define (directory-for-each proc dirname . args)
  (define selector
    (cond ((null? args) identity)
	  ((> (length args) 1)
	   (slib:error 'directory-for-each 'too-many-arguments (cdr args)))
	  ((procedure? (car args)) (car args))
	  ((string? (car args)) (filename:match?? (car args)))
	  (else
	   (slib:error 'directory-for-each 'filter? (car args)))))
  (call-with-tmpnam
   (lambda (tmp)
     (and (dir:lister dirname tmp)
	  (file-exists? tmp)
	  (call-with-input-file tmp
	    (lambda (port)
	      (do ((filename (read-line port) (read-line port)))
		  ((or (eof-object? filename) (equal? "" filename)))
		(and (selector filename) (proc filename)))))))))
