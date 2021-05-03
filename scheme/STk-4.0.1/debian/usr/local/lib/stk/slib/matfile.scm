; "matfile.scm", Read MAT-File Format version 4 (MATLAB)
; Copyright (c) 2001, 2002 Aubrey Jaffer
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

(require 'byte)
(require 'logical)
(require 'array)
(require 'array-for-each)

;;@code{(require 'matfile)}
;;@ftindex matfile
;;
;;@uref{http://www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf}
;;
;;@noindent
;;This package reads MAT-File Format version 4 (MATLAB) binary data
;;files.  MAT-files written from big-endian or little-endian computers
;;having IEEE format numbers are currently supported.  Support for files
;;written from VAX or Cray machines could also be added.
;;
;;@noindent
;;The numeric and text matrix types handled; support for @dfn{sparse}
;;matrices awaits a sample file.
(define (bytes->ulong lst)
  (define lng 0)
  (for-each (lambda (byt) (set! lng (+ byt (* 256 lng)))) lst)
  lng)
(define (bytes->long lst)
  (define lng (bytes->ulong lst))
  (if (>= lng #x80000000)
      (logior #x-80000000 (logand #x7FFFFFFF lng))
      lng))
(define (bytes->ushort lst)
  (+ (* 256 (car lst)) (cadr lst)))
(define (bytes->short lst)
  (define lng (bytes->ushort lst))
  (if (>= lng #x8000)
      (logior #x-8000 (logand #x7FFF lng))
      lng))

(define (bytes->ieee-float lst)
  (define S (logbit? 7 (car lst)))
  (define E (+ (ash (logand #x7F (car lst)) 1)
	       (ash (logand #x80 (cadr lst)) -7)))
  (define F 0)
  (if (not (eqv? 4 (length lst)))
      (slib:error 'bytes->ieee-float 'wrong 'length (length lst)))
  (for-each (lambda (byt) (set! F (+ byt (/ F 256))))
	    (reverse (cons (logand #x7F (cadr lst)) (cddr lst))))
  (set! F (/ F 128))
  (cond ((< 0 E 255) (* (if S -1 1) (expt 2 (- E 127)) (+ 1 F)))
	((zero? E)
	 (if (zero? F)
	     (if S -0.0 0.0)
	     (* (if S -1 1) (expt 2 -126) F)))
	;; E must be 255
	((not (zero? F)) (/ 0.0 0.0))
	(else (/ (if S -1.0 1.0) 0.0))))

(define (bytes->ieee-double lst)
  (define S (logbit? 7 (car lst)))
  (define E (+ (ash (logand #x7F (car lst)) 4)
	       (ash (logand #xF0 (cadr lst)) -4)))
  (define F 0)
  (if (not (eqv? 8 (length lst)))
      (slib:error 'bytes->ieee-double 'wrong 'length (length lst)))
  (for-each (lambda (byt) (set! F (+ byt (/ F 256))))
	    (reverse (cons (logand #x0F (cadr lst)) (cddr lst))))
  (set! F (/ F 16))
  (cond ((< 0 E 2047) (* (if S -1 1) (expt 2 (- E 1023)) (+ 1 F)))
	((zero? E)
	 (if (zero? F)
	     (if S -0.0 0.0)
	     (* (if S -1 1) (expt 2 -1022) F)))
	;; E must be 2047
	((not (zero? F)) (/ 0.0 0.0))
	(else (/ (if S -1.0 1.0) 0.0))))

(define (matfile:read-bytes cnt port)
  (do ((idx (+ -1 cnt) (+ -1 idx))
       (lst '() (cons (read-byte port) lst)))
      ((negative? idx) lst)))

(define (matfile:read-matrix port)
  (define null (integer->char 0))
  (define lst (matfile:read-bytes 4 port))
  (define (read1 endian)
    (define type (bytes->long (endian lst)))
    (let ((d-prot (modulo (quotient type 10) 10))
	  (d-endn (case (quotient type 1000)
		    ((0			;ieee-little-endian
		      2			;vax-d-float
		      3) identity)	;vag-g-float
		    ((1			;ieee-big-endian
		      4) reverse)	;cray
		    (else #f)))
	  (m-type (case (modulo type 10)
		    ((0) 'numeric)
		    ((1) 'text)
		    ((2) 'sparse)
		    (else #f))))
      (define d-leng (case d-prot
		       ((0) 8)
		       ((1 2) 4)
		       ((3 4) 2)
		       ((5) 1)
		       (else #f)))
      (define d-conv (case d-prot
		       ((0) (case (quotient type 1000)
			      ((0 1) bytes->ieee-double)
			      ((2) bytes->vax-d-double)
			      ((3) bytes->vax-g-double)
			      ((4) bytes->cray-double)))
		       ((1) (case (quotient type 1000)
			      ((0 1) bytes->ieee-float)
			      ((2) bytes->vax-d-float)
			      ((3) bytes->vax-g-float)
			      ((4) bytes->cray-float)))
		       ((2) bytes->long)
		       ((3) bytes->short)
		       ((4) bytes->ushort)
		       ((5) (if (eqv? 'text m-type)
				(lambda (lst) (integer->char (car lst)))
				car))
		       (else #f)))
      (cond ((and (= 0 (modulo (quotient type 100) 10) (quotient type 65536))
		  d-leng d-endn m-type)
	     (let* ((mrows (bytes->long (endian (matfile:read-bytes 4 port))))
		    (ncols (bytes->long (endian (matfile:read-bytes 4 port))))
		    (imagf (bytes->long (endian (matfile:read-bytes 4 port))))
		    (namlen
		     (+ -1 (bytes->long (endian (matfile:read-bytes 4 port))))))
	       ;;(@print d-leng d-endn m-type type mrows ncols imagf)
	       (set! imagf (case imagf
			     ((0) #f)
			     ((1) #t)
			     (else (slib:error 'bad 'imagf imagf))))
	       (let ((namstr (make-string namlen))
		     (mat (case m-type
			    ((numeric) (create-array
					(case d-prot
					  ((0) ((if imagf Ac64 Ar64)))
					  ((1) ((if imagf Ac32 Ar32)))
					  ((2) (As32))
					  ((3) (As16))
					  ((4) (Au16))
					  ((5) (Au8))
					  (else (slib:error 'p 'type d-prot)))
					mrows ncols))
			    ((text)    (create-array "." mrows ncols))
			    ((sparse)  (slib:error 'sparse '?)))))
		 (do ((idx 0 (+ 1 idx)))
		     ((>= idx namlen))
		   (string-set! namstr idx (read-char port)))
		 ;;(@print namstr)
		 (if (not (eqv? null (read-char port)))
		     (slib:error 'matfile 'string 'missing null))
		 (do ((jdx 0 (+ 1 jdx)))
		     ((>= jdx ncols))
		   (do ((idx 0 (+ 1 idx)))
		       ((>= idx mrows))
		     (array-set! mat (d-conv (d-endn (matfile:read-bytes
						      d-leng port)))
				 idx jdx)))
		 (if imagf
		     (do ((jdx 0 (+ 1 jdx)))
			 ((>= jdx ncols))
		       (do ((idx 0 (+ 1 idx)))
			   ((>= idx mrows))
			 (array-set! mat
				     (+ (* (d-conv (d-endn (matfile:read-bytes
							    d-leng port)))
					   +i)
					(array-ref mat idx jdx))
				     idx jdx))))
		 (list namstr mat))))
	    (else #f))))
  (or (read1 identity) (read1 reverse)))

;;@body @1 should be a string naming an existing file containing a
;;MATLAB Version 4 MAT-File.  The @0 procedure reads matrices from the
;;file and returns a list of the results; a list of the name string and
;;array for each matrix.
(define (matfile:read filename)
  (call-with-open-ports
   (open-file filename 'rb)
   (lambda (port)
     (do ((mats '() (cons (matfile:read-matrix port) mats)))
	 ((eof-object? (peek-char port))
	  (reverse mats))))))

;;@body @1 should be a string naming an existing file containing a
;;MATLAB Version 4 MAT-File.  The @0 procedure reads matrices from the
;;file and defines the @code{string-ci->symbol} for each matrix to its
;;corresponding array.  @0 returns a list of the symbols defined.
(define (matfile:load filename)
  (require 'string-case)
  (let ((mats (matfile:read filename)))
    (for-each (lambda (nam-mat)
		(slib:eval
		 (list 'define
		       (string-ci->symbol (car nam-mat))
		       (list 'quote (cadr nam-mat)))))
	      mats)
    (map string-ci->symbol (map car mats))))
