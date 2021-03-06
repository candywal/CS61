; "hashtab.scm", hash tables for Scheme.
; Copyright (c) 1992, 1993 Aubrey Jaffer
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

(require 'hash)
(require 'alist)

(define (predicate->hash pred)
  (cond ((eq? pred eq?) hashq)
	((eq? pred eqv?) hashv)
	((eq? pred equal?) hash)
	((eq? pred =) hashv)
	((eq? pred char=?) hashv)
	((eq? pred char-ci=?) hashv)
	((eq? pred string=?) hash)
	((eq? pred string-ci=?) hash)
	(else (slib:error "unknown predicate for hash" pred))))

(define (make-hash-table k) (make-vector k '()))

(define (predicate->hash-asso pred)
  (let ((hashfun (predicate->hash pred))
	(asso (predicate->asso pred)))
    (lambda (key hashtab)
      (asso key
	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))

(define (hash-inquirer pred)
  (let ((hashfun (predicate->hash pred))
	(ainq (alist-inquirer pred)))
    (lambda (hashtab key)
      (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
	    key))))

(define (hash-associator pred)
  (let ((hashfun (predicate->hash pred))
	(asso (alist-associator pred)))
    (lambda (hashtab key val)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (asso (vector-ref hashtab num) key val)))
      hashtab)))

(define (hash-remover pred)
  (let ((hashfun (predicate->hash pred))
	(arem (alist-remover pred)))
    (lambda (hashtab key)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (arem (vector-ref hashtab num) key)))
      hashtab)))

(define (hash-map proc ht)
  (define nht (make-vector (vector-length ht)))
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i) nht)
    (vector-set!
     nht i
     (alist-map proc (vector-ref ht i)))))

(define (hash-for-each proc ht)
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i))
    (alist-for-each proc (vector-ref ht i))))

(define (hash-rehasher pred)
  (let ((hashfun (predicate->hash pred)))
    (lambda (hashtab newk)
      (let ((newtab (make-hash-table newk)))
	(hash-for-each
	 (lambda (key value)
	   (let ((num (hashfun key newk)))
	     (vector-set! newtab num
			  (cons (cons key value)
				(vector-ref newtab num)))))
	 hashtab)
	newtab))))

