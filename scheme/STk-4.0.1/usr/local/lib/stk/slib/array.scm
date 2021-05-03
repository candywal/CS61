;;;;"array.scm" Arrays for Scheme
; Copyright (C) 2001 Aubrey Jaffer
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

;;@code{(require 'array)}
;;@ftindex array

(require 'record)

(define array:rtd
  (make-record-type "array"
		    '(shape
		      scales		;list of dimension scales
		      offset		;exact integer
		      store		;data
		      )))

(define array:shape
  (let ((shape (record-accessor array:rtd 'shape)))
    (lambda (array)
      (cond ((vector? array) (list (list 0 (+ -1 (vector-length array)))))
	    ((string? array) (list (list 0 (+ -1 (string-length array)))))
	    (else (shape array))))))

(define array:scales
  (let ((scales (record-accessor array:rtd 'scales)))
    (lambda (obj)
      (cond ((string? obj) '(1))
	    ((vector? obj) '(1))
	    (else (scales obj))))))

(define array:store
  (let ((store (record-accessor array:rtd 'store)))
    (lambda (obj)
      (cond ((string? obj) obj)
	    ((vector? obj) obj)
	    (else (store obj))))))

(define array:offset
  (let ((offset (record-accessor array:rtd 'offset)))
    (lambda (obj)
      (cond ((string? obj) 0)
	    ((vector? obj) 0)
	    (else (offset obj))))))

(define array:construct
  (record-constructor array:rtd '(shape scales offset store)))

;;@args obj
;;Returns @code{#t} if the @1 is an array, and @code{#f} if not.
(define array?
  (let ((array:array? (record-predicate array:rtd)))
    (lambda (obj) (or (string? obj) (vector? obj) (array:array? obj)))))

;;@noindent
;;@emph{Note:} Arrays are not disjoint from other Scheme types.  Strings
;;and vectors also satisfy @code{array?}.  A disjoint array predicate can
;;be written:
;;
;;@example
;;(define (strict-array? obj)
;;  (and (array? obj) (not (string? obj)) (not (vector? obj))))
;;@end example

;;@body
;;Returns @code{#t} if @1 and @2 have the same rank and shape and the
;;corresponding elements of @1 and @2 are @code{equal?}.
;;
;;@example
;;(array=? (create-array '#(foo) 3 3) (create-array '#(foo) '(0 2) '(0 2)))
;;  @result{} #t
;;@end example
(define (array=? array1 array2)
  (and (equal? (array:shape array1) (array:shape array2))
       (equal? (array:store array1) (array:store array2))))

(define (array:dimensions->shape dims)
  (map (lambda (dim) (if (list? dim) dim (list 0 (+ -1 dim)))) dims))

;;@args prototype bound1 bound2 @dots{}
;;
;;Creates and returns an array of type @1 with dimensions @2, @3,
;;@dots{} and filled with elements from @1.  @1 must be an array,
;;vector, or string.  The implementation-dependent type of the returned
;;array will be the same as the type of @1; except if that would be a
;;vector or string with non-zero origin, in which case some variety of
;;array will be returned.
;;
;;If the @1 has no elements, then the initial contents of the returned
;;array are unspecified.  Otherwise, the returned array will be filled
;;with the element at the origin of @1.
(define (create-array prototype . dimensions)
  (define range2length (lambda (bnd) (- 1 (apply - bnd))))
  ;;(if (not (array? prototype)) (set! prototype (vector prototype)))
  (let* ((shape (array:dimensions->shape dimensions))
	 (dims (map range2length shape))
	 (scales
	  (do ((dims (reverse (cdr dims)) (cdr dims))
	       (scls '(1) (cons (* (car dims) (car scls)) scls)))
	      ((null? dims) scls))))
    (array:construct
     shape
     scales
     (- (apply + (map * (map car shape) scales)))
     (if (string? prototype)
	 (case (string-length prototype)
	   ((0) (make-string (apply * dims)))
	   (else (make-string (apply * dims)
			      (string-ref prototype 0))))
	 (let ((pshape (array:shape prototype)))
	   (case (apply * (map range2length pshape))
	     ((0) (make-vector (apply * dims)))
	     (else (make-vector (apply * dims)
				(apply array-ref prototype
				       (map car pshape))))))))))

;;@noindent
;;These functions return a uniform array prototype enclosing the
;;optional argument (which must be of the correct type).  If the
;;uniform-array type is supported by the implementation, then it is
;;returned; promoting to the next larger precision type; promoting
;;finally to vector.

;;@args z
;;@args
;;Returns a high-precision complex uniform-array prototype.
(define Ac64 vector)
;;@args z
;;@args
;;Returns a complex uniform-array prototype.
(define Ac32 vector)

;;@args x
;;@args
;;Returns a high-precision real uniform-array prototype.
(define Ar64 vector)
;;@args x
;;@args
;;Returns a real uniform-array prototype.
(define Ar32 vector)

;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;64 bits of precision.
(define As64 vector)
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;32 bits of precision.
(define As32 vector)
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;16 bits of precision.
(define As16 vector)
;;@args n
;;@args
;;Returns an exact signed integer uniform-array prototype with at least
;;8 bits of precision.
(define As8  vector)

;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 64 bits of precision.
(define Au64 vector)
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 32 bits of precision.
(define Au32 vector)
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 16 bits of precision.
(define Au16 vector)
;;@args k
;;@args
;;Returns an exact non-negative integer uniform-array prototype with at
;;least 8 bits of precision.
(define Au8  vector)

;;@args bool
;;@args
;;Returns a boolean uniform-array prototype.
(define At1  vector)

;;@args initial-value bound1 bound2 @dots{}
;;
;;Creates and returns an array with dimensions @2,
;;@3, @dots{} and filled with @1.
;;
;;@0 is a legacy function -- now defined in terms of
;;@code{create-array}.
;;
;;@example
;;(define (make-array initial-value . dimensions)
;;  (apply create-array (vector initial-value) dimensions))
;;@end example
(define (make-array initial-value . dimensions)
  (apply create-array (vector initial-value) dimensions))

;;@noindent
;;When constructing an array, @var{bound} is either an inclusive range of
;;indices expressed as a two element list, or an upper bound expressed as
;;a single integer.  So
;;
;;@example
;;(create-array '#(foo) 3 3) @equiv{} (make-array '#(foo) '(0 2) '(0 2))
;;@end example

;;@args array mapper bound1 bound2 @dots{}
;;@0 can be used to create shared subarrays of other
;;arrays.  The @var{mapper} is a function that translates coordinates in
;;the new array into coordinates in the old array.  A @var{mapper} must be
;;linear, and its range must stay within the bounds of the old array, but
;;it can be otherwise arbitrary.  A simple example:
;;
;;@example
;;(define fred (create-array '#(#f) 8 8))
;;(define freds-diagonal
;;  (make-shared-array fred (lambda (i) (list i i)) 8))
;;(array-set! freds-diagonal 'foo 3)
;;(array-ref fred 3 3)
;;   @result{} FOO
;;(define freds-center
;;  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j)))
;;                     2 2))
;;(array-ref freds-center 0 0)
;;   @result{} FOO
;;@end example
(define (make-shared-array array mapper . dimensions)
  (define odl (array:scales array))
  (define rank (length dimensions))
  (define shape (array:dimensions->shape dimensions))
  (do ((idx (+ -1 rank) (+ -1 idx))
       (uvt (append (cdr (vector->list (make-vector rank 0))) '(1))
	    (append (cdr uvt) '(0)))
       (uvts '() (cons uvt uvts)))
      ((negative? idx)
       (let* ((ker0 (apply + (map * odl (apply mapper uvt))))
	      (scales (map (lambda (uvt)
			     (- (apply + (map * odl (apply mapper uvt))) ker0))
			   uvts)))
	 (array:construct
	  shape
	  scales
	  (- (apply + (array:offset array)
		    (map * odl (apply mapper (map car shape))))
	     (apply + (map * (map car shape) scales)))
	  (array:store array))))))

;;@body
;;Returns the number of dimensions of @1.  If @1 is not an array, 0 is
;;returned.
(define (array-rank obj)
  (if (array? obj) (length (array:shape obj)) 0))

;;@args array
;;Returns a list of inclusive bounds.
;;
;;@example
;;(array-shape (create-array '#() 3 5))
;;   @result{} ((0 2) (0 4))
;;@end example
(define array-shape array:shape)

;;@body
;;@code{array-dimensions} is similar to @code{array-shape} but replaces
;;elements with a 0 minimum with one greater than the maximum.
;;
;;@example
;;(array-dimensions (create-array '#() 3 5))
;;   @result{} (3 5)
;;@end example
(define (array-dimensions array)
  (map (lambda (bnd) (if (zero? (car bnd)) (+ 1 (cadr bnd)) bnd))
       (array:shape array)))

(define (array:in-bounds? array indices)
  (do ((bnds (array:shape array) (cdr bnds))
       (idxs indices (cdr idxs)))
      ((or (null? bnds)
	   (null? idxs)
	   (not (integer? (car idxs)))
	   (not (<= (caar bnds) (car idxs) (cadar bnds))))
       (and (null? bnds) (null? idxs)))))

;;@args array index1 index2 @dots{}
;;Returns @code{#t} if its arguments would be acceptable to
;;@code{array-ref}.
(define (array-in-bounds? array . indices)
  (array:in-bounds? array indices))

;;@args array index1 index2 @dots{}
;;Returns the (@2, @3, @dots{}) element of @1.
(define (array-ref array . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (slib:error 'array-ref 'bad-indices indices))
  ((if (string? store) string-ref vector-ref)
   store (apply + (array:offset array) (map * (array:scales array) indices))))

;;@args array obj index1 index2 @dots{}
;;Stores @2 in the (@3, @4, @dots{}) element of @1.  The value returned
;;by @0 is unspecified.
(define (array-set! array obj . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (slib:error 'array-set! 'bad-indices indices))
  ((if (string? store) string-set! vector-set!)
   store (apply + (array:offset array) (map * (array:scales array) indices))
   obj))

;;; Legacy functions

;; These procedures are fast versions of @code{array-ref} and
;; @code{array-set!} for non-string arrays; they take a fixed number of
;; arguments and perform no bounds checking.
(define array-1d-ref array-ref)
(define array-2d-ref array-ref)
(define array-3d-ref array-ref)
(define array-1d-set! array-set!)
(define array-2d-set! array-set!)
(define array-3d-set! array-set!)
