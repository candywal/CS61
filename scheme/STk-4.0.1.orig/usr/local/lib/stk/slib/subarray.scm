;;;;"subarray.scm" Scheme array accessory procedures.
; Copyright (C) 2002 Aubrey Jaffer and Radey Shouman
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

(require 'array)

;;@code{(require 'subarray)}
;;@ftindex subarray

;;@body
;;selects a subset of an array.  For @1 of rank n, there must be at least
;;n @2 arguments.  For 0 <= @i{j} < n, @2@i{j} is either an integer, a
;;list of two integers within the range for the @i{j}th index, or #f.
;;
;;When @2@i{j} is a list of two integers, then the @i{j}th index is
;;restricted to that subrange in the returned array.
;;
;;When @2@i{j} is #f, then the full range of the @i{j}th index is
;;accessible in the returned array.  An elided argument is equivalent to #f.
;;
;;When @2@i{j} is an integer, then the rank of the returned array is
;;less than @1, and only elements whose @i{j}th index equals @2@i{j} are
;;shared.
;;
;;@example
;;> (define ra '#2A((a b c) (d e f)))
;;#<unspecified>
;;> (subarray ra 0 #f)
;;#1A(a b c)
;;> (subarray ra 1 #f)
;;#1A(d e f)
;;> (subarray ra #f 1)
;;#1A(b e)
;;> (subarray ra '(0 1) #f)
;;#2A((a b c) (d e f))
;;> (subarray ra #f '(0 1))
;;#2A((a b) (d e))
;;> (subarray ra #f '(1 2))
;;#2A((b c) (e f))
;;@end example
(define (subarray array . selects)
  (apply make-shared-array array
	 (lambda args
	   (let loop ((sels selects)
		      (args args)
		      (lst '()))
	     (cond ((null? sels)
                    (if (null? args)
                        (reverse lst)
                        (loop sels (cdr args) (cons (car args) lst))))
		   ((number? (car sels))
		    (loop (cdr sels) args (cons (car sels) lst)))
		   (else
		    (loop (cdr sels) (cdr args) (cons (car args) lst))))))
         (let loop ((sels selects)
                    (shp (array-shape array))
                    (nshp '()))
           (cond ((null? shp)
                  (if (null? sels)
                      (reverse nshp)
                      (slib:error 'subarray 'rank (array-rank array) 'mismatch selects)))
                 ((null? sels)
                  (loop sels (cdr shp) (cons (car shp) nshp)))
                 ((not (car sels))
                  (loop (cdr sels) (cdr shp) (cons (car shp) nshp)))
                 ((integer? (car sels))
                  (loop (cdr sels) (cdr shp) nshp))
                 (else
                  (loop (cdr sels) (cdr shp) (cons (car sels) nshp)))))))

;;@body
;;Behaves like @r{subarray}, but @r{align}s the returned array origin to
;;0 @dots{}.
(define (subarray0 array . selects)
  (define ra (apply subarray array selects))
  (apply array-align ra (map (lambda (x) 0) (array-shape ra))))

;;@body
;;
;;Returns an array shared with @1 but with a different origin.  The @2
;;are the exact integer coordinates of the new origin.  Indexes
;;corresponding to missing or #f coordinates are not realigned.
;;
;;For example:
;;@example
;;(define ra2 (create-array '#(5) '(5 9) '(-4 0)))
;;(array-shape ra2)                       @result{} ((5 9) (-4 0))
;;(array-shape (array-align ra2 0 0))     @result{} ((0 4) (0 4))
;;(array-shape (array-align ra2 0))       @result{} ((0 4) (-4 0))
;;(array-shape (array-align ra2))         @result{} ((5 9) (-4 0))
;;(array-shape (array-align ra2 0 #f))    @result{} ((0 4) (-4 0))
;;(array-shape (array-align ra2 #f 0))    @result{} ((5 9) (0 4))
;;@end example
(define (array-align array . coords)
  (let* ((shape (array-shape array))
	 (offs (let recur ((shp shape)
			   (crd coords))
		 (cond ((null? shp) '())
		       ((null? crd) (map (lambda (x) 0) shp))
		       ((not (car crd)) (cons 0 (recur (cdr shp) (cdr crd))))
		       (else (cons (- (car crd) (caar shp))
				   (recur (cdr shp) (cdr crd))))))))
    (apply make-shared-array
	   array (lambda inds (map - inds offs))
	   (map (lambda (spec off)
		  (list (+ (car spec) off) (+ (cadr spec) off)))
		shape offs))))
  
;;@body
;;
;;Returns a subarray sharing contents with @1 except for slices removed
;;from either side of each dimension.  Each of the @2 is an exact
;;integer indicating how much to trim.  A positive @var{s} trims the
;;data from the lower end and reduces the upper bound of the result; a
;;negative @var{s} trims from the upper end and increases the lower
;;bound.
;;
;;For example:
;;@example
;;(array-trim '#(0 1 2 3 4) 1)  @result{} #1A(1 2 3 4) ;; shape is ((0 3))
;;(array-trim '#(0 1 2 3 4) -1) @result{} #1A(0 1 2 3) ;; shape is ((1 4))
;;
;;(require 'array-for-each)
;;(define (centered-difference ra)
;;  (array-map - (array-trim ra 1) (array-trim ra -1)))
;;(define (forward-difference ra)
;;  (array-map - (array-trim ra 1) ra))
;;(define (backward-difference ra)
;;  (array-map - ra (array-trim ra -1)))
;;
;;(centered-difference '#(0 1 3 5 9 22))
;;  @result{} #1A(3 4 6 17) ;;shape is ((1 4))
;;(backward-difference '#(0 1 3 5 9 22))
;;  @result{} #1A(1 2 2 4 13) ;; shape is ((1 5))
;;(forward-difference '#(0 1 3 5 9 22))
;;  @result{} #(1 2 2 4 13)  ;; shape is ((0 4))
;;@end example
(define (array-trim array . trims)
  (let* ((shape (array-shape array))
	 (trims (let recur ((shp shape)
			     (ss trims))
		   (cond ((null? shp) '())
			 ((null? ss) (map (lambda (x) 0) shp))
			 ((integer? (car ss))
			  (cons (car ss) (recur (cdr shp) (cdr ss))))
			 (else
			  (error 'array-trim 'bad 'trim (car ss)))))))
    (apply make-shared-array
	   array
	   (lambda inds (map + inds trims))
	   (map (lambda (spec trim)
		  (cond ((negative? trim)
			 (cons (- (car spec) trim) (cdr spec)))
			((positive? trim)
			 (list (car spec) (- (cadr spec) trim)))
			(else spec)))
		shape trims))))
