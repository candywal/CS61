;;;; "differ.scm" O(NP) Sequence Comparison Algorithm.
;;; Copyright (C) 2001, 2002 Aubrey Jaffer
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

;;@noindent
;;@code{diff:edit-length} implements the algorithm:
;;
;;@ifinfo
;;@example
;;S. Wu, E. Myers, U. Manber, and W. Miller,
;;   "An O(NP) Sequence Comparison Algorithm,"
;;   Information Processing Letters 35, 6 (1990), 317-323.
;;   @url{http://www.cs.arizona.edu/people/gene/vita.html}
;;@end example
;;@end ifinfo
;;@ifset html
;;S. Wu, <A HREF="http://www.cs.arizona.edu/people/gene/vita.html">
;;E. Myers,</A> U. Manber, and W. Miller,
;;<A HREF="http://www.cs.arizona.edu/people/gene/PAPERS/np_diff.ps">
;;"An O(NP) Sequence Comparison Algorithm,"</A>
;;Information Processing Letters 35, 6 (1990), 317-323.
;;@end ifset
;;
;;@noindent
;;The values returned by @code{diff:edit-length} can be used to gauge
;;the degree of match between two sequences.
;;
;;@noindent
;;Surprisingly, "An O(NP) Sequence Comparison Algorithm" does not derive
;;the edit sequence; only the sequence length.  Developing an algorithm
;;for computing the edit sequence preserving the "O(NP)" time bound in
;;O(N) space required a month of evenings and weekends.  Read the
;;details in:
;;
;;@ifinfo
;;@example
;;A. Jaffer,
;;   "An O(NP), Linear Space Algorithm for Computing Shortest Edit
;;   Sequences",
;;   Go Figure! 2002.
;;   @url{http://swissnet.ai.mit.edu/~jaffer/ONPACSES.html}
;;@end example
;;@end ifinfo
;;@ifset html
;;<A HREF="http://swissnet.ai.mit.edu/~jaffer/">A. Jaffer,</A>
;;<A HREF="http://swissnet.ai.mit.edu/~jaffer/ONPACSES.html">
;;"An O(NP), Linear Space Algorithm for Computing Shortest Edit
;;Sequences",</A>
;;Go Figure! 2002.
;;@end ifset
;;
;;@noindent
;;If the items being sequenced are text lines, then the computed
;;edit-list is equivalent to the output of the @dfn{diff} utility
;;program.  If the items being sequenced are words, then it is like the
;;lesser known @dfn{spiff} program.

(require 'array)
;;(require 'array-for-each)

;;; Traces runs of matches until they end; then set fp[k]=y.
;;; If CC is supplied, set each CC[y] = min(CC[y], cost) for run.
(define (fp:run fp k array1 len1 array2 len2 =? CC p)
  (define y (max (+ 1 (array-ref fp (+ -1 k))) (array-ref fp (+ 1 k))))
  (define cost (+ k p p))
  (let snloop ((x (- y k))
	       (y y))
    (and CC (<= y len2)
	 (let ((cst (- len1 x)))
	   (cond ((negative? cst))
		 (else (set! cst (+ cst cost))
		       (array-set! CC (min cst (array-ref CC y)) y)))))
    (cond ((and (< x len1) (< y len2)
		(=? (array-ref array1 x) (array-ref array2 y)))
	   (snloop (+ 1 x) (+ 1 y)))
	  (else (array-set! fp y k)
		y))))

;;; p-lim is half the number of gratuitous edits for strings of given lengths
(define (fp:compare CC array1 len1 array2 len2 =? p-lim)
  (define fp (create-array (As32 -1) (list (- (+ 1 len1)) (+ 1 len2))))
  (define Delta (- len2 len1))
  ;;(if (negative? Delta) (slib:error 'fp:compare (fp:subarray array1 0 len1) '> (fp:subarray array2 0 len2)))
  (let loop ((p 0))
    (do ((k (- p) (+ 1 k)))
	((>= k Delta))
      (fp:run fp k array1 len1 array2 len2 =? CC p))
    (do ((k (+ Delta p) (+ -1 k)))
	((<= k Delta))
      (fp:run fp k array1 len1 array2 len2 =? CC p))
    (let ((fpval (fp:run fp Delta array1 len1 array2 len2 =? CC p)))
      ;; At this point, the cost to (fpval-Delta, fpval) is Delta + 2*p
      (cond ((and (not CC) (<= len2 fpval)) (+ Delta (* 2 p)))
	    ((and p-lim (>= p p-lim)) #f)
	    (else (loop (+ 1 p)))))))

;;; Return 0-based shared array.
;;; Reverse RA if END < START.
(define (fp:subarray RA start end)
  (define n-len (abs (- end start)))
  (if (< end start)
      (make-shared-array RA (lambda (idx) (list (- start 1 idx))) n-len)
      (make-shared-array RA (lambda (idx) (list (+ start idx))) n-len)))

;;; Compute the edit length from A[] to each sub-array
;;; B[0..0], ..., B[0..N] and return the array of costs CC.
;;;
;;; p-lim is half the number of gratuitous edits required
;;; for whole sequences.  It is computed once by diff2edits.
(define (diff->costs A len-a B len-b =? p-lim)
  (if (> len-a len-b) (slib:error 'fast-Cy 'reversed len-a len-b))
  (let ((CC (create-array (As32 (+ len-a len-b)) (+ 1 len-b))))
    (fp:compare CC A len-a B len-b =? (min p-lim len-a))
    CC))

;;; Split A[start-a..end-a] (the shorter array) into smaller and smaller chunks.
;;; DELETE is procedure to delete A[j] in sequence.
;;; INSERT is procedure to insert B[j] in sequence.
(define (diff:divide-and-conquer A start-a end-a B start-b end-b insert delete =? p-lim)
  (define (fp:p D M N) (quotient (- D (- N M)) 2))
  (define mid-a (quotient (+ start-a end-a) 2))
  (define len-b (- end-b start-b))
  (let* ((CC (diff->costs (fp:subarray A start-a mid-a) (- mid-a start-a)
			  (fp:subarray B start-b end-b) len-b
			  =? p-lim))
	 (RR (diff->costs (fp:subarray A end-a mid-a)   (- end-a mid-a)
			  (fp:subarray B end-b start-b) len-b
			  =? p-lim)))
    (define mincst (+ (- end-a start-a) len-b))
    (define mid-b len-b)		;Default
    ;; RR is not longer than CC.  So do for each element of RR.
    (do ((cdx 0 (+ 1 cdx))
	 (rdx len-b (+ -1 rdx)))
	((negative? rdx))
      (let ((tcst (+ (array-ref CC cdx) (array-ref RR rdx))))
	(cond ((< tcst mincst) (set! mid-b cdx) (set! mincst tcst)))))
    ;;(print 'mid-b mid-b '==> mincst '= (array-ref CC mid-b) '+ (array-ref RR (- len-b mid-b)))
    ;;(print 'p-lim p-lim '==> (fp:p mincst (- end-a start-a) len-b) '= (fp:p (array-ref CC mid-b) (- mid-a start-a) mid-b) '+ (fp:p (array-ref RR (- len-b mid-b)) (- end-a mid-a) (- len-b mid-b)))
    (let* ((cost-l
	    (diff2e A start-a mid-a B start-b (+ start-b mid-b) insert delete =?
		    (fp:p (array-ref CC mid-b) (- mid-a start-a) mid-b)))
	   (cost-r
	    (diff2e A mid-a end-a B (+ start-b mid-b) end-b insert delete =?
		    (fp:p (array-ref RR (- len-b mid-b)) (- end-a mid-a) (- len-b mid-b)))))
      (+ cost-l cost-r))))

;;;; diff:divide-and-conquer coroutine:
;;; Diff sub-arrays, either one longer
(define (diff2e A start-a end-a B start-b end-b insert delete =? p-lim)
  (if (> (- end-a start-a) (- end-b start-b))
      (diff2 B start-b end-b A start-a end-a delete insert =? p-lim)
      (diff2 A start-a end-a B start-b end-b insert delete =? p-lim)))

;;; Diff sub-arrays, A is shorter than B.
(define (diff2 A start-a end-a B start-b end-b insert delete =? p-lim)
  (define len-a (- end-a start-a))
  (define len-b (- end-b start-b))
  (cond ((and (zero? p-lim) (= len-b len-a)) 0)
	((zero? len-a)
	 (do ((idx start-b (+ 1 idx)))
	     ((>= idx end-b))
	   (insert idx))
	 end-b)
	((zero? len-b)
	 (do ((idx start-a (+ 1 idx)))
	     ((>= idx end-a))
	   (delete idx))
	 end-a)
	((not (eqv? 1 len-a))
	 (diff:divide-and-conquer
	  A start-a end-a B start-b end-b insert delete =? p-lim))
	((not (eqv? 1 len-b))
	 (let ((found? #f)
	       (elt-a (array-ref A start-a)))
	   (do ((idx start-b (+ 1 idx)))
	       ((>= idx end-b))
	     (cond (found? (insert idx))
		   ((=? (array-ref B idx) elt-a) (set! found? #t))
		   (else (insert idx))))
	   (cond (found? (+ -1 end-b))
		 (else (delete start-a)
		       (+ 1 end-b)))))
	((=? (array-ref A start-a) (array-ref B start-b)) 0)
	(else (delete start-a) (insert start-b) 2)))

(define (edits2lcs edits editlen A len-a len-b)
  (define lcs (create-array A (quotient (- (+ len-b len-a) editlen) 2)))
  (let loop ((edx 0)
	     (sdx 0)
	     (adx 0))
    (let ((edit (if (< edx editlen)
		    (array-ref edits edx)
		    0)))
      (cond ((>= adx len-a) lcs)
	    ((positive? edit)
	     (loop (+ 1 edx) sdx adx))
	    ((zero? edit)
	     (array-set! lcs (array-ref A adx) sdx)
	     (loop edx (+ 1 sdx) (+ 1 adx)))
	    ((>= adx (- -1 edit))
	     (loop (+ 1 edx) sdx (+ 1 adx)))
	    (else
	     (array-set! lcs (array-ref A adx) sdx)
	     (loop edx (+ 1 sdx) (+ 1 adx)))))))

;;@args array1 array2 =? p-lim
;;@args array1 array2 =?
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.
;;
;;The non-negative integer @4, if provided, is maximum number of
;;deletions of the shorter sequence to allow.  @0 will return @code{#f}
;;if more deletions would be necessary.
;;
;;@0 returns a one-dimensional array of length @code{(quotient (- (+
;;len1 len2) (diff:edit-length @1 @2)) 2)} holding the longest sequence
;;common to both @var{array}s.
(define (diff:longest-common-subsequence A B =? . p-lim)
  (define len-a (car (array-dimensions a)))
  (define len-b (car (array-dimensions b)))
  (set! p-lim (if (null? p-lim) #f (car p-lim)))
  (if (< len-b len-a)
      (let ((edits (diff2edits B len-b A len-a #f =? p-lim)))
	(define editlen (car (array-dimensions edits)))
	(and edits (edits2lcs edits editlen B len-b len-a)))
      (let ((edits (diff2edits A len-a B len-b #f =? p-lim)))
	(define editlen (car (array-dimensions edits)))
	(and edits (edits2lcs edits editlen A len-a len-b)))))

(define (diff2edits A len-a B len-b swap? =? p-lim)
  (require 'sort)
  (if (> len-a len-b) (slib:error 'diff2edits len-a '> len-b))
  (let ((cost (fp:compare #f A len-a B len-b =? p-lim)))
    (define edx 0)
    (define edits (and cost (make-vector cost)))
    (define (insert bdx)
      (array-set! edits (+ 1 bdx) edx)
      (set! edx (+ 1 edx)))
    (define (delete adx)
      (array-set! edits (- -1 adx) edx)
      (set! edx (+ 1 edx)))
    (and cost
	 (diff2 A 0 len-a B 0 len-b
		(if swap? delete insert)
		(if swap? insert delete)
		=?
		(quotient (- cost (- len-b len-a)) 2)) ; given cost, compute p
	 (sort! edits (lambda (idx jdx)
			(define dff (- (abs jdx) (abs idx)))
			(cond ((positive? dff) #t)
			      ((negative? dff) #f)
			      ((< idx jdx) #t)
			      (else #f)))))))

;;@args array1 array2 =? p-lim
;;@args array1 array2 =?
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.
;;
;;The non-negative integer @4, if provided, is maximum number of
;;deletions of the shorter sequence to allow.  @0 will return @code{#f}
;;if more deletions would be necessary.
;;
;;@0 returns a vector of length @code{(diff:edit-length @1 @2)} composed
;;of a shortest sequence of edits transformaing @1 to @2.
;;
;;Each edit is an integer:
;;@table @asis
;;@item @var{k} > 0
;;Inserts @code{(array-ref @1 (+ -1 @var{j}))} into the sequence.
;;@item @var{k} < 0
;;Deletes @code{(array-ref @2 (- -1 @var{k}))} from the sequence.
;;@end table
(define (diff:edits A B =? . p-lim)
  (define len-a (car (array-dimensions a)))
  (define len-b (car (array-dimensions b)))
  (set! p-lim (if (null? p-lim) #f (car p-lim)))
  (if (< len-b len-a)
      (diff2edits B len-b A len-a #t =? p-lim)
      (diff2edits A len-a B len-b #f =? p-lim)))

;;@args array1 array2 =? p-lim
;;@args array1 array2 =?
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.
;;
;;The non-negative integer @4, if provided, is maximum number of
;;deletions of the shorter sequence to allow.  @0 will return @code{#f}
;;if more deletions would be necessary.
;;
;;@0 returns the length of the shortest sequence of edits transformaing
;;@1 to @2.
(define (diff:edit-length A B =? . p-lim)
  (define len-a (car (array-dimensions a)))
  (define len-b (car (array-dimensions b)))
  (set! p-lim (if (null? p-lim) #f (car p-lim)))
  (if (< len-b len-a)
      (fp:compare #f B len-b A len-a =? p-lim)
      (fp:compare #f A len-a B len-b =? p-lim)))

;;@example
;;(diff:longest-common-subsequence "fghiejcklm" "fgehijkpqrlm" eqv?)
;;@result{} "fghijklm"
;;
;;(diff:edit-length "fghiejcklm" "fgehijkpqrlm" eqv?)
;;@result{} 6
;;
;;(diff:edits "fghiejcklm" "fgehijkpqrlm" eqv?)
;;@result{} #As32(3 -5 -7 8 9 10)
;;       ; e  c  h p q  r
;;@end example
