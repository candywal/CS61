;;;; "charplot.scm", plotting on character devices for Scheme
;;; Copyright (C) 1992, 1993, 2001 Aubrey Jaffer
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

(require 'printf)
(require 'array)
(require 'array-for-each)

;;; These determine final graph size.
(define charplot:dimensions #f)

;;; The left margin and legends
(define charplot:y-margin 12)

(define charplot:xborder #\_)
(define charplot:yborder #\|)
(define charplot:xaxchar #\-)
(define charplot:yaxchar #\:)
(define charplot:xtick   #\.)
(define charplot:bar     #\I)
(define charplot:curves  "*+x@#$%&='")

;;;Converts X to a string whose length is at most MWID.
(define (charplot:number->string x mwid)
  (define str (sprintf #f "%g" x))
  (if (> (string-length str) mwid)
      (substring str 0 mwid)
      str))

;;;SCALE is a list of numerator and denominator.
(define charplot:scale-it
  (if (provided? 'inexact)
      (lambda (z scale)
	(inexact->exact (round (/ (* z (car scale)) (cadr scale)))))
      (lambda (z scale)
	(quotient (+ (* z (car scale)) (quotient (cadr scale) 2))
		  (cadr scale)))))

;;;Given the width or height (in characters) and the data-span, returns
;;;a list of numerator and denominator suitable for passing as a second
;;;argument to CHARPLOT:SCALE-IT.
(define (charplot:find-scale isize delta)
  (define (fs2)
    (cond ((< (* delta 8) isize) 8)
	  ((< (* delta 6) isize) 6)
	  ((< (* delta 5) isize) 5)
	  ((< (* delta 4) isize) 4)
	  ((< (* delta 3) isize) 3)
	  ((< (* delta 2) isize) 2)
	  (else 1)))
  (cond ((zero? delta) (set! delta 1))
	((inexact? delta) (set! isize (exact->inexact isize))))
  (do ((d 1 (* d 10)))
      ((<= delta isize)
       (do ((n 1 (* n 10)))
	   ((>= (* delta 10) isize)
	    (list (* n (fs2)) d))
	 (set! delta (* delta 10))))
    (set! isize (* isize 10))))

(define (charplot:make-array)
  (let* ((dims (or charplot:dimensions
		   (list (output-port-height (current-output-port))
			 (output-port-width (current-output-port)))))
	 (height (or (car dims) (output-port-height (current-output-port))))
	 (width (or (cadr dims) (output-port-width (current-output-port)))))
    (define pra (create-array " "  height (+ 1 width)))
    ;;Put newlines on right edge
    (do ((idx (+ -1 height) (+ -1 idx)))
	((negative? idx))
      (array-set! pra #\newline idx width))
    pra))

;;;Creates and initializes character array with axes, scales, and
;;;labels.
(define (charplot:init-array pra xlabel ylabel xmin xscale ymin yscale)
  (define plot-height (- (car (array-dimensions pra)) 3))
  (define plot-width (- (cadr (array-dimensions pra)) charplot:y-margin 4))
  (define xaxis (- (charplot:scale-it ymin yscale)))
  (define yaxis (- (charplot:scale-it xmin xscale)))
  ;;CL is the left edge of WIDTH field
  (define (center-field str width ln cl)
    (define len (string-length str))
    (if (< width len)
	(center-field (substring str 0 width) width ln cl)
	(do ((cnt (+ -1 len) (+ -1 cnt))
	     (adx (+ (quotient (- width len) 2) cl) (+ 1 adx))
	     (idx 0 (+ 1 idx)))
	    ((negative? cnt))
	  (array-set! pra (string-ref str idx) ln adx))))

  ;;y-label
  (center-field ylabel (+ charplot:y-margin -1) 0 0)

  ;;horizontal borders, x-axis, and ticking
  (let* ((bb (+ 1 plot-height))
	 (bn (+ 1 bb))
	 (xstep (if (= 1 (gcd (car xscale) 3)) 10 12))
	 (xstep/2 (quotient (- xstep 2) 2))
	 (faxis (modulo (+ charplot:y-margin yaxis) xstep))
	 (faxis/2 (modulo (+ charplot:y-margin yaxis xstep/2 1) xstep))
	 (fudge (modulo yaxis xstep)))
    (do ((cl (+ charplot:y-margin -1) (+ 1 cl)))
	((>= cl (+ plot-width charplot:y-margin)))
      (array-set! pra charplot:xborder 0 cl)
      (array-set! pra
		  (cond ((eqv? faxis (modulo cl xstep)) charplot:yaxchar)
			((eqv? faxis/2 (modulo cl xstep)) charplot:xtick)
			(else charplot:xborder))
		  bb cl)
      (if (<= 0 xaxis plot-height)
	  (array-set! pra charplot:xaxchar (- plot-height xaxis) cl)))

    ;;x-label
    (center-field xlabel (+ -1 charplot:y-margin) bn 0)

    ;;horizontal coordinates
    (do ((i fudge (+ i xstep))
	 (cl (+ charplot:y-margin fudge (- xstep/2)) (+ xstep cl)))
	((> i plot-width))
      (center-field (charplot:number->string
		     (/ (* (- i yaxis) (cadr xscale))
			(car xscale))
		     xstep)
		    xstep bn cl)))

  ;;vertical borders and y-axis
  (do ((ht plot-height (- ht 1)))
      ((negative? ht))
    (array-set! pra charplot:yborder (+ 1 ht) (+ charplot:y-margin -2))
    (array-set! pra charplot:yborder (+ 1 ht) (+ charplot:y-margin plot-width))
    (if (< -1 yaxis plot-width)
	(array-set! pra charplot:yaxchar (+ 1 ht) (+ charplot:y-margin yaxis))))

  ;;vertical ticking and coordinates
  (do ((ht (- plot-height 1) (- ht 1))
       (ln 1 (+ 1 ln)))
      ((negative? ht))
    (let ((ystep (if (= 1 (gcd (car yscale) 3)) 2 3)))
      (if (zero? (modulo (- ht xaxis) ystep))
	  (let* ((v (charplot:number->string (/ (* (- ht xaxis) (cadr yscale))
						(car yscale))
					     (+ charplot:y-margin -2)))
		 (len (string-length v)))
	    (center-field v len ln (- charplot:y-margin 2 len)) ;Actually flush right
	    (array-set! pra charplot:xaxchar ln (+ charplot:y-margin -1))))))
  pra)

;;;Converts data to list of coordinates (list).
(define (charplot:data->lists data)
  (cond ((array? data)
	 (case (array-rank data)
	   ((1) (set! data (map list
				(let ((ra (apply create-array '#()
						 (array-shape data))))
				  (array-index-map! ra identity)
				  (array->list ra))
				(array->list data))))
	   ((2) (set! data (array->list data)))))
	((and (pair? (car data)) (not (list? (car data))))
	 (set! data (map (lambda (lst) (list (car lst) (cdr lst))) data))))
  (cond ((list? (cadar data))
	 (set! data (map (lambda (lst) (cons (car lst) (cadr lst))) data))))
  data)

;;;An extremum is a list of the maximum and minimum values.
;;;COORDINATE-EXTREMA returns a rank-length list of these.
(define (coordinate-extrema data)
  (define extrema (map (lambda (x) (list x x)) (car data)))
  (for-each (lambda (lst)
	      (set! extrema (map (lambda (x max-min)
				   (list (max x (car max-min))
					 (min x (cadr max-min))))
				 lst extrema)))
	    data)
  extrema)

;;;Count occurrences of numbers within evenly spaced ranges; and return
;;;lists of coordinates for graph.
(define (histobins data plot-width)
  (define datcnt (length data))
  (define xmax (apply max data))
  (define xmin (apply min data))
  (let* ((xscale (charplot:find-scale plot-width (- xmax xmin)))
	 (ix-min (charplot:scale-it xmin xscale))
	 (actual-width (- (charplot:scale-it xmax xscale)
			  (charplot:scale-it xmin xscale)
			  -1))
	 (xinc (/ (- xmax xmin) actual-width))
	 (bins (make-vector actual-width 0)))
    (for-each (lambda (x)
		(define idx (- (charplot:scale-it x xscale) ix-min))
		(if (< -1 idx actual-width)
		    (vector-set! bins idx (+ 1 (vector-ref bins idx)))
		    (slib:error x (/ (* x (car xscale)) (cadr xscale))
				(+ ix-min idx))))
	      data)
    (map list
	 (do ((idx (+ -1 (vector-length bins)) (+ -1 idx))
	      (xvl xmax (- xvl xinc))
	      (lst '() (cons xvl lst)))
	     ((negative? idx) lst))
	 (vector->list bins))))

;;;Plot histogram of DATA.
(define (histograph data label)
  (if (vector? data) (set! data (vector->list data)))
  (charplot:plot (histobins data
			    (- (or (and charplot:dimensions
					(cadr charplot:dimensions))
				   (output-port-width (current-output-port)))
			       charplot:y-margin 3))
		 label "" #t))

(define (charplot:plot data xlabel ylabel . histogram?)
  (define clen (string-length charplot:curves))
  (set! histogram? (if (null? histogram?) #f (car histogram?)))
  (set! data (charplot:data->lists data))
  (let* ((pra (charplot:make-array))
	 (plot-height (- (car (array-dimensions pra)) 3))
	 (plot-width (- (cadr (array-dimensions pra)) charplot:y-margin 4))
	 (extrema (coordinate-extrema data))
	 (xmax (caar extrema))
	 (xmin (cadar extrema))
	 (ymax (apply max (map car (cdr extrema))))
	 (ymin (apply min (map cadr (cdr extrema))))
	 (xscale (charplot:find-scale plot-width (- xmax xmin)))
	 (yscale (charplot:find-scale plot-height (- ymax ymin)))
	 (ix-min (- (charplot:scale-it xmin xscale) charplot:y-margin))
	 (ybot (charplot:scale-it ymin yscale))
	 (iy-min (+ ybot plot-height)))
    (charplot:init-array pra xlabel ylabel xmin xscale ymin yscale)
    (for-each (if histogram?
		  ;;display data bars
		  (lambda (datum)
		    (define x (- (charplot:scale-it (car datum) xscale) ix-min))
		    (do ((y (charplot:scale-it (cadr datum) yscale) (+ -1 y)))
			((< y ybot))
		      (array-set! pra charplot:bar (- iy-min y) x)))
		  ;;display data points
		  (lambda (datum)
		    (define x (- (charplot:scale-it (car datum) xscale) ix-min))
		    (define cdx 0)
		    (for-each
		     (lambda (y)
		       (array-set! pra (string-ref charplot:curves cdx)
				   (- iy-min (charplot:scale-it y yscale)) x)
		       (set! cdx (modulo (+ 1 cdx) clen)))
		     (cdr datum))))
	      data)
    (array-for-each write-char pra)))

(define (charplot:plot-function func vlo vhi . npts)
  (set! npts (if (null? npts) 64 (car npts)))
  (let ((dats (create-array (Ar32) npts 2)))
    (array-index-map! (make-shared-array dats (lambda (idx) (list idx 0)) npts)
		      (lambda (idx) (+ vlo (* (- vhi vlo) (/ idx npts)))))
    (array-map! (make-shared-array dats (lambda (idx) (list idx 1)) npts)
		func
		(make-shared-array dats (lambda (idx) (list idx 0)) npts))
    (charplot:plot dats "" "")))

(define (plot . args)
  (if (procedure? (car args))
      (apply charplot:plot-function args)
      (apply charplot:plot args)))
(define plot-function plot)
(define plot! plot)
