;;; "pnm.scm" Read PNM image files.
; Copyright 2000 Aubrey Jaffer
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

(require 'scanf)
(require 'printf)
(require 'array)
(require 'subarray)
(require 'array-for-each)
(require 'byte)
(require 'line-i/o)

(define (pnm:read+integer port)
  (define uint #f)
  (do ((chr (peek-char port) (peek-char port)))
      ((not (and (char? chr) (or (char-whitespace? chr) (eqv? #\# chr)))))
    (if (eqv? #\# chr)
	(read-line port)
	(read-char port)))
  (if (eof-object? (peek-char port))
      (peek-char port)
      (and (eqv? 1 (fscanf port " %u" uint)) uint)))

(define (pnm:read-pbm-char port)
  (let loop ((chr (read-char port)))
    (case chr
      ((#\0) #f)
      ((#\1) #t)
      ((#\#)
       (read-line port)
       (loop (read-char port)))
      (else
       (if (char-whitespace? chr)
           (loop (read-char port))
           (slib:error chr 'unexpected 'character))))))

(define (pnm:type-dimensions port)
  (if (input-port? port)
      (let* ((c1 (read-char port))
	     (c2 (read-char port)))
	(cond
	 ((and (eqv? #\P c1)
	       (char? c2)
	       (char-numeric? c2)
	       (char-whitespace? (peek-char port)))
	  (let* ((format (string->symbol (string #\p c2)))
		 (width (pnm:read+integer port))
		 (height (pnm:read+integer port))
		 (ret
		  (case format
		    ((p1) (list 'pbm     width height 1))
		    ((p4) (list 'pbm-raw width height 1))
		    ((p2) (list 'pgm     width height (pnm:read+integer port)))
		    ((p5) (list 'pgm-raw width height (pnm:read+integer port)))
		    ((p3) (list 'ppm     width height (pnm:read+integer port)))
		    ((p6) (list 'ppm-raw width height (pnm:read+integer port)))
		    (else #f))))
	    (and (char-whitespace? (read-char port)) ret)))
	 (else #f)))
      (call-with-open-ports (open-file port 'rb') pnm:type-dimensions)))

(define (pnm:read-bit-vector! array port)
  (define dims (array-dimensions array))
  (let* ((height (car (array-dimensions array)))
	 (width (cadr (array-dimensions array)))
	 (wid8 (logand -8 width)))
    (do ((jdx 0 (+ 1 jdx)))
	((>= jdx height))
      (let ((row (subarray array jdx)))
	(do ((idx 0 (+ 8 idx)))
	    ((>= idx wid8)
	     (if (< idx width)
		 (let ((byt (read-byte port)))
		   (do ((idx idx (+ 1 idx))
			(bdx 7 (+ -1 bdx)))
		       ((>= idx width))
		     (array-set! row (logbit? bdx byt) idx)))))
	  (let ((byt (read-byte port)))
	    (array-set! row (logbit? 7 byt) (+ 0 idx))
	    (array-set! row (logbit? 6 byt) (+ 1 idx))
	    (array-set! row (logbit? 5 byt) (+ 2 idx))
	    (array-set! row (logbit? 4 byt) (+ 3 idx))
	    (array-set! row (logbit? 3 byt) (+ 4 idx))
	    (array-set! row (logbit? 2 byt) (+ 5 idx))
	    (array-set! row (logbit? 1 byt) (+ 6 idx))
	    (array-set! row (logbit? 0 byt) (+ 7 idx)))))))
  (if (eof-object? (peek-char port))
      array
      (do ((chr (read-char port) (read-char port))
	   (cnt 0 (+ 1 cnt)))
	  ((eof-object? chr) (slib:error type cnt 'bytes 'remain 'in port)))))

(define (pnm:image-file->array path . array)
  (set! array (and (not (null? array)) (car array)))
  (call-with-open-ports
   (open-file path 'rb)
   (lambda (port)
     (apply (lambda (type width height max-pixel)
	      (define (read-binary)
		(array-map! array (lambda () (read-byte port)))
		(if (eof-object? (peek-char port)) array
		    (slib:error type 'not 'at 'file 'end)))
	      (define (read-text)
		(array-map! array (lambda () (pnm:read+integer port))) ;too slow
		;;(array-map! array (lambda () (read port))) ;fast but dangerous
		(if (not (eof-object? (pnm:read+integer port)))
		    (slib:warn type 'not 'at 'file 'end))
		array)
	      (define (read-pbm)
		(array-map! array (lambda () (pnm:read-pbm-char port)))
		(if (not (eof-object? (pnm:read+integer port)))
		    (slib:warn type 'not 'at 'file 'end))
		array)
	      (case type
		((pbm)
		 (or array
		     (set! array (create-array (At1) height width)))
		 (read-pbm))
		((pgm)
		 (or array
		     (set! array (create-array
				  ((if (<= max-pixel 256) Au8 Au16))
				  height width)))
		 (read-text))
		((ppm)
		 (or array
		     (set! array (create-array
				  ((if (<= max-pixel 256) Au8 Au16))
				  height width 3)))
		 (read-text))
		((pbm-raw)
		 (or array
		     (set! array (create-array (At1) height width)))
		 (pnm:read-bit-vector! array port))
		((pgm-raw)
		 (or array
		     (set! array (create-array (Au8) height width)))
		 (read-binary))
		((ppm-raw)
		 (or array
		     (set! array (create-array (Au8) height width 3)))
		 (read-binary))))
	    (pnm:type-dimensions port)))))

;; ARRAY is required to be zero-based.
(define (pnm:array-write type array maxval port . comments)
  (define (write-header type height width maxval)
    (let ((magic
	   (case type
	     ((pbm) "P1")
	     ((pgm) "P2")
	     ((ppm) "P3")
	     ((pbm-raw) "P4")
	     ((pgm-raw) "P5")
	     ((ppm-raw) "P6")
	     (else (error 'pnm:array-write "bad type" type)))))
      (fprintf port "%s\\n" magic)
      (for-each (lambda (str) (fprintf port "#%s\\n" str)) comments)
      (fprintf port "%d %d" width height)
      (if maxval (fprintf port "\\n%d" maxval))))
  (define (write-pixels type array maxval)
    (let* ((shp (array-dimensions array))
	   (height (car shp))
	   (width (cadr shp)))
      (case type
	((pbm-raw)
	 (newline port)
	 (if (not (boolean? (array-ref array 0 0)))
	     (error 'pnm:array-write "expected bit-array" array))
	 (uniform-array-write array port))
	((pgm-raw ppm-raw)
	 (newline port)
	 ;;(uniform-array-write array port)
	 (array-for-each (lambda (byt) (write-byte byt port)) array)
	 )
	((pbm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 35)) #\newline #\space) port)
	     (display (if (array-ref array i j) #\1 #\0) port)))
	 (newline port))
	((pgm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 17)) #\newline #\space) port)
	     (display (array-ref array i j) port)))
	 (newline port))
	((ppm)
	 (do ((i 0 (+ i 1)))
	     ((>= i height))
	   (do ((j 0 (+ j 1)))
	       ((>= j width))
	     (display (if (zero? (remainder j 5)) #\newline "  ") port)
	     (display (array-ref array i j 0) port)
	     (display #\space port)
	     (display (array-ref array i j 1) port)
	     (display #\space port)
	     (display (array-ref array i j 2) port)))
	 (newline port)))))

  (if (output-port? port)
      (let ((rnk (array-rank array))
	    (shp (array-dimensions array)))
	(case type
	  ((pbm pbm-raw)
	   (or (and (eqv? 2 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (or (eqv? 1 maxval)
	       (error 'pnm:array-write "maxval supplied not 1" type))
	   (write-header type (car shp) (cadr shp) #f)
	   (write-pixels type array 1))
	  ((pgm pgm-raw)
	   (or (and (eqv? 2 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (write-header type (car shp) (cadr shp) maxval)
	   (write-pixels type array maxval))
	  ((ppm ppm-raw)
	   (or (and (eqv? 3 rnk)
		    (integer? (car shp))
		    (integer? (cadr shp))
		    (eqv? 3 (caddr shp)))
	       (error 'pnm:array-write "bad shape" type array))
	   (write-header type (car shp) (cadr shp) maxval)
	   (write-pixels type array maxval))
	  (else (error 'pnm:array-write type 'unrecognized 'type))))
      (call-with-open-ports
       (open-file port 'wb')
       (lambda (port) (pnm:array-write type array maxval port)))))
