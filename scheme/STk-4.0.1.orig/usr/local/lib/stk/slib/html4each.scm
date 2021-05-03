;;;; HTML scan calls procedures for word, tag, whitespac, and newline.
;;; Copyright 2002 Aubrey Jaffer
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

(require 'line-i/o)
(require 'string-search)

;;@body
;;@1 is a string naming an existing file containing HTML text.
;;@2 is a procedure of one argument or #f.
;;@3 is a procedure of one argument or #f.
;;@4 is a procedure of one argument or #f.
;;@5 is a procedure of no arguments or #f.
;;
;;@0 opens and reads characters from the file named by @1.  Sequential
;;groups of characters are assembled into strings which are either
;;
;;@itemize @bullet
;;@item
;;enclosed by @samp{<} and @samp{>} (hypertext markups);
;;@item
;;end-of-line;
;;@item
;;whitespace; or
;;@item
;;none of the above (words).
;;@end itemize
;;
;;Procedures are called according to these distinctions in order of
;;the string's occurrence in @1.
;;
;;@5 is called with no arguments for end-of-line @emph{not within a
;;hypertext markup}.
;;
;;@4 is called with strings of non-newline whitespace.
;;
;;@3 is called with hypertext markup strings (including @samp{<} and
;;@samp{>}).
;;
;;@2 is called with the remaining strings.
;;
;;@0 returns an unspecified value.
(define (html-for-each file word-proc tag-proc white-proc newline-proc)
  (define (proc-words line edx)
    (let loop ((idx 0))
      (define ldx idx)
      (do ((idx idx (+ 1 idx)))
	  ((or (>= idx edx)
	       (not (char-whitespace? (string-ref line idx))))
	   (do ((jdx idx (+ 1 jdx)))
	       ((or (>= jdx edx)
		    (char-whitespace? (string-ref line jdx)))
		(and white-proc (not (= ldx idx))
		     (white-proc (substring line ldx idx)))
		(and word-proc (not (= idx jdx))
		     (word-proc (substring line idx jdx)))
		(if (< jdx edx) (loop jdx))))))))
  (call-with-input-file file
    (lambda (iport)
      (do ((line (read-line iport) (read-line iport)))
	  ((eof-object? line))
	(do ((idx (string-index line #\<) (string-index line #\<)))
	    ((not idx) (proc-words line (string-length line)))
					; seen '<'
	  (proc-words line idx)
	  (let loop ((lne (substring line idx (string-length line)))
		     (tag ""))
	    (define edx (or (eof-object? lne) (string-index lne #\>)))
	    (cond
	     ((not edx)
	      (loop (read-line iport)	; still inside tag
		    (and tag-proc (string-append tag lne " "))))
	     ((eqv? #t edx)
	      (slib:warn 'unterminated 'HTML 'entity file)
	      (and tag-proc (tag-proc tag)))
	     (else			; found matching '>'
	      (set! edx (+ 1 edx))
	      (and tag-proc
		   (tag-proc (string-append tag (substring lne 0 edx))))
					; process words after '>'
	      (set! line (substring lne edx (string-length lne)))))))
	(and newline-proc (newline-proc))))))
