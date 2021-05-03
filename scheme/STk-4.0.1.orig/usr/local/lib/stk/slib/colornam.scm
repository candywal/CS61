;;; "colornam.scm" color name databases
;Copyright 2001, 2002 Aubrey Jaffer
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

(require 'databases)
(require 'color)

;;@code{(require 'color-names)}

;;@noindent
;;Rather than ballast the color dictionaries with numbered grays,
;;@code{file->color-dictionary} discards them.  They are provided
;;through the @code{grey} procedure:

;;@body
;;Returns @code{(inexact->exact (round (* k 2.55)))}, the X11 color
;;grey@i{<k>}.
(define (grey k)
  (define int (inexact->exact (round (* k 2.55))))
  (color:sRGB int int int))

;;@noindent
;;A color dictionary is a database table relating @dfn{canonical}
;;color-names to color-strings
;;(@pxref{Color Data-Type, External Representation}).
;;
;;@noindent
;;The column names in a color dictionary are unimportant; the first
;;field is the key, and the second is the color-string.

;;@body Returns a downcased copy of the string or symbol @1 with
;;@samp{_}, @samp{-}, and whitespace removed.
(define (color-name:canonicalize name)
  (list->string
   (apply append (map (lambda (c) (if (or (char-alphabetic? c)
					  (char-numeric? c))
				      (list (char-downcase c))
				      '()))
		      (string->list (if (symbol? name)
					(symbol->string name)
					name))))))

;;@args name table1 table2 @dots{}
;;
;;@2, @3, @dots{} must be color-dictionary tables.  @0 searches for the
;;canonical form of @1 in @2, @3, @dots{} in order; returning the
;;color-string of the first matching record; #f otherwise.
(define (color-name->color name . tables)
  (define cancol (color-name:canonicalize name))
  (define found #f)
  (do ((tabs tables (cdr tabs)))
      ((or found (null? tabs)) (and found (string->color found)))
    (set! found (((car tabs) 'get 2) cancol))))

;;@args table1 table2 @dots{}
;;
;;@1, @2, @dots{} must be color-dictionary tables.  @0 returns a
;;procedure which searches for the canonical form of its string argument
;;in @1, @2, @dots{}; returning the color-string of the first matching
;;record; and #f otherwise.
(define (color-dictionaries->lookup . tables)
  (define procs (map (lambda (tab) (tab 'get 2)) tables))
  (lambda (name)
    (define cancol (color-name:canonicalize name))
    (define found #f)
    (do ((procs procs (cdr procs)))
	((or found (null? procs)) (and found (string->color found)))
      (set! found ((car procs) cancol)))))

;;@args name rdb base-table-type
;;
;;@2 must be a string naming a relational database file; and the symbol
;;@1 a table therein.  The database will be opened as
;;@var{base-table-type}.  @0 returns the read-only table @1 in database
;;@1 if it exists; #f otherwise.
;;
;;@args name rdb
;;
;;@2 must be an open relational database or a string naming a relational
;;database file; and the symbol @1 a table therein.  @0 returns the
;;read-only table @1 in database @1 if it exists; #f otherwise.
(define (color-dictionary table-name . *db*)
  (define rdb (apply open-database *db*))
  (and rdb ((rdb 'open-table) table-name #f)))


;;@args name rdb base-table-type
;;@args name rdb
;;
;;@2 must be a string naming a relational database file; and the symbol
;;@1 a table therein.  If the symbol @3 is provided, the database will
;;be opened as @3.  @0 creates a top-level definition of the symbol @1
;;to a lookup procedure for the color dictionary @1 in @2.
;;
;;The value returned by @0 is unspecified.
(define (load-color-dictionary table-name . db)
  (slib:eval
   `(define ,table-name
      (color-dictionaries->lookup
       (color-dictionary ',table-name
			 ,@(map (lambda (arg) (list 'quote arg)) db))))))

;;@subsubheading Dictionary Creation

;;@args file table-name rdb base-table-type
;;@args file table-name rdb
;;
;;@3 must be an open relational database or a string naming a relational
;;database file, @2 a symbol, and the string @1 must name an existing
;;file with colornames and their corresponding xRGB (6-digit hex)
;;values.  @0 creates a table @2 in @3 and enters the associations found
;;in @1 into it.
(define (file->color-dictionary file table-name . *db*)
  (define rdb (apply open-database! *db*))
  (define-tables rdb
    `(,table-name
      ((name string))
      ((color string)
       (order uint))
      ()))
  (let ((table ((rdb 'open-table) table-name #t)))
    (and table (load-rgb-txt file table))))

;;@args url table-name rdb base-table-type
;;@args url table-name rdb
;;
;;@3 must be an open relational database or a string naming a relational
;;database file and @2 a symbol.  @0 retrieves the resource named by the
;;string @1 using the @dfn{wget} program; then calls
;;@code{file->color-dictionary} to enter its associations in @2 in @1.
(define (url->color-dictionary url table-name . rdb)
  (require 'transact)
  (call-with-tmpnam
   (lambda (file)
     (system (string-append "wget -c -O" file " -USLIB" *SLIB-VERSION* " " url))
     (apply file->color-dictionary file table-name rdb))))

(define (load-rgb-txt path color-table)
  (require 'string-search)
  (require 'line-i/o)
  (require 'scanf)
  (cond ((not (file-exists? path))
	 (slib:error 'load-color-dictionary! 'file-exists? path)))
  (write 'load-rgb-txt) (display #\ ) (write path) (newline)
  (let ((color-table:row-insert (color-table 'row:insert))
	(color-table:row-retrieve (color-table 'row:retrieve))
	(method-id #f))
    (define (floats->rgb . rgbi)
      (apply color:sRGB
	     (map (lambda (x) (inexact->exact (round (* 255 x)))) rgbi)))
    (define (parse-rgb-line line)
      (let ((rgbx #f) (r #f) (g #f) (b #f)
	    (ri #f) (gi #f) (bi #f) (name #f) (junk #f) (ans #f))
	(define (check-match line color1 . colors)
	  (cond ((null? colors) (color->string color1))
		((> (CMC:DE* color1 (car colors)) 5.0)
		 (newline) (display line) (force-output)
		 (slib:warn (round (CMC:DE* color1 (car colors)))
			    'mismatch (color->string color1)
			    (color->string (car colors)))
		 (apply check-match line colors))
		(else (apply check-match line colors))))
	(for-each
	 (lambda (method)
	   (or ans
	       (let ((try (method line)))
		 (cond (try (set! ans try)
			    (display "**** Using method ")
			    (display method-id) (newline)
			    (set! parse-rgb-line method))))))
	 (list
	  (lambda (line)
	    (define en #f) (define fr #f) (define de #f)
	    (define es #f) (define cz #f) (define hu #f)
	    (case (sscanf line "#%6x	%[^	]	%[^	]	%[^	]	%[^	]	%[^	]	%[^	]%s"
			  rgbx en fr de es cz hu junk)
	      ((7)
	       (set! method-id 'm77)
	       (cons (check-match line (xRGB->color rgbx))
		     (map color-name:canonicalize (list en fr de es cz hu))))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %24[a-zA-Z0-9_ ] %d %d %d %e %e %e %s"
			  name r g b ri gi bi junk)
	      ((7)
	       (set! method-id 'm7)
	       (list (check-match line (color:sRGB r g b) (floats->rgb ri gi bi))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %[a-zA-Z0-9_] %6x %d %d %d %e %e %e %s"
			  name rgbx r g b ri gi bi junk)
	      ((8)
	       (set! method-id 'm8)
	       (list (check-match line (xrgb->color rgbx)
				  (color:sRGB r g b)
				  (floats->rgb ri gi bi))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %[a-zA-Z0-9] %6x %d,%d,%d" name rgbx r g b)
	      ((5)
	       (set! method-id 'm5)
	       (list (check-match line (xrgb->color rgbx) (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %[- a-zA-Z0-9_'] #%6x %d %d %d %s"
			  name rgbx r g b junk)
	      ((6 5)
	       (set! method-id 'm65)
	       (list (check-match line (xrgb->color rgbx) (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %d %d %d %[a-zA-Z0-9 ]%s" r g b name junk)
	      ((4) (set! method-id 'm4a)
	       (list (check-match line (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %[- a-zA-Z.] %d %d %d %s"
			  name r g b junk)
	      ((4) (set! method-id 'm4b)
	       (list (check-match line (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line "\" Resene %[^\"]\" %d %d %d %s"
			  name r g b junk)
	      ((4) (set! method-id 'm4b)
	       (list (check-match line (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line "\" %[^\"]\" %d %d %d %s"
			  name r g b junk)
	      ((4) (set! method-id 'm4c)
	       (list (check-match line (color:sRGB r g b))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line " %[a-zA-Z0-9_] #%x%6x%s" name rgbx junk)
	      ((2) (set! method-id 'm2a)
	       (list (check-match line (xrgb->color rgbx))
		     (color-name:canonicalize name)))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line "%[- a-zA-Z']=#%6x<br>" name rgbx)
	      ((2) (set! method-id 'm2b)
	       (let ((idx (substring? "rgb" name)))
		 (and (eqv? idx (+ -3 (string-length name)))
		      (list (check-match line (xrgb->color rgbx))
			    (color-name:canonicalize (substring name 0 idx))))))
	      (else #f)))
	  (lambda (line)
	    (case (sscanf line "\" %[^\"]\" %s" name junk)
	      ((2) (set! method-id 'm2c)
	       (let ((clr (string->color junk)))
		 (and clr (list (check-match line clr)
				(color-name:canonicalize name)))))
	      (else #f)))))
	ans))
    (define (numbered-gray? str)
      (define idx #f)
      (and (or (eqv? 0 (substring-ci? "gray" str))
	       (eqv? 0 (substring-ci? "grey" str)))
	   (eqv? 1 (sscanf (substring str 4 (string-length str))
			   "%d%s" idx str))))
    (call-with-input-file path
      (lambda (port)
	(define *idx* 0)
	(define *rcs-header* (read-line port))
	(do ((line (read-line port) (read-line port)))
	    ((eof-object? line)
	     (printf "Inserted %d colors\\n" *idx*)
	     *rcs-header*)
	  (let ((colin (parse-rgb-line line)))
	    (cond ((equal? "" line))
		  ;;((char=? #\# (string-ref line 0)))
		  ((not colin) (write-line line))
		  ((numbered-gray? (cadr colin)))
		  (else
		   (for-each
		    (lambda (name)
		      (let ((oclin (color-table:row-retrieve name)))
			(cond
			 ((and oclin (equal? (car colin) (cadr oclin))))
			 ((not oclin)
			  (set! *idx* (+ 1 *idx*))
			  (color-table:row-insert
			   (list name (car colin) *idx*)))
			 (else (slib:warn 'collision name oclin)))))
		    (cdr colin))))))))))

;;@noindent
;;This section has detailed the procedures for creating and loading
;;color dictionaries.  So where are the dictionaries to load?
;;
;;@uref{http://swissnet.ai.mit.edu/~jaffer/Color/Dictionaries.html}
;;
;;@noindent
;;Describes and evaluates several color-name dictionaries on the web.
;;The following procedure creates a database containing two of these
;;dictionaries.

;;@body
;;Creates an @r{alist-table} relational database in @r{library-vicinity}
;;containing the @dfn{Resene} and @dfn{saturate} color-name
;;dictionaries.
;;
;;If the files @file{resenecolours.txt} and @file{saturate.txt} exist in
;;the @r{library-vicinity}, then they used as the source of color-name
;;data.  Otherwise, @0 calls url->color-dictionary with the URLs of
;;appropriate source files.
(define (make-slib-color-name-db)
  (define cndb (create-database (in-vicinity (library-vicinity) "clrnamdb.scm")
				'alist-table))
  (for-each
   (lambda (lst)
     (apply
      (lambda (url path name)
	(define filename (in-vicinity (library-vicinity) path))
	(if (file-exists? filename)
	    (file->color-dictionary filename name cndb)
	    (url->color-dictionary url name cndb)))
      lst))
   '(("http://swissnet.ai.mit.edu/~jaffer/Color/saturate.txt"
      "saturate.txt"
      saturate)
     ("http://swissnet.ai.mit.edu/~jaffer/Color/resenecolours.txt"
      "resenecolours.txt"
      resene)))
  (close-database cndb))
