;;; "dbutil.scm" relational-database-utilities
; Copyright 1994, 1995, 1997, 2000, 2001, 2002 Aubrey Jaffer
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

(require 'relational-database)
(require 'dynamic-wind)

;;@code{(require 'databases)}
;;@ftindex databases
;;
;;@noindent
;;This enhancement wraps a utility layer on @code{relational-database}
;;which provides:
;;
;;@itemize @bullet
;;@item
;;Identification of open databases by filename.
;;@item
;;Automatic sharing of open (immutable) databases.
;;@item
;;Automatic loading of base-table package when creating a database.
;;@item
;;Detection and automatic loading of the appropriate base-table package
;;when opening a database.
;;@item
;;Table and data definition from Scheme lists.
;;@end itemize

;;;Each entry in mdbm:*databases* is a list of:

;;;  *  database (procedure)
;;;  *  number of opens (integer)
;;;  *  type (symbol)

;;;Because of WRITE-DATABASE, database filenames can change, so we must
;;;have a global lock.
(define mdbm:*databases* (make-exchanger '()))
(define (mdbm:return-dbs dbs)
  (if (mdbm:*databases* dbs)
      (slib:error 'mdbm:*databases* 'double 'set!)))

(define (mdbm:find-db? rdb dbs)
  (and dbs
       (do ((dbs dbs (cdr dbs)))
	   ((or (null? dbs)
		(equal? ((caar dbs) 'filename)
			(if (procedure? rdb) (rdb 'filename) rdb)))
	    (and (not (null? dbs))
		 (if (and (procedure? rdb)
			  (not (eq? ((caar dbs) 'filename) (rdb 'filename))))
		     (slib:error ((caar dbs) 'filename) 'open 'twice)
		     (car dbs)))))))

(define (mdbm:remove-entry dbs entry)
  (cond ((null? dbs) (slib:error 'mdbm:remove-entry 'not 'found entry))
	((eq? entry (car dbs)) (cdr dbs))
	(else (cons (car dbs) (mdbm:remove-entry (cdr dbs) entry)))))

;;@subsubheading Database Sharing

;;@noindent
;;@dfn{Auto-sharing} refers to a call to the procedure
;;@code{open-database} returning an already open database (procedure),
;;rather than opening the database file a second time.
;;
;;@quotation
;;@emph{Note:} Databases returned by @code{open-database} do not include
;;wrappers applied by packages like @ref{Embedded Commands}.  But
;;wrapped databases do work as arguments to these functions.
;;@end quotation
;;
;;@noindent
;;When a database is created, it is mutable by the creator and not
;;auto-sharable.  A database opened mutably is also not auto-sharable.
;;But any number of readers can (open) share a non-mutable database file.

;;@noindent
;;This next set of procedures mirror the whole-database methods in
;;@ref{Database Operations}.  Except for @code{create-database}, each
;;procedure will accept either a filename or database procedure for its
;;first argument.

(define (mdbm:try-opens filename mutable?)
  (define (try base)
    (let ((rdb (base 'open-database)))
      (and rdb (rdb filename mutable?))))
  (if (null? *base-table-implementations*)
      (require 'alist-table))
  (do ((bti *base-table-implementations* (cdr bti))
       (rdb (try (cadar *base-table-implementations*))
	    (try (cadar bti))))
      ((or (null? bti) (procedure? rdb))
       (and (procedure? rdb) (list rdb 1 (caar bti))))))

(define (mdbm:open-type filename type mutable?)
  (require type)
  (let* ((sys (cadr (assq type *base-table-implementations*)))
	 (open (and sys (sys 'open-database)))
	 (ndb (and open (open filename mutable?))))
    (and ndb (list ndb 1 type))))

;;@args filename base-table-type
;;Returns an open relational database (with base-table type
;;@2) associated with @1.
(define (create-database filename type)
  (require type)
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (define entry (mdbm:find-db? filename dbs))
	  (cond (entry (slib:warn 'close ((car entry) 'filename)
				  'before 'create-database) #f)
		(else
		 (let ((ndb (((cadr (assq type *base-table-implementations*))
			      'create-database)
			     filename)))
		   (if (and ndb dbs) (set! dbs (cons (list ndb 1 type) dbs)))
		   ndb))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

;;@noindent
;;Only @code{alist-table} and base-table modules which have been loaded
;;will dispatch correctly from the @code{open-database} procedures.
;;Therefore, either pass two arguments to @code{open-database}, or
;;require the base-table your database file uses before calling
;;@code{open-database} with one argument.

;;@args rdb base-table-type
;;Returns @emph{mutable} open relational database or #f.
(define (open-database! filename . type)
  (set! type (and (not (null? type)) (car type)))
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (cond ((and (procedure? filename) (not (filename 'delete-table)))
		 (slib:warn (filename 'filename) 'not 'mutable) #f)
		((mdbm:find-db? filename dbs)
		 (cond ((procedure? filename) filename)
		       (else (slib:warn filename 'already 'open) #f)))
		(else (let ((entry (if type
				       (mdbm:open-type filename type #t)
				       (mdbm:try-opens filename #t))))
			(cond (entry (and dbs (set! dbs (cons entry dbs)))
				     (car entry))
			      (else #f))))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

;;@args rdb base-table-type
;;Returns an open relational database associated with @1.  The
;;database will be opened with base-table type @2).
;;
;;@args rdb
;;Returns an open relational database associated with @1.
;;@0 will attempt to deduce the correct base-table-type.
(define (open-database rdb . type)
  (set! type (and (not (null? type)) (car type)))
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (define entry (mdbm:find-db? rdb dbs))
	  (and entry (set! rdb (car entry)))
	  (cond ((and entry type (not (eqv? (caddr entry) type)))
		 (slib:warn (rdb 'filename) 'type type '<> (caddr entry)) #f)
		((and (procedure? rdb) (rdb 'delete-table))
		 (slib:warn (rdb 'filename) 'mutable) #f)
		(entry (set-car! (cdr entry) (+ 1 (cadr entry))) rdb)
		(else
		 (set! entry
		       (cond ((procedure? rdb) (list rdb 1 type))
			     (type (mdbm:open-type rdb type #f))
			     (else (mdbm:try-opens rdb #f))))
		 (cond (entry (and dbs (set! dbs (cons entry dbs)))
			      (car entry))
		       (else #f)))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

;;@body
;;Writes the mutable relational-database @1 to @2.
(define (write-database rdb filename)
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (define entry (mdbm:find-db? rdb dbs))
	  (and entry (set! rdb (car entry)))
	  (cond ((and (not entry) (procedure? rdb))
		 (set! entry (list rdb 1 #f))
		 (and dbs (set! dbs (cons entry dbs)))))
	  (cond ((not entry) #f)
		((and (not (equal? filename (rdb 'filename)))
		      (mdbm:find-db? filename dbs))
		 (slib:warn filename 'already 'open) #f)
		(else (let ((dbwrite (rdb 'write-database)))
			(and dbwrite (dbwrite filename))))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

(define (syncify-database operation)
  (lambda (rdb)
    (let ((dbs #f))
      (dynamic-wind
	  (lambda () (set! dbs (mdbm:*databases* #f)))
	  (lambda ()
	    (define entry (mdbm:find-db? rdb dbs))
	    (and entry (set! rdb (car entry)))
	    (cond ((and (not entry) (procedure? rdb))
		   (set! entry (list rdb 1 #f))
		   (and dbs (set! dbs (cons entry dbs)))))
	    (cond (entry (let ((db-op (rdb operation)))
			   (and db-op (db-op))))
		  (else #f)))
	  (lambda () (and dbs (mdbm:return-dbs dbs)))))))

;;@args rdb
;;Writes the mutable relational-database @1 to the filename it was
;;opened with.
(define sync-database (syncify-database 'sync-database))

;;@args rdb
;;Syncs @1 and makes it immutable.
(define solidify-database (syncify-database 'solidify-database))

;;@body
;;@1 will only be closed when the count of @code{open-database} -
;;@0 calls for @1 (and its filename) is 0.
(define (close-database rdb)
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (define entry (mdbm:find-db? rdb dbs))
	  (and entry (set! rdb (car entry)))
	  (cond ((and entry (not (eqv? 1 (cadr entry))))
		 (set-car! (cdr entry) (+ -1 (cadr entry)))
		 #f)
		((not (procedure? rdb))
		 #f)
		(else (let* ((dbclose (rdb 'close-database))
			     (ret (and dbclose (dbclose))))
			(cond ((not dbclose) (slib:warn 'database? rdb))
			      ((not entry))
			      (dbs (set! dbs (mdbm:remove-entry dbs entry))))
			ret))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

;;@body
;;Prints a table of open database files.  The columns are the base-table
;;type, number of opens, @samp{!} for mutable, and the filename.
;;
;;@example
;;(mdbm:report)
;;@print{}
;;    alist-table 003   /usr/local/lib/slib/clrnamdb.scm
;;    alist-table 001 ! sdram.db
;;@end example
(define (mdbm:report)
  (require 'printf)
  (let ((dbs #f))
    (dynamic-wind
	(lambda () (set! dbs (mdbm:*databases* #f)))
	(lambda ()
	  (cond (dbs (for-each (lambda (entry)
				 (printf "%15s %03d %1s %s\\n"
					 (or (caddr entry) "?")
					 (cadr entry)
					 (if ((car entry) 'delete-table) '! "")
					 ((car entry) 'filename)))
			       dbs))
		(else (printf "%s lock broken.\\n" 'mdbm:*databases*))))
	(lambda () (and dbs (mdbm:return-dbs dbs))))))

;;;; Tables
(define (add-domain rdb row5)
  ((((rdb 'open-table) '*domains-data* #t) 'row:update)
   row5))

;;@subsubheading Defining Tables

;;@args rdb spec-0 @dots{}
;;Adds tables as specified in @var{spec-0} @dots{} to the open
;;relational-database @1.  Each @var{spec} has the form:
;;
;;@lisp
;;(@r{<name>} @r{<descriptor-name>} @r{<descriptor-name>} @r{<rows>})
;;@end lisp
;;or
;;@lisp
;;(@r{<name>} @r{<primary-key-fields>} @r{<other-fields>} @r{<rows>})
;;@end lisp
;;
;;where @r{<name>} is the table name, @r{<descriptor-name>} is the symbol
;;name of a descriptor table, @r{<primary-key-fields>} and
;;@r{<other-fields>} describe the primary keys and other fields
;;respectively, and @r{<rows>} is a list of data rows to be added to the
;;table.
;;
;;@r{<primary-key-fields>} and @r{<other-fields>} are lists of field
;;descriptors of the form:
;;
;;@lisp
;;(@r{<column-name>} @r{<domain>})
;;@end lisp
;;or
;;@lisp
;;(@r{<column-name>} @r{<domain>} @r{<column-integrity-rule>})
;;@end lisp
;;
;;where @r{<column-name>} is the column name, @r{<domain>} is the domain
;;of the column, and @r{<column-integrity-rule>} is an expression whose
;;value is a procedure of one argument (which returns @code{#f} to signal
;;an error).
;;
;;If @r{<domain>} is not a defined domain name and it matches the name of
;;this table or an already defined (in one of @var{spec-0} @dots{}) single
;;key field table, a foriegn-key domain will be created for it.
(define (define-tables rdb . spec-list)
  (define new-tables '())
  (define dom:typ (((rdb 'open-table) '*domains-data* #f) 'get 4))
  (define create-table (rdb 'create-table))
  (define open-table (rdb 'open-table))
  (define table-exists? (rdb 'table-exists?))
  (define (check-domain dname)
    (cond ((dom:typ dname))
	  ((member dname new-tables)
	   (let* ((ftab (open-table
			 (string->symbol
			  (string-append "desc:" (symbol->string dname)))
			 #f)))
	     ((((rdb 'open-table) '*domains-data* #t) 'row:insert)
	      (list dname dname #f
		    (dom:typ ((ftab 'get 'domain-name) 1)) 1))))))
  (define (define-table name prikeys slots data)
    (cond
     ((table-exists? name)
      (let* ((tab (open-table name #t))
	     (row:update (tab 'row:update)))
	(for-each row:update data)))
     ((and (symbol? prikeys) (eq? prikeys slots))
      (cond ((not (table-exists? slots))
	     (slib:error "Table doesn't exist:" slots)))
      (set! new-tables (cons name new-tables))
      (let* ((tab (create-table name slots))
	     (row:insert (tab 'row:insert)))
	(for-each row:insert data)
	((tab 'close-table))))
     (else
      (let* ((descname
	      (string->symbol (string-append "desc:" (symbol->string name))))
	     (tab (create-table descname))
	     (row:insert (tab 'row:insert))
	     (j 0))
	(set! new-tables (cons name new-tables))
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #t (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  prikeys)
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #f (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  slots)
	((tab 'close-table))
	(set! tab (create-table name descname))
	(set! row:insert (tab 'row:insert))
	(for-each row:insert data)
	((tab 'close-table))))))
  (for-each (lambda (spec) (apply define-table spec)) spec-list))

;;@subsubheading Listing Tables

;;@body
;;If symbol @2 exists in the open relational-database
;;@1, then returns a list of the table-name, its primary key names
;;and domains, its other key names and domains, and the table's records
;;(as lists).  Otherwise, returns #f.
;;
;;The list returned by @0, when passed as an
;;argument to @code{define-tables}, will recreate the table.
(define (list-table-definition rdb table-name)
  (cond (((rdb 'table-exists?) table-name)
	 (let* ((table ((rdb 'open-table) table-name #f))
		(prilimit (table 'primary-limit))
		(coldefs (map list
			      (table 'column-names)
			      (table 'column-domains))))
	   (list table-name
		 (butnthcdr prilimit coldefs)
		 (nthcdr prilimit coldefs)
		 ((table 'row:retrieve*)))))
	(else #f)))
;;(trace-all "/home/jaffer/slib/dbutil.scm") (untrace define-tables)
