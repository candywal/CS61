;;;; "dbsyn.scm" -- Syntactic extensions for RDMS (within-database)
;;; Copyright (C) 2002 Ivan Shmakov <ivan@theory.dcn-asu.ru>
;;
;; Permission to copy this software, to modify it, to redistribute it,
;; to distribute modified versions, and to use it for any purpose is
;; granted, subject to the following restrictions and understandings.
;;
;; 1.  Any copy made of this software must include this copyright notice
;; in full.
;;
;; 2.  I have made no warrantee or representation that the operation of
;; this software will be error-free, and I am under no obligation to
;; provide any services, by way of maintenance, update, or otherwise.
;;
;; 3.  In conjunction with products arising from the use of this
;; material, there shall be no use of my name in any advertising,
;; promotional, or sales literature without prior written consent in
;; each case.

;;; History:

;; 2002-08-01: I've tired of tracking database description elements
;; (such as `(define-tables ...)'); so I decided to use `etags'.  But
;; its hard (if possible) to create regexp to match against RDMS' table
;; specs.  So I wrote `within-database' syntax extension and now I can
;; simply use something like:

;; $ etags -l scheme \
;;         -r '/ *(define-\(command\|table\) (\([^; \t]+\)/\2/' \
;;         source1.scm ...

;; ... and get TAGS table with all of my database commands and tables.

;;(require 'macro)                        ;This is done by its catalog entry
(require 'relational-database)
(require 'database-commands)
(require 'databases)

(define-syntax within-database
  (syntax-rules (define-table define-command)

    ((within-database database)
     database)

    ((within-database database
                      (define-table (name primary columns) row ...)
                      rest ...)
     (begin (define-tables database '(name primary columns (row ...)))
            (within-database database rest ...)))

    ((within-database database
                      (define-command template arg-1 arg-2 ...)
                      rest ...)
     (begin (define-*commands* database '(template arg-1 arg-2 ...))
            (within-database database rest ...)))))

;;; Emacs stuff
;; Local variables:
;; mode:scheme
;; mode:outline-minor
;; outline-regexp:          ";;;\\|(def"
;; fill-column:             72
;; ispell-local-dictionary: "english"
;; indent-tabs-mode:        nil
;; End:
