;;;; package.lisp

(defpackage #:db3
  (:use #:cl)
  (:export #:*external-format*
           #:db3			; the main class
	   #:load-header
	   #:record-count
	   #:load-record
	   #:write-record
	   #:dump-db3
	   #:sample-db3))
