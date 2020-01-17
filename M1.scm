#! /usr/bin/env guile --no-auto-compile -e main -L /usr/local/share/guile/site/2.2 -C /usr/local/lib/guile/2.2/site-ccache -s
!#
;;; slow-utils --- Slow utilities for bootstrapping
;;; Copyright 2019 Jeremiah Orians
;;;
;;; This file is part of slow-utils.
;;;
;;; slow-utils is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; slow-utils is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with slow-utils.  If not, see <http://www.gnu.org/licenses/>.


;; For keeping our labels
(define <entry> (make-record-type "<entry>" '(type text expression filename linenumber)))
(define make-entry (record-constructor <entry> '(type text expression filename linenumber)))
(define entry-expression (record-accessor <entry> 'expression))
(define entry-filename (record-accessor <entry> 'filename))
(define entry-linenumber (record-accessor <entry> 'linenumber))
(define entry-text (record-accessor <entry> 'text))
(define entry-type (record-accessor <entry> 'type))
(define set-entry-expression! (record-modifier <entry> 'expression))
(define set-entry-text! (record-modifier <entry> 'text))
(define set-entry-type! (record-modifier <entry> 'type))

;; Constants
(define KNIGHT 0)
(define X86 1)
(define AMD64 2)
(define ARMV7L 40)
(define AARM64 80)
(define EXIT_FAILURE 1)

;; Types
(define BLANK 0)
(define MACRO 1)
(define STR 2)
(define NEWLINE 3)

;; Globals
(define BigEndian #t)
(define Architecture KNIGHT)
(define input_files '())
(define output (current-output-port))
(define ByteMode 16)
(define linenumber 0)
(define head '())


;; Figure out what target architecture
;; Throw error if not known
(define (set-arch! arch ret)
  (begin
	(cond
	 ((string=? "knight" arch) (set! Architecture KNIGHT))
	 ((string=? "x86" arch) (set! Architecture X86))
	 ((string=? "amd64" arch) (set! Architecture AMD64))
	 ((string=? "armv7l" arch) (set! Architecture ARMV7L))
	 ((string=? "aarch64" arch) (set! Architecture AARM64))
	 (#t (invalid-input)))
	ret))

;; Generate a list of filenames to read
(define (set-input-files! name)
  (set! input_files (cons name input_files)))

;; Simple stub for setting output file
(define (set-output-file! name)
  (set! output (open-file name "wb")))

;; Function for dealing with invalid input
(define (invalid-input)
  (begin
	(display "Invalid input\n")
	(exit EXIT_FAILURE)))

;; Determines if passed char is in passed string
(define (in_set char string)
  (integer? (string-index string char)))

;; Append newline
(define (append-newline source-file)
  (if (not (pair? head))
	  (begin
		(set! head (cons (make-entry NEWLINE "\n" '() (port-filename source-file) linenumber) head))
		(set! linenumber (+ linenumber 1)))
	  (begin
		(set! linenumber (+ linenumber 1))
		(when (not (= (entry-type (car head)) NEWLINE))
		  (set! head (cons (make-entry NEWLINE "\n" '() (port-filename source-file) (- linenumber 1)) head))))))

;; Drop entire line comments
(define (line-comment file)
  (let ((c (read-char file)))
	(begin
	  (while (and (not (in_set c "\n\r")) (not (eof-object? c)))
			 (set! c (read-char file)))
	  (append-newline file))))

;; Store atoms
(define (store-atom c source-file)
  (let ((hold (list c))
		(c (read-char source-file)))
	(begin
	  (while (not (in_set c " \t\n"))
			 (begin
			   (set! hold (cons c hold))
			   (set! c (read-char source-file))))
	  (set! head (cons (make-entry BLANK (list->string (reverse hold)) '() (port-filename source-file) linenumber) head))
	  (when (char=? c #\newline) (append-newline source-file)))))

;; Store strings
(define (store-string c source-file)
  (let ((hold (list c))
		(ch (read-char source-file)))
	(begin
	  (while (not (char=? c ch))
			 (begin
			   (set! hold (cons ch hold))
			   (set! ch (read-char source-file))))
	  (set! head (cons (make-entry STR (list->string (reverse hold)) '() (port-filename source-file) linenumber) head)))))

;; Tokenize lines
(define (Tokenize_Line input)
  (when (not (null? input))
	  (begin
		(Tokenize_Line (cdr input))
		(let ((source-file (open-file (car input) "r"))
			  (c 0))
		  (begin
			(set! c (read-char source-file))
			(set! linenumber 1)
			(while (not (eof-object? c))
				   (begin
					 (cond
					  ((char=? c #\newline) (append-newline source-file))
					  ((in_set c ";#") (line-comment source-file))
					  ((in_set c "\t ") c)
					  ((in_set c "'\"") (store-string c source-file))
					  (#t (store-atom c source-file)))
					 (set! c (read-char source-file))))
			(close-port source-file))))))

;; A little stupid primitive needed to flatten a list of lists
(define (flatten x)
  (cond ((null? x) '())
		((pair? x) (append (flatten (car x)) (flatten (cdr x))))
		(#t (list x))))

;; Convert to hex one char at a time
(define (hexit a)
  (let ((i (char->integer a)))
	(list (string-ref "0123456789ABCDEF" (ash i -4)) (string-ref "0123456789ABCDEF" (logand i #xF)))))

;; Turn string into hex
(define (hexify-string i)
  (set-entry-expression! i (list->string (reverse (cons* #\0 #\0 (reverse (flatten (map hexit (string->list (substring (entry-text i) 1))))))))))

;; Process strings
(define (process-string p)
  (when (not (null? p))
	(begin
	  (when (= (entry-type (car p)) STR)
		(if (char=? (string-ref (entry-text (car p)) 0) #\')
			(set-entry-expression! (car p) (substring (entry-text (car p)) 1))
			(hexify-string (car p))))
	  (process-string (cdr p)))))

;; Identify MACROs
(define (identify-macros p)
  (when (not (null? p))
	(begin
	  (when (string=? "DEFINE" (entry-text (car p)))
		(begin
		  (set-entry-type! (car p) MACRO)
		  (set-entry-text! (car p) (entry-text (cadr p)))
		  (if (= (entry-type (caddr p)) STR)
			  (set-entry-expression! (car p) (substring (entry-text (caddr p)) 1))
			  (set-entry-expression! (car p) (entry-text (caddr p))))
		  (set-cdr! p (cdddr p))))
	  (identify-macros (cdr p)))))

;; Set macro expressions
(define (set-expression p text exp)
  (when (not (null? p))
	(begin
	  (when (string=? text (entry-text (car p)))
		(set-entry-expression! (car p) exp))
	  (set-expression (cdr p) text exp))))

;; Apply MACROs
(define (line-macro p)
  (when (not (null? p))
	(begin
	  (when (= (entry-type (car p)) MACRO)
		(set-expression (cdr p) (entry-text (car p)) (entry-expression (car p))))
	  (line-macro (cdr p)))))

;; Simplify debugging
(define (debug-head head)
  (while (not (null? head))
		 (begin
		   (write (car head))
		   (set! head (cdr head))
		   (newline))))

;; Deal with parsing of arguments and performing the main process
(define (main args)
  (begin
	;; Skip the name of this file
	(set! args (cdr args))
	;; Loop through arguments
	(while (not (null? args))
		   (let ((opt (car args)))
			 (cond
			  ((string=? "--BigEndian" opt) (set! BigEndian #t))
			  ((string=? "--LittleEndian" opt) (set! BigEndian #f))
			  ((string=? "--binary" opt) (begin (set! ByteMode 2) (set! default-toggle 0)))
			  ((string=? "--octal" opt) (begin (set! ByteMode 8) (set! default-toggle 0)))
			  ((string=? "--architecture" opt) (set! args (set-arch! (cadr args) (cdr args))))
			  ((or (string=? "-f" opt) (string=? "--file" opt)) (begin (set-input-files! (cadr args)) (set! args (cdr args))))
			  ((or (string=? "-o" opt) (string=? "--output" opt)) (begin (set-output-file! (cadr args)) (set! args (cdr args))))
			  (#t (invalid-input)))
			 (write opt output)
			 (newline)
			 (set! args (cdr args))))
	;; Don't run when there are no input files
	(when (null? input_files)
	  (invalid-input))

	;; Tokenize
	(Tokenize_Line input_files)
	(set! head (reverse head))
	(identify-macros head)
	(line-macro head)
	(process-string head)
;	(eval-immediates head)
;	(preserve-other head)
;	(print-hex head)
	(debug-head head)))
