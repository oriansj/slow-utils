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

;; Globals needed for computation
(define jump_table '())
(define output (current-output-port))
(define input_files '())
(define bitsize 32)
(define scratch "")


;; Function for dealing with invalid input
(define (invalid-input)
  (begin
	(display "Invalid input\n")
	(exit EXIT_FAILURE)))


;; Generate a list of filenames to read
(define (set-input-files! name ret)
  (begin
	(set! input_files (cons name input_files))
	ret))


;; Simple stub for setting output file
(define (set-output-file! name ret)
  (begin
	(set! output (open-file name "wb"))
	ret))


;; simple stub for enabling 64bit models
(define (set-64bit-mode! ret)
  (begin
	(set! bitsize 64)
	ret))


;; Add label entries to jump_table list
(define (store-label file)
  (let ((c (consume-token file)))
	(begin
	  (set! jump_table (cons scratch jump_table))
	  c)))


;; Put contents of token into scratch and return whitespace char
(define (consume-token file)
  (let ((hold '())
		(c (read-char file)))
	(begin
	  (while (not (in_set c " \t\n>"))
			 (begin
			   (set! hold (cons c hold))
			   (set! c (read-char file))))
	  (set! scratch (list->string (reverse hold)))
	  c)))


;; Determines if passed char is in passed string
(define (in_set char string)
  (integer? (string-index string char)))


;; Drop entire line comments
(define (line_comment file)
  (let ((c (read-char file)))
	(while (and (not (in_set c "\n\r")) (not (eof-object? c)))
		   (set! c (read-char file)))))


;; Drop entire raw string
(define (purge-string file)
  (let ((c (read-char file)))
	(while (and (not (in_set c "\"")) (not (eof-object? c)))
		   (set! c (read-char file)))))


;; Perform the core of work
(define (first-pass input)
  (when (not (null? input))
	  (begin
		(first-pass (cdr input))
		(let ((source-file (open-file (car input) "r"))
			  (c 0))
		  (begin
			(set! c (read-char source-file))
			(while (not (eof-object? c))
				   (begin
					 ;; Check for and deal with label
					 (cond
					  ((char=? c #\:) (store-label source-file))
					  ((or (char=? c #\#) (char=? c #\;)) (line_comment source-file))
					  ((char=? c #\") (purge-string source-file)))
					 (set! c (read-char source-file))))
			(close-port source-file))))))


;; Walks down list outputing values in correct format
(define (output-debug node stage)
  (while (not (null? node))
		 (begin
		   (cond
			(stage
			 (begin
			   (display ":ELF_str_" output)
			   (display (car node) output)
			   (display "\n\"" output)
			   (display (car node) output)
			   (display "\"\n" output)))
			((= 64 bitsize)
			 (begin
			   (display "%ELF_str_" output)
			   (display (car node) output)
			   (display ">ELF_str\n!2\n!0\n@1\n&" output)
			   (display (car node) output)
			   (display " %0\n%10000\n%0\n" output)))
			(#t
			 (begin
			   (display "%ELF_str_" output)
			   (display (car node) output)
			   (display ">ELF_str\n&" output)
			   (display (car node) output)
			   (display "\n%10000\n!2\n!0\n@1\n" output))))
		   (set! node (cdr node)))))


;; Deal with parsing of arguments and performing the main process
(define (main args)
  (begin
	;; Skip the name of this file
	(set! args (cdr args))
	;; Loop through arguments
	(while (not (null? args))
		   (let ((opt (car args)))
			 (cond
			  ((string= "--64" opt) (set-64bit-mode! (cdr args)))
			  ((or (string= "-f" opt) (string= "--file" opt)) (set! args (set-input-files! (cadr args) (cdr args))))
			  ((or (string= "-o" opt) (string= "--output" opt)) (set! args (set-output-file! (cadr args) (cdr args))))
			  (#t (invalid-input)))
			 ;;(write opt output)
			 ;;(newline)
			 (set! args (cdr args))))
	;; Don't run when there are no input files
	(when (null? input_files)
	  (invalid-input))
	(first-pass input_files)
	(set! jump_table (reverse jump_table))
	(display ":ELF_str\n!0\n" output)
	(output-debug jump_table #t)
	(if (= 64 bitsize)
		(display "%0\n:ELF_sym\n%0\n!0\n!0\n@1\n%0 %0\n%0 %0\n" output)
		(display "%0\n:ELF_sym\n%0\n%0\n%0\n!0\n!0\n@1\n" output))
	(output-debug jump_table #f)
	(display "\n:ELF_end\n" output)))
