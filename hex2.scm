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


;; Needed to support record type
(use-modules (srfi srfi-9))

;; For keeping our labels
(define-record-type <entry>
  (make-entry target name)
  entry?
  (target entry-target)
  (name entry-name))

;; Constants
(define KNIGHT 0)
(define X86 1)
(define AMD64 2)
(define ARMV7L 40)
(define AARM64 80)
(define EXIT_FAILURE 1)

;; Globals
(define ALIGNED #f)
(define BigEndian #t)
(define jump_table '())
(define Architecture KNIGHT)
(define Base_Address 0)
(define input_files '())
(define output (current-output-port))
(define exec_enable #f)
(define ByteMode 16)
(define ip 0)
(define linenumber 0)
(define scratch "")
(define hold 0)
(define toggle #f)

;; Figure out what target architecture
;; Throw error if not known
(define (set-arch! arch ret)
  (begin
	(cond
	 ((string= "knight" arch) (set! Architecture KNIGHT))
	 ((string= "x86" arch) (set! Architecture X86))
	 ((string= "amd64" arch) (set! Architecture AMD64))
	 ((string= "armv7l" arch) (set! Architecture ARMV7L))
	 ((string= "aarch64" arch) (set! Architecture AARM64))
	 (#t (invalid-input)))
	ret))

;; Since string->number doesn't support "0x12345"
;; Convert those strings to "#x12345" format
;; Otherwise do nothing
(define (fixup-number number)
  (if (string= "0x" (substring number 0 2))
	  (string-append "#\x78" (substring number 2))
	  number))

;; Basic input validation for setting Base address
(define (set-BaseAddress! number ret)
  (if (and (integer? number) (positive? number))
	  (begin
		(set! Base_Address number)
		ret)
  (invalid-input)))

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

;; Function for dealing with invalid input
(define (invalid-input)
  (begin
	(display "Invalid input\n")
	(exit EXIT_FAILURE)))

;; Add label entries to jump_table list
(define (store-label file ip)
  (let ((c (consume-token file)))
	(begin
	  (set! jump_table (cons (make-entry ip (string-copy scratch)) jump_table))
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

;; Updates ip relative to size of pointer
(define (Update-pointer char)
  (cond
   ((in_set char "%&") (set! ip (+ ip 4)))
   ((in_set char "@$") (set! ip (+ ip 2)))
   ((in_set char "~") (set! ip (+ ip 3)))
   ((in_set char "!") (set! ip (+ ip 1)))
   (#t (begin
		 (line_error)
		 (display "storePointer given unknown\n" (current-error-port))
		 (exit EXIT_FAILURE)))))

;; Ignore token
(define (throwaway-token file)
  (let ((c (read-char file)))
	(begin
	  (while (not (in_set c " \t\n>"))
			 (set! c (read-char file)))
	  c)))

;; Lookup the address of labels
(define (get-target name)
  (let ((node jump_table)
		(i (car jump_table)))
	(while (not (null? node))
		   (if (string= name (entry-name i))
			   (break (entry-target i))
			   (begin
				 (set! node (cdr node))
				 (set! i (car node)))))))

;; Use architecture specific rules to determine
;; Proper displacement
(define (architectural-displacement target base)
  (cond
   ((= Architecture KNIGHT) (- target base))
   ((= Architecture X86) (- target base))
   ((= Architecture AMD64) (- target base))
   (#t (begin
		 (write "Unknown Architecture, aborting before harm is done" (current-error-port))
		 (newline (current-error-port))
		 (exit EXIT_FAILURE)))))


(define (outputPointer displacement bytes)
  (begin
	; (range-check displacement bytes)
	(set! displacement (logand #xFFFFFFFF displacement))
	(if BigEndian
		(begin
		  ;; Deal with Big Endian output
		  (when (= 4 bytes) (write-char (integer->char (logand #xFF (ash displacement -24))) output))
		  (when (<= 3 bytes) (write-char (integer->char (logand #xFF (ash displacement -16))) output))
		  (when (<= 2 bytes) (write-char (integer->char (logand #xFF (ash displacement -8))) output))
		  (when (<= 1 bytes) (write-char (integer->char (logand #xFF displacement)) output)))
		(while (< 0 bytes)
			   (begin
				 ;; Deal with Little Endian output
				 (write-char (integer->char (logand #xFF displacement)) output)
				 (set! bytes (- bytes 1))
				 (set! displacement (quotient displacement 256)))))))

;; Output pointers in correct format
(define (store-pointer char file)
  (begin
	(set! scratch "")
	(Update-pointer char)
	(let ((base_sep_p (consume-token file))
		  (target (get-target scratch))
		  (displacement 0)
		  (base ip))
	  (begin
		(if (char=? base_sep_p #\>)
			;; Deal with %foo>bar pointers
			(begin
			  (set! scratch "")
			  (consume-token file)
			  (set! base (get-target scratch))
			  ;; Force universality of behavior
			  (set! displacement (- target base)))
			;; Deal with the more common case
			(set! displacement (architectural-displacement target base)))
		;; Output Calculated differences
		(cond
		 ((char=? char #\!) (outputPointer displacement 1))
		 ((char=? char #\$) (outputPointer target 2))
		 ((char=? char #\@) (outputPointer displacement 2))
		 ((char=? char #\~) (outputPointer displacement 3))
		 ((char=? char #\&) (outputPointer target 4))
		 ((char=? char #\%) (outputPointer displacement 4))
		 (#t (begin
			   (line_error)
			   (write "error: store-pointer reached impossible case: ch=" (current-error-port))
			   (write char (current-error-port))
			   (newline (current-error-port))
			   (exit EXIT_FAILURE))))))))


;; Deal with arm family alignment restrictions
(define (pad-to-align state) 3)


;; Drop entire line comments
(define (line_comment file)
  (let ((c (read-char file)))
	(begin
	  (while (and (not (in_set c "\n\r")) (not (eof-object? c)))
			 (set! c (read-char file)))
	  (set! linenumber (+ 1 linenumber)))))

;; Deal with Hex input
(define (hex char file)
  (let ((c (char->integer char)))
	(cond
	 ((in_set char "0123456789") (- c 48))
	 ((in_set char "ABCDEF") (- c 55))
	 ((in_set char "abcdef") (- c 87))
	 ((in_set char "#;") (begin
						   (line_comment file)
						   -1))
	 ((char=? char #\newline) (begin
								(set! linenumber (+ 1 linenumber))
								-1))
	 ((in_set char " \t") -1)
	 (#t (begin
		   (display "Invalid input char: " (current-error-port))
		   (display c (current-error-port))
		   (newline (current-error-port))
		   (exit EXIT_FAILURE))))))

;; Do the real work
(define (process-byte char file state)
  (let ((value -1))
	(cond
	 ;; Deal with hex input
	 ((= 16 ByteMode)
	  (begin
		(set! value (hex char file))
		(when (<= 0 value)
		  (begin
			(if toggle
				(begin
				  (set! hold (+ (* 16 hold) value))
				 ; (write hold)
				 ; (newline)
				  (when state (write-char (integer->char hold) output))
				  (set! ip (+ ip 1))
				  (set! hold 0))
				(set! hold value))
			(set! toggle (not toggle))))))
	 (#t (display "not implemented yet\n")))))

;; Collect addresses of all labels
(define (first-pass input)
  (when (not (null? input))
	  (begin
		(first-pass (cdr input))
		(let ((source-file (open-file (car input) "r"))
			  (c 0))
		  (begin
			(set! c (read-char source-file))
			(set! linenumber 1)
			(set! toggle #f)
			(while (not (eof-object? c))
				   (begin
					 ;; Check for and deal with label
					 (when (char=? c #\:)
					   (set! c (store-label source-file ip)))
					 ;; /* check for and deal with relative/absolute pointers to labels */
					 (cond
					  ((in_set c "!@$~%&")
					   (begin
						 ;; deal with 1byte pointer !; 2byte pointers (@ and $); 3byte pointers ~; 4byte pointers (% and &)
						 (Update-pointer c)
						 (set! c (throwaway-token source-file))
						 ;; deal with label>base
						 (when (char=? c #\>) (set! c (throwaway-token source-file)))))
					  ((char=? c #\<) (pad-to-align #f))
					  ((char=? c #\^) '())
					  (#t (process-byte c source-file #f)))
					; (write c)
					; (newline)
					 (set! c (read-char source-file))))
			(close-port source-file))))))

;; Generate the actual output
(define (second-pass input)
  (when (not (null? input))
	  (begin
		(second-pass (cdr input))
		(let ((source-file (open-file (car input) "r"))
			  (c 0))
		  (begin
			(set! c (read-char source-file))
			(set! linenumber 1)
			(set! toggle #f)
			(set! hold 0)
			(while (not (eof-object? c))
				   (begin
					 (cond
					  ;; Ignore labels
					  ((char=? c #\:) (throwaway-token source-file))
					  ;; Output pointers in correct formats
					  ((in_set c "!@$~%&") (store-pointer c source-file))
					  ;; Insert required padding
					  ((char=? c #\<) (pad-to-align #t))
					  ;; Make sure Alignment is tracked
					  ((char=? c #\^) (set! ALIGNED #t))
					  ;; Deal with common bytes
					  (#t (process-byte c source-file #t)))
					 (set! c (read-char source-file))))
			(close-port source-file))))))

;; Deal with parsing of arguments and performing the main process
(define (main args)
  (begin
	;; Skip the name of this file
	(set! args (cdr args))
	;; Loop through arguments
	(while (not (null? args))
		   (let ((opt (car args)))
			 (cond
			  ((string= "--BigEndian" opt) (set! BigEndian #t))
			  ((string= "--LittleEndian" opt) (set! BigEndian #f))
			  ((string= "--exec_enable" opt) (set! exec_enable #t))
			  ((string= "--binary" opt) (set! ByteMode 2))
			  ((string= "--octal" opt) (set! ByteMode 8))
			  ((string= "--architecture" opt) (set! args (set-arch! (cadr args) (cdr args))))
			  ((string= "--BaseAddress" opt) (set! args (set-BaseAddress! (string->number (fixup-number (cadr args))) (cdr args))))
			  ((or (string= "-f" opt) (string= "--file" opt)) (set! args (set-input-files! (cadr args) (cdr args))))
			  ((or (string= "-o" opt) (string= "--output" opt)) (set! args (set-output-file! (cadr args) (cdr args))))
			  (#t (invalid-input)))
			 ;;(write opt output)
			 ;;(newline)
			 (set! args (cdr args))))
	;; Don't run when there are no input files
	(when (null? input_files)
	  (invalid-input))
	;; Get all of the labels
	(set! ip Base_Address)
	(first-pass input_files)
	(display jump_table)
	(newline)
	;; Fix all of the references
	(set! ip Base_Address)
	(second-pass input_files)
	;; Set file as executable
	(when exec_enable (chmod output 488))))
