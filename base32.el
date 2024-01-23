;;; base32.el --- Base32 encoding in Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 2023--2024 Dima Akater
;; Author: Dima Akater <nuclearspace@gmail.com>
;; Maintainer: Dima Akater <nuclearspace@gmail.com>
;; Version: 0.0.20240123.215407
;; Package-Requries: ((emacs "27.1") (cl-bytes "0.0.20240122.0"))


;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; [[file:base32.org::*Runtime and build-time dependencies][Runtime and build-time dependencies:1]]
(eval-when-compile (require 'cl-macs))
(require 'cl-bytes)
;; Runtime and build-time dependencies:1 ends here

;; [[file:base32.org::*Misc deps][Misc deps:1]]
(defalias 'cl-char 'aref)
(defalias 'cl-char-code 'identity)
(defalias 'cl-char<= '<=)
;; Misc deps:1 ends here

;; [[file:base32.org::*Encoding digits][Encoding digits:1]]
(defvar base32-alphabet "abcdefghijklmnopqrstuvwxyz234567")
;; Encoding digits:1 ends here

;; [[file:base32.org::*Encoding digits][Encoding digits:2]]
(defun base32--encode-word (a-word)
  "Return the digit in the base32 alphabet corresponding to a word A-WORD."
  (cl-char base32-alphabet a-word))
;; Encoding digits:2 ends here

;; [[file:base32.org::*Encoding digits][Encoding digits:3]]
(defun base32--decode-word (a-digit)
  "Return the word encoded as a digit A-DIGIT in the base32 alphabet."
  (let ((code (cl-char-code a-digit)))
    (cond
     ((cl-char<= ?a a-digit ?z)
      (- code (cl-char-code ?a)))
     ((cl-char<= ?2 a-digit ?7)
      (+ 26 (- code (cl-char-code ?2))))
     ;; upper case
     ((cl-char<= ?A a-digit ?Z)
      (- code (cl-char-code ?A))))))
;; Encoding digits:3 ends here

;; [[file:base32.org::*Encoding digits][Encoding digits:4]]
(defmacro with-base32-upcase (&rest body)
  (declare (indent 0))
  `(let ((base32-alphabet (upcase base32-alphabet)))
     ,@body))
;; Encoding digits:4 ends here

;; [[file:base32.org::*Read, write][Read, write:1]]
(defun base32--read-word (some-bytes word-index)
  "Return the word (a 5-bit integer) found in SOME-BYTES located at WORD-INDEX."
  (let* ((bytes-length (length some-bytes))
	 ;; don't be confused : bit indexes aren't really pointing to
	 ;; the bit as understood by Lisp--they are more virtual in nature,
	 ;; which assumes that bit 0 is the MSB rather than bit 8 being MSB
         (start-bit-index (* 5 word-index))
         (end-bit-index (+ 4 start-bit-index))
         (part1-byte-index (floor start-bit-index 8)) 
         (part2-byte-index (floor end-bit-index 8))
         (part1-size (min 5 (- 8 (mod start-bit-index 8))))
         (part2-size (- 5 part1-size))
	 ;; here we translate the bit indexes so that the MSB is bit 8
	 ;; and the LSB is bit 0
         (source-part1 (cl-byte part1-size 
                                (- (- 8 (mod start-bit-index 8)) part1-size)))
         (source-part2 (cl-byte part2-size 
                                (- (- 8 (mod end-bit-index 8)) 1)))
	 ;; becomes the upper bits in value
         (dest-part1 (cl-byte part1-size part2-size)) 
	 ;; becomes the lower bits in value
         (dest-part2 (cl-byte part2-size 0)) 
         (value 0))
    
    (setf (cl-ldb dest-part1 value)
          (cl-ldb source-part1 (aref some-bytes part1-byte-index)))
    (cond
     ((>= part1-byte-index bytes-length)
      (setq value 0))
     ((<= part2-byte-index part1-byte-index))
     ((< part2-byte-index (length some-bytes))
      (setf (cl-ldb dest-part2 value)
            (cl-ldb source-part2 (aref some-bytes part2-byte-index))))
     (t
      (setf (cl-ldb dest-part2 value) 0)))
    value))
;; Read, write:1 ends here

;; [[file:base32.org::*Read, write][Read, write:2]]
(defun base32--write-word (some-bytes word-index word)
  "Write the WORD into the bits located at WORD-INDEX in SOME-BYTES."
  (let* ((bytes-length (length some-bytes))
         ;; don't be confused : bit indexes aren't really pointing to
         ;; the bit as understood by Lisp--they are more virtual in nature,
         ;; which assumes that bit 0 is the MSB rather than bit 8 being MSB
         (start-bit-index (* 5 word-index))
         (end-bit-index (+ 4 start-bit-index))
         (part1-byte-index (floor start-bit-index 8)) 
         (part2-byte-index (floor end-bit-index 8))
         (part1-size (min 5 (- 8 (mod start-bit-index 8))))
         (part2-size (- 5 part1-size))
	 ;; here we translate the bit indexes so that the MSB is bit 8
	 ;; and the LSB is bit 0
         (dest-part1 (cl-byte part1-size 
                              (- (- 8 (mod start-bit-index 8)) part1-size)))
         (dest-part2 (cl-byte part2-size 
                              (- (- 8 (mod end-bit-index 8)) 1) ))
	 ;; becomes the upper bits in value
         (source-part1 (cl-byte part1-size part2-size)) 
	 ;; becomes the lower bits in value
         (source-part2 (cl-byte part2-size 0))   
         (part1-byte (aref some-bytes part1-byte-index))
         (part2-byte (when (and (< part2-byte-index bytes-length)
                                (> part2-size 0)) 
                       (aref some-bytes part2-byte-index))))
    (setf (cl-ldb dest-part1 part1-byte)
          (cl-ldb source-part1 word))
    (when part2-byte
      (setf (cl-ldb dest-part2 part2-byte)
            (cl-ldb source-part2 word)))    
    (setf (aref some-bytes part1-byte-index) part1-byte)
    (when part2-byte
      (setf (aref some-bytes part2-byte-index) part2-byte))))
;; Read, write:2 ends here

;; [[file:base32.org::*Dependencies][Dependencies:1]]
(require 'cl-seq)
;; Dependencies:1 ends here

;; [[file:base32.org::*Definition][Definition:1]]
(defun base32--unpadded-length (base32-string)
  "Given a BASE32-STRING, return the size of the raw string, without any = padding."
  (1+ (or (cl-position ?= base32-string :from-end t :test-not #'char-equal)
          -1)))
;; Definition:1 ends here

;; [[file:base32.org::*byte-length][byte-length:1]]
(defun base32-byte-length (base32-string)
  "Given a BASE32-STRING, compute the number of bytes in the decoded data."
  (let* ((padded-length (length base32-string))
         (unpadded-length padded-length)
         (padding 0)
         (block-count (ceiling padded-length 8)))
    (if (<= padded-length 0)
        0
      (dotimes (i padded-length)
        (when (char-equal ?= (aref base32-string (- padded-length i 1)))
          (cl-decf unpadded-length)
          (cl-incf padding)))
      (- (* 5 block-count)
         (cl-ecase padding
	   (0 0)
	   (6 4)
	   (4 3)
	   (3 2)
	   (1 1))))))
;; byte-length:1 ends here

;; [[file:base32.org::*byte-length][byte-length:2]]
(defun base32-length-from-bytes (some-bytes)
  "Determine the length of the base32-encoded string corresponding to SOME-BYTES."
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5) )
         (digit-count (* 8 (ceiling word-count 8))))
    (cl-values digit-count word-count)))
;; byte-length:2 ends here

;; [[file:base32.org::*From bytes][From bytes:1]]
(defun base32-from-bytes (some-bytes)
  "Return a base32 string encoding of the provided vector of SOME-BYTES."
  (let* ((word-count (ceiling (* 8 (length some-bytes)) 5))
         (digit-count (* 8 (ceiling word-count 8)))
         (base32-string (make-string digit-count ?=)))
    (dotimes (i word-count)
      (setf (aref base32-string i)
            (base32--encode-word (base32--read-word some-bytes i))))
    base32-string))
;; From bytes:1 ends here

;; [[file:base32.org::*Definition][Definition:1]]
(defun base32-to-bytes (base32-string)
  "Return the bytes decoded from the supplied BASE32-STRING."
  (let* ((byte-count (base32-byte-length base32-string))
         (base32-bytes (make-vector byte-count
                                    ;; :element-type '(unsigned-byte 8) 
                                    0)))
    (cl-dotimes (i (length base32-string))
      (let ((word (base32--decode-word (aref base32-string i))))
        (if word
            (base32--write-word base32-bytes i word)
          (cl-return))))
    base32-bytes))
;; Definition:1 ends here

(provide 'base32)

;;; base32.el ends here
