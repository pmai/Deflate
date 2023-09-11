;;;; Deflate --- RFC 1951 Deflate Decompression
;;;;
;;;; Copyright (C) 2000-2023 PMSF IT Consulting Pierre R. Mai.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;
;;;; $Id$

(cl:defpackage #:deflate-test
  (:use #:common-lisp)
  (:import-from
   #+sbcl :sb-gray
   #+allegro :excl
   #+cmu :ext
   #+(or clisp ecl mocl clasp) :gray
   #+openmcl :ccl
   #+lispworks :stream
   #+(or abcl genera) :gray-streams
   #+mezzano :mezzano.gray
   #-(or sbcl allegro cmu clisp openmcl lispworks ecl clasp abcl mocl genera mezzano) ...
   #:fundamental-binary-input-stream #:fundamental-binary-output-stream
   #:stream-read-byte #:stream-write-byte)
  (:export
   #:perform-all-tests
   #:perform-deflate-tests
   #:perform-zlib-tests
   #:perform-gzip-tests
   #:benchrun))

(cl:in-package #:deflate-test)

;;;; %File Description:
;;;; 
;;;; This file contains infrastructure for testing the deflate
;;;; decompression library, as well as test suites for basic
;;;; operation, corner cases, and error situations.
;;;; 
;;;; The main entry points are the functions perform-all-tests, and
;;;; its cousins perform-deflate-tests, perform-zlib-tests and
;;;; perform-gzip-tests, which perform the relevant tests for the
;;;; given functionality, as well as benchrun, which is a simple
;;;; helper used in benchmarking.
;;;;
;;;; Some of the test cases for deflate corner cases were derived
;;;; from the test suite of Project Nayuki's deflate decompresser:
;;;; https://www.nayuki.io/page/simple-deflate-decompressor
;;;;

;;;
;;; Basic binary in/out streams
;;;

(defclass octet-input-stream (fundamental-binary-input-stream)
  ((data :initarg :data :type (simple-array (unsigned-byte 8) (*)))
   (position :initform 0)))

(defmethod stream-element-type ((stream octet-input-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream octet-input-stream))
  (with-slots (data position) stream
    (if (< position (length data))
        (prog1 (aref data position)
          (incf position))
        :eof)))

(defclass octet-output-stream (fundamental-binary-output-stream)
  ((data :accessor octet-output-stream-data
         :initform (make-array '(100) :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8)) :type (vector (unsigned-byte 8)))))

(defmethod stream-element-type ((stream octet-output-stream))
  '(unsigned-byte 8))

(defmethod stream-write-byte ((stream octet-output-stream) integer)
  (with-slots (data) stream
    (vector-push-extend integer data)
    integer))

;;;
;;; Basic Test Primitives
;;;

(defun map-bits-to-bytes (bit-pattern)
  (loop with result = (make-array (ceiling (length bit-pattern) 8) :element-type '(unsigned-byte 8))
        with bit-length = (length bit-pattern)
        for index upfrom 0 below (length result)
        for bit-index = (* index 8)
        do
          (setf (aref result index)
                (loop for value = 1 then (* value 2)
                      for pos upfrom bit-index below (min bit-length (+ bit-index 8))
                      sum (* (bit bit-pattern pos) value)))
        finally
          (return result)))

(defun test-sequence (name bit-pattern reference &optional expected-error)
  (handler-bind ((error #'(lambda (condition)
                            (cond
                             ((and expected-error (typep condition expected-error))
                              (format t "SUCCESS: Expected Error: ~A~%" condition)
                              (return-from test-sequence t))
                             (t
                              (format t "FAILURE: Unexpected Error: ~A~%" condition)
                              (return-from test-sequence nil))))))
    (format t "~&Test ~45A: " name)
    (let ((input (map-bits-to-bytes bit-pattern)))
      (loop with bit-length = (length bit-pattern)
            for index upfrom 0 below (length input)
            for bit-index = (* index 8)
            do
              (setf (aref input index)
                    (loop for value = 1 then (* value 2)
                          for pos upfrom bit-index below (min bit-length (+ bit-index 8))
                          sum (* (bit bit-pattern pos) value))))
      (with-open-stream (instream (make-instance 'octet-input-stream :data input))
        (with-open-stream (outstream (make-instance 'octet-output-stream))
          (deflate:inflate-stream instream outstream)
          (cond
           ((equalp (octet-output-stream-data outstream) reference)
            (format t "SUCCESS: Expected Result: ~A~%" (octet-output-stream-data outstream))
            t)
           (t
            (format t "FAILURE: Unexpected Result: ~A, expected ~A~%" (octet-output-stream-data outstream) reference)
            nil)))))))

(defun test-zlib-sequence (name input reference &optional expected-error)
  (handler-bind ((error #'(lambda (condition)
                            (cond
                             ((and expected-error (typep condition expected-error))
                              (format t "SUCCESS: Expected Error: ~A~%" condition)
                              (return-from test-zlib-sequence t))
                             (t
                              (format t "FAILURE: Unexpected Error: ~A~%" condition)
                              (return-from test-zlib-sequence nil))))))
    (format t "~&Test ~45A: " name)
    (let ((input (make-array (length input) :element-type '(unsigned-byte 8) :initial-contents input)))
      (with-open-stream (instream (make-instance 'octet-input-stream :data input))
        (with-open-stream (outstream (make-instance 'octet-output-stream))
          (deflate:inflate-zlib-stream instream outstream :check-checksum t)
          (cond
           ((equalp (octet-output-stream-data outstream) reference)
            (format t "SUCCESS: Expected Result: ~A~%" (octet-output-stream-data outstream))
            t)
           (t
            (format t "FAILURE: Unexpected Result: ~A, expected ~A~%" (octet-output-stream-data outstream) reference)
            nil)))))))

(defun test-gzip-sequence (name input reference &optional expected-error)
  (handler-bind ((error #'(lambda (condition)
                            (cond
                             ((and expected-error (typep condition expected-error))
                              (format t "SUCCESS: Expected Error: ~A~%" condition)
                              (return-from test-gzip-sequence t))
                             (t
                              (format t "FAILURE: Unexpected Error: ~A~%" condition)
                              (return-from test-gzip-sequence nil))))))
    (format t "~&Test ~45A: " name)
    (let ((input (make-array (length input) :element-type '(unsigned-byte 8) :initial-contents input)))
      (with-open-stream (instream (make-instance 'octet-input-stream :data input))
        (with-open-stream (outstream (make-instance 'octet-output-stream))
          (deflate:inflate-gzip-stream instream outstream :check-checksum t)
          (cond
           ((equalp (octet-output-stream-data outstream) reference)
            (format t "SUCCESS: Expected Result: ~A~%" (octet-output-stream-data outstream))
            t)
           (t
            (format t "FAILURE: Unexpected Result: ~A, expected ~A~%" (octet-output-stream-data outstream) reference)
            nil)))))))

;;;
;;; Test Cases
;;;

(defun perform-deflate-tests ()
  (format t "~2&Performing deflate tests~2%")
  (macrolet ((every-form (&body body) `(let ((success t)) ,@(loop for form in body collect `(unless ,form (setq success nil))) success)))
    (every-form
     ;; Simple Tests
     (test-sequence "Empty Uncompressed Block" #*1000000000000000000000001111111111111111 #())
     (test-sequence "Two Empty Uncompressed Blocks" #*00000000000000000000000011111111111111111000000000000000000000001111111111111111 #())
     (test-sequence "Two Empty Fixed Huffman Blocks" #*01000000001100000000 #())
     (test-sequence "Two Empty Dynamic Huffman Blocks" #*0010000010000111100000010000000000000000000000000000000000000000000010000001111111110101011000110100000100001111000000100000000000000000000000000000000000000000000100000011111111101010110001 #())
     (test-sequence "Fixed ABC Literal Block" #*1100111000101110010011100110000000 #(65 66 67))
     (test-sequence "Fixed ABC Repeating Block" #*1100111000101110010011100110000001000100000001000100000000 #(65 66 67 65 66 67 65 66 67))
     (test-sequence "Fixed ABC Repeating Overlapping Block" #*1100111000101110010011100110000111000100000000 #(65 66 67 65 66 67 65 66 67 65 66 67))
     (test-sequence "Fixed ABC Repeating Out Of Range Block" #*1100111000101110010011100110001000001000000000 #(65 66 67 00 00 65 66 67 00 00 65 66 67))
     (test-sequence "Dynamic ABC Literal Block" #*10100000000001111000000010010000000000000000000000000000000000010000000000100110110010101101111111101110010010000011011 #(65 66 67))
     (test-sequence "Dynamic AB Repeating Block Dist 1" #*101100000000011110000000100100000000000000000000000000000000000100000100001101101101010111111111110001010101001000111010 #(65 66 66 66 66))
     (test-sequence "Dynamic AB Repeating Block Dist 2" #*1010100010000111100000001001000000000000000000000000000000000001000001000011011011010101111111111100010101000100001000111010 #(65 66 65 66 65 66))

     (test-sequence "EOF Start Of Block" #* #() 'end-of-file)
     (test-sequence "Reserved Block Type" #*11100000 #() 'deflate:deflate-decompression-error)
     (test-sequence "EOF In Block Type" #*10 #() 'end-of-file)
     (test-sequence "Uncompressed Empty" #*1000000000000000000000001111111111111111 #())
     (test-sequence "Uncompressed ThreeBytes" #*1000000011000000000000000011111111111111101000000010100011000100 #(#x05 #x14 #x23))
     (test-sequence "Uncompressed TwoBlocks" #*00000000010000000000000010111111111111111010000000101000100000001000000000000000011111111111111111000100 #(#x05 #x14 #x23))
     (test-sequence "Uncompressed EOF Before Length" #*100000 #() 'end-of-file)
     (test-sequence "Uncompressed EOF In Length" #*100000000000000000 #() 'end-of-file)
     (test-sequence "Uncompressed Mismatched Length" #*1000000000100000000100001111100100110101 #() 'deflate:deflate-decompression-error)
     (test-sequence "Uncompressed EOF In Data" #*10011111011000000000000010011111111111111010101001110111 #() 'end-of-file)
     (test-sequence "Uncompressed Block No Final Block" #*0000000000000000000000001111111111111111 #() 'end-of-file)
     (test-sequence "Uncompressed Block No Discard Bits" #*0101100100001101000011111111110000000100010000000000000010111111111111111101010110110011 #(#x90 #xA1 #xFF #xAB #xCD))
     (test-sequence "Fixed Huffman Empty" #*1100000000 #())
     (test-sequence "Fixed Huffman Literals" #*1100011000010110000101111111100100001110000001111111110000000 #(#x00 #x80 #x8F #x90 #xC0 #xFF))
     (test-sequence "Fixed Huffman Non Overlapping Run" #*1100011000000110001001100100000001000100000000 #(#x00 #x01 #x02 #x00 #x01 #x02))
     (test-sequence "Fixed Huffman Overlapping Run 0" #*110001100010000010000000000000 #(#x01 #x01 #x01 #x01 #x01))
     (test-sequence "Fixed Huffman Overlapping Run 1" #*11010111110101111110000011000010000000 #(#x8E #x8F #x8E #x8F #x8E #x8F #x8E))
     (test-sequence "Fixed Huffman Invalid Length Code 286" #*11011000110 #() 'deflate:deflate-decompression-error)
     (test-sequence "Fixed Huffman Invalid Length Code 287" #*11011000111 #() 'deflate:deflate-decompression-error)
     (test-sequence "Fixed Huffman Invalid Distance Code 30" #*11000110000000000111110 #() 'deflate:deflate-decompression-error)
     (test-sequence "Fixed Huffman Invalid Distance Code 31" #*11000110000000000111111 #() 'deflate:deflate-decompression-error)
     (test-sequence "Fixed Huffman EOF In Huffman Symbol" #*11000000 #() 'end-of-file)
     (test-sequence "Fixed Huffman EOF In Run Extension Bits" #*1100011000000011011 #() 'end-of-file)
     (test-sequence "Fixed Huffman EOF In Distance Extension Bits" #*11000110000110001010000000000010100000 #() 'end-of-file)
     (test-sequence "Dynamic Huffman Empty" #*10100000100001111000000100000000000000000000000000000000000000000000100000011111111101010110001 #())
     (test-sequence "Dynamic Huffman Empty No Distance Code" #*1010000000000011100000010001000000000000000000000000000000000000000001001111111001010111111101 #())
     (test-sequence "Dynamic Huffman Code Length Repeat At Start" #*101000000000001111000000000000000000000000000000000000000000000000001001 #() 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Too Many Code Length Items" #*10100000000000111000000100000000000000000000000000000000000000000000100001111111110011011 #() 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Overfull Code 0" #*101000000000000001001001000000000000000000000000  #() 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Overfull Code 1"#*101000000000000001001001001000000000000000000000  #() 'end-of-file) ;; Should maybe be deflate:deflate-decompression-error
     (test-sequence "Dynamic Huffman Unpaired Code" #*101000000000000001000101100000000000000000000000 #() 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Empty Code" #*101000000000000000000000000000000000000000000000 #() 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Underfull Code 0" #*101000000000000000000001000000000000000000000000 #() 'end-of-file) ;; Should maybe be deflate:deflate-decompression-error
     (test-sequence "Dynamic Huffman Underfull Code 1"#*101000000000000000101000000000000000000000000000 #() 'end-of-file) ;; Should maybe be deflate:deflate-decompression-error
     (test-sequence "Dynamic Huffman One Distance Code" #*101100000000001110000000100100000000000000000000000000000000000100000100010111111111111001011011001101100 #(#x01 #x01 #x01 #x01))
     (test-sequence "Dynamic Huffman One Distance Code Invalid" #*101100000000001110000000100100000000000000000000000000000000000100000100010111111111111001011011001101110101100 #(#x01 #x01 #x01 #x01) 'deflate:deflate-decompression-error)
     (test-sequence "Dynamic Huffman Use Of Null Distance Code" #*101100000000001110000000100100000000000000000000000000000000000100000101011111111111010101101100010110000000000000000 #() 'deflate:deflate-decompression-error))))

(defun perform-zlib-tests ()
  (format t "~2&Performing zlib tests~2%")
  (macrolet ((every-form (&body body) `(let ((success t)) ,@(loop for form in body collect `(unless ,form (setq success nil))) success)))
    (every-form
     (test-zlib-sequence "Small File"
                         #(#x78 #x01 #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x05 #x1C #x01 #x90)
                         #(#x44 #x65 #x6D #x6F #x0A))
     (test-zlib-sequence "Small File, Wrong FCHECK"
                         #(#x78 #x07 #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x05 #x1C #x01 #x90)
                         #(#x44 #x65 #x6D #x6F #x0A) 'deflate:zlib-decompression-error)
     (test-zlib-sequence "Small File, Invalid Adler32"
                         #(#x78 #x01 #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x00 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A) 'deflate:zlib-decompression-error)
     (test-zlib-sequence "Small File, Dict"
                         #(#x78 #x20 #xDE #xAD #xBE #xFF #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x00 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A) 'deflate:zlib-decompression-error)
     (test-zlib-sequence "Small File, Dict, Invalid Adler32"
                         #(#x78 #x20 #xDE #xAD #xBE #xFF #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x00 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A) 'deflate:zlib-decompression-error)
     (test-zlib-sequence "Samll File, Wrong CM"
                         #(#x77 #x09 #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #x05 #x1C #x01 #x90)
                         #(#x44 #x65 #x6D #x6F #x0A) 'deflate:zlib-decompression-error)
     (test-zlib-sequence "Empty File"
                         #(#x78 #x01 #x01 #x00 #x00 #xff #xff #x00 #x00 #x00 #x01)
                         #())
     (test-zlib-sequence "Empty File, Invalid Adler32"
                         #(#x78 #x01 #x01 #x00 #x00 #xff #xff #x05 #x1C #x01 #x90)
                         #() 'deflate:zlib-decompression-error))))

(defun perform-gzip-tests ()
  (format t "~2&Performing gzip tests~2%")
  (macrolet ((every-form (&body body) `(let ((success t)) ,@(loop for form in body collect `(unless ,form (setq success nil))) success)))
    (every-form
     (test-gzip-sequence "Small File, FNAME"
                         #(#x1F #x8B #x08 #x08 #x0F #x66 #x6A #x64 #x02 #x03 #x74 #x65 #x73 #x74 #x2E #x74 #x78 #x74 #x00
                                #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #xA0 #xC8 #x16 #x25 #x05 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A))
     (test-gzip-sequence "Small File, FNAME, FCOMMENT, FEXTRA"
                         #(#x1F #x8B #x08 #x1D #x0F #x66 #x6A #x64 #x02 #x03 #x06 #x00 #xDE #xAD #x02 #x00 #xBE #xEF #x74 #x65 #x73 #x74 #x2E #x74 #x78 #x74 #x00
                                #x4D #x79 #x20 #x43 #x6F #x6D #x6D #x65 #x6E #x74 #x00 #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #xA0 #xC8 #x16 #x25 #x05 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A))
     (test-gzip-sequence "Small File, FNAME, Invalid CRC32"
                         #(#x1F #x8B #x08 #x09 #x0F #x66 #x6A #x64 #x02 #x03 #x74 #x65 #x73 #x74 #x2E #x74 #x78 #x74 #x00
                                #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #xA0 #xA8 #x16 #x25 #x05 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A)
                         'deflate:gzip-decompression-error)
     (test-gzip-sequence "Small File, FNAME, Wrong CM"
                         #(#x1F #x8B #x05 #x08 #x0F #x66 #x6A #x64 #x02 #x03 #x74 #x65 #x73 #x74 #x2E #x74 #x78 #x74 #x00
                                #x73 #x49 #xCD #xCD #xE7 #x02 #x00 #xA0 #xC8 #x16 #x25 #x05 #x00 #x00 #x00)
                         #(#x44 #x65 #x6D #x6F #x0A)
                         'deflate:gzip-decompression-error)
     (test-gzip-sequence "Empty, FCOMMENT, No FHCRC CRC16"
                         #(#x1f #x8b #x08 #x10 #x00 #x09 #x6e #x88 #x00 #xff #x48 #x65 #x6c #x6c #x6f #x00 #x01 #x00 #x00 #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
                         #())
     (test-gzip-sequence "Empty, FCOMMENT, FHCRC CRC16"
                         #(#x1f #x8b #x08 #x12 #x00 #x09 #x6e #x88 #x00 #xff #x48 #x65 #x6c #x6c #x6f #x00 #x99 #xd6 #x01 #x00 #x00 #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
                         #())
     (test-gzip-sequence "Empty, FCOMMENT, FHCRC Invalid CRC16"
                         #(#x1f #x8b #x08 #x12 #x00 #x09 #x6e #x88 #x00 #xff #x48 #x65 #x6c #x6c #x6f #x00 #x49 #xd6 #x01 #x00 #x00 #xff #xff #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
                         #() 'deflate:gzip-decompression-error))))

(defun perform-all-tests ()
  (macrolet ((every-form (&body body) `(let ((success t)) ,@(loop for form in body collect `(unless ,form (setq success nil))) success)))
    (every-form
     (perform-deflate-tests)
     (perform-zlib-tests)
     (perform-gzip-tests))))

;;;
;;; Benchmarking helper
;;;

(defun benchrun (file destination &optional check-checksum)
  (declare (optimize (speed 3)))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (with-open-file (output destination :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (deflate:inflate-gzip-stream stream output :check-checksum check-checksum))))
