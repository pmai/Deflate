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

;;;; %File Description:
;;;; 
;;;; This file contains the system definition form for the
;;;; Deflate Decompression Library.  System definitions use the
;;;; ASDF system definition facility.
;;;; 

(defsystem "deflate"
  :description "Deflate Decompression Library"
  :author "Pierre R. Mai <pmai@pmsf.de>"
  :maintainer "Pierre R. Mai <pmai@pmsf.de>"
  :licence "MIT"
  :version "1.0.4"
  :components ((:file "deflate"))
  :in-order-to ((test-op (test-op "deflate/test"))))

(defsystem "deflate/test"
  :depends-on ("deflate" #+abcl (:require :gray-streams))
  :components ((:file "deflate-test"))
  :perform (test-op (o c) (symbol-call '#:deflate-test '#:perform-all-tests)))
