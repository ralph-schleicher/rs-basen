;;; tests.lisp --- base N encoding/decoding test suite

;; Copyright (C) 2023 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.basen-tests
  (:nicknames :rs-basen-tests)
  (:use :common-lisp
        :iterate
        :lisp-unit
        :rs-basen)
  (:export
   #:main))

(in-package :rs-basen-tests)

(defparameter *prefix* (asdf:system-source-directory "rs-basen"))

(define-test octets-from-string
  (assert-equal
   '(206 187 226 129 187 194 185)
   (rs-basen::with-octets-from-string (stream "λ⁻¹")
     (iter (for octet = (read-byte stream nil))
           (until (null octet))
           (collecting octet))))
  (assert-equal
   '(206 187 226 129 187 194 185)
   (rs-basen::with-octets-from-string (stream "λ⁻¹")
     (let ((octets (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
       (iter (for len = (read-sequence octets stream))
             (until (zerop len))
             (nconcing (coerce (subseq octets 0 len) 'list))))))
  (assert-equal
   '(206 187 226 129 187 194 185)
   (rs-basen::with-octets-from-string (stream "λ⁻¹")
     (let ((octets (make-list 3 :initial-element 0)))
       (iter (for len = (read-sequence octets stream))
             (until (zerop len))
             (nconcing (subseq octets 0 len))))))
  (assert-equal
   '(239 187 191 206 187 226 129 187 194 185)
   (rs-basen::with-octets-from-string (stream "λ⁻¹" :byte-order-mark t)
     (let ((octets (make-list 3 :initial-element 0)))
       (iter (for len = (read-sequence octets stream))
             (until (zerop len))
             (nconcing (subseq octets 0 len))))))
  ())

(define-test encode
  ;; Test vectors from RFC 4648.
  (assert-equal "" (basen-encode nil "" :alphabet human-base32-alphabet))
  (assert-equal "my" (basen-encode nil "f" :alphabet human-base32-alphabet))
  (assert-equal "mzxq" (basen-encode nil "fo" :alphabet human-base32-alphabet))
  (assert-equal "mzxw7" (basen-encode nil "foo" :alphabet human-base32-alphabet))
  (assert-equal "mzxw7yq" (basen-encode nil "foob" :alphabet human-base32-alphabet))
  (assert-equal "mzxw7ytb" (basen-encode nil "fooba" :alphabet human-base32-alphabet))
  (assert-equal "mzxw7ytboi" (basen-encode nil "foobar" :alphabet human-base32-alphabet))
  (assert-equal "" (basen-encode nil "" :alphabet human-base32-alphabet :pad t))
  (assert-equal "my======" (basen-encode nil "f" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxq====" (basen-encode nil "fo" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxw7===" (basen-encode nil "foo" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxw7yq=" (basen-encode nil "foob" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxw7ytb" (basen-encode nil "fooba" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxw7ytboi======" (basen-encode nil "foobar" :alphabet human-base32-alphabet :pad t))
  (assert-equal "mzxw7ytboi++++++" (basen-encode nil "foobar" :alphabet human-base32-alphabet :pad-character #\+ :pad t))
  (assert-equal "" (rfc4648-base64-encode nil ""))
  (assert-equal "Zg==" (rfc4648-base64-encode nil "f"))
  (assert-equal "Zm8=" (rfc4648-base64-encode nil "fo"))
  (assert-equal "Zm9v" (rfc4648-base64-encode nil "foo"))
  (assert-equal "Zm9vYg==" (rfc4648-base64-encode nil "foob"))
  (assert-equal "Zm9vYmE=" (rfc4648-base64-encode nil "fooba"))
  (assert-equal "Zm9vYmFy" (rfc4648-base64-encode nil "foobar"))
  (assert-equal "" (rfc4648-base32-encode nil ""))
  (assert-equal "MY======" (rfc4648-base32-encode nil "f"))
  (assert-equal "MZXQ====" (rfc4648-base32-encode nil "fo"))
  (assert-equal "MZXW6===" (rfc4648-base32-encode nil "foo"))
  (assert-equal "MZXW6YQ=" (rfc4648-base32-encode nil "foob"))
  (assert-equal "MZXW6YTB" (rfc4648-base32-encode nil "fooba"))
  (assert-equal "MZXW6YTBOI======" (rfc4648-base32-encode nil "foobar"))
  (assert-equal "" (rfc4648-base32hex-encode nil ""))
  (assert-equal "CO======" (rfc4648-base32hex-encode nil "f"))
  (assert-equal "CPNG====" (rfc4648-base32hex-encode nil "fo"))
  (assert-equal "CPNMU===" (rfc4648-base32hex-encode nil "foo"))
  (assert-equal "CPNMUOG=" (rfc4648-base32hex-encode nil "foob"))
  (assert-equal "CPNMUOJ1" (rfc4648-base32hex-encode nil "fooba"))
  (assert-equal "CPNMUOJ1E8======" (rfc4648-base32hex-encode nil "foobar"))
  (assert-equal "" (rfc4648-base16-encode nil ""))
  (assert-equal "66" (rfc4648-base16-encode nil "f"))
  (assert-equal "666F" (rfc4648-base16-encode nil "fo"))
  (assert-equal "666F6F" (rfc4648-base16-encode nil "foo"))
  (assert-equal "666F6F62" (rfc4648-base16-encode nil "foob"))
  (assert-equal "666F6F6261" (rfc4648-base16-encode nil "fooba"))
  (assert-equal "666F6F626172" (rfc4648-base16-encode nil "foobar"))
  ;; Disable padding.
  (assert-equal "Zm9vYg" (rfc4648-base64-encode nil "foob" :pad nil))
  (assert-equal "MZXW6" (rfc4648-base32-encode nil "foo" :pad nil))
  (assert-equal "CPNMU" (rfc4648-base32hex-encode nil "foo" :pad nil))
  (assert-equal "666F6F" (rfc4648-base16-encode nil "foo" :pad nil))
  (assert-equal "666F6F" (rfc4648-base16-encode nil "foo" :pad t))
  ())

(defun main (&optional (tests :all))
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t)
        (lisp-unit:*print-summary* t))
    (run-tests tests :rs-basen-tests)))

;; tests.lisp ends here
