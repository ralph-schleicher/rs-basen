;;; packages.lisp --- package definitions

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

(defpackage #:de.ralph-schleicher.basen
  (:nicknames :rs-basen)
  (:use :common-lisp
        :iterate
        :trivial-gray-streams)
  (:import-from #:flexi-streams
                #:with-input-from-sequence
                #:with-output-to-sequence)
  (:export
   #:*line-length*
   #:*line-separator*
   #:*ignore-whitespace*
   #:standard-alphabet
   #:standard-pad-character
   #:rfc4648-base64-alphabet
   #:rfc4648-base64url-alphabet
   #:rfc4648-base32-alphabet
   #:rfc4648-pad-character
   #:human-base32-alphabet
   #:basen-encode
   #:basen-decode
   #:rfc4648-base64-encode
   #:rfc4648-base64-decode
   #:rfc4648-base64url-encode
   #:rfc4648-base64url-decode
   #:rfc4648-base32-encode
   #:rfc4648-base32-decode
   #:rfc4648-base32hex-encode
   #:rfc4648-base32hex-decode
   #:rfc4648-base16-encode
   #:rfc4648-base16-decode
   #:rfc1421-base64-encode
   #:rfc1421-base64-decode
   #:rfc2045-base64-encode
   #:rfc2045-base64-decode)
  (:documentation
   "A generic base N encoding/decoding library."))

;;; packages.lisp ends here
