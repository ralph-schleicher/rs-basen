;;; basen.lisp --- generic base N encoding

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

(in-package :rs-basen)

(defconst standard-alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The standard base 36 alphabet.

Uses the decimal digits ‘0’ to ‘9’ and the letters ‘A’ to ‘Z’.")
(declaim (type simple-string standard-alphabet))

(defconst standard-pad-character #\=
  "The standard pad character.")
(declaim (type character standard-pad-character))

(defconst rfc4648-base64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The base 64 alphabet as per RFC 4648.

Uses the letters ‘A’ to ‘Z’ and ‘a’ to ‘z’, the decimal
digits ‘0’ to ‘9’, and the characters ‘+’ and ‘/’.")
(declaim (type simple-string rfc4648-base64-alphabet))

(defconst rfc4648-base64url-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  "The base 64 URL and file name safe alphabet as per RFC 4648.

Uses the letters ‘A’ to ‘Z’ and ‘a’ to ‘z’, the decimal
digits ‘0’ to ‘9’, and the characters ‘-’ and ‘_’.")
(declaim (type simple-string rfc4648-base64url-alphabet))

(defconst rfc4648-base32-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
  "The base 32 alphabet as per RFC 4648.

Uses the letters ‘A’ to ‘Z’ and the decimal digits ‘2’ to ‘7’.")
(declaim (type simple-string rfc4648-base32-alphabet))

(defconst rfc4648-pad-character #\=
  "The pad character as per RFC 4648.")
(declaim (type character rfc4648-pad-character))

;; Depending on the font, people may have difficulties to distinguish
;; the characters 0/O, 1/I, 2/Z, 3/B, 5/S, 6/G, 7/J, 7/T, 8/B.  With
;; lowercase letters, the visual differences are more clear, i.e. 0/o,
;; 1/i, 2/z, 3/b, 5/s, 6/g, 7/j, 7/t, 8/b.  However, there are other
;; critical pairs, e.g. 1/l, 6/b, 9/g, 9/q.
(defconst human-base32-alphabet "abcdefghijklmnopqrstuvwxyz234578"
  "A human readable base 32 alphabet.

Uses the letters ‘a’ to ‘z’ and the decimal digits ‘2’, ‘3’, ‘4’,
‘5’, ‘7’, and ‘8’.  The decimal digit ‘6’ is omitted since it is
too close to the letter ‘b’.")
(declaim (type simple-string human-base32-alphabet))

(defvar *alphabet* standard-alphabet
  "The alphabet.")
(declaim (type simple-string *alphabet*))

(defvar *pad-character* standard-pad-character
  "The pad character.")
(declaim (type character *pad-character*))

;;;; Encoding

(defmacro define-encoder (name (full-quantum-size digit-size) &optional doc)
  "Define an encoder function."
  (%define-encoder name doc full-quantum-size digit-size))

(defun %define-encoder (name doc bit/full-quantum bit/digit)
  (let ((digit/full-quantum (/ bit/full-quantum bit/digit))
        (byte/full-quantum (/ bit/full-quantum 8)))
    ;; Return value.
    (if (= byte/full-quantum 1)
        `(defun ,name (output input pad)
           ,@(when doc (list doc))
           (declare (ignore pad))
           (iter (for octet = (read-byte input nil))
                 (until (null octet))
                 ;; Output encoded characters.
                 ,@(iter (repeat digit/full-quantum)
                         (for pos :from (- bit/full-quantum bit/digit) :by (- bit/digit))
                         (collecting `(write-char (char *alphabet* (ldb (byte ,bit/digit ,pos) octet)) output)))))
      `(defun ,name (output input pad)
         ,@(when doc (list doc))
         (let (;; Input buffer.
               (octets (make-array ,byte/full-quantum :element-type 'octet :initial-element 0))
               ;; An encoding quantum.
               (int 0))
           ;; An integer with FULL-QUANTUM-SIZE bit.
           (declare (type (integer 0 ,(1- (expt 2 bit/full-quantum)) int)))
           ;; Do the encoding.
           (iter (for len = (read-sequence octets input))
                 (if (= len ,byte/full-quantum)
                     (progn
                       ;; A full encoding quantum.  Load octets.
                       ,@(iter (repeat byte/full-quantum)
                               (for index :from 0)
                               (for pos :from (- bit/full-quantum 8) :by -8)
                               (collecting `(setf (ldb (byte 8 ,pos) int) (aref octets ,index))))
                       ;; Output encoded characters.
                       ,@(iter (repeat digit/full-quantum)
                               (for pos :from (- bit/full-quantum bit/digit) :by (- bit/digit))
                               (collecting `(write-char (char *alphabet* (ldb (byte ,bit/digit ,pos) int)) output))))
                   (progn
                     ;; Remaining encoding quantum.
                     (case len
                       ,@(iter (for bit/quantum :from 8 :below bit/full-quantum :by 8)
                               (for digit/quantum = (ceiling bit/quantum bit/digit))
                               (for byte/quantum = (/ bit/quantum 8))
                               (collecting
                                 ;; The ‘case’ clause.
                                 `(,byte/quantum
                                   ;; Clear encoding quantum.
                                   (setf int 0)
                                   ;; Load octets.
                                   ,@(iter (repeat byte/quantum)
                                           (for index :from 0)
                                           (for pos :from (- bit/full-quantum 8) :by -8)
                                           (collecting `(setf (ldb (byte 8 ,pos) int) (aref octets ,index))))
                                   ;; Output encoded characters.
                                   ,@(iter (repeat digit/quantum)
                                           (for pos :from (- bit/full-quantum bit/digit) :by (- bit/digit))
                                           (collecting `(write-char (char *alphabet* (ldb (byte ,bit/digit ,pos) int)) output)))
                                   ;; Output pad characters.
                                   (when (not (null pad))
                                     ,@(iter (repeat (- digit/full-quantum digit/quantum))
                                             (collecting `(write-char *pad-character* output))))))))
                     ;; Done.
                     (leave)))))))))

;; See RFC 4648, §4 “Base 64 Encoding”.
(define-encoder encode64 (24 6)
  "Generic base 64 encoding.")

;; See RFC 4648, §6 “Base 32 Encoding”.
(define-encoder encode32 (40 5)
  "Generic base 32 encoding.")

(define-encoder encode16 (8 4)
  "Generic base 16 encoding.")

(define-encoder encode8 (24 3)
  "Generic base 8 encoding.")

(define-encoder encode4 (8 2)
  "Generic base 4 encoding.")

(define-encoder encode2 (8 1)
  "Generic base 2 encoding.")

(defun %encod2 (fun output source pad)
  "Second stage – divert on input."
  (etypecase source
    (stream
     (let ((input source))
       (funcall fun output input pad)))
    (string
     (with-octets-from-string (input source)
       (funcall fun output input pad)))
    (sequence
     (with-input-from-sequence (input source)
       (funcall fun output input pad)))
    (pathname
     (with-open-file (input source :element-type 'octet)
       (funcall fun output input pad)))
    ((member t)
     (let ((input *standard-input*))
       (funcall fun output input pad))))
  ;; Return value.
  nil)

(defun %encod1 (fun destination input pad)
  "First stage – divert on output."
  (etypecase destination
    (stream
     (let ((output destination))
       (%encod2 fun output input pad)))
    (string
     (with-output-to-string (output destination)
       (%encod2 fun output input pad)))
    (pathname
     (with-open-file (output destination :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :external-format (uiop:encoding-external-format :utf-8))
       (%encod2 fun output input pad)))
    ((member t)
     (let ((output *standard-output*))
       (%encod2 fun output input pad)))
    (null
     (with-output-to-string (output)
       (%encod2 fun output input pad)))))

(defun basen-encode (destination source
                     &key
                       (base 32)
                       (alphabet standard-alphabet)
                       (pad-character standard-pad-character)
                       pad)
  "Generic base N encoding.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
Keyword argument BASE is the radix used for the encoding.  Valid
 values are 64, 32, 16, 8, 4, or 2.  Default is 32.
Keyword argument ALPHABET is the alphabet for the encoding.  Value
 has to be a string with at least BASE characters.  Default are the
 decimal digits ‘0’ to ‘9’ and the letters ‘A’ to ‘Z’.
Keyword argument PAD-CHARACTER is the pad character.  Value has to
 be a character.  Default is the ‘=’ (equals sign) character.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.
 Padding only occurs with base 32 encoding (40 bit encoding quantum)
 and base 8 encoding (24 bit encoding quantum).

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (check-type base (member 64 32 16 8 4 2))
  (check-type alphabet simple-string)
  (check-type pad-character character)
  (when (< (length alphabet) base)
    (error "Alphabet is too small"))
  (let ((*alphabet* alphabet)
        (*pad-character* pad-character))
    (%encod1 (ecase base
               (64 #'encode64)
               (32 #'encode32)
               (16 #'encode16)
               (8 #'encode8)
               (4 #'encode4)
               (2 #'encode2))
             destination source pad)))

(defun rfc4648-base64-encode (destination source &key (pad t))
  "Base 64 encoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.
 Enabled by default.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (basen-encode destination source
                :base 64
                :alphabet rfc4648-base64-alphabet
                :pad-character rfc4648-pad-character
                :pad pad))

(defun rfc4648-base64url-encode (destination source &key (pad t))
  "Base 64 encoding as per RFC 4648 but with the URL safe alphabet.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.
 Enabled by default.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (basen-encode destination source
                :base 64
                :alphabet rfc4648-base64url-alphabet
                :pad-character rfc4648-pad-character
                :pad pad))

(defun rfc4648-base32-encode (destination source &key (pad t))
  "Base 32 encoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.
 Enabled by default.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (basen-encode destination source
                :base 32
                :alphabet rfc4648-base32-alphabet
                :pad-character rfc4648-pad-character
                :pad pad))

(defun rfc4648-base32hex-encode (destination source &key (pad t))
  "Base 32 encoding as per RFC 4648 but with the standard alphabet.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.
 Enabled by default.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (basen-encode destination source
                :base 32
                :alphabet standard-alphabet
                :pad-character rfc4648-pad-character
                :pad pad))

(defun rfc4648-base16-encode (destination source &key pad)
  "Base 16 encoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is either
 a stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a string.
Second argument SOURCE is the input object.  Value is either a stream,
 a string, a sequence, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument PAD is true, append pad characters to the output
 if the input is not an integral multiple of a full encoding quantum.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output."
  (basen-encode destination source
                :base 16
                :alphabet standard-alphabet
                :pad-character rfc4648-pad-character
                :pad pad))

;;; basen.lisp ends here
