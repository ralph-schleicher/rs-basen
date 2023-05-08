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

(defvar *line-length* nil
  "The maximum line length of the encoded data.
Value has to be a non-negative integer or ‘nil’.  The actual line
length is the given value rounded down to the nearest multiple of
a full encoding quantum.  If the actual line length is a positive
integer, the encoded data is split into multiple lines.  A value
of ‘nil’ disables line breaks.  Default is ‘nil’.")
(declaim (type (or (integer 0) null) *line-length*))

(defconst nl (string #\Newline)
  "The default newline character sequence.")
(declaim (type simple-string nl))

(defconst lf (string #\Linefeed)
  "Line feed (Unicode U+000A) character sequence.")
(declaim (type simple-string lf))

(defconst cr (string #\Return)
  "Carriage return (Unicode U+000D) character sequence.")
(declaim (type simple-string cr))

(defconst crlf (concatenate 'string cr lf)
  "Carriage return (Unicode U+000D) and line feed (Unicode U+000A) character sequence.")
(declaim (type simple-string crlf))

(defvar *line-separator* :default
  "The line separator for chunked output.
Value is ‘:default’, ‘:lf’, ‘:cr’, or ‘crlf’ to utilize the default
newline character sequence, a line feed character, a carriage return
character, or a carriage return and line feed character sequence
respectively.

See also the ‘*line-length*’ special variable.")
(declaim (type (member :default :lf :cr :crlf) *line-separator*))

(defvar *default-line-separator* nl
  "The default line separator.

See also the ‘*line-separator*’ special variable.")
(declaim (type simple-string *default-line-separator*))

(defun line-separator ()
  "Return the newline character sequence.

Affected by ‘*line-separator*’ and ‘*default-line-separator*’."
  (ecase (or *line-separator* :default)
    (:default *default-line-separator*) (:lf lf) (:cr cr) (:crlf crlf)))

(defvar *ignore-whitespace* nil
  "Whether or not to ignore whitespace characters on input.
If enabled, whitespace characters around the encoded data and between
full encoding quantums is ignored.  Disabled by default.")

;;;; Encoding

(defmacro define-encoder (name (full-quantum-size digit-size) &optional doc)
  "Define an encoder function."
  (%define-encoder name doc full-quantum-size digit-size))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %define-encoder (name doc bit/full-quantum bit/digit)
    (let ((digit/full-quantum (/ bit/full-quantum bit/digit))
          (byte/full-quantum (/ bit/full-quantum 8)))
      ;; Return value.
      (if (= byte/full-quantum 1)
          `(defun ,name (output input pad)
             ,@(when doc (list doc))
             (declare (ignore pad))
             (iter (with current-column = 0)
                   (with fill-column = (truncate (or *line-length* 0) ,digit/full-quantum))
                   (with separator = (when (plusp fill-column) (line-separator)))
                   (for octet = (read-byte input nil))
                   (until (null octet))
                   ;; Manage line breaks.
                   (when (and separator (>= current-column fill-column))
                     (write-string separator output)
                     (setf current-column 0))
                   (incf current-column)
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
             (declare (type (integer 0 ,(1- (expt 2 bit/full-quantum))) int))
             ;; Do the encoding.
             (iter (with current-column = 0)
                   (with fill-column = (truncate (or *line-length* 0) ,digit/full-quantum))
                   (with separator = (when (plusp fill-column) (line-separator)))
                   (for len = (read-sequence octets input))
                   (if (= len ,byte/full-quantum)
                       (progn
                         ;; A full encoding quantum.
                         (when (and separator (>= current-column fill-column))
                           (write-string separator output)
                           (setf current-column 0))
                         (incf current-column)
                         ;; Load octets.
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
                                     (when (and separator (>= current-column fill-column))
                                       (write-string separator output))
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
  ())

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
    ((eql t)
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
                                         :element-type 'character
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :external-format (uiop:encoding-external-format :utf-8))
       (%encod2 fun output input pad)))
    ((eql t)
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
 Padding only occurs with base 64 encoding (24 bit encoding quantum),
 base 32 encoding (40 bit encoding quantum), and base 8 encoding
 (24 bit encoding quantum).

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a string containing the
output.

Affected by ‘*line-length*’ and ‘*line-separator*’."
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

;;;; Decoding

(defvar *char-equal* #'char=
  "Equality test function for two characters.")
(declaim (type (function (character character) t) *char-equal*))

(defvar *junk-allowed* nil
  "True means to stop parsing at the first invalid character.")

(define-condition decoding-error (basen-error)
  ()
  (:documentation "Condition type for a decoding error.

Class precedence list:

     ‘decoding-error’, ‘basen-error’, ...")
  (:report (lambda (condition stream)
             (format stream "Decoding error")
             (alexandria:when-let ((input (stream-error-stream condition)))
               (format stream " in ~S" input)
               (alexandria:when-let ((position (stream-error-position condition)))
                 (format stream " at ~S" position)))
             (format stream ".")
             (when (stringp (simple-condition-format-control condition))
               (terpri stream)
               (apply #'format stream
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition))))))

(defun unexpected-character (input char)
  "Signal a ‘decoding-error’.

First argument INPUT is the input stream.
Second argument CHAR is the invalid character."
  (error 'decoding-error
         :stream input
         :position (file-position input)
         :format-control "Unexpected character ‘~A’."
         :format-arguments (list char)))

(defun invalid-last-digit (input char)
  "Signal a ‘decoding-error’.

First argument INPUT is the input stream.
Second argument CHAR is the invalid character."
  (error 'decoding-error
         :stream input
         :position (file-position input)
         :format-control "Invalid last digit ‘~A’, pad bits are not zero."
         :format-arguments (list char)))

(defun invalid-sequence-length (input length expected-length)
  "Signal a ‘decoding-error’.

First argument INPUT is the input stream.
Second argument LENGTH is the actual number of remaining digits.
Third argument EXPECTED-LENGTH is a list of valid lengths.

For example, base 16 encoding always generates digit pairs with no
rest.  Base 16 decoding a sequence with an odd number of characters
leaves a rest of one character."
  (error 'decoding-error
         :stream input
         :position (file-position input)
         ;; The ‘~?’ format directive consumes two arguments,
         ;; a format control and the list of arguments.
         :format-control "Wrong number of remaining digits.~%Expect ~? but got ~A."
         ;; Argument EXPECTED-LENGTH is a list of non-negative
         ;; integers.  The format control enumerates the list
         ;; elements as ‘none’, ‘1’, ‘1 or 2’, ‘1, 2, or 3’,
         ;; and so on.
         :format-arguments (list "~#[none~;~A~;~A or ~A~:;~@{~#[~;or ~]~A~^, ~}~]" expected-length length)))

(defun skip-whitespace (input)
  "Skip over whitespace characters."
  (iter (for char = (read-char input nil nil))
        (cond ((null char)
               (finish))
              ((not (unicode-whitespace-p char))
               (unread-char char input)
               (finish)))))

(defun decode-input (weights input)
  "Attempt to read a full encoding quantum.

First argument WEIGHTS stores the resulting digit weights.
Second argument INPUT is the character input stream.

Return value is the number of digits read.  If secondary value is
true, the input was filled with pad characters."
  (when *ignore-whitespace*
    (skip-whitespace input))
  (iter (with index = 0) ;number of digits read
        (with pad = 0) ;number of pad characters
        (with weight) ;weight of a digit
        (repeat (length weights))
        (for char = (read-char input nil nil))
        (cond ((null char)
               (finish))
              ((char= char *pad-character*)
               (incf pad))
              (t
               (setf weight (position char *alphabet* :test *char-equal*))
               (when (or (null weight) (plusp pad))
                 ;; CHAR is neither a digit nor a pad character.
                 ;; Digits after the first pad character are not
                 ;; valid.
                 (unread-char char input)
                 (when (or (not *junk-allowed*) (plusp pad))
                   (unexpected-character input char))
                 (finish))
               ;; A valid digit.
               (setf (aref weights index) weight)
               (incf index)))
        (finally
         (when (and (plusp pad) (< (+ index pad) (length weights)))
           (error 'end-of-file :stream input))
         (return (values index (plusp pad))))))

(defmacro define-decoder (name (full-quantum-size digit-size) &optional doc)
  "Define a decoder function."
  (%define-decoder name doc full-quantum-size digit-size))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %define-decoder (name doc bit/full-quantum bit/digit)
    (let ((digit/full-quantum (/ bit/full-quantum bit/digit))
          (byte/full-quantum (/ bit/full-quantum 8)))
      ;; Return value.
      `(defun ,name (output input)
         ,@(when doc (list doc))
         (let (;; Input buffer (digit weights, not characters).
               (octets (make-array ,digit/full-quantum :element-type 'octet :initial-element 0))
               ;; An encoding quantum.
               (int 0))
           ;; An integer with FULL-QUANTUM-SIZE bit.
           (declare (type (integer 0 ,(1- (expt 2 bit/full-quantum))) int))
           ;; Do the decoding.
           (iter (for (values len pad) = (decode-input octets input))
                 (if (= len ,digit/full-quantum)
                     (progn
                       ;; A full encoding quantum.  Load digits.
                       ,@(iter (repeat digit/full-quantum)
                               (for index :from 0)
                               (for pos :from (- bit/full-quantum bit/digit) :by (- bit/digit))
                               (collecting `(setf (ldb (byte ,bit/digit ,pos) int) (aref octets ,index))))
                       ;; Output decoded octets.
                       ,@(iter (repeat byte/full-quantum)
                               (for pos :from (- bit/full-quantum 8) :by -8)
                               (collecting `(write-byte (ldb (byte 8 ,pos) int) output))))
                   (progn
                     ;; Remaining encoding quantum.
                     ,(let (expected-length)
                        `(case len
                           (0)
                           ,@(iter (for bit/quantum :from 8 :below bit/full-quantum :by 8)
                                   (for digit/quantum = (ceiling bit/quantum bit/digit))
                                   (for byte/quantum = (/ bit/quantum 8))
                                   (collecting
                                     ;; The ‘case’ clause.
                                     `(,digit/quantum
                                       ;; Clear encoding quantum.
                                       (setf int 0)
                                       ;; Load digits.
                                       ,@(iter (repeat digit/quantum)
                                               (for index :from 0)
                                               (for pos :from (- bit/full-quantum bit/digit) :by (- bit/digit))
                                               (collecting `(setf (ldb (byte ,bit/digit ,pos) int) (aref octets ,index))))
                                       ;; Sanity check.
                                       (unless (zerop (ldb (byte ,(- bit/full-quantum bit/quantum) 0) int))
                                         (invalid-last-digit input (char *alphabet* (aref octets ,(1- digit/quantum)))))
                                       ;; Output decoded octets.
                                       ,@(iter (repeat byte/quantum)
                                               (for pos :from (- bit/full-quantum 8) :by -8)
                                               (collecting `(write-byte (ldb (byte 8 ,pos) int) output)))))
                                   (push digit/quantum expected-length))
                           (t (invalid-sequence-length input len ',(nreverse expected-length)))))
                     ;; Done.
                     (when (and *ignore-whitespace* (plusp len))
                       (skip-whitespace input))
                     (when pad
                       ;; Ensure end of file.
                       (let ((char (peek-char nil input nil nil)))
                         (when (and char (not *junk-allowed*))
                           (unexpected-character input char))))
                     (leave))))))))
  ())

(define-decoder decode64 (24 6)
  "Generic base 64 decoding.")

(define-decoder decode32 (40 5)
  "Generic base 32 decoding.")

(define-decoder decode16 (8 4)
  "Generic base 16 decoding.")

(define-decoder decode8 (24 3)
  "Generic base 8 decoding.")

(define-decoder decode4 (8 2)
  "Generic base 4 decoding.")

(define-decoder decode2 (8 1)
  "Generic base 2 decoding.")

(defun %decod2 (fun output source)
  "Second stage – divert on input."
  (etypecase source
    (stream
     (let ((input source))
       (funcall fun output input)))
    (string
     (with-input-from-string (input source)
       (funcall fun output input)))
    (pathname
     (with-open-file (input source :element-type 'character
                                   :external-format (uiop:encoding-external-format :utf-8))
       (funcall fun output input)))
    ((eql t)
     (let ((input *standard-input*))
       (funcall fun output input))))
  ;; Return value.
  nil)

(defun %decod1 (fun destination input result-type)
  "First stage – divert on output."
  (etypecase destination
    (stream
     (let ((output destination))
       (%decod2 fun output input)))
    (string
     (with-output-to-string (stream destination)
       (write-sequence (babel:octets-to-string
                        (with-output-to-sequence (output :element-type 'octet)
                          (%decod2 fun output input))
                        :errorp t :encoding :utf-8)
                       stream)))
    (pathname
     (with-open-file (output destination :direction :output
                                         :element-type 'octet
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
       (%decod2 fun output input)))
    ((eql t)
     (let ((output *standard-output*))
       (%decod2 fun output input)))
    (null
     (ecase (or result-type 'vector)
       (string
        (babel:octets-to-string
         (with-output-to-sequence (output :element-type 'octet)
           (%decod2 fun output input))
         :errorp t :encoding :utf-8))
       (vector
        (with-output-to-sequence (output :element-type 'octet)
          (%decod2 fun output input)))
       (list
        (with-output-to-sequence (output :element-type 'octet :as-list t)
          (%decod2 fun output input)))))))

(defun basen-decode (destination source
                     &key
                       (base 32)
                       (alphabet standard-alphabet)
                       (pad-character standard-pad-character)
                       case-fold
                       junk-allowed
                       result-type)
  "Generic base N decoding.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
Keyword argument BASE is the radix used for the decoding.  Valid
 values are 64, 32, 16, 8, 4, or 2.  Default is 32.
Keyword argument ALPHABET is the alphabet for the decoding.  Value
 has to be a string with at least BASE characters.  Default are the
 decimal digits ‘0’ to ‘9’ and the letters ‘A’ to ‘Z’.
Keyword argument PAD-CHARACTER is the pad character.  Value has to
 be a character.  Default is the ‘=’ (equals sign) character.
If keyword argument CASE-FOLD is true, ignore differences in case
 when looking up characters in the alphabet.  Disabled by default.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (check-type base (member 64 32 16 8 4 2))
  (check-type alphabet simple-string)
  (check-type pad-character character)
  (when (< (length alphabet) base)
    (error "Alphabet is too small"))
  (let ((*alphabet* alphabet)
        (*pad-character* pad-character)
        (*char-equal* (if case-fold #'char-equal #'char=))
        (*junk-allowed* (not (null junk-allowed))))
    (%decod1 (ecase base
               (64 #'decode64)
               (32 #'decode32)
               (16 #'decode16)
               (8 #'decode8)
               (4 #'decode4)
               (2 #'decode2))
             destination source result-type)))

;;; RFC 4648 – The Base16, Base32, and Base64 Data Encodings

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

(defun rfc4648-base64-decode (destination source &rest options &key junk-allowed result-type)
  "Base 64 decoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (declare (ignore junk-allowed result-type))
  (apply #'basen-decode destination source
         :base 64
         :alphabet rfc4648-base64-alphabet
         :pad-character rfc4648-pad-character
         options))

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

(defun rfc4648-base64url-decode (destination source &rest options &key junk-allowed result-type)
  "Base 64 decoding as per RFC 4648 but with the URL safe alphabet.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (declare (ignore junk-allowed result-type))
  (apply #'basen-decode destination source
         :base 64
         :alphabet rfc4648-base64url-alphabet
         :pad-character rfc4648-pad-character
         options))

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

(defun rfc4648-base32-decode (destination source &rest options &key junk-allowed result-type)
  "Base 32 decoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (declare (ignore junk-allowed result-type))
  (apply #'basen-decode destination source
         :base 32
         :alphabet rfc4648-base32-alphabet
         :pad-character rfc4648-pad-character
         options))

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

(defun rfc4648-base32hex-decode (destination source &rest options &key junk-allowed result-type)
  "Base 32 decoding as per RFC 4648 but with the standard alphabet.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (declare (ignore junk-allowed result-type))
  (apply #'basen-decode destination source
         :base 32
         :alphabet standard-alphabet
         :pad-character rfc4648-pad-character
         options))

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

(defun rfc4648-base16-decode (destination source &rest options &key junk-allowed result-type)
  "Base 16 decoding as per RFC 4648.

First argument DESTINATION is the output object.  Value is
 either a stream or a pathname.  The special value ‘t’ is equal
 to ‘*standard-output*’ and ‘nil’ means to return a sequence.
Second argument SOURCE is the input object.  Value is either a
 stream, a string, or a pathname.  The special value ‘t’ is equal
 to ‘*standard-input*’.
If keyword argument JUNK-ALLOWED is true, do not signal an error of
 type ‘decoding-error’ if more input is available after the encoded
 data.  Disabled by default.
Keyword argument RESULT-TYPE specifies the sequence type of the return
 value if DESTINATION is ‘nil’.  Value is either ‘string’, ‘vector’,
 or ‘list’.  Default is to return a vector of octets.

If DESTINATION is a stream, a string, a pathname, or ‘t’, then the
result is ‘nil’.  Otherwise, the result is a sequence containing the
output.  If the output object designates a string, the decoded input
is interpreted as a stream of UTF-8 encoded characters."
  (declare (ignore junk-allowed result-type))
  (apply #'basen-decode destination source
         :base 16
         :alphabet standard-alphabet
         :pad-character rfc4648-pad-character
         options))

;;; RFC 1421 – Privacy Enhancement for Internet Electronic Mail

(defun rfc1421-base64-encode (destination source)
  "Base 64 encoding as per RFC 1421.
Also known as PEM printable encoding.  Utilizes the base 64 alphabet
of RFC 4648 with padding and a maximum line length of 64 characters.

See the ‘basen-encode’ function for a description of the arguments
and return values.  The ‘rfc1421-base64-encode’ function implicitly
binds ‘*line-length*’ to 64 and changes the default line separator
to ‘:crlf’."
  (let ((*line-length* 64)
        (*default-line-separator* crlf))
    (rfc4648-base64-encode destination source :pad t)))

(defun rfc1421-base64-decode (destination source &rest options &key junk-allowed result-type)
  "Base 64 decoding as per RFC 1421.
Also known as PEM printable encoding.  Utilizes the base 64 alphabet
of RFC 4648 with padding and a maximum line length of 64 characters.

See the ‘basen-decode’ function for a description of the arguments
and return values.  The ‘rfc1421-base64-decode’ function implicitly
binds ‘*ignore-whitespace*’ to true."
  (declare (ignore junk-allowed result-type))
  (let ((*ignore-whitespace* t))
    (apply #'rfc4648-base64-decode destination source options)))

;;; RFC 2045 – Multipurpose Internet Mail Extensions

(defun rfc2045-base64-encode (destination source)
  "Base 64 encoding as per RFC 2045.
Also known as MIME base 64 content transfer encoding.  Utilizes the
base 64 alphabet of RFC 4648 with padding and a maximum line length
of 76 characters.

See the ‘basen-encode’ function for a description of the arguments
and return values.  The ‘rfc2045-base64-encode’ function implicitly
binds ‘*line-length*’ to 76 and changes the default line separator
to ‘:crlf’."
  (let ((*line-length* 76)
        (*default-line-separator* crlf))
    (rfc4648-base64-encode destination source :pad t)))

(defun rfc2045-base64-decode (destination source &rest options &key junk-allowed result-type)
  "Base 64 decoding as per RFC 2045.
Also known as MIME base 64 content transfer encoding.  Utilizes the
base 64 alphabet of RFC 4648 with padding and a maximum line length
of 76 characters.

See the ‘basen-decode’ function for a description of the arguments
and return values.  The ‘rfc2045-base64-decode’ function implicitly
binds ‘*ignore-whitespace*’ to true."
  (declare (ignore junk-allowed result-type))
  (let ((*ignore-whitespace* t))
    (apply #'rfc4648-base64-decode destination source options)))

;;; basen.lisp ends here
