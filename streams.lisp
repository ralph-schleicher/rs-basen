;;; streams.lisp --- binary streams

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

(defclass octet-input-stream (trivial-gray-stream-mixin fundamental-binary-input-stream)
  ())

(defclass octets-from-character-stream (octet-input-stream)
  ((stream
    :reader stream-of
    :initarg :stream
    :initform *standard-input*
    :documentation "The underlying character input stream.")
   (character-encoding
    :initarg :character-encoding
    :initform :utf-8
    :documentation "The Babel character encoding.")
   (byte-order-mark
    :initarg :byte-order-mark
    :initform :default
    :documentation "Whether or not to output a BOM.")
   (string
    :initform (make-array 20 :element-type 'babel:unicode-char :initial-element #\Space)
    :documentation "The input buffer.")
   (octets
    ;; Caveat: The output buffer must be large enough for any character
    ;; encoding, usually four times the size of the input buffer.  Also
    ;; consider the length of an optional BOM.
    :initform (make-array 84 :element-type 'octet :initial-element 0)
    :documentation "The output buffer.")
   (point
    :initform 0
    :documentation "The read position in the output buffer.")
   (mark
    :initform 0
    :documentation "The end bounding index of the output buffer."))
  (:documentation
   "Class for an octets from character stream.
An octets from character stream converts a character input stream
into a binary input stream.  When reading from such a stream, input
characters are decoded on the fly into a sequence of octets.  The
character encoding is defined when the octets from character stream
is created.

Keyword argument STREAM is the underlying character input stream.
Keyword argument CHARACTER-ENCODING is a Babel character encoding.
 See the ‘babel:list-character-encodings’ function.  The default
 is UTF-8.
If keyword argument BYTE-ORDER-MARK is true, consider adding the
 character encoding's BOM to the output.  Special value ‘:default’
 means to use the character encoding's default setting.

See also the ‘with-octets-from-string’ macro."))

(defmethod initialize-instance :after ((stream octets-from-character-stream) &key)
  (let ((encoding (or (slot-value stream 'character-encoding) :utf-8)))
    ;; Resolve the character encoding.
    (unless (typep encoding 'babel-encodings:character-encoding)
      (setf encoding (babel-encodings:get-character-encoding encoding))
      (setf (slot-value stream 'character-encoding) encoding))
    ;; Whether or not to actually output a BOM.
    (when (slot-value stream 'byte-order-mark)
      (setf (slot-value stream 'byte-order-mark)
            (and (plusp (length (babel-encodings:enc-bom-encoding encoding)))
                 (if (eq (slot-value stream 'byte-order-mark) :default)
                     (babel-encodings:enc-use-bom encoding)
                   t))))))

(defmacro with-octets-from-string ((var string &key index (start 0) end character-encoding (byte-order-mark :default)) &body body)
  "Read the decoded byte sequence of a string.
This macro is like ‘with-input-from-string’ except that the input
stream is a binary stream supplying octets; i.e. the characters of
the input string are decoded on the fly into a stream of octets.

First argument VAR is the variable binding the binary input stream.
 The ‘read-byte’ and ‘read-sequence’ function can be used to read
 octets from this stream.
Second argument STRING is the string being decoded.
Keyword arguments INDEX, START, and END have the same meaning as for
 the ‘with-input-from-string’ macro.
Keyword argument CHARACTER-ENCODING is a Babel character encoding.
 See the ‘babel:list-character-encodings’ function.  The default
 is UTF-8.
If keyword argument BYTE-ORDER-MARK is true, consider adding the
 character encoding's BOM to the output.  Special value ‘:default’
 means to use the character encoding's default setting.

Return value is the value of BODY."
  (let ((stream (gensym "STREAM")))
    `(with-input-from-string (,stream ,string :index ,index :start ,start :end ,end)
       (let ((,var (make-instance 'octets-from-character-stream
                                  :stream ,stream
                                  :character-encoding ,character-encoding
                                  :byte-order-mark ,byte-order-mark)))
         (declare (ignorable ,var))
         ,@body))))

(defsubst octets-from-character-stream-read-byte (stream)
  "Return either an octet or ‘:eof’."
  (declare (type octets-from-character-stream stream))
  (let ((octet :eof) buffer end)
    (loop
      (when (< (slot-value stream 'point) (slot-value stream 'mark))
        (setf octet (aref (slot-value stream 'octets) (slot-value stream 'point)))
        (incf (slot-value stream 'point))
        (return))
      ;; The output buffer is empty.  Attempt to read more
      ;; characters from the underlying input stream.
      (setf end (read-sequence (slot-value stream 'string) (slot-value stream 'stream)))
      (when (zerop end)
        (return))
      ;; Perform the character conversion.  Yes, that's ugly.
      ;; Babel should write directly into the output buffer.
      (setf buffer (babel:string-to-octets (slot-value stream 'string)
                                           :encoding (slot-value stream 'character-encoding)
                                           :use-bom (slot-value stream 'byte-order-mark)
                                           :errorp t
                                           :end end))
      (setf end (length buffer))
      (replace (slot-value stream 'octets) buffer :end1 end :end2 end)
      ;; Adjust the state.
      (setf (slot-value stream 'byte-order-mark) nil
            (slot-value stream 'point) 0
            (slot-value stream 'mark) end))
    ;; Return value.
    octet))

(defmethod close ((stream octets-from-character-stream) &key abort)
  (when (open-stream-p (stream-of stream))
    (close (stream-of stream) :abort abort)))

(defmethod open-stream-p ((stream octets-from-character-stream))
  (open-stream-p (stream-of stream)))

(defmethod stream-element-type ((stream octets-from-character-stream))
  (array-element-type (slot-value stream 'octets)))

(defmethod stream-read-byte ((stream octets-from-character-stream))
  (octets-from-character-stream-read-byte stream))

(defmethod stream-read-sequence ((stream octet-input-stream) (seq vector) start end &key)
  (iter (for pos :from start :below end)
        (for octet = (stream-read-byte stream))
        (until (eq octet :eof))
        (setf (aref seq pos) octet)
        (finally (return pos))))

(defmethod stream-read-sequence ((stream octet-input-stream) (seq list) start end &key)
  (iter (for pos :from start :below end)
        (for list :on (nthcdr start seq))
        (for octet = (stream-read-byte stream))
        (until (eq octet :eof))
        (setf (car list) octet)
        (finally (return pos))))

;;; streams.lisp ends here
