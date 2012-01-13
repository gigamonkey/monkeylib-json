(in-package :com.gigamonkeys.json)

;;; JSON-EXPS:
;;;
;;; strings  => strings, e.g. "foo" => "foo"
;;; numbers  => numbers, e.g. 10 => 10
;;; t, :true => true
;;; keywords => lowercase name.
;;; :false   => false
;;; :null    => null
;;; list     => {} with keys and values taken pairwise from list. (Empty list, i.e. nil, is empty object)
;;; vector   => [] with elements taken from vector.

(defun write-json (data &optional (stream *standard-output*))
  "Write DATA to STREAM in JSON format."
  (emit-json data stream)
  nil)

(defun json (data)
  "The top-level function for converting Lisp objects into a string in
the JSON format. It can convert any object that can be converted to a
json-exp via the to-json generic function."
  (with-output-to-string (out)
    (emit-json (to-json data) out)))

(defgeneric to-json (thing)
  (:documentation "Generic function that can convert an arbitrary Lisp
  object to a json-exp, i.e. a sexp that can then be rendered as json.
  To make an arbitrary class convertable to JSON, add a method to this
  generic function that generates a json-exp."))

(defgeneric json-stringify (object)
  (:documentation "Convert object directly to a JSON representation as
  a string. Default methods are provided for strings, symbols (which
  must be keywords), and numbers but there may be situations where it
  is appropriate to define new methods on this function. In general,
  however, it is probably better to define a method on to-json to
  convert the object to a sexp that can be rendered as JSON."))


(defgeneric emit-json (object stream)
  (:documentation "Emit object to stream as JSON."))

(defmethod emit-json ((object t) stream)
  (emit-json (to-json object) stream))

(defmethod emit-json ((object (eql nil)) stream)
  (write-string "{}" stream))

(defmethod emit-json ((object cons) stream)
  (write-char #\{ stream)
  (loop for (key value . rest) on object by #'cddr do
       (emit-json (json-stringify key) stream)
       (write-char #\: stream)
       (emit-json (to-json value) stream)
       when rest do (write-char #\, stream))
  (write-char #\} stream))

(defmethod emit-json ((object vector) stream)
  (write-char #\[ stream)
  (loop with len = (length object)
     for i from 0 below len
     do (emit-json (to-json (aref object i)) stream)
     when (< i (1- len)) do (write-char #\, stream))
  (write-char #\] stream))

(defmethod emit-json ((object symbol) stream)
  (write-string (json-stringify object) stream))

(defmethod emit-json ((object string) stream)
  (write-char #\" stream)
  (loop for char across object do (emit-json-char char stream))
  (write-char #\" stream))

(defun emit-json-char (char stream)
  (case char
    (#\" (write-string "\\\"" stream))
    (#\\ (write-string "\\\\" stream))
    #+(or)(#\/ (write-string "\\/" stream))
    (#.(code-char 8) (write-string "\\b" stream))
    (#.(code-char 9) (write-string "\\t" stream))
    (#.(code-char 10) (write-string "\\n" stream))
    (#.(code-char 12) (write-string "\\f" stream))
    (#.(code-char 13) (write-string "\\r" stream))
    (t
     (cond
       ((<= 0 (char-code char) #x1f)
        (format stream "\\u~4,'0x" (char-code char)))
       (t (write-char char stream))))))

(defmethod emit-json ((object number) stream)
  (write-string (json-stringify object) stream))

(defmethod emit-json ((object (eql t)) stream)
  (emit-json :true stream))

(defmethod json-stringify ((object t)) (error "Can't stringify ~a" object))

(defmethod json-stringify ((object string)) object)

(defmethod json-stringify ((object symbol))
  (unless (keywordp object)
    (error "Only keywords allowed in JSON-EXPs. Got ~a in package ~a"
           object (package-name (symbol-package object))))
  (string-downcase (symbol-name object)))

(defmethod json-stringify ((object number))
  (let ((*read-default-float-format* 'double-float))
    (let ((float (float object 0d0)))
      (if (= (round float) float)
          (prin1-to-string (round float))
          (prin1-to-string float)))))

(defmethod to-json ((thing t)) thing)

(defmethod to-json ((thing hash-table))
  (loop for k being the hash-keys of thing using (hash-value v) collect k collect v))
