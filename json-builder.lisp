(in-package :com.gigamonkeys.json.parser)

;; Parser for JSON syntax (<http://www.json.org/>)

(defvar *object-type* :vector)

(defvar *empty-object* (make-symbol "EMPTY-OBJECT"))

(defchartype string-char '(not (member #\\ #\")))

(defchartype digit1-9
  '(member #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defchartype digit
  '(or (eql #\0) digit1-9))

(defchartype hex
  '(or digit (member #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)))

(defprod ws ()
  (* (/ #\Space #\Tab #\Newline)))

;; API for building objects. Currently internal only and implemented
;; to produce either a plist or a hash-table.

(defgeneric make-object (type)
  (:documentation "Make a new object for a given named type."))

(defgeneric save-key-value (key-value-pair object)
  (:documentation "Save the key and value in the data structure returned by MAKE-OBJECT."))

(defgeneric finish-object (object)
  (:documentation "Return the actual object that will be put into the data structure."))

;; vectors as objects (returned as a plist)

(defmethod make-object ((type (eql :vector)))
  (make-array 5 :adjustable t :fill-pointer 0))

(defmethod save-key-value (key-value-pair (object vector))
  (destructuring-bind (key . value) key-value-pair
    (vector-push-extend key object)
    (vector-push-extend value object)))

(defmethod finish-object ((object vector))
  (or (coerce object 'list) *empty-object*))

;; hash-tables as objects

(defmethod make-object ((type (eql :hash-table)))
  (make-hash-table :test #'equal))

(defmethod save-key-value (key-value-pair (object hash-table))
  (destructuring-bind (key . value) key-value-pair
    (setf (gethash key object) value)))

(defmethod finish-object ((object hash-table)) object)


;;; Main productions

(defprod object ()
  ((^ "{" (make-object *object-type*))
   ws
   (? (@ key-value-pair (save-key-value key-value-pair object)))
   (* ws "," ws (@ key-value-pair (save-key-value key-value-pair object)))
   ws (^ "}" (finish-object object))))

(defprod key-value-pair ()
  (^ (string ws ":" ws value) (cons string value)))

(defprod array ()
  ((^ "[" (make-array 5 :adjustable t :fill-pointer 0))
   ws
   (? (@ value (vector-push-extend value array)))
   (* ws "," ws (@ value (vector-push-extend value array)))
   ws "]"))

(defprod value ()
  (/ (^ string)
     (^ number)
     (^ object)
     (^ array)
     (^ "true" :true)
     (^ "false" :false)
     (^ "null" :null)))

(defprod xvalue ()
  (^ array (coerce array 'list)))


(defprod string ()
  ;; In JSON syntax, unlike full Javascript, only double-quoted strings are allowed.
  ((^ "\"" (make-array 10 :adjustable t :fill-pointer 0 :element-type 'character))
   (* (@ char-or-escape (vector-push-extend char-or-escape string)))
   "\""))

(defprod char-or-escape ()
  (^ (/ escape string-char)))

(defprod escape ()
  ("\\"
   (/ (^ "\"" #\")
      (^ "\\" #\\)
      (^ "/" #\/)
      (^ "b" #\Backspace)
      (^ "f" #\Page)
      (^ "n" #\Newline)
      (^ "r" #\Return)
      (^ "t" #\Tab)
      ("u" (^ hex4 (code-char (parse-integer hex4 :radix 16)))))))

(defprod hex4 () (hex hex hex hex))

(defprod number ()
  (^ number-syntax (let ((*read-default-float-format* 'double-float)) (read-from-string number-syntax))))

(defprod number-syntax ()
  (int (? (/ (frac (? exp)) exp))))

(defprod int ()
  ((? "-") (/ (digit1-9 (* digit)) "0")))

(defprod frac () ("." (* digit)))

(defprod exp () (e (* digit)))

(defprod e () ((/ "e" "E") (? (/ "-" "+"))))

(defparser json-parser (^ value))

(defun parse-json (text)
  "Parse json text into Lisp objects. Hash tables are used to
represent Javascript objects and vectors to represent arrays."
  (fix-empty-object (nth-value 1 (json-parser text))))

(defun fix-empty-object (json)
  (cond
    ((eql json *empty-object*) ())
    ((consp json) (mapcar #'fix-empty-object json))
    ((stringp json) json)
    ((vectorp json) (map 'vector #'fix-empty-object json))
    (t json)))

(defmacro tjp (production input)
  `((lambda (x)
      (parselet ((foo (^ ,production)))
        (foo x))) ,input))
