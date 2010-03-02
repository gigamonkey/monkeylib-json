(in-package :com.gigamonkeys.json)

;; JSON: The top-level function for converting Lisp objects into a
;; string in the JSON format. It can convert any object that can be
;; converted to a json-exp.

;; TO-JSON: generic function for converting an arbitrary Lisp object
;; into a json-exp that the JSON function knows how to render into
;; JSON format. To make an arbitrary class convertable to JSON, add a
;; method to this generic function that generates a json-exp.

;; TO-JAVASCRIPT: Converts a json-exp into FOO Javascript language.
;; It's unlikely there'd be a need to 

(defun json (data)
  "This should only be used with sexps containing types that can
  be encoded in JSON syntax via the to-json generic function.
  By default this include hashtables, sequences, numbers,
  strings, NIL, T, and the keywords :true, :false, and :null."
  (with-output-to-string (out)
    (with-foo-output (out :pretty t)
      (let ((javascript (make-instance 'javascript)))
	(process 
	 javascript
	 (new-pretty-printer)
	 (to-javascript (to-json data))
	 (new-env 'statement-or-expression :expression (top-level-environment javascript)))))))

(defgeneric to-json (thing)
  (:documentation "Convert an arbitrary Lisp object to a json-exp.
  This method is probably the right thing for "))

(defmethod to-json ((thing t)) thing)

(defmethod to-json ((thing hash-table))
  (loop for k being the hash-keys of thing using (hash-value v) collect k collect v))

;;; This function converts a json-exp to the format understood by 

(defgeneric to-javascript (thing)
  (:documentation "Convert data to the an sexp that can be converted to Javascript code (i.e. JSON)."))

(defmethod to-javascript ((thing t))
  (error "JSON doesn't support encoding objects of class ~a." (class-name (class-of thing))))

(defmethod to-javascript ((thing string)) thing)

(defmethod to-javascript ((thing number)) thing)

(defmethod to-javascript ((thing (eql t))) (to-javascript :true))

(defmethod to-javascript ((thing symbol))
  (cond
    ((keywordp thing) (intern (string-downcase (symbol-name thing)) :keyword))
    (t (error "Only keyword symbols allowed in json-exps: ~s." thing))))

(defmethod to-javascript ((thing vector)) 
  `(array ,@(mapcar #'(lambda (x) (to-javascript (to-json x))) (coerce thing 'list))))

(defmethod to-javascript ((thing cons))
  `(object ,@(loop for (k v) on thing by #'cddr collect (intern (string-downcase k) :keyword) collect (to-javascript (to-json v)))))

(defmethod to-javascript ((thing (eql nil))) '(object))


