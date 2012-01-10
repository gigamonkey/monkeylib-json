(in-package :cl-user)

(defpackage :com.gigamonkeys.json.parser
  (:use :cl :com.gigamonkeys.parser)
  (:shadow :string :exp)
  (:export :parse-json :*object-type*))

(defpackage :com.gigamonkeys.json
  (:use :cl
        :com.gigamonkeys.utilities
        :com.gigamonkeys.json.parser)
  (:export
   :json
   :parse-json
   :*object-type*
   :to-json
   :json-stringify))
