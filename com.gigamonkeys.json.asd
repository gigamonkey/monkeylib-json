;;; Copyright (c) 2009-2012, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem com.gigamonkeys.json
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Library for reading and writing JSON-formatted data."
  :components
  ((:file "packages")
   (:file "json"               :depends-on ("packages"))
   (:file "json-builder"       :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.parser
   :com.gigamonkeys.utilities))
