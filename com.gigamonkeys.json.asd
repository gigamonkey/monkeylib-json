;;
;; Copyright (c) 2009-2010, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.json
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :components
  ((:file "packages")
   (:file "json"               :depends-on ("packages"))
   (:file "json-builder"       :depends-on ("packages")))
  :depends-on 
  (:com.gigamonkeys.foo
   :com.gigamonkeys.pathnames
   :com.gigamonkeys.parser
   :com.gigamonkeys.utilities))



