(in-package #:cl-user)

(defpackage #:markout-asdf
  (:use #:cl #:asdf))

(in-package #:markout-asdf)

(defsystem #:markout
  :description "A tree based HTML markup output system"
  :author "Svante von Erichsen <svante.v.erichsen@web.de>"
  :license "CC0"
  :serial t
  :components ((:file "package")
               (:file "markout"))
  :in-order-to ((test-op (test-op #:markout-test))))

(defsystem #:markout-test
  :perform (test-op (o c) 'todo))
