(in-package :asdf-user)
(defsystem "canim"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("imago")
  :components ((:module "src"
                :components
		((:file "package")
		 (:file "pos" :depends-on ("package"))
		 (:file "julia" :depends-on ("pos"))
		 (:file "main" :depends-on ("julia")))))
  :description "A lib for creating images and animations of complex functions - complex animations (canim)"
  :in-order-to ((test-op (test-op "canim/tests"))))


(defsystem "canim/tests"
  :author ""
  :license ""
  :depends-on ("canim"
               "rove")
  :components ((:module "tests"
                :components
		((:file "main"))))
  :description "Test system for canim"
  :perform (test-op (op c) (symbol-call :rove :run c)))
