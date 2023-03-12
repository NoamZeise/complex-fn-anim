(in-package :asdf-user)
(defsystem "cl-images"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("imago")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-images/tests"))))


(defsystem "cl-images/tests"
  :author ""
  :license ""
  :depends-on ("cl-images"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-images"
  :perform (test-op (op c) (symbol-call :rove :run c)))
