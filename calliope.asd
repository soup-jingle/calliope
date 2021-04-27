(defsystem "calliope"
  :version "0.1.0"
  :author "Patrick Bunetic"
  :license ""
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "calliope/tests"))))

(defsystem "calliope/tests"
  :author "Patrick Bunetic"
  :license ""
  :depends-on ("calliope"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for calliope"
  :perform (test-op (op c) (symbol-call :rove :run c)))
