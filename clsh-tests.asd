(defpackage :clsh-tests-asd
  (:use :cl
        :asdf))

(in-package :clsh-tests-asd)

(defsystem clsh-tests
  :depends-on ("clsh"
               "prove")
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "clsh-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
