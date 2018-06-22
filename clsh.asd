(defpackage :clsh-asd
  (:use :cl
        :asdf))

(in-package :clsh-asd)

(defsystem clsh
  :name "clsh"
  :version "0.5.0"
  :maintainer "Max Taylor"
  :author "Max Taylor"
  :license "MIT"
  :description "Provides the ability to run and compose UNIX programs"
  :serial t
  :depends-on ("bordeaux-threads"
               "cffi"
               "named-readtables"
               "osicat"
               "prove"
               "split-sequence"
               "trivial-gray-streams")
  :components ((:file "defpackage")
               (:file "utils")
               (:file "unix-streams")
               (:file "clexec")
               (:file "clsh-addons"))
  :in-order-to ((test-op (test-op clsh-test))))

(defsystem clsh-test
  :depends-on ("clsh"
               "prove")
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "clsh-tests"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
