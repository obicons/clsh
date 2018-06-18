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
  :depends-on ("cffi"
               "split-sequence"
               "trivial-gray-streams"
               "osicat")
  :components ((:file "defpackage")
               (:file "utils")
               (:file "unix-streams")
               (:file "clexec")))
