(in-package :clsh-addons)

(defun read-env-variable (stream char)
  (declare (ignore char))
  `(environment-variable
    ,(let ((*readtable* (copy-readtable)))
       (setf (readtable-case *readtable*) :preserve)
       (SYMBOL-NAME (READ STREAM)))))


(defreadtable :clsh-syntax
  (:merge :standard)
  (:macro-char #\$ #'read-env-variable))
