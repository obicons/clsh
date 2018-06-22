(defpackage :unix-streams
  (:use :cffi
        :cl
        :trivial-gray-streams)
  (:export lisp-open
           make-unix-io-stream
           unix-close
           unix-input-stream
           unix-output-stream
           unix-stream-file-descriptor
           with-file-descriptor-as-ostream))

(defpackage :utils
  (:use :cffi
        :cl
        :osicat)
  (:export check-string-list
           environment-simple-list
           free-array-strings
           string-as-symbol
           string-list-to-array))

(defpackage :clexec
  (:use :bordeaux-threads
        :cffi
        :cl
        :osicat
        :split-sequence
        :unix-streams
        :utils)
  (:export childp
           execute-program
           lisp-execve
           make-dup-handle
           parentp
           pipe
           pipe-lines-to-fn
           search-for-program
           to
           unix-fork
           with-programs))

(defpackage :clsh-addons
  (:use :cl
        :named-readtables
        :osicat
        :utils)
  (:export enable-var-reader))

(defpackage :clsh-user
  (:use :cl
        :clsh-addons
        :clexec
        :named-readtables
        :osicat))

(defpackage :clsh-tests
  (:use :cl
        :clexec
        :prove
        :unix-streams))
