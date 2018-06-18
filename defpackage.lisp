(defpackage :unix-streams
  (:use cffi
        cl
        trivial-gray-streams)
  (:export make-unix-io-stream
           unix-close
           unix-input-stream
           unix-output-stream
           with-file-descriptor-as-ostream))

(defpackage :utils
  (:use cffi
        cl)
  (:export getenv
           free-array-strings
           string-list-to-array))

(defpackage :clexec
  (:use cffi
        cl
        split-sequence
        unix-streams
        utils)
  (:export childp
           execute-program
           lisp-execve
           parentp
           search-for-program
           unix-fork))
   
