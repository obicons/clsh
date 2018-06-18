(defpackage :unix-streams
  (:use cffi
        cl
        trivial-gray-streams)
  (:export make-unix-io-stream
           unix-close
           unix-input-stream
           unix-output-stream
           unix-stream-file-descriptor
           with-file-descriptor-as-ostream))

(defpackage :utils
  (:use cffi
        cl
        osicat)
  (:export environment-simple-list
           free-array-strings
           string-as-symbol
           string-list-to-array))

(defpackage :clexec
  (:use cffi
        cl
        osicat
        split-sequence
        unix-streams
        utils)
  (:export childp
           execute-program
           lisp-execve
           parentp
           search-for-program
           unix-fork))
   
