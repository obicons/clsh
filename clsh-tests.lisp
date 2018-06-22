(defpackage :clsh-tests
  (:use :cl
        :clexec
        :prove
        :unix-streams))

(in-package :clsh-tests)

(defparameter *file-fd* nil)
(defparameter *file-output-stream* nil)
(defparameter *file-input-stream* nil)

(subtest "Testing unix-streams"
         (plan 9)

         (setf *file-fd* (lisp-open "clsh:test" '(:create :output)))

         (ok (and (integerp *file-fd*) (not (zerop *file-fd*)))
             "File opened with create and output mode")

         (setf *file-output-stream*
           (make-instance 'unix-output-stream :file-descriptor *file-fd*))

         (is (format *file-output-stream* "hello world~%") nil
             "`format` succeeds on existing file")

         (is (unix-close *file-fd*) 0
             "`unix-close` call works on open file descriptor")

         (is-error (format *file-output-stream* "hello again~%")
                   'unix-stream-error
                   "`format` call fails on closed file descriptor")

         (setf *file-fd* (lisp-open "clsh:test" '(:input)))

         (ok (and (integerp *file-fd*) (not (zerop *file-fd*)))
             "File opened with input mode")

         (setf *file-input-stream*
               (make-instance 'unix-input-stream :file-descriptor *file-fd*))

         (is (read-line *file-input-stream* nil) "hello world" :test #'string=)

         (is (read-line *file-input-stream* nil :eof) :eof)

         (is (unix-close *file-fd*) 0
             "`unix-close` call looks normal")

         (delete-file "clsh:test")
         
         (is-error (lisp-open "clsh:test" nil) 'simple-error)

         (finalize))

(subtest "Testing clexec"
  (plan 5)

  (is (childp 0) t
      "a pid of 0 is a child's")

  (is (childp 1) nil
      "a pid not 0 is not a child's")

  (is (parentp 0) nil
      "a pid of 0 is not a parent's")

  (is (parentp 1) t
      "a pid of not 0 is a parent's")

  ;; `echo` should be findable in $PATH
  (isnt (search-for-program "echo") nil)
  (finalize))
