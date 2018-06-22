(defpackage :clsh-tests
  (:use :cl
        :clexec
        :prove
        :unix-streams))

(in-package :clsh-tests)

(subtest "Testing unix-streams"
         (plan 8)

         (setf *file-fd* (lisp-open "clsh:test" '(:create :output)))

         (ok (and (integerp *file-fd*) (not (zerop *file-fd*)))
             "File opened with create and output mode")

         (setf *file-output-stream*
           (make-instance 'unix-output-stream :file-descriptor *file-fd*))

         (is (format *file-output-stream* "hello world~%") nil
             "`format` call looks normal")

         (is (unix-close *file-fd*) 0
             "`unix-close` call looks normal")

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
