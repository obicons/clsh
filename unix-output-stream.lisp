(in-package :unix-streams)

(defclass unix-output-stream (fundamental-character-output-stream)
  ((file-descriptor :initarg :file-descriptor
                    :accessor unix-stream-file-descriptor)))

(defmethod stream-output-type ((stream unix-output-stream))
  'standard-char)

(defmethod output-stream-p ((stream unix-output-stream))
  t)

(defmethod input-stream-p ((stream unix-output-stream))
  nil)

(defcfun ("write" unix-write) :long
    "writes count bytes of buf to a fd"
  (fd :int)
  (buf :pointer)
  (count :long))

(defun lisp-write-string (fd str)
  (with-foreign-string (c-str str)
    (unix-write fd c-str (length str))))

(defmethod stream-write-char ((stream unix-output-stream) char)
  (lisp-write-string (unix-stream-file-descriptor stream) (string char)))

(defmethod stream-write-string ((stream unix-output-stream) string
                                &optional (start 0) (end (length string)))
  (lisp-write-string (unix-stream-file-descriptor stream)
                     (subseq string start end)))

(defmacro with-file-descriptor-as-ostream ((stream-name fd) &rest body)
  `(let ((,stream-name (make-instance 'unix-output-stream
                                      :file-descriptor ,fd)))
     ,@body))

(with-file-descriptor-as-ostream (test 1)
  (format test "foo~%"))
