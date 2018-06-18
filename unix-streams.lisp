(in-package :unix-streams)

(defclass unix-stream () ())

(defclass unix-output-stream (unix-stream
                              fundamental-character-output-stream)
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

(defclass unix-input-stream (unix-stream
                             fundamental-character-output-stream)
  ((file-descriptor :initarg :file-descriptor
                    :accessor unix-stream-file-descriptor)))

(defmethod stream-input-type ((stream unix-input-stream))
  'standard-char)

(defmethod output-stream-p ((stream unix-input-stream))
  nil)

(defmethod input-stream-p ((stream unix-input-stream))
  t)

(defcfun ("read" unix-read) :long
  "reads up to count bytes into buf from fd"
  (fd :int)
  (buf :pointer)
  (count :long))

(defun lisp-read-char (fd)
  (with-foreign-object (char :char)
    (let ((read-result (unix-read fd char 1)))
      (if (zerop read-result)
          :eof
          (char (foreign-string-to-lisp char :count 1) 0)))))

(defmethod stream-read-char ((stream unix-input-stream))
  (lisp-read-char (unix-stream-file-descriptor stream)))

(defmethod stream-read-line ((stream unix-input-stream))
  (let ((char-buffer (make-array 0
                                 :element-type 'character
                                 :adjustable t
                                 :fill-pointer 0)))
    (loop for x = (read-char stream nil :eof)
          if (eq x :eof)
            do (return (values (coerce char-buffer 'string) t))
          else if (eql x #\newline)
            do (return (values (coerce char-buffer 'string) nil))
          else do (vector-push-extend x char-buffer))))

(defmacro with-file-descriptor-as-istream ((stream-name fd) &rest body)
  `(let ((,stream-name (make-instance 'unix-input-stream
                                      :file-descriptor ,fd)))
     ,@body))

;;; NOTE - this section is just here for testing! DO NOT USE ELSEWHERE!

(defconstant +read-only+ 0)
(defconstant +write-only+ 1)
(defconstant +read-write+ 2)

(defcfun ("open" unix-open) :int
  "Opens pathname with flags"
  (pathname :string)
  (flags :int))

(defun lisp-open (path flags)
  (let ((fd (unix-open path flags)))
    (if (< fd 0)
        (error "Error opening ~S" path) fd)))

(defcfun ("close" unix-close) :int
  "closes fd"
  (fd :int))

(defmacro with-open-fd ((name path mode) &rest body)
  (let ((body-result-var (gensym "body-result")))
    `(let* ((,name (lisp-open ,path ,mode))
            (,body-result-var (progn ,@body)))
       (unix-close ,name)
       ,body-result-var)))

;;; END TEST SUPPORT SECTION

(defun make-unix-io-stream (input-fd output-fd)
  (make-two-way-stream
   (make-instance 'unix-input-stream :file-descriptor input-fd)
   (make-instance 'unix-output-stream :file-descriptor output-fd)))
