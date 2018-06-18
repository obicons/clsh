(in-package :utils)

(defun string-list-to-array (string-list)
  (if string-list
    (let ((array (foreign-alloc :pointer :count (1+ (length string-list)))))
      (dotimes (i (length string-list))
        (setf (mem-aref array :pointer i)
              (foreign-string-alloc (nth i string-list))))
      (setf (mem-aref array :pointer (length string-list)) (null-pointer))
      array)
    (null-pointer)))

(defun free-array-strings (array-strings length)
  "frees a char** of length `length`"
  (dotimes (i length (foreign-free array-strings))
    (foreign-string-free (mem-aref array-strings :pointer i))))

(defun string-as-symbol (string)
  "returns string read as a symbol"
  (check-type string string)
  (with-input-from-string (stream string)
    (read stream)))

(defun environment-simple-list ()
  "returns the environment variable as a simple list"
  (loop for (k . v) in (environment)
        collecting (concatenate 'string k "=" v)))

(defun check-string-list (string-list)
  (check-type string-list list)
  (dolist (x string-list)
    (check-type x string)))

