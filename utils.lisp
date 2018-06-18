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

(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))
