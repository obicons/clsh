(in-package :clsh-addons)

(defvar *dispatch-table* (make-hash-table :test 'equal))

(defun enable-var-reader ()
  (let ((readtable (copy-readtable)))
  (set-macro-character #\$
                       #'(lambda (stream char)
                           (declare (ignore char))
                           `(environment-variable
                             ,(let ((*readtable*
                                      (copy-readtable readtable)))
                                (setf (readtable-case *readtable*)
                                      :preserve)
                                (SYMBOL-NAME (READ STREAM))))))))

(defun enable-carrot-reader ()
  (set-macro-character #\^
                       #'(lambda (stream char)
                           (declare (ignore char))
                           (let* ((next (read-char stream))
                                  (dispatch-fn (gethash next
                                                        *dispatch-table*)))
                             (unless dispatch-fn
                               (error "Error: no macro defined for ~s"
                                      next))
                             (funcall dispatch-fn next stream)))))

(defun set-carrot-macro (char macro-fn)
  (setf (gethash char *dispatch-table*) macro-fn))

