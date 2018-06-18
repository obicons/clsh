(in-package :clexec)

(defcvar "errno" :int)

(defcfun ("fork" unix-fork) :long
  "forks a process")

(defcfun ("pipe" unix-pipe) :int
  "pipefd is a int[2]. pipefd[0] is the read-end, pipefd[1] is write-end"
  (pipefd :pointer))

(defun lisp-pipe ()
  (with-foreign-object (pipe-array :int 2)
    (when (not (zerop (unix-pipe pipe-array)))
      (error "FATAL ERROR! Unable to create pipe @ lisp-pipe"))
    (values (translate-from-foreign (mem-aref pipe-array :int 0) :int)
            (translate-from-foreign (mem-aref pipe-array :int 1) :int))))

(defcfun ("execve" unix-execve) :int
  "filename, argv, envp"
  (filename :string)
  (argv :pointer)
  (envp :pointer))

(defcfun "dup2" :int
  (oldfd :int)
  (newfd :int))

(defun parentp (pid)
  "returns T when a pid obtained from fork indicates this is the parent"
  (not (zerop pid)))

(defun childp (pid)
  "returns T when a pid obtained from fork indicates this is the child"
  (zerop pid))

(defun lisp-execve (filename arguments environment)
  "Executes execve. 
   Filename must be an absolute path to an executable.
   Arguments are the executable's argv.
   Environment is the environment list for the program."
  (let ((argv (string-list-to-array arguments))
        (envp (string-list-to-array environment)))
    (unix-execve filename argv envp)))

(defun path-search-list ()
  "returns the PATH environment variable string"
  (environment-variable "PATH"))

(defun search-for-program-in-directory (program-name directory)
  (check-type directory string)
  (loop for file in (directory (concatenate 'string
                                            directory
                                            "/*"))
        when (string= (file-namestring file) program-name)
        do (return (truename file))))

(defun search-for-program (program-name)
  "returns the absolute path to program-name or nil"
  (let ((paths (split-sequence #\: (path-search-list))))
    (if (probe-file program-name)
        program-name
      (loop for path in paths
            for maybe-program = (search-for-program-in-directory
                                 program-name path)
            when maybe-program do (return maybe-program)))))

(defun execute-program (name &key arguments environment execute-thunk)
  (let* ((program-path (namestring
                        (or (search-for-program name)
                            (error "Cannot find ~S in PATH" name))))
         (pid (unix-fork)))
    (cond ((parentp pid) pid)
          ((childp pid)
           (when execute-thunk
             (funcall execute-thunk))
           (lisp-execve program-path
                        (cons program-path arguments)
                        environment)))))

(defun execute-as-shell (name &rest arguments )
  (multiple-value-bind (stdin stdout) (lisp-pipe)
    (execute-program name
                     :arguments arguments
                     :environment (environment-simple-list)
                     :execute-thunk
                     #'(lambda ()
                         (dup2 stdin 0)
                         (dup2 stdout 1)
                         (unix-close stdin)
                         (unix-close stdout)))
    (values stdin stdout)))

(defun make-dup-handle (input-fd output-fd)
  #'(lambda ()
      (when (not (= input-fd 0))
        (dup2 input-fd 0)
        (unix-close input-fd))
      (when (not (= output-fd 1))
        (dup2 output-fd 1)
        (unix-close output-fd))))

(defmacro with-programs (program-list &rest body)
  (let ((input-stream-var (gensym "input-stream"))
        (read-end-var (gensym "read-end"))
        (write-end-var (gensym "write-end")))
    `(let ((,input-stream-var (make-instance 'unix-input-stream
                                             :file-descriptor 0)))
       (labels
           ,(loop for program in program-list
                  collecting
                  (list (string-as-symbol program)
                        `(&key (stdin ,input-stream-var)
                               arguments)
                        `(multiple-value-bind (,read-end-var ,write-end-var)
                             (lisp-pipe)
                           (check-string-list arguments)
                           (execute-program ,program
                                            :arguments arguments
                                            :environment
                                            (environment-simple-list)
                                            :execute-thunk
                                            (make-dup-handle
                                             (unix-stream-file-descriptor
                                              stdin)
                                             ,write-end-var))
                           (unix-close ,write-end-var)
                           (make-instance 'unix-input-stream
                                          :file-descriptor ,read-end-var))))
         ,@body))))

(defun to (destination-stream source-stream &optional (close-on-eof t))
  "Directs the output of stream-b to stream-a, closing source-stream
   on end of file when close-on-eof is enabled (default)."
  (loop for line = (read-line source-stream nil)
        while line
        do (format destination-stream "~a~%" line)
        finally
        (when close-on-eof
          (unix-close (unix-stream-file-descriptor source-stream)))))

(defmacro pipe-helper (previous-form-result &rest forms)
  (cond ((null forms) previous-form-result)
        ((consp (car forms))
         `(pipe-helper ,(append (list (caar forms)
                                      :arguments (list 'quote (cdar forms)))
                                (list :stdin previous-form-result))
                       ,@(cdr forms)))
        ((atom (car forms))
         `(pipe-helper ,(append (list (car forms))
                                (list :stdin previous-form-result))
                       ,@(cdr forms)))))

(defmacro pipe (&rest forms)
  "Pipes stdout of the first form to stdin of the next form.
   The first form can be either an atom or a function-call."
  (cond ((consp (car forms))
         `(pipe-helper ,(list (caar forms)
                              :arguments (list 'quote (cdar forms)))
                       ,@(cdr forms)))
        ((atom (car forms))
         `(pipe-helper ,(list (car forms)) ,@(cdr forms)))))
