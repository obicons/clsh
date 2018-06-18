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
  (getenv "PATH"))

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
                     :environment (sb-ext:posix-environ)
                     :execute-thunk
                     #'(lambda ()
                         (dup2 stdin 0)
                         (dup2 stdout 1)
                         (unix-close stdin)
                         (unix-close stdout)))
    (values stdin stdout)))
