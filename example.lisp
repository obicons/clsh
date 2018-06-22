(in-package :clsh-user)

(in-readtable :clsh-syntax)

;; lists the contents of the directory and outputs to *standard-output*
(with-programs ("ls")
  (to *standard-output* (ls)))

;; echoes the HOSTNAME environment variable
(with-programs ("echo")
  (to *standard-output*
      (echo :arguments (list $HOSTNAME))))

;; lists the contents of the directory and finds contents containing "bin"
;; notice in a pipe context, you dont specify arguments as a list
(with-programs ("ls" "grep")
  (to *standard-output*
      (pipe ls
            (grep "bin"))))

;; creates an environment variable named MY_ENV_VAR set to "foo"
(setf $MY_ENV_VAR "foo")
(with-programs ("echo")
  (to *standard-output*
      (echo :arguments (list $MY_ENV_VAR))))
