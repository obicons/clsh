# Clsh

Clsh is a set of Lispy bindings for running and composing *nix processes.

## Rationale

Common Lisp is amazing, but unfortunately sometimes libraries are not present that we need. More often than not, we have access to a command line tool that we can use to perform a given task.

## Example
```lisp
(in-package :clsh-user)

(enable-var-reader)

;; outputs "hello world"
(with-programs ("echo")
  (to *standard-output*
      (echo :arguments '("hello world"))))

;; outputs lines that match "bin"
(with-programs ("ls" "grep")
    (to *standard-output*
        (pipe ls
              (grep "bin"))))

;; outputs computer's hostname
(with-programs ("echo")
   (to *standard-output*
       (echo :arguments (list $HOSTNAME))))

;; creates an environment variable FOO set to "val"
(setf $FOO "val")

;; outputs "val"
(with-programs ("echo")
   (to *standard-output*
       (echo :arguments (list $FOO))))
```

For more example usage, see `example.lisp`

## CL Extensions
Common Lisp functions can arbitrarily be mixed with shell functions.
To do this, one might try something like:
```lisp
(use-package :unix-streams)

(defun my-outputter (&key stdin arguments)
  (declare (ignore arguments))
  (when (not stdin)
    (error "ERROR missing stdin in call to my-outputter"))
  (loop for x = (read-line stdin nil)
        while x
        do (format t "~a~%" x)
        finally (unix-close (unix-stream-file-descriptor stdin))))

(with-programs ("echo")
  (pipe (echo "hello world")  my-outputter))
```

This way, it is possible to parse complex data structure (e.g. JSON, etc) returned from external processes.

## License and Copyright
Copyright 2018, Maxwell Taylor. Provided under the terms specified in the MIT license.
