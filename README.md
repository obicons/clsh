# Clsh

Clsh is a set of Lispy bindings for running and composing *nix processes.

## Rationale

Common Lisp is amazing, but unfortunately sometimes libraries are not present that we need. More often than not, we have access to a command line tool that we can use to perform a given task.

## Example
```lisp
(in-package :clsh-user)

;; outputs "hello world"
(with-programs ("echo")
  (to *standard-output*
      (echo :arguments '("hello world"))))

;; outputs lines that match "bin"
(with-programs ("ls" "grep")
    (to *standard-output*
        (pipe ls
              (grep "bin"))))
```

For more example usage, see `examples.lisp`

## License and Copyright
Copyright 2018, Maxwell Taylor. Provided under the terms specified in the MIT license. 