;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.8
;;;;

(load "../ch01.scm")

(definitial apply (lambda (args)
                    (apply (car args)
                           (cdr args))))
