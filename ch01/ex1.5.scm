;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.5
;;;;

(load "../ch01.scm")

; just pretend #f is the-false-value
(definitial < (lambda (pair)
                 (if (< (car pair)
                        (cadr pair))
                   #t
                   #f)))
