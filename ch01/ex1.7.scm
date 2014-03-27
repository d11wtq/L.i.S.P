;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.7
;;;;

(load "../ch01.scm")

(definitial call/cc (lambda (args)
                      (car
                        (call/cc (lambda (c)
                                   ((car args)
                                    (list c)))))))
