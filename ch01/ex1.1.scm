;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.1.
;;;;

(load "../ch01.scm")

(define old-invoke invoke)

(define (invoke f args)
  "Invoke a function and trace its invoation in the console."
  (let ((result (old-invoke f args)))
    (display (format "Invoked ~a => ~a~%"
                     (cons f args)
                     result))
    result))
