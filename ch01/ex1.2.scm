;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.2
;;;;

(load "../ch01.scm")

(define (evlis sexps env)
  (if (pair? sexps)
    (if (pair? (cdr sexps))
      (cons (evaluate (car sexps) env)
            (evlis (cdr sexps) env))
      (list (evaluate (car sexps) env)))
    (list)))
