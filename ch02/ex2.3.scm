;;;;
;;; Lisp in Small Pieces, Chapter 2, ex 2.3.
;;;;

(load "../ch02.scm")

(define (list-accessor n)
  "Return a closure for a number to access a list element."
  (define (nth lst n)
    (if (= n 0)
      (car lst)
      (nth (cdr lst)
           (- n 1))))

  (lambda (args denv)
    (if (= 1 (length args))
      (let ((lst (car args)))
        (if (list? lst)
          (nth lst n)
          (error "Invalid argument, expected list")))
      (error "Bad arity" 'list-accessor))))

(define (invoke f args denv)
  "Call a function in the interpreter."
  (let ((proc (cond
                ((procedure? f) f)
                ((number? f)
                 (list-accessor f))
                (else (error "Cannot apply value:" f)))))
    (proc args denv)))
