;;;;
;;; Lisp in Small Pieces, Chapter 1, ex 1.3
;;;;

(load "../ch01.scm")

(define (extend env ids values)
  (cons (cons ids values) env))

(define (lookup id env)
  (define (find-value ids values)
    (if (pair? ids)
      (if (eq? (car ids) id)
        (car values)
        (find-value (cdr ids)
                    (cdr values)))
      (lookup id (cdr env))))

  (if (pair? env)
    (find-value (caar env) (cdar env))
    (error "No such binding" id)))

(define (update! id env value)
  (define (update-value! ids values)
    (if (pair? ids)
      (if (eq? (car ids) id)
        (set-car! values value)
        (update-value! (cdr ids)
                       (cdr values)))
      (update! id (cdr env) value)))

  (if (pair? env)
    (update-value! (caar env) (cdar env))
    (error "No such binding" id)))
