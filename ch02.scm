;;;;
;;; Lisp in Small Pieces, Chapter 2.
;;;
;;; A very naive direct-execution Lisp2 interpreter.
;;;;

(define *env.init*   (list))
(define *env.global* *env.init*)

(define (evaluate sexp env denv)
  "Defines the core evaluator of this LiSP."
  (if (atom? sexp)
    (if (symbol? sexp)
      (lookup sexp env)
      sexp)
    (case (car sexp)
      ((quote) (cadr sexp))
      ((if) (if (evaluate (cadr sexp) env denv)
              (evaluate (caddr sexp) env denv)
              (evaluate (cadddr sexp) env denv)))
      ((begin) (eprogn (cdr sexp) env denv))
      ((set!) (update! (cadr sexp)
                       env
                       (evaluate (caddr sexp) env denv)))
      ((lambda) (make-function (cadr sexp)
                               (cddr sexp)
                               env))
      (else (invoke (evaluate (car sexp) env denv)
                    (evlis (cdr sexp) env denv)
                    denv)))))

(define (atom? sexp)
  "Return #t if the sexp is not a pair."
  (not (pair? sexp)))

(define (lookup-with-error-function id env error-fn)
  "Find the value of id in the a-list env, otherwise invoke error-fn."
  (if (pair? env)
    (if (eq? (caar env) id)
      (cdar env)
      (lookup-with-error-function id (cdr env) error-fn))
    (error-fn id)))

(define (lookup id env)
  "Find the value of id in the a-list env, otherwise error."
  (lookup-with-error-function
    id
    env
    (lambda (args denv)
      (error "No such binding" (car args)))))

(define (update! id env value)
  "Change the value stored in the binding id in the a-list env."
  (if (pair? env)
    (if (eq? (caar env) id)
      (begin
        (set-cdr! (car env) value)
        value)
      (update! id (cdr env) value))
    (error "No such binding" id)))

(define (eprogn sexps env denv)
  "Evaluate a sequence of sexps and return the last one."
  (if (pair? sexps)
    (if (pair? (cdr sexps))
      (begin
        (evaluate (car sexps) en denvv)
        (eprogn (cdr sexps) env denv))
      (evaluate (car sexps) env denv))
    (list)))

(define (evlis sexps env denv)
  "Evaluate a list of sexps and return the evaluated list."
  (if (pair? sexps)
    (cons (evaluate (car sexps) env denv)
          (evlis (cdr sexps) env denv))
    (list)))

(define (extend env ids values)
  "Extend the environment env with new ids and values."
  (cond
    ((pair? ids) (if (pair? values)
                   (cons (cons (car ids)
                               (car values))
                         (extend env (cdr ids) (cdr values)))
                   (error "Not enough values")))
    ((null? ids) (if (null? values)
                   env
                   (error "Too many values")))
    ((symbol? ids) (cons (cons ids values)
                         env))))

(define (make-function params body env)
  "Create a function with params and body in env."
  (lambda (values denv)
    (eprogn body
            (extend env params values)
            denv)))

(define (invoke f args denv)
  "Call a function in the interpreter."
  (if (procedure? f)
    (f args denv)
    (error "Cannot apply value:" f)))

(define (run-repl)
  "Run a simple repl session for our interpreter."
  (define (repl)
    (let ((value (evaluate (read)
                           *env.global*
                           *env.init*)))
      (if (eof-object? value)
        (newline)
        (begin
          (display value)
          (newline)
          (repl)))))
  (display "Entering interactive ch01 repl.")
  (newline)
  (display "To exit, type (quit) or hit ctrl-d.")
  (newline)
  (newline)
  (repl))

(define-syntax definitial
  (syntax-rules ()
    "Macro to define an initial variable in the global env."
    ((definitial name)
     (begin
       (set! *env.global*
         (cons (cons 'name 'void)
               *env.global*))
       'name))
    ((definitial name value)
     (begin
       (set! *env.global*
         (cons (cons 'name value) *env.global*))
       'name))))

(define-syntax defprimitive
  (syntax-rules ()
    "Macro to bind a primitive scheme function to the global env."
    ((defprimitive name fn arity)
     (definitial name
       (lambda (values denv)
         (if (= arity (length values))
           (apply fn values)
           (error "Bad arity" (list 'name values))))))))

(define (quit)
  "Provides a function to exit the repl."
  (read (open-input-string "")))

(definitial bind/de
            (lambda (args denv)
              (if (= 3 (length args))
                (let ((id (car args))
                      (value (cadr args))
                      (thunk (caddr args)))
                  (invoke thunk
                          (list)
                          (extend denv
                                  (list id)
                                  (list value))))
                (error "Bad arity" 'bind/de))))

(definitial assoc/de
            (lambda (args denv)
              (if (= 2 (length args))
                (let ((id (car args))
                      (default (cadr args)))
                  (lookup-with-error-function id denv default))
                (error "Bad arity" 'assoc/de))))

;;; Primitives

(definitial t #t)
(definitial f #f)
(definitial nil (list))

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive * * 2)
(defprimitive / / 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive = = 2)
(defprimitive quit quit 0)
