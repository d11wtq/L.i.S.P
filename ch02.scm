;;;;
;;; Lisp in Small Pieces, Chapter 2.
;;;
;;; A very naive direct-execution Lisp2 interpreter.
;;;;

(define *env.init*   (list))
(define *env.global* *env.init*)
(define *fenv.global* *env.init*)

(define (evaluate sexp env fenv)
  "Defines the core evaluator of this LiSP."
  (if (atom? sexp)
    (if (symbol? sexp)
      (lookup sexp env)
      sexp)
    (case (car sexp)
      ((quote) (cadr sexp))
      ((if) (if (evaluate (cadr sexp) env fenv)
              (evaluate (caddr sexp) env fenv)
              (evaluate (cadddr sexp) env fenv)))
      ((begin) (eprogn (cdr sexp) env fenv))
      ((set!) (update! (cadr sexp)
                       env
                       (evaluate (caddr sexp) env fenv)))
      ((flet) (eprogn (cddr sexp)
                      env
                      (extend fenv
                              (map car (cadr sexp))
                              (map (lambda (defn)
                                     (make-function (cadr defn)
                                                    (cddr defn)
                                                    env
                                                    fenv))
                                   (cadr sexp)))))
      ((function) (cond
                    ((symbol? (cadr sexp)) (lookup (cadr sexp) fenv))
                    (else (error "Bad function" (cadr sexp)))))
      ((lambda) (make-function (cadr sexp)
                               (cddr sexp)
                               env fenv))
      (else (evaluate-application (car sexp)
                                  (evlis (cdr sexp) env fenv)
                                  env
                                  fenv)))))

(define (atom? sexp)
  "Return #t if the sexp is not a pair."
  (not (pair? sexp)))

(define (lookup id env)
  "Find the value of id in the a-list env."
  (if (pair? env)
    (if (eq? (caar env) id)
      (cdar env)
      (lookup id (cdr env)))
    (error "No such binding" id)))

(define (update! id env value)
  "Change the value stored in the binding id in the a-list env."
  (if (pair? env)
    (if (eq? (caar env) id)
      (begin
        (set-cdr! (car env) value)
        value)
      (update! id (cdr env) value))
    (error "No such binding" id)))

(define (eprogn sexps env fenv)
  "Evaluate a sequence of sexps and return the last one."
  (if (pair? sexps)
    (if (pair? (cdr sexps))
      (begin
        (evaluate (car sexps) env fenv)
        (eprogn (cdr sexps) env fenv))
      (evaluate (car sexps) env fenv))
    (list)))

(define (evlis sexps env fenv)
  "Evaluate a list of sexps and return the evaluated list."
  (if (pair? sexps)
    (cons (evaluate (car sexps) env fenv)
          (evlis (cdr sexps) env fenv))
    (list)))

(define (make-function params body env fenv)
  "Create a function with params and body in env."
  (lambda (values)
    (eprogn body
            (extend env params values)
            fenv)))

(define (evaluate-application id args env fenv)
  "Evaluate a function call to id with args in env."
  (cond
    ((symbol? id) (invoke (lookup id fenv) args))
    ((and (pair? id) (eq? (car id) 'lambda)) (eprogn (cddr id)
                                                     (extend env
                                                             (cadr id)
                                                             args)
                                                     fenv))
    (else (error "Bad functional term" id))))

(define (invoke f args)
  "Call a function in the interpreter."
  (if (procedure? f)
    (f args)
    (error "Cannot apply value:" f)))

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

(define-syntax definitial-function
  (syntax-rules ()
    "Macro to define an initial function in the global env."
    ((definitial-function name)
     (begin
       (set! *fenv.global*
         (cons (cons 'name 'void)
               *fenv.global*))
       'name))
    ((definitial-function name value)
     (begin
       (set! *fenv.global*
         (cons (cons 'name value) *fenv.global*))
       'name))))

(define-syntax defprimitive
  (syntax-rules ()
    "Macro to bind a primitive scheme function to the global env."
    ((defprimitive name fn arity)
     (definitial-function name
       (lambda (values)
         (if (= arity (length values))
           (apply fn values)
           (error "Bad arity" (list 'name values))))))))

(define (run-repl)
  "Run a simple repl session for our interpreter."
  (define (repl)
    (let ((value (evaluate (read) *env.global* *fenv.global*)))
      (if (eof-object? value)
        (newline)
        (begin
          (display value)
          (newline)
          (repl)))))
  (display "Entering interactive ch02 repl.")
  (newline)
  (display "To exit, type (quit) or hit ctrl-d.")
  (newline)
  (newline)
  (repl))

(define (quit)
  "Provides a function to exit the repl."
  (read (open-input-string "")))

;;; Bootstrap

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

(definitial-function funcall
                     (lambda (args)
                      (if (> (length args) 1)
                        (invoke (car args) (cdr args))
                        (error "Bad arity" 'funcall))))
