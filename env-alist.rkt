#lang racket

(provide (contract-out
          [empty-env (-> null?)]
          [extend-env (-> symbol? any/c list? list?)]
          ))

;; Exercise 2.5
;; Env ::= () | ((Var . Val) . Env)
;; Var ::= Symbol

;; empty-env : () -> Env
(define (empty-env) '())

;; extend-env* : Listof(Symbol) Listof(SchemeVal) Env -> Env
(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

;; extend-env : Symbol Val Env -> Env
(define (extend-env var val env)
  (extend-env* (list var) (list val) env))

;; apply-env : Env symbol? -> any/c
(define (apply-env env var)
  (if (empty-env? env)
      (report-no-binding-found var)
      (let apply-lsts ([vars (caar env)]
                       [vals (cdar env)])
        (if (empty? vars)
            (apply-env (cdr env) var)
            (if (eq? (car vars) var)
                (car vals)
                (apply-lsts (cdr vars)
                            (cdr vals))))
        )
      ))

;; empty-env? : Env -> Bool
(define (empty-env? env)
  (equal? env
          (empty-env)))

;; has-binding? : Env Var -> Bool
(define (has-binding? env var)
  (if (empty-env? env)
      #f
      (let has-binding-lst ([lst (caar env)]
                            [var var])
        (if (member var lst)
            #t
            (has-binding? (cdr env) var))
        )))

(define report-no-binding-found
  (lambda (var)
    (error 'apply-env "No binding for ~s" var)))

(define (report-invalid-env env)
  (error 'apply-env "Bad environment: ~s" env))


(module+ test

  (require rackunit rackunit/text-ui)

  (let* ([e1 (empty-env)]
         [e2 (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env))))]
         [e3 (extend-env* '(d e a) '(4 5 6) e2)])
    (check-true (empty-env? e1))

    (check-equal? (apply-env e2 'b) 2)
    (check-equal? (apply-env e3 'a) 6)
    (check-equal? (apply-env e3 'e) 5)

    (check-false (has-binding? e1 'a))
    (check-true (has-binding? e2 'a) "e2 has binding of a")
    (check-true (has-binding? e3 'e) "e3 has binding of e")
    (check-false (has-binding? e2 'e))

    (check-equal? e3
                  '(((d e a) 4 5 6) ((a) 1) ((b) 2) ((c) 3)))
    )
)
