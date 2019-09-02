#lang racket

;; Exercise 2.6
;; Env ::= Var -> SchemeVal
;; Var ::= Symbol

;; empty-env : () -> Env
(define (empty-env)
  (lambda (search-var)
    (report-no-binding-found search-var)))

;; extend-env : symbol? any/c Env -> Env
(define (extend-env var val env)
  (lambda (search-var)
    (if (eq? var search-var)
        val
        (apply-env env search-var))))

;; apply-env : Env symbol? -> any/c
(define (apply-env env search-var)
  (env search-var)
  )

(define report-no-binding-found
  (lambda (var)
    (error 'apply-env "No binding for ~s" var)))

(define (report-invalid-env env)
  (error 'apply-env "Bad environment: ~s" env))


(module+ test

  (require rackunit rackunit/text-ui)

  (let ([e1 (empty-env)]
        [e2 (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env))))])
    (check-equal? (apply-env e2 'b) 2)

    )
  )
