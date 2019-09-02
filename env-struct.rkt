#lang racket

;; Exercise 2.6
;; Env ::= (Var Val Env)
;; Var ::= Symbol

;; struct env
(struct env (var val env)
  #:transparent)

;; empty-env : () -> Env
(define (empty-env)
  (env null null null))

;; empty-env? : Env -> Bool
(define (empty-env? e)
  (equal? e (empty-env)))

;; extend-env : symbol? any/c Env -> Env
(define (extend-env var val e)
  (env var val e))

;; apply-env : Env symbol? -> any/c
(define (apply-env e var)
  (if (empty-env? e)
      (report-no-binding-found var)
      (if (eq? var (env-var e))
          (env-val e)
          (apply-env (env-env e) var)))
  )

(define report-no-binding-found
  (lambda (var)
    (error 'apply-env "No binding for ~s" var)))

(define (report-invalid-env e)
  (error 'apply-env "Bad environment: ~s" e))

(module+ test

  (require rackunit rackunit/text-ui)

  (let* ([e1 (empty-env)]
         [e2 (extend-env 'a 1 e1)]
         [e3 (extend-env 'b 2 e2)]
         )
    (check-equal? (apply-env e2 'a) 1)
    )

  )
