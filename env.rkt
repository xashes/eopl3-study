#lang racket

;; empty-env : () -> Env
(define (empty-env)
  (list 'empty-env))

;; extend-env : Var SchemeVal Env -> Env
(define (extend-env var val env)
  (list 'extend-env var val env))

;; apply-env : Env Var -> SchemeVal
(define (apply-env env var)
  (cond
    [(eq? (car env) 'empty-env) (report-no-binding-found var)]
    [(eq? (car env) 'extend-env)
     (if (eq? (second env) var)
         (third env)
         (apply-env (fourth env) var))]
    [else (report-invalid-env env)]))

(define report-no-binding-found
  (λ (search-var)
    (error 'apply-env "No bindingg for ~s" search-var)))

(define (report-invalid-env env)
  (error 'apply-env "Bad environment: ~s" env))

(module+ test

  (require rackunit rackunit/text-ui)
'(extend-env a 1 (empty-env))
  (check-equal? (empty-env) '(empty-env))
  (check-equal? (extend-env 'a 1 (empty-env)) '(extend-env a 1 (empty-env)))
  (check-equal? (apply-env (extend-env 'a 1 (empty-env)) 'a) 1)

  )
