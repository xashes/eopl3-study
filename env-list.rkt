#lang racket

;; Env ::= () | (Var Val Env)
;; Var ::= Symbol

;; empty-env : () -> Env
(define (empty-env)
  '())

;; extend-env : Var Val Env -> Env
(define (extend-env var val env)
  (list var val env))

;; apply-env : Env Var -> Val
(define (apply-env env var)
  (if (empty? env)
      (report-no-binding-found var)
      (if (eq? var (first env))
          (second env)
          (apply-env (third env) var))
      )
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
    (check-equal? e1 '())
    (check-equal? e2 '(a 1 (b 2 (c 3 ()))))

    (check-equal? (apply-env e2 'b) 2)
    )
  )
