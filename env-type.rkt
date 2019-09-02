#lang typed/racket

(provide empty-env
         extend-env
         apply-env
         has-binding?)

(define-type Env (U Null extend-env))
(struct extend-env ([var : Symbol] [val : Any] [saved-env : Env]))

(: empty-env (-> Env))
(define (empty-env)
  null)

(: empty-env? (-> Env Boolean))
(define (empty-env? env)
  (null? env))

(: extend-env* (-> (Listof Symbol) (Listof Any) Env Env))
(define (extend-env* var-lst val-lst env)
  (: equal-length? (-> (Listof Symbol) (Listof Any) Boolean))
  (define (equal-length? l1 l2)
    (equal? (length l1) (length l2)))
  (assert (equal-length? var-lst val-lst))
  (if (null? var-lst)
      env
      (extend-env* (cdr var-lst)
                   (cdr val-lst)
                   (extend-env (car var-lst) (car val-lst) env)))
  )

(: apply-env (-> Symbol Env Any))
(define (apply-env search-var env)
  (if (null? env)
      (error 'apply-env "No binding for ~s" search-var)
      (if (eq? search-var (extend-env-var env))
          (extend-env-val env)
          (apply-env search-var (extend-env-saved-env env))))
  )

(: has-binding? (-> Symbol Env Boolean))
(define (has-binding? search-var env)
  (if (null? env)
      #f
      (if (eq? search-var (extend-env-var env))
          #t
          (has-binding? search-var (extend-env-saved-env env)))))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([e1 (empty-env)]
         [e2 (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env))))]
         [e3 (extend-env* '(d e a) '(4 5 6) e2)])
    (check-true (empty-env? e1))

    (check-equal? (apply-env 'b e2) 2)
    (check-equal? (apply-env 'a e3) 6)
    (check-equal? (apply-env 'e e3) 5)

    (check-false (has-binding? 'a e1))
    (check-true (has-binding? 'a e2) "e2 has binding of a")
    (check-true (has-binding? 'e e3) "e3 has binding of e")
    (check-false (has-binding? 'e e2))
    )

  )
