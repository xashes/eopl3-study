#lang typed/racket

(define-type Lcexp (U Symbol lambda-exp app-exp))
(struct lambda-exp ([bound-var : Symbol] [body : Lcexp]))
(struct app-exp ([rator : Lcexp] [rand : Lcexp]))

(: occurs-free (-> Symbol Lcexp Boolean))
(define (occurs-free s le)
  (cond
    [(symbol? le) (eqv? s le)]
    [(lambda-exp? le) (and (not (eqv? s (lambda-exp-bound-var le)))
                           (occurs-free s (lambda-exp-body le)))]
    [else (or (occurs-free s (app-exp-rator le))
              (occurs-free s (app-exp-rand le)))]
    ))


(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([le (lambda-exp 'x (app-exp 'x 'y))]
         [ae (app-exp le (app-exp 'x 'z))]
         )
    (check-true (occurs-free 'x 'x))
    (check-false (occurs-free 'x 'y))

    (check-false (occurs-free 'x le))
    (check-true (occurs-free 'y le))

    (check-true (occurs-free 'x ae))
    (check-false (occurs-free 'a ae))
    (check-true (occurs-free 'y ae))
    )

  )
