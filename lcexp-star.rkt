#lang racket

;; Lcexp ::= Identifier
;;         | (lambda ({Identifier}*) Lcexp)
;;         | (Lcexp {Lcexp}*)

(provide
 (contract-out
  [struct var-exp ([var symbol?])]
  [struct lambda-exp ([bound-vars (listof symbol?)] [body lcexp?])]
  [struct app-exp ([rator lcexp?] [rands (listof lcexp?)])]
  [parse-exp (-> any/c lcexp?)]
  ))

(struct var-exp (var) #:transparent)
(struct lambda-exp (bound-vars body) #:transparent)
(struct app-exp (rator rands) #:transparent)

(define (lcexp? e)
  (or (var-exp? e)
      (lambda-exp? e)
      (app-exp? e)))

;; parser : S-exp -> Lcexp
(define (parse-exp datum)
  (match datum
    [(? symbol? var) (var-exp var)]
    [`(lambda ,bound-vars ,body)
     (lambda-exp bound-vars (parse-exp body))]
    [(list rator rands ...)
     (app-exp (parse-exp rator) (map parse-exp rands))]
    [else (error 'parse-exp "Invalid S-expression ~s" datum)]
    )
  )

(module+ test

  (require rackunit rackunit/text-ui)

  (let* ([e1 'a]
         [e2 '(lambda (x y) (+ x y))]
         )
    (check-equal? (parse-exp e1) (var-exp 'a))
    (check-equal? (parse-exp e2) (lambda-exp '(x y) (app-exp (var-exp '+) (list (var-exp 'x) (var-exp 'y)))))
    )

  )
