#lang racket

;; concrete syntax
;; Lcexp ::= Identifier
;;         | (lambda (Identifier) Lcexp)
;;         | (Lcexp Lcexp)

;; (define-type Lcexp (U var-exp lambda-exp app-exp))
(struct var-exp (var) #:transparent)
(struct lambda-exp (bound-var body) #:transparent)
(struct app-exp (rator rand) #:transparent)
(define (lcexp? exp)
  (or (var-exp? exp)
      (lambda-exp? exp)
      (app-exp? exp)))

;; TODO write with match
;; (: parse-lcexp (-> (U Symbol (Listof Any)) Lcexp))
;; (define (parse-lcexp datum)
;;   (cond
;;     [(symbol? datum) (var-exp datum)]
;;     [(pair? datum)
;;      (if (eq? 'lambda (car datum))
;;          (lambda-exp (car (cadr datum))
;;                      (parse-lcexp (caddr datum)))
;;          (app-exp (parse-lcexp (car datum))
;;                   (parse-lcexp (cadr datum)))
;;          )]
;;     [else (error 'parse-lcexp "Invalid concrete syntax: ~s" datum)]
;;     ))

(define (parse-lcexp datum)
  (match datum
    [(? symbol? var) (var-exp datum)]
    [(list 'lambda (list bound-var) body)
     (lambda-exp bound-var (parse-lcexp body))]
    [(list rator rand)
     (app-exp (parse-lcexp rator) (parse-lcexp rand))]
    [else (error 'parse-lcexp "Invalid concrete syntax: ~s" datum)]
    )
  )

;; (: unparse-lcexp (-> Lcexp (U Symbol (Listof Any))))
;; (define (unparse-lcexp exp)
;;   (cond
;;     [(var-exp? exp) (var-exp-var exp)]
;;     [(lambda-exp? exp)
;;      (list 'lambda (list (lambda-exp-bound-var exp)) (unparse-lcexp (lambda-exp-body exp)))]
;;     [(app-exp? exp)
;;      (list (unparse-lcexp (app-exp-rator exp))
;;            (unparse-lcexp (app-exp-rand exp)))]
;;     [else (error 'unparse-lcexp "Invalid Lcexp: -s" exp)]
;;     )
;;   )

(define (unparse-lcexp exp)
  (if (lcexp? exp)
      (match exp
        [(var-exp var) var]
        [(lambda-exp bound-var body)
         `(lambda (,bound-var) ,(unparse-lcexp body))]
        [(app-exp rator rand)
         `(,(unparse-lcexp rator)
           ,(unparse-lcexp rand))]
        )
      (error 'unparse-lcexp "Invalid Lcexp: -s" exp)
      )
  )

;; concrete syntax -> as a set of strings
;; Lcexp ::= Identifier
;;         | proc Identifier => Lcexp
;;         | Lcexp(Lcexp)
;; (: unparse-lcexp-string (-> Lcexp String))
(define (unparse-lcexp-string lcexp)
  (if (lcexp? lcexp)
      (match lcexp
        [(var-exp var) (symbol->string var)]
        [(lambda-exp bound-var body)
         (format "proc ~a => ~a" bound-var (unparse-lcexp-string body))]
        [(app-exp rator rand)
         (format "~a(~a)" (unparse-lcexp-string rator) (unparse-lcexp-string rand))]
        )
      (error 'unparse-lcexp "Invalid Lcexp: -s" exp)))

(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (parse-lcexp 'x) (var-exp 'x))
  (check-equal? (parse-lcexp '(lambda (x) (x y)))
                (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
  (check-equal?
   (unparse-lcexp (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
   '(lambda (x) (x y)))
  (check-equal?
   (unparse-lcexp-string (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
   "proc x => x(y)")

  )
