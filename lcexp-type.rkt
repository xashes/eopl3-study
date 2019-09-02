#lang typed/racket

;; Lc-exp ::= Identifier
;;          | (lambda (Identifier) Lc-exp)
;;          | (Lc-exp Lc-exp)

;; 每一个子类型有确定的结构，所以可以用struct构造
;; 而要研究的类型本身，并没有确定的结构，而是可能为三种变体结构之一（或），
;; 因而用define-type构造，即，某几种变体结构中的任意一种构成此类型，正符合
;; 上方的描述方法所定义的逻辑。
(define-type Lc-exp (U Symbol lambda-exp app-exp))
(define-predicate lc-exp? Lc-exp)
(struct lambda-exp ([bound-var : Symbol] [body : Lc-exp]))
(struct app-exp ([rator : Lc-exp] [rand : Lc-exp]))

;; 用match似乎简洁，但是依赖了各类型的内部表示形式
;; 因此，仅作为练习，实际在此处应直接使用API中的 obserbers (predicates and extractors)
(: occurs-free? (-> Symbol Lc-exp Boolean))
(define (occurs-free? search-var lcexp)
  (match lcexp
    [(? symbol?) (eqv? search-var lcexp)]
    [(lambda-exp bound-var body)
     (and (not (eqv? search-var bound-var))
          (occurs-free? search-var body))]
    [(app-exp rator rand)
     (or (occurs-free? search-var rator)
         (occurs-free? search-var rand))]
    ))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let* ([e1 (lambda-exp 'x (app-exp 'x 'y))]
         [e2 (app-exp e1 (app-exp 'y 'z))]
         [e3 (lambda-exp 'lambda (app-exp 'lambda 'lambda-exp))]
         )
    (check-true (occurs-free? 'y e1))
    (check-false (occurs-free? 'x e1))
    (check-false (occurs-free? 'x e2))
    (check-true (occurs-free? 'z e2))
    (check-true (occurs-free? 'lambda-exp e3))
    )
  )
