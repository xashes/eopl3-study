#lang racket

;; Lc-exp ::= Identifier
;;          | (lambda (Identifier) Lc-exp)
;;          | (Lc-exp Lc-exp)

;; constructors

;; var-exp : Var -> Lc-exp
(define (var-exp var)
  var)

;; lambda-exp : Var Lc-exp -> Lc-exp
(define (lambda-exp var lcexp)
  `(lambda ,var ,lcexp))

;; app-exp : Lc-exp Lc-exp -> Lc-exp
(define (app-exp lcexp1 lcexp2)
  (list lcexp1 lcexp2))

;; two kinds of observers
;;predicates

;; var-exp? : Lc-exp -> Bool
(define (var-exp? lcexp)
  (symbol? lcexp))

;; lambda-exp? : Lc-exp -> Bool
(define (lambda-exp? lcexp)
  (eqv? (car lcexp) 'lambda))

;; app-exp? : Lc-exp -> Bool
(define (app-exp? lcexp)
  (not (or (var-exp? lcexp)
           (lambda-exp? lcexp))))

;; extractors

;; var-exp->var : Lc-exp -> Var
(define (var-exp->var vexp)
  vexp)

;; lambda-exp->bound-var : Lc-exp -> Var
(define (lambda-exp->bound-var lexp)
  (cadr lexp))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define (lambda-exp->body lexp)
  (third lexp))

;; app-exp->rator : Lc-exp -> Lc-exp
(define (app-exp->rator aexp)
  (car aexp))

;; app-exp->rand : Lc-exp -> Lc-exp
(define (app-exp->rand aexp)
  (second aexp))

;; occurs-free? : Sym LcExp -> Boll
;; usage: (occurs-free? search-var lcexp) -> #t if s occurs free in exp, #f otherwise
;; var occurs free in a Lc-exp是指，出现在表达式中，但若表达式是lambda-expression的形式时，与它绑定
;; 的标识符不一样，即与某个自由变量相同。
;; LcExp ::= Identifier
;;         | (lambda (Identifier) LcExp)
;;         | (LcExp LcExp)
;; 解决思路：
;; 1. 根据LcExp的定义建立相应的数据结构，定义它的Interface, 即 constructors and observers
;; 2. 利用predicate observers区分case, 进而利用extractor observers提取要比较的相应元素
;; 3. 根据数据结构，利用Interface覆盖所有atom级（无法细分的结构）的比较（i.e. terminator）
;; 4. 在子结构上进行递归
;; 5. 测试constructors
;; 6. 利用constructors构造测试用例
(define (occurs-free? search-var lcexp)
  (cond
    [(var-exp? lcexp) (eq? search-var (var-exp->var lcexp))]
    [(lambda-exp? lcexp) (and (not (eq? search-var (lambda-exp->bound-var lcexp)))
                              (occurs-free? search-var (lambda-exp->body lcexp)))]
    [else (or (occurs-free? search-var (app-exp->rator lcexp))
              (occurs-free? search-var (app-exp->rand lcexp)))]))

(module+ test

  (require rackunit rackunit/text-ui)

  (let ([ve (var-exp 'x)]
        [le (lambda-exp 'x '(x y))]
        [ae (app-exp (lambda-exp 'x '(x z)) (app-exp 'x 'y))]
        )
    (check-equal? (var-exp->var ve) 'x)
    (check-equal? (lambda-exp->bound-var le) 'x)
    (check-equal? (lambda-exp->body le) '(x y))

    (check-true (var-exp? ve))
    (check-false (var-exp? le))
    (check-true (lambda-exp? le))
    (check-true (app-exp? ae))

    (check-true (occurs-free? 'x ve))
    (check-false (occurs-free? 'y ve))
    (check-false (occurs-free? 'x le))
    (check-true (occurs-free? 'y le))
    (check-true (occurs-free? 'z ae))
    (check-true (occurs-free? 'x ae))
    )

  )
