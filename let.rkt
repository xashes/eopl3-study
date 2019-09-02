#lang racket

;; Syntax for the LET language
;; Program ::= Expression
;;             (a-program (exp1))
;; Expression ::= Number
;;                const-exp (num)
;;              | - (Expression , Expression)
;;                diff-exp (exp1 exp2)
;;              | zero? (Expression)
;;                zero?-exp (exp1)
;;              | if Expression then Expression else Expression
;;                if-exp (exp1 exp2 exp3)
;;              | Identifier
;;                var-exp (var)
;;              | let Identifier = Expression in Expression
;;                let-exp (var exp1 body)

;; observer
;; value-of : Exp Env -> ExpVal
