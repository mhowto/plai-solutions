#lang plai-typed

(require "expr.rkt")
(require "test_data.rkt")

(test (parse '-1) (numS -1))
(test (parse '#f) (boolS #f))
(test (parse '#t) (boolS #t))

(test (parse `foo) (idS 'foo))
(test (parse `foo_bar) (idS 'foo_bar))

(test (parse '(+ 1 foo)) (plusS (numS 1) (idS 'foo)))

(test (parse plus-s-exp) plus-exprs)
(test (parse bminus-s-exp) bminus-exprs)
(test (parse uminus-s-exp) uminus-exprs)
(test (parse mult-s-exp) mult-exprs)

;(test (parse '(/ 1 2)) ('parse "invalid list input"))

(test (parse '(* (- 2) (- #f 1))) (multS (uminusS (numS 2)) (bminusS (boolS #f) (numS 1))))
(test (parse '(* (- 2) (- #f foo))) (multS (uminusS (numS 2)) (bminusS (boolS #f) (idS 'foo)))) 

(test (parse lambda-s-exp) lambda-exprs)

(test (parse box-s-exp) box-exprs)
(test (parse unbox-s-exp) unbox-exprs)
(test (parse setbox-s-exp) setbox-exprs)

(test (parse seq-s-exp) seq-exprs)

(test (parse let-s-exp) let-exprs)

(test (parse app-s-exp) app-exprs)

;(test (parse '(/ 3 3)) (multS (numS 3) (numS 3)))