#lang plai-typed

(require "expr.rkt")

(define num-s-exp '-1)
(define num-exprs (numS -1))
(define num-exprc (numC -1))
(define num-str "-1")

(define true-s-exp '#t)
(define true-exprs (boolS #t))
(define true-exprc (boolC #t))
(define true-str "#t")

(define false-s-exp '#f)
(define false-exprs (boolS #f))
(define false-exprc (boolC #f))
(define false-str "#f")

(define plus-s-exp '(+ 1 2))
(define plus-exprs (plusS (numS 1) (numS 2)))
(define plus-exprc (plusC (numC 1) (numC 2)))
(define plus-str "3")

(define bminus-s-exp '(- 1 2))
(define bminus-exprs (bminusS (numS 1) (numS 2)))
(define bminus-exprc (plusC (numC 1) (multC (numC -1) (numC 2))))
(define bminus-str "-1")

(define uminus-s-exp '(- 1))
(define uminus-exprs (uminusS (numS 1)))
(define uminus-exprc (plusC (numC 0) (multC (numC -1) (numC 1))))
(define uminus-str "-1")

(define mult-s-exp '(* 1 2))
(define mult-exprs (multS (numS 1) (numS 2)))
(define mult-exprc (multC (numC 1) (numC 2)))
(define mult-str "2")

(define lambda-s-exp '(lambda x (+ x 1)))
(define lambda-exprs (lamS 'x
                           (plusS (idS 'x) (numS 1))))
(define lambda-exprc (lamC 'x
                           (plusC (idC 'x) (numC 1))))
(define lambda-str "closure")

(define box-s-exp '(box 3))
(define box-exprs (boxS (numS 3)))
(define box-exprc (boxC (numC 3)))
(define box-str "3")

(define unbox-s-exp '(unbox (box 3)))
(define unbox-exprs (unboxS (boxS (numS 3))))
(define unbox-exprc (unboxC (boxC (numC 3))))
(define unbox-str "3")

(define setbox-s-exp '(setbox (box 3) 4))
(define setbox-exprs (setboxS (boxS (numS 3)) (numS 4)))
(define setbox-exprc (setboxC (boxC (numC 3)) (numC 4)))
(define setbox-str "4")

(define seq-s-exp '(seq 2 3))
(define seq-exprs (seqS (numS 2) (numS 3)))
(define seq-exprc (seqC (numC 2) (numC 3)))
(define seq-str "3")

(define let-s-exp '(let a 4 (* 3 a)))
(define let-exprs (letS 'a (numS 4) (multS (numS 3) (idS 'a))))
(define let-exprc (letC 'a (numC 4) (multC (numC 3) (idC 'a))))
(define let-str "12")

(define app-s-exp '(app (lambda x (+ 1 x)) 2))
(define app-exprs (appS (lamS 'x (plusS (numS 1) (idS 'x))) (numS 2)))
(define app-exprc (appC (lamC 'x (plusC (numC 1) (idC 'x))) (numC 2)))
(define app-str "3")