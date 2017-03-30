#lang plai-typed

(require "expr.rkt")
(require "test_data.rkt")

(test (desugar (parse num-s-exp)) num-exprc)

(test (desugar (parse true-s-exp)) true-exprc)
(test (desugar (parse false-s-exp)) false-exprc)

(test (desugar (parse plus-s-exp)) plus-exprc)
(test (desugar (parse bminus-s-exp)) bminus-exprc)
(test (desugar (parse uminus-s-exp)) uminus-exprc)
(test (desugar (parse mult-s-exp)) mult-exprc)

(test (desugar (parse lambda-s-exp)) lambda-exprc)

(test (desugar (parse box-s-exp)) box-exprc)
(test (desugar (parse unbox-s-exp)) unbox-exprc)
(test (desugar (parse setbox-s-exp)) setbox-exprc)

(test (desugar (parse seq-s-exp)) seq-exprc)

(test (desugar (parse let-s-exp)) let-exprc)

(test (desugar (parse app-s-exp)) app-exprc)