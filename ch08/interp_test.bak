#lang plai-typed

(require "expr.rkt")
(require "test_data.rkt")

(test (get-result-string (interpret (desugar (parse num-s-exp)))) num-str)

(test (get-result-string (interpret (desugar (parse true-s-exp)))) true-str)
(test (get-result-string (interpret (desugar (parse false-s-exp)))) false-str)

(test (get-result-string (interpret (desugar (parse plus-s-exp)))) plus-str)
(test (get-result-string (interpret (desugar (parse bminus-s-exp)))) bminus-str)
(test (get-result-string (interpret (desugar (parse uminus-s-exp)))) uminus-str)
(test (get-result-string (interpret (desugar (parse mult-s-exp)))) mult-str)

(test (get-result-string (interpret (desugar (parse lambda-s-exp)))) lambda-str)

(test (get-result-string (interpret (desugar (parse box-s-exp)))) box-str)
(test (get-result-string (interpret (desugar (parse unbox-s-exp)))) unbox-str)
(test (get-result-string (interpret (desugar (parse setbox-s-exp)))) setbox-str)

(test (get-result-string (interpret (desugar (parse seq-s-exp)))) seq-str)

(test (get-result-string (interpret (desugar (parse let-s-exp)))) let-str)

(test (get-result-string (interpret (desugar (parse app-s-exp)))) app-str)

(test (get-result-string (interpret (desugar (parse '(box (+ 1 0)))))) "1")
(test (get-result-string (interpret (desugar (parse '(unbox (box (+ 1 0))))))) "1")

(test (get-result-string (interpret (desugar (parse '(let b
                                                       (box 0)
                                                       (seq (seq (setbox b (+ 1 (unbox b)))
                                                                 (setbox b (+ 1 (unbox b))))
                                                            (unbox b)))))))
      "2")

(test (get-result-string (interpret (desugar (parse '(let b (box 0)
                                                       (let incr (lambda x (+ x 1))
                                                         (if #f
                                                             b
                                                             (seq (setbox b
                                                                          (app incr (unbox b)))
                                                                  b))))))))
      "1")

(test (get-result-string (interpret (desugar (parse '(let b (box dummy)
                                                       (seq (setbox b b)
                                                            b))))))
      "b")