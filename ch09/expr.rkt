#lang plai-typed

; ExprC core language representation
(define-type ExprC
  [numC (n : number)]
  [boolC (n : boolean)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [ifC (test : ExprC) (t : ExprC) (f : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)]
  [letC (what : symbol) (to : ExprC) (in : ExprC)]
  [nilC]
  [consC (head : ExprC) (tail : ExprC)]
  [nodeC (value : ExprC) (left : ExprC) (right : ExprC)])

; ExprC extended language representation
(define-type ExprS
  [numS (n : number)]
  [boolS (e : boolean)]
  [idS (s : symbol)]
  [appS (fun : ExprS) (arg : ExprS)]
  [plusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)] 
  [ifS (test : ExprS) (t : ExprS) (f : ExprS)]
  [lamS (arg : symbol) (body : ExprS)]
  [boxS (arg : ExprS)]
  [unboxS (arg : ExprS)]
  [setboxS (b : ExprS) (v : ExprS)]
  [seqS (b1 : ExprS) (b2 : ExprS)]
  [letS (what : symbol) (to : ExprS) (in : ExprS)]
  [nilS]
  [consS (head : ExprS) (tail : ExprS)]
  [nodeS (value : ExprS) (left : ExprS) (right : ExprS)])

;;; PARSER ;;;
; s-expr -> ExprS
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-boolean? s) (boolS (s-exp->boolean s))]
    [(s-exp-symbol? s) (cond
                         [(equal? (symbol->string (s-exp->symbol s)) "nil") (nilS)]
                         [else (idS (s-exp->symbol s))])]
    [(s-exp-list? s)  (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(-) (case (length sl)
                [(2) (uminusS (parse (second sl)))]
                [(3) (bminusS (parse (second sl)) (parse (third sl)))])]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(app) (appS (parse (second sl)) (parse (third sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))]
         [(box) (boxS (parse (second sl)))]
         [(unbox) (unboxS (parse (second sl)))]
         [(setbox) (setboxS (parse (second sl)) (parse (third sl)))]
         [(seq) (seqS (parse (second sl)) (parse (third sl)))]
         [(let) (letS (s-exp->symbol (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
        ;  [(nil) (nilS)]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(node) (nodeS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]))

;;; DESUGAR ;;;
; minus-helper
(define minus-helper
  (lambda (ac bc)
    (plusC ac (multC (numC -1) bc))))

; ExprS -> ExprC
(define desugar
  (lambda (as)
    (type-case ExprS as
      [numS (n) (numC n)]
      [boolS (b) (boolC b)]
      [idS (s) (idC s)]
      [appS (f a) (appC (desugar f) (desugar a))]
      [plusS (l r) (plusC (desugar l) (desugar r))]
      [uminusS (e) (minus-helper (numC 0) (desugar e))]
      [bminusS (l r) (minus-helper (desugar l) (desugar r))]
      [multS (l r) (multC (desugar l) (desugar r))]
      [lamS (a b) (lamC a (desugar b))]
      [boxS (a) (boxC (desugar a))]
      [unboxS (a) (unboxC (desugar a))]
      [setboxS (b v) (setboxC (desugar b) (desugar v))]
      [seqS (b1 b2) (seqC (desugar b1) (desugar b2))]
      [letS (what to in) (letC what (desugar to) (desugar in))]
      [ifS (test t f) (ifC (desugar test) (desugar t) (desugar f))]
      [nilS () (nilC)]
      [consS (h t) (consC (desugar h) (desugar t))]
      [nodeS (v l r) (nodeC (desugar v) (desugar l) (desugar r))])))

;;; Interpreter ;;;
(define-type-alias Location number)

;; new-loc: -> Location
; produce a new unuseed location
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Binding
  [bound (name : symbol) (val : Location)])

(define-type Env
  [mt-env]
  [extend-env (with : Binding) (rest : Env)])

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)]
  [nilV]
  [consV (head : Value) (tail : Value)]
  [nodeV (value : Value) (left : Value) (right : Value)])

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type Store
  [mt-store]
  [override-store (with : Storage) (rest : Store)])

; Symbol Env -> Location or (error "u (nbound id")
(define (lookup [what : symbol] [in : Env]) : Location
  (type-case Env in
    [mt-env () (error 'lookup ":unbound identifier")]
    [extend-env (binding rst)
                (cond
                  [(equal? (bound-name binding) what) (bound-val binding)]
                  [else (lookup what rst)])]))

; Location Store -> Value
; lookup the vallue in Store when associated with the given location
(define (fetch [what : Location] [in : Store]) : Value
  (type-case Store in
    [mt-store () (error 'fetch ":location not found")]
    [override-store (cell rst)
                    (cond
                      [(equal? (cell-location cell) what) (cell-val cell)]
                      [else (fetch what rst)])]))

; num+ : numV numV -> numV
; num* : numV numV -> numV
; num= : numV numV -> boolV
(define (num-lr op l r) (op (numV-n l) (numV-n r)))
;(define (num-lr op [l : Value] [r : Value]) : Value
;  (cond
;    [(and (numV? l) (numV? r))
;     (cond
;       [(equal? op =) (boolV (= (numV-n l) (numV-n r)))]
;       [(equal? op +) (numV (+ (numV-n l) (numV-n r)))])]
;    [else (error 'num-lr ":op illegal")]))

(define (num+ l r) (numV (num-lr + l r)))
(define (num* l r) (numV (num-lr * l r)))
(define (num= l r) (boolV (num-lr = l r)))

(define-type Result
  [v*s (v : Value) (s : Store)])

(define (interp [expr : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC expr
    [numC (n) (v*s (numV n) sto)]
    [plusC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num+ v-l v-r) s-r)])])]
    [multC (l r) (type-case Result (interp l env sto)
                   [v*s (v-l s-l)
                        (type-case Result (interp r env s-l)
                          [v*s (v-r s-r)
                               (v*s (num* v-l v-r) s-r)])])]                           
    [boolC (b) (v*s (boolV b) sto)]
    [idC (s) (v*s (fetch (lookup s env) sto) sto)]
    [lamC (a b) (v*s (closV a b env) sto)]
    [appC (f a) (type-case Result (interp f env sto)
                  [v*s (v-f s-f)
                       (type-case Result (interp a env s-f)
                         [v*s (v-a s-a)
                             (let ([where (new-loc)])
                               (interp (closV-body v-f)
                                       (extend-env (bound (closV-arg v-f)
                                                          where)
                                                   (closV-env v-f))
                                       (override-store (cell where v-a)
                                                       s-a)))])])]
;          (local [(define f-value (interp f env))]
;                  (interp (closV-body f-value)
;                          (extend-env (bound (closV-arg f-value)
;                                             (interp a env))
;                                      (closV-env f-value))))]
    [boxC (a) (type-case Result (interp a env sto)
                [v*s (v-a s-a)
                     (let ([where (new-loc)])
                       (v*s (boxV where)
                            (override-store (cell where v-a) s-a)))])]
    [unboxC (b) (type-case Result (interp b env sto)
                  [v*s (v-b s-b)
                       (v*s (fetch (boxV-l v-b) s-b) s-b)])]
    [setboxC (b v) (type-case Result (interp b env sto)
                     [v*s (v-b s-b)
                          (type-case Result (interp v env s-b)
                            [v*s (v-v s-v)
                                 (v*s v-v
                                      (override-store (cell (boxV-l v-b) v-v)
                                                      s-v))])])]
    [seqC (b1 b2) (type-case Result (interp b1 env sto)
                    [v*s (v-b1 s-b1)
                         (interp b2 env s-b1)])]
    [letC (what to in) (type-case Result (interp to env sto)
                         [v*s (v-to s-to)
                              (let ([where (new-loc)])
                                (interp in
                                        (extend-env (bound what where) env)
                                        (override-store (cell where v-to) s-to)))])]
    [ifC (test t f) (type-case Result (interp test env sto)
                      [v*s (v-test s-test)
                           (cond
                             [(equal? (boolV-b v-test) #t) (interp t env s-test)]
                             [else (interp f env s-test)])])]
    [nilC () (v*s (nilV) sto)]
    [consC (head tail) (type-case Result (interp head env sto)
                         [v*s (v-head s-head)
                              (type-case Result (interp tail env s-head)
                                [v*s (v-tail s-tail)
                                     (v*s (consV v-head v-tail) s-tail)])])]
    [nodeC (value left right) (type-case Result (interp value env sto)
                                [v*s (v-v s-v)
                                     (type-case Result (interp left env s-v)
                                       [v*s (v-l s-l)
                                            (type-case Result (interp right env s-l)
                                              [v*s (v-r s-r)
                                                   (v*s (nodeV v-v v-l v-r) s-r)])])])]))

(define (interpret [expr : ExprC]) : Result
  (interp expr (mt-env) (mt-store)))

(define (list-to-string [head : Value] [tail : Value]) : string
  "list")

(define (tree-to-string [value : Value] [left : Value] [right : Value]) : string
  "tree")

(define (get-result-string [r : Result]) : string
  (type-case Result r
    [v*s (v-r s-r)
         (local [(define (val [v : Value])
                   (type-case Value v
                     [numV (n) (to-string n)]
                     [boolV (b) (to-string b)]
                     [closV (f b e) "closure"]
                     [boxV (l) (val (fetch l s-r))]
                     [nilV () "nil"]
                     [consV (h t) (string-append "list ("
                                                 (string-append (val h)
                                                                (string-append ") ("
                                                                               (string-append (val t)
                                                                                              ")"))))]
                     [nodeV (v l r) (string-append "tree (val = "
                                                   (string-append (val v)
                                                                  (string-append ", left = "
                                                                                 (string-append (val l)
                                                                                                (string-append ", right = "
                                                                                                               (string-append (val r)
                                                                                                                              ")"))))))]))]
           (val v-r))]))
         
  