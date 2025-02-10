#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Representing expressions

;; We will be using these structures:
(struct Int  (i)      #:prefab)
(struct Bool (b)      #:prefab)
(struct Var  (v)      #:prefab)
(struct App  (e1 e2)  #:prefab)
(struct Lam  (x e)    #:prefab)

;; SExpr -> Expr
(define (sexpr->expr s)
  (match s
    [(? integer? s)     (Int s)]
    [(? boolean? b)     (Bool b)]
    [(? symbol? v)      (Var v)]
    [(list e1 e2)       (App (sexpr->expr e1) (sexpr->expr e2))]
    [(list 'lambda (list (? symbol? x)) e)
     (Lam x (sexpr->expr e))]))

(module+ test
  ;; [Listof a] [Listof a] -> Boolean
  ;; Are the two lists equal up to re-ordering and repetition?
  (define (list-set-equal? xs ys)
    (equal? (list->set xs) (list->set ys)))

  (check-equal? (list-set-equal? '() '()) #t)
  (check-equal? (list-set-equal? (list 1 2) (list 2 1)) #t)
  (check-equal? (list-set-equal? (list 1 1 2) (list 2 2 1)) #t)
  (check-equal? (list-set-equal? (list 1 1 2) (list 2 3 2 1)) #f))

;; Expr -> [Listof Integer]
;; Computes a list of all integer literals that appear in the expression
(define (expr-integers e)
  (match e
    [(Int i) (list i)]
    [(Bool _) '()]
    [(Var _) '()]
    [(App e1 e2) (append (expr-integers e1) (expr-integers e2))]
    [(Lam _ e) (expr-integers e)]))

(module+ test
  (check list-set-equal? (expr-integers (sexpr->expr 123)) '(123))
  (check list-set-equal? (expr-integers (sexpr->expr 'x)) '())
  (check list-set-equal? (expr-integers (sexpr->expr '((lambda (x) x) 123))) '(123))
  (check list-set-equal? (expr-integers (sexpr->expr '((lambda (x) 42) 123))) '(123 42)))

;; Expr -> [Listof Variable]
;; Compute a list of all lambda-bound variables in the expression
(define (expr-lambda-vars e)
  (match e
    [(Int _) '()]
    [(Bool _) '()]
    [(Var _) '()]
    [(App e1 e2) (append (expr-lambda-vars e1) (expr-lambda-vars e2))]
    [(Lam x e) (cons x (expr-lambda-vars e))]))

(module+ test
  (check list-set-equal? (expr-lambda-vars (sexpr->expr 123)) '())
  (check list-set-equal? (expr-lambda-vars (sexpr->expr 'x)) '())
  (check list-set-equal? (expr-lambda-vars (sexpr->expr '((lambda (x) x) 123))) '(x))
  (check list-set-equal? (expr-lambda-vars (sexpr->expr '((lambda (x) 42) 123))) '(x)))

;; Expr -> [Listof Variable]
;; Compute a list of all free variables, i.e., variables which occur outside
;; of any lambda that binds them.
(define (expr-free-vars e [bound '()])
  (match e
    [(Int _) '()]
    [(Bool _) '()]
    [(Var v) (if (member v bound) '() (list v))]
    [(App e1 e2) (append (expr-free-vars e1 bound) (expr-free-vars e2 bound))]
    [(Lam x e) (expr-free-vars e (cons x bound))]))

(module+ test
  (check list-set-equal? (expr-free-vars (sexpr->expr 123)) '())
  (check list-set-equal? (expr-free-vars (sexpr->expr 'x)) '(x))
  (check list-set-equal? (expr-free-vars (sexpr->expr '((lambda (x) x) 123))) '())
  (check list-set-equal? (expr-free-vars (sexpr->expr '((lambda (x) 42) 123))) '()))

