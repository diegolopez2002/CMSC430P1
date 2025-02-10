#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary trees of numbers

;; We will represent Binary trees in racket with structures.
(struct leaf ()             #:prefab)
(struct node (n left right) #:prefab)

;; BTNumber -> Natural
;; Compute the height of a binary tree (leaf has height 0)
(define (btn-height bt)
  (match bt
    [(leaf) 0]
    [(node _ left right) (add1 (max (btn-height left) (btn-height right)))]))

(module+ test
  (check-equal? (btn-height (leaf)) 0)
  (check-equal? (btn-height (node 5 (leaf) (leaf))) 1)
  (check-equal? (btn-height (node 5 (node 1 (leaf) (leaf)) (leaf))) 2))

;; BTNumber -> Natural
;; Count the nodes of a binary tree
(define (btn-count bt)
  (match bt
    [(leaf) 0]
    [(node _ left right) (+ 1 (btn-count left) (btn-count right))]))

(module+ test
  (check-equal? (btn-count (leaf)) 0)
  (check-equal? (btn-count (node 5 (leaf) (leaf))) 1)
  (check-equal? (btn-count (node 5 (node 1 (leaf) (leaf)) (leaf))) 2))

;; BTNumber -> BTNumber
;; Compute the mirror image of binary tree
(define (btn-mirror bt)
  (match bt
    [(leaf) (leaf)]
    [(node n left right) (node n (btn-mirror right) (btn-mirror left))]))

(module+ test
  (check-equal? (btn-mirror (leaf)) (leaf))
  (check-equal? (btn-mirror (node 5 (leaf) (leaf))) (node 5 (leaf) (leaf)))
  (check-equal? (btn-mirror (node 5 (node 1 (leaf) (leaf)) (leaf)))
                (node 5 (leaf) (node 1 (leaf) (leaf)))))

;; BTNumber -> Number
;; Sum the numbers of a binary tree
(define (btn-sum bt)
  (match bt
    [(leaf) 0]
    [(node n left right) (+ n (btn-sum left) (btn-sum right))]))

(module+ test
  (check-equal? (btn-sum (leaf)) 0)
  (check-equal? (btn-sum (node 5 (leaf) (leaf))) 5)
  (check-equal? (btn-sum (node 5 (node 1 (leaf) (leaf)) (leaf))) 6))

;; Natural Number -> BTNumber
;; Generate a full binary tree of height h containing given number n at each node
(define (btn-gen-full h n)
  (if (= h 0)
      (leaf)
      (node n (btn-gen-full (- h 1) n) (btn-gen-full (- h 1) n))))

(module+ test
  (check-equal? (btn-gen-full 0 8) (leaf))
  (check-equal? (btn-gen-full 1 8) (node 8 (leaf) (leaf)))
  (check-equal? (btn-gen-full 2 8) (node 8 (node 8 (leaf) (leaf)) (node 8 (leaf) (leaf)))))

;; BTNumber Number -> Boolean
;; Does the binary tree contain number n?
(define (btn-contains? bt n)
  (match bt
    [(leaf) #f]
    [(node m left right)
     (or (= m n) (btn-contains? left n) (btn-contains? right n))]))

(module+ test
  (check-equal? (btn-contains? (leaf) 8) #f)
  (check-equal? (btn-contains? (node 8 (leaf) (leaf)) 8) #t)
  (check-equal? (btn-contains? (node 5 (leaf) (leaf)) 8) #f)
  (check-equal? (btn-contains? (node 5 (leaf) (node 8 (leaf) (leaf))) 8) #t))

;; BTNumber -> [Listof Number]
;; Generate the list of numbers in binary tree in preorder
(define (btn-preorder bt)
  (match bt
    [(leaf) '()]
    [(node n left right) (append (list n) (btn-preorder left) (btn-preorder right))]))

(module+ test
  (check-equal? (btn-preorder (leaf)) '())
  (check-equal? (btn-preorder (node 5 (leaf) (leaf))) '(5))
  (check-equal? (btn-preorder (node 5 (node 8 (leaf) (leaf)) (node 9 (leaf) (leaf))))
                '(5 8 9)))
