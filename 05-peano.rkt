#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peano numbers

;; Unary encoding of the natural numbers

;; type N = (Z) | (S N)

;; We will represent Peano numbers in racket with structures.
(struct Z ()      #:prefab)
(struct S (N)     #:prefab)
;; this structure 'S' should be recursive -
;; it's parameter should be another Peano Number

;; Natural -> N
;; Convert natural to Peano
(define (nat->peano n)
  (if (= n 0)
      (Z)
      (S (nat->peano (- n 1)))))

(module+ test
  (check-equal? (nat->peano 0) (Z))
  (check-equal? (nat->peano 1) (S (Z)))
  (check-equal? (nat->peano 2) (S (S (Z))))
  (check-equal? (nat->peano 3) (S (S (S (Z))))))

;; N -> Natural
;; Convert Peano to natural
(define (peano->nat n)
  (match n
    [(Z) 0]
    [(S pred) (+ 1 (peano->nat pred))]))

(module+ test
  (check-equal? (peano->nat (Z)) 0)
  (check-equal? (peano->nat (S (Z))) 1)
  (check-equal? (peano->nat (S (S (Z)))) 2)
  (check-equal? (peano->nat (S (S (S (Z))))) 3))

;; Do not use conversions to implement the following functions

;; N N -> N
;; Add two Peano numbers together
(define (plus n1 n2)
  (match n1
    [(Z) n2]
    [(S pred) (S (plus pred n2))]))

(module+ test
  (check-equal? (plus (Z) (Z)) (Z))
  (check-equal? (plus (Z) (S (Z))) (S (Z)))
  (check-equal? (plus (S (Z)) (Z)) (S (Z)))
  (check-equal? (plus (S (Z)) (S (Z))) (S (S (Z)))))

;; N N -> N
;; Multiply two Peano numbers together
(define (mult n1 n2)
  (match n1
    [(Z) (Z)]
    [(S pred) (plus n2 (mult pred n2))]))

(module+ test
  (check-equal? (mult (Z) (Z)) (Z))
  (check-equal? (mult (Z) (S (Z))) (Z))
  (check-equal? (mult (S (Z)) (Z)) (Z))
  (check-equal? (mult (S (Z)) (S (Z))) (S (Z)))
  (check-equal? (mult (S (S (Z))) (S (S (Z)))) (S (S (S (S (Z)))))))

;; ∀ (α) N (α -> α) -> (α -> α)
(define (iter n f)
  (match n
    [(Z) (λ (a) a)]
    [(S pred) (λ (a) (f ((iter pred f) a)))]))

(module+ test
  ;; Natural -> Natural
  (define (succ n) (+ n 1))

  (check-equal? ((iter (Z) succ) 0) 0)
  (check-equal? ((iter (S (Z)) succ) 0) 1)
  (check-equal? ((iter (S (S (Z))) succ) 0) 2)

  ;; Boolean -> Boolean
  (define (neg b) (not b))

  (check-equal? ((iter (Z) neg) #t) #t)
  (check-equal? ((iter (S (Z)) neg) #t) #f)
  (check-equal? ((iter (S (S (Z))) neg) #t) #t)

  ;; String -> String
  (define (addhi s) (string-append "hi " s))

  (check-equal? ((iter (Z) addhi) "there") "there")
  (check-equal? ((iter (S (Z)) addhi) "there") "hi there")
  (check-equal? ((iter (S (S (Z))) addhi) "there") "hi hi there"))
