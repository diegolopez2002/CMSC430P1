#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String functions

;; Read up on string functions in Racket to implement these.

;; String String -> String
;; Select the longer of the two strings (or first if same length)
(define (longer s1 s2)
  (if (>= (string-length s1) (string-length s2))
      s1
      s2))

(module+ test
  (check-equal? (longer "" "") "")
  (check-equal? (longer "abc" "d") "abc")
  (check-equal? (longer "a" "bcd") "bcd")
  (check-equal? (longer "ab" "cd") "ab"))

;; String -> [Listof String]
;; Explode a string into a list of length-1 strings
(define (explode s)
  (map string (string->list s)))

(module+ test
  (check-equal? (explode "") '())
  (check-equal? (explode "a") '("a"))
  (check-equal? (explode "abc") '("a" "b" "c")))

;; String -> [Listof [List String String]]
;; Compute list of bigrams (pairs of adjacent letters) in a string
(define (bigrams s)
  (let ([chars (explode s)])
    (if (< (length chars) 2)
        '()
        (map list (take chars (- (length chars) 1)) (cdr chars)))))

(module+ test
  (check-equal? (bigrams "") '())
  (check-equal? (bigrams "a") '())
  (check-equal? (bigrams "ab") '(("a" "b")))
  (check-equal? (bigrams "abc") '(("a" "b") ("b" "c"))))
