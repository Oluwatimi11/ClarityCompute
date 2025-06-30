;; A simplified mathematical utility library with basic operations

;; ========== Constants ==========
(define-constant ERR-DIVIDE-BY-ZERO u"Cannot divide by zero")

;; ========== Basic Arithmetic Functions ==========

;; Simple addition
(define-read-only (add (a int) (b int))
  (+ a b))

;; Simple subtraction
(define-read-only (subtract (a int) (b int))
  (- a b))

;; Simple multiplication
(define-read-only (multiply (a int) (b int))
  (* a b))

;; Division with basic zero check
(define-read-only (divide (a int) (b int))
  (if (is-eq b 0)
      (err ERR-DIVIDE-BY-ZERO)
      (ok (/ a b))))

;; ========== Simple Math Functions ==========

;; Square function
(define-read-only (square (a int))
  (* a a))

;; Power function (basic version)
(define-read-only (power (base int) (exponent uint))
  (if (is-eq exponent u0)
      1
      (if (is-eq exponent u1)
          base
          (* base (power base (- exponent u1))))))

;; Absolute value
(define-read-only (abs (a int))
  (if (< a 0)
      (- 0 a)
      a))

;; ========== Comparison Functions ==========

;; Maximum of two values
(define-read-only (max (a int) (b int))
  (if (>= a b) a b))

;; Minimum of two values
(define-read-only (min (a int) (b int))
  (if (<= a b) a b))

;; ========== Utility Functions ==========

;; Check if number is even
(define-read-only (is-even (a int))
  (is-eq (mod a 2) 0))

;; Check if number is odd
(define-read-only (is-odd (a int))
  (not (is-even a)))

;; Sign function
(define-read-only (sign (a int))
  (if (> a 0)
      1
      (if (< a 0)
          -1
          0)))

;; Simple average
(define-read-only (average (a int) (b int))
  (/ (+ a b) 2))