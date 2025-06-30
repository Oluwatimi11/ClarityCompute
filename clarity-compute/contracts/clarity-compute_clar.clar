;; Enhanced Robust Math Library in Clarity
;; A comprehensive mathematical utility library with robust error handling
;; and a diverse set of common mathematical operations using proper Clarity syntax.

;; ========== Error Constants ==========

(define-constant ERR-ARITHMETIC-OVERFLOW (err u1001))
(define-constant ERR-ARITHMETIC-UNDERFLOW (err u1002))
(define-constant ERR-DIVISION-BY-ZERO (err u1003))
(define-constant ERR-NEGATIVE-SQUARE-ROOT (err u1004))
(define-constant ERR-INVALID-LOGARITHM (err u1005))
(define-constant ERR-DOMAIN-VIOLATION (err u1006))
(define-constant ERR-INVALID-CONVERSION (err u1007))

;; ========== Mathematical Constants ==========

(define-constant MAX-INT 2147483647)
(define-constant MIN-INT -2147483648)
(define-constant MAX-UINT u4294967295)

;; ========== Basic Arithmetic Functions ==========

;; Safe addition with overflow detection
;; @param first-operand (int) - First integer operand
;; @param second-operand (int) - Second integer operand
;; @returns (response int uint) - Returns result or error code
(define-read-only (safe-add (first-operand int) (second-operand int))
  (let ((calculation-result (+ first-operand second-operand)))
    (if (or 
          ;; Positive overflow: both positive, result negative
          (and (> first-operand 0) (> second-operand 0) (< calculation-result 0))
          ;; Negative overflow: both negative, result positive
          (and (< first-operand 0) (< second-operand 0) (> calculation-result 0)))
        ERR-ARITHMETIC-OVERFLOW
        (ok calculation-result))))

;; Safe subtraction with underflow detection
;; @param minuend (int) - Number being subtracted from
;; @param subtrahend (int) - Number being subtracted
;; @returns (response int uint) - Returns result or error code
(define-read-only (safe-subtract (minuend int) (subtrahend int))
  (let ((calculation-result (- minuend subtrahend)))
    (if (or
          ;; Underflow: positive - negative = overflow
          (and (> minuend 0) (< subtrahend 0) (< calculation-result minuend))
          ;; Underflow: negative - positive = underflow
          (and (< minuend 0) (> subtrahend 0) (> calculation-result minuend)))
        ERR-ARITHMETIC-UNDERFLOW
        (ok calculation-result))))

;; Safe multiplication with overflow detection
;; @param multiplicand (int) - First integer operand
;; @param multiplier (int) - Second integer operand
;; @returns (response int uint) - Returns result or error code
(define-read-only (safe-multiply (multiplicand int) (multiplier int))
  (if (or (is-eq multiplicand 0) (is-eq multiplier 0))
      (ok 0)
      (let ((calculation-result (* multiplicand multiplier)))
        (if (and 
              (not (is-eq (/ calculation-result multiplicand) multiplier))
              (not (is-eq (/ calculation-result multiplier) multiplicand)))
            ERR-ARITHMETIC-OVERFLOW
            (ok calculation-result)))))

;; Safe division with zero-division protection
;; @param dividend (int) - Number being divided
;; @param divisor (int) - Number dividing by
;; @returns (response int uint) - Returns result or error code
(define-read-only (safe-divide (dividend int) (divisor int))
  (if (is-eq divisor 0)
      ERR-DIVISION-BY-ZERO
      (ok (/ dividend divisor))))

;; Protected division with fallback value
;; @param dividend (int) - Number being divided
;; @param divisor (int) - Number dividing by
;; @param fallback-value (int) - Value to return if division by zero
;; @returns (int) - Result of division or fallback value
(define-read-only (divide-with-fallback (dividend int) (divisor int) (fallback-value int))
  (if (is-eq divisor 0)
      fallback-value
      (/ dividend divisor)))

;; ========== Power and Root Functions ==========

;; Safe square function with overflow protection
;; @param base-value (int) - Input value to square
;; @returns (response int uint) - Returns squared value or error code
(define-read-only (safe-square (base-value int))
  (safe-multiply base-value base-value))

;; Safe cube function with overflow protection
;; @param base-value (int) - Input value to cube
;; @returns (response int uint) - Returns cubed value or error code
(define-read-only (safe-cube (base-value int))
  (match (safe-multiply base-value base-value)
    square-result (safe-multiply square-result base-value)
    error-code error-code))

;; Integer power function using fast exponentiation
;; @param base-number (int) - Base value
;; @param exponent-value (uint) - Exponent (non-negative)
;; @returns (response int uint) - Returns result or error code
(define-read-only (integer-power (base-number int) (exponent-value uint))
  (if (is-eq exponent-value u0)
      (ok 1)  ;; Any number to power 0 equals 1
      (if (is-eq base-number 0)
          (ok 0)  ;; Zero to any positive power equals 0
          (if (is-eq exponent-value u1)
              (ok base-number)  ;; Any number to power 1 equals itself
              (power-helper base-number exponent-value)))))

;; Helper function for fast exponentiation
(define-private (power-helper (base-number int) (exponent-value uint))
  (let ((half-exponent (/ exponent-value u2))
        (is-odd-exponent (is-eq (mod exponent-value u2) u1)))
    (match (integer-power base-number half-exponent)
      half-power-result 
        (match (safe-multiply half-power-result half-power-result)
          squared-result 
            (if is-odd-exponent
                (safe-multiply squared-result base-number)
                (ok squared-result))
          error-code error-code)
      error-code error-code)))

;; Integer square root using Newton's method
;; @param input-number (uint) - Input value (non-negative)
;; @returns (response uint uint) - Returns integer square root or error code
(define-read-only (integer-square-root (input-number uint))
  (if (is-eq input-number u0)
      (ok u0)
      (if (is-eq input-number u1)
          (ok u1)
          (ok (sqrt-newton-method input-number (+ (/ input-number u2) u1))))))

;; Newton's method helper for square root
(define-private (sqrt-newton-method (target-number uint) (current-guess uint))
  (let ((next-guess (/ (+ current-guess (/ target-number current-guess)) u2)))
    (if (>= next-guess current-guess)
        current-guess
        (sqrt-newton-method target-number next-guess))))

;; ========== Modular Arithmetic ==========

;; Safe modulo operation
;; @param dividend (int) - Number being divided
;; @param divisor (int) - Modulo divisor
;; @returns (response int uint) - Returns remainder or error code
(define-read-only (safe-modulo (dividend int) (divisor int))
  (if (is-eq divisor 0)
      ERR-DIVISION-BY-ZERO
      (ok (mod dividend divisor))))

;; ========== Absolute Value and Sign Functions ==========

;; Safe absolute value with overflow protection
;; @param input-value (int) - Input value
;; @returns (response int uint) - Returns absolute value or error code
(define-read-only (safe-absolute (input-value int))
  (if (< input-value 0)
      (let ((absolute-result (- 0 input-value)))
        (if (< absolute-result 0)  ;; Check for MIN_INT overflow
            ERR-ARITHMETIC-OVERFLOW
            (ok absolute-result)))
      (ok input-value)))

;; Sign determination function
;; @param input-value (int) - Input value
;; @returns (int) - Returns -1, 0, or 1 based on sign
(define-read-only (get-sign (input-value int))
  (if (> input-value 0)
      1
      (if (< input-value 0)
          -1
          0)))

;; ========== Statistical Functions ==========

;; Safe average calculation
;; @param first-value (int) - First value
;; @param second-value (int) - Second value
;; @returns (response int uint) - Returns average or error code
(define-read-only (safe-average (first-value int) (second-value int))
  (match (safe-add first-value second-value)
    sum-result (safe-divide sum-result 2)
    error-code error-code))

;; Weighted average calculation
;; @param first-value (int) - First value
;; @param first-weight (uint) - Weight for first value
;; @param second-value (int) - Second value
;; @param second-weight (uint) - Weight for second value
;; @returns (response int uint) - Returns weighted average or error code
(define-read-only (weighted-average-calculation 
                   (first-value int) (first-weight uint) 
                   (second-value int) (second-weight uint))
  (let ((total-weight (+ first-weight second-weight)))
    (if (is-eq total-weight u0)
        ERR-DIVISION-BY-ZERO
        (match (safe-multiply first-value (to-int first-weight))
          weighted-first 
            (match (safe-multiply second-value (to-int second-weight))
              weighted-second 
                (match (safe-add weighted-first weighted-second)
                  sum-weighted (safe-divide sum-weighted (to-int total-weight))
                  error-code error-code)
              error-code error-code)
          error-code error-code))))

;; ========== Comparison and Utility Functions ==========

;; Maximum of two values
;; @param first-value (int) - First value
;; @param second-value (int) - Second value
;; @returns (int) - Larger value
(define-read-only (get-maximum (first-value int) (second-value int))
  (if (>= first-value second-value) first-value second-value))

;; Minimum of two values
;; @param first-value (int) - First value
;; @param second-value (int) - Second value
;; @returns (int) - Smaller value
(define-read-only (get-minimum (first-value int) (second-value int))
  (if (<= first-value second-value) first-value second-value))

;; Clamp value within range
;; @param target-value (int) - Value to clamp
;; @param minimum-bound (int) - Minimum allowed value
;; @param maximum-bound (int) - Maximum allowed value
;; @returns (int) - Clamped value
(define-read-only (clamp-value (target-value int) (minimum-bound int) (maximum-bound int))
  (if (< target-value minimum-bound)
      minimum-bound
      (if (> target-value maximum-bound)
          maximum-bound
          target-value)))

;; ========== Number Property Tests ==========

;; Check if number is even
;; @param test-value (int) - Value to test
;; @returns (bool) - True if even
(define-read-only (is-even-number (test-value int))
  (is-eq (mod test-value 2) 0))

;; Check if number is odd
;; @param test-value (int) - Value to test
;; @returns (bool) - True if odd
(define-read-only (is-odd-number (test-value int))
  (not (is-eq (mod test-value 2) 0)))

;; Check if number is positive
;; @param test-value (int) - Value to test
;; @returns (bool) - True if positive
(define-read-only (is-positive-number (test-value int))
  (> test-value 0))

;; Check if number is negative
;; @param test-value (int) - Value to test
;; @returns (bool) - True if negative
(define-read-only (is-negative-number (test-value int))
  (< test-value 0))

;; Check if number is zero
;; @param test-value (int) - Value to test
;; @returns (bool) - True if zero
(define-read-only (is-zero-number (test-value int))
  (is-eq test-value 0))

;; Check divisibility
;; @param dividend (int) - Number being divided
;; @param divisor (int) - Divisor to check
;; @returns (response bool uint) - Returns divisibility result or error code
(define-read-only (check-divisibility (dividend int) (divisor int))
  (if (is-eq divisor 0)
      ERR-DIVISION-BY-ZERO
      (ok (is-eq (mod dividend divisor) 0))))

;; ========== Type Conversion Functions ==========

;; Safe conversion from int to uint
;; @param integer-value (int) - Integer to convert
;; @returns (response uint uint) - Returns uint or error code
(define-read-only (convert-int-to-uint (integer-value int))
  (if (< integer-value 0)
      ERR-INVALID-CONVERSION
      (ok (to-uint integer-value))))

;; Safe conversion from uint to int
;; @param unsigned-value (uint) - Unsigned integer to convert
;; @returns (response int uint) - Returns int or error code
(define-read-only (convert-uint-to-int (unsigned-value uint))
  (if (> unsigned-value (to-uint MAX-INT))
      ERR-ARITHMETIC-OVERFLOW
      (ok (to-int unsigned-value))))

;; ========== Bitwise Operations ==========

;; Bitwise AND operation
;; @param first-operand (uint) - First operand
;; @param second-operand (uint) - Second operand
;; @returns (uint) - Result of bitwise AND
(define-read-only (bitwise-and (first-operand uint) (second-operand uint))
  (bit-and first-operand second-operand))

;; Bitwise OR operation
;; @param first-operand (uint) - First operand
;; @param second-operand (uint) - Second operand
;; @returns (uint) - Result of bitwise OR
(define-read-only (bitwise-or (first-operand uint) (second-operand uint))
  (bit-or first-operand second-operand))

;; Bitwise XOR operation
;; @param first-operand (uint) - First operand
;; @param second-operand (uint) - Second operand
;; @returns (uint) - Result of bitwise XOR
(define-read-only (bitwise-xor (first-operand uint) (second-operand uint))
  (bit-xor first-operand second-operand))

;; Bitwise NOT operation
;; @param operand (uint) - Input operand
;; @returns (uint) - Result of bitwise NOT
(define-read-only (bitwise-not (operand uint))
  (bit-not operand))

;; Safe left shift operation
;; @param target-value (uint) - Value to shift
;; @param shift-amount (uint) - Number of positions to shift
;; @returns (uint) - Result of left shift
(define-read-only (safe-left-shift (target-value uint) (shift-amount uint))
  (if (>= shift-amount u128)
      u0  ;; Shifting beyond bit width results in zero
      (bit-shift-left target-value shift-amount)))

;; Safe right shift operation
;; @param target-value (uint) - Value to shift
;; @param shift-amount (uint) - Number of positions to shift
;; @returns (uint) - Result of right shift
(define-read-only (safe-right-shift (target-value uint) (shift-amount uint))
  (if (>= shift-amount u128)
      u0  ;; Shifting beyond bit width results in zero
      (bit-shift-right target-value shift-amount)))

;; ========== Advanced Mathematical Functions ==========

;; Greatest Common Divisor using Euclidean algorithm
;; @param first-number (uint) - First number
;; @param second-number (uint) - Second number
;; @returns (uint) - Greatest common divisor
(define-read-only (greatest-common-divisor (first-number uint) (second-number uint))
  (if (is-eq second-number u0)
      first-number
      (greatest-common-divisor second-number (mod first-number second-number))))

;; Least Common Multiple
;; @param first-number (uint) - First number
;; @param second-number (uint) - Second number
;; @returns (response uint uint) - Returns LCM or error code
(define-read-only (least-common-multiple (first-number uint) (second-number uint))
  (if (or (is-eq first-number u0) (is-eq second-number u0))
      (ok u0)
      (let ((gcd-result (greatest-common-divisor first-number second-number))
            (product (* first-number second-number)))
        (ok (/ product gcd-result)))))

;; Factorial function (limited to prevent overflow)
;; @param input-number (uint) - Number to calculate factorial for
;; @returns (response uint uint) - Returns factorial or error code
(define-read-only (calculate-factorial (input-number uint))
  (if (> input-number u20)  ;; Limit to prevent overflow
      ERR-ARITHMETIC-OVERFLOW
      (ok (factorial-helper input-number u1))))

;; Helper function for factorial calculation
(define-private (factorial-helper (remaining uint) (accumulator uint))
  (if (is-eq remaining u0)
      accumulator
      (factorial-helper (- remaining u1) (* accumulator remaining))))

;; Fibonacci sequence generator
;; @param position (uint) - Position in Fibonacci sequence
;; @returns (response uint uint) - Returns Fibonacci number or error code
(define-read-only (fibonacci-number (position uint))
  (if (> position u100)  ;; Limit to prevent overflow
      ERR-ARITHMETIC-OVERFLOW
      (ok (fibonacci-helper position u0 u1))))

;; Helper function for Fibonacci calculation
(define-private (fibonacci-helper (remaining uint) (previous uint) (current uint))
  (if (is-eq remaining u0)
      previous
      (fibonacci-helper (- remaining u1) current (+ previous current))))