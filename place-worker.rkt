#lang racket

(provide place-main)

;; It is sufficient to check numbers with the following prime numbers as witnesses up to:
;; n < 3.317.044.064.679.887.385.961.981
;; This number is larger than 64bit integer range.
;; https://en.wikipedia.org/wiki/Miller–Rabin_primality_test
(define SIGNIFICANT-PRIMES (list 2 3 5 7 11 13 17 19 23 29 31 37 41))
(define DETERMINISM-LIMIT 318665857834031151167461)

(define (square x) (* x x))
(define (halve x) (/ x 2))
(define (divides? a b) (= (remainder b a) 0))
(define (even? n) (divides? 2 n))
(define (next n)
  (cond
    ((< n 2) 2)
    ((even? n) (+ n 1))
    (else (+ n 2))))

(define (check-rabin-miller-with-significant-primes potential-prime)
  (define (one-rabin-miller-check base exp m)
    ;; This is the actual test for non-trivial square roots of 1 congruent regarding m.
    (cond
      [(= exp 0) 1]
      [(even? exp)
       ;; If the exponent is an even number, we can halve it, and the squaring happens outside!
       ;; Why this is possible? Check the rabin-miller-test.pdf / .odt.
       (remainder (square (one-rabin-miller-check base
                                                  (halve exp)
                                                  m))
                  m)]
      [else
       ;; in the else branch we take a factor outside instead:
       (remainder (* base (one-rabin-miller-check base
                                                  (- exp 1)
                                                  m))
                  m)]))

  ;; This procedure is only for calling one-rabin-miller-check with the correct parameters.
  (define (rabin-miller-test number-to-check a)
    (cond
      [(> 1 number-to-check) false]  ; safeguard against 0 or lower parameter
      [else
       (= (one-rabin-miller-check a (- number-to-check 1) number-to-check)
          1)]))

  (define (iter number-to-check remaining-significant-primes)
    (cond [(empty? remaining-significant-primes) true]
          [else
           (let ([current-significant-prime (first remaining-significant-primes)])
             (cond [(>= current-significant-prime number-to-check) true]
                   [(rabin-miller-test number-to-check current-significant-prime)
                    (iter number-to-check (rest remaining-significant-primes))]
                   [else false]))]))

  ;; start iterating
  (cond [(>= potential-prime DETERMINISM-LIMIT)
         (displayln "The result might not be correct, because the given number is not in range of the deterministic variation of this prime number test.")
         (iter potential-prime SIGNIFICANT-PRIMES)]
      [else (iter potential-prime SIGNIFICANT-PRIMES)]))

(define (find-primes-limited min max result)
  (cond [(< min max)
         (cond [(check-rabin-miller-with-significant-primes min)
                (find-primes-limited (next min)
                                     max
                                     (cons min result))]
               [else (find-primes-limited (next min)
                                          max
                                          result)])]
        [else (reverse result)]))

(define (place-main p-channel)
  ;; put on the channel the result of the computation
  (let* ([min-max-from-channel (place-channel-get p-channel)]
         [min (car min-max-from-channel)]
         [max (cdr min-max-from-channel)])
    (place-channel-put p-channel (find-primes-limited min max empty))))
