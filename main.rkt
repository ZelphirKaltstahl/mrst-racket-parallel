#lang racket

(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 256))
(custodian-limit-memory (current-custodian) MAX-BYTES)

#;(require cpuinfo)
(require racket/future)

;; we need to export the main procedure so that we can call it from command line
(provide main)

#;(define (get-number-parallel-units)
  (define (find-siblings remaining-core-info)
    (cond [(empty? remaining-core-info) empty]
          [(equal? (car (first remaining-core-info)) 'siblings)
           (string->number (cdr (first remaining-core-info)))]
          [else
           (displayln "not:")
           (displayln (car (first remaining-core-info)))
           (find-siblings (rest remaining-core-info))]))
  (find-siblings (car (get-cpuinfo))))

(define PLACES-COUNT (processor-count))
(define MIN 0)
(define MAX 10000000)

(define (main)
  (let
      ;; dynamically create places
      ([places (for/list ([i (in-range PLACES-COUNT)])
                 (dynamic-place "place-worker.rkt" 'place-main))]
       [chunk-size (/ MAX PLACES-COUNT)])
    ;; give places work to do
    (for ([i (in-range PLACES-COUNT)]
          [p places])
      (place-channel-put p
                         (cons (* i chunk-size)
                               (+ (* i chunk-size) chunk-size)))
      #;(displayln (place-channel-get p)))

    (for ([i (in-range (length places))]
          [a-place places])
      #;(place-channel-get a-place)
      (place-wait a-place)

      ;; finished after waiting
      (display "place ")
      (display i)
      (displayln " finished."))))

#|
Call with:
racket -tm file.rkt
-t says that the file should be used
-m means that the main function shall be called
|#
