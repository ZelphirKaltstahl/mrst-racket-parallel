#lang racket

(provide main)

(define PLACES-COUNT 1)
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
