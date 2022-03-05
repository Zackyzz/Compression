#lang racket
(require "../helpers/bitwr.rkt")
(require racket/format)

(define << arithmetic-shift)
(define (binarize val [len 32]) (~r val #:base 2 #:min-width len #:pad-string "0"))

(define SIZE 32)
(define N 257)

(define low 0)
(define high (- (<< 1 SIZE) 1))
(define 1/high (+ (<< high (- 3)) 1))
(define 2/high (<< 1/high 1))
(define 3/high (<< 1/high 2))

(define counts (make-list N 1))

(define bit-reader (new bit-reader% [path "test.txt"]))

(define (get-probability index counts)
  (define count (list-ref counts index))
  (define sum (apply + (take counts index)))
  (define total-sum (apply + counts))
  (list sum (+ sum count) total-sum))

(get-probability 68 counts)

(define file-length (file-size "test.txt"))

(for ([i file-length])
  (send bit-reader read-bits 8))
                                
