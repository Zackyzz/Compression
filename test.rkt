#lang racket
(require "bitwr.rkt")

(define br (new bit-reader% [path "test.txt"]))
(define bw (new bit-writer% [path "testo.txt"]))
(define file-length (* 8 (file-size "test.txt")))
file-length

(let loop ([nbr file-length])
  (cond
    [(= nbr 0) (send bw write-bits 0 7)]
    [else
     (define nb (random 1 100))
     (when (> nb nbr) (set! nb nbr))
     (define val (send br read-bits nb))
     (send bw write-bits val nb)
     (loop (- nbr nb))]))

(send br close-file)
(send bw close-file)

(equal? (file->string "test.txt") (file->string "testo.txt"))