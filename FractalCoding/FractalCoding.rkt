#lang racket
(provide (all-defined-out))

(define SIZE 512)

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))
(define (matrix-set matrix i j val)
  (vector-set! (vector-ref matrix i) j val))

(define (get-ranges matrix [nr 64] [size 8] [step 8])
  (for/list ([i (in-range 0 (* nr step) step)])
    (for/list ([j (in-range 0 (* nr step) step)])
      (define sum 0)
      (define sum^2 0)
      (define block
        (for/list ([a (in-range i (+ i size))])
          (for/list ([b (in-range j (+ j size))])
            (define bi (matrix-get matrix a b))
            (set! sum (+ sum bi))
            (set! sum^2 (+ sum^2 (sqr bi)))
            bi)))
      (list block sum sum^2))))

(define (get-domains matrix [nr 63] [size 16] [step 8])
  (for/list ([i (in-range 0 (* nr step) step)])
    (for/list ([j (in-range 0 (* nr step) step)])
      (define sum 0)
      (define sum^2 0)
      (define block
        (for/list ([a (in-range i (+ i size) 2)])
          (for/list ([b (in-range j (+ j size) 2)])
            (define bi
              (quotient (+ (matrix-get matrix a b)
                           (matrix-get matrix a (add1 b))
                           (matrix-get matrix (add1 a) b)
                           (matrix-get matrix (add1 a) (add1 b)))
                        4))
            (set! sum (+ sum bi))
            (set! sum^2 (+ sum^2 (sqr bi)))
            bi)))
      (list block sum sum^2))))