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

(define (make-isometry matrix iso [size 8])
  (for/vector ([i size])
    (for/vector ([j size])
      (cond
        [(= iso 0) (matrix-get matrix i j)]
        [(= iso 1) (matrix-get matrix i (- size 1 j))]
        [(= iso 2) (matrix-get matrix (- size 1 i) j)]
        [(= iso 3) (matrix-get matrix (- size 1 j) (- size 1 i))]
        [(= iso 4) (matrix-get matrix j i)]
        [(= iso 5) (matrix-get matrix (- size 1 j) i)]
        [(= iso 6) (matrix-get matrix (- size 1 i) (- size 1 j))]
        [(= iso 7) (matrix-get matrix j (- size 1 i))]))))

(define (get-ranges matrix [nr 64] [size 8] [step 8])
  (apply append
         (for/list ([i (in-range 0 (* nr step) step)])
           (for/list ([j (in-range 0 (* nr step) step)])
             (define sum 0)
             (define sum^2 0)
             (define block
               (for/vector ([a (in-range i (+ i size))])
                 (for/vector ([b (in-range j (+ j size))])
                   (define bi (matrix-get matrix a b))
                   (set! sum (+ sum bi))
                   (set! sum^2 (+ sum^2 (sqr bi)))
                   bi)))
             (list block sum sum^2)))))

(define (get-domains matrix [nr 63] [size 16] [step 8])
  (apply
   append
   (for/list ([i (in-range 0 (* nr step) step)])
     (apply
      append
      (for/list ([j (in-range 0 (* nr step) step)])
        (define sum 0)
        (define sum^2 0)
        (define block
          (for/vector ([a (in-range i (+ i size) 2)])
            (for/vector ([b (in-range j (+ j size) 2)])
              (define bi
                (quotient (+ (matrix-get matrix a b)
                             (matrix-get matrix a (add1 b))
                             (matrix-get matrix (add1 a) b)
                             (matrix-get matrix (add1 a) (add1 b)))
                          4))
              (set! sum (+ sum bi))
              (set! sum^2 (+ sum^2 (sqr bi)))
              bi)))
        (for/list ([i 8]) (list (make-isometry block i) sum sum^2)))))))

(define (flatten-matrix matrix)
  (apply vector-append (vector->list matrix)))

(define (matrix->bytes matrix)
  (list->bytes
   (apply append (map (λ(x) (list 255 x x x)) (vector->list (flatten-matrix matrix))))))
