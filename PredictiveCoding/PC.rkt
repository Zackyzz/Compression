#lang racket
(provide (all-defined-out))

(define (->128 A B C) 128)
(define (A A B C) A)
(define (B A B C) B)
(define (C A B C) C)
(define (A+B-C A B C) (+ A B (- C)))
(define (A+B/2-C/2 A B C) (+ A (quotient (- B C) 2)))
(define (B+A/-C/2 A B C) (+ B (quotient (- A C) 2)))
(define (A/2+B/2 A B C) (quotient (+ A B) 2))
(define (jpeg-ls A B C)
  (define min-AB (min A B))
  (define max-AB (max A B))
  (cond
    [(>= C max-AB) min-AB]
    [(<= C min-AB) max-AB]
    [else (A+B-C A B C)]))

(define coder%
  (class object%
    (super-new)
    (init original-matrix)
    (init size)
    (init range)
    (init k)
    (init predictor)

    (define (matrix-get matrix i j)
      (vector-ref (vector-ref matrix i) j))
    (define (matrix-set matrix i j val)
      (vector-set! (vector-ref matrix i) j val))
    
    (define ep/Q (for/vector ([i size]) (make-vector size 0)))
    (define decoded (for/vector ([i size]) (make-vector size 0)))
    
    (define (normalize x)
      (cond
        [(< x 0) 0]
        [(> x range) range]
        [else x]))
    
    (define (quantize x) (floor (/ (+ x k) (add1 (* 2 k)))))
    (define (dequantize x) (* x (add1 (* 2 k))))
    
    (define (get-pred i j)
      (cond
        [(= i j 0) (add1 (quotient range 2))]
        [(= i 0) (matrix-get decoded i (sub1 j))]
        [(= j 0) (matrix-get decoded (sub1 i) j)]
        [else
         (normalize (predictor (matrix-get decoded i (sub1 j))
                               (matrix-get decoded (sub1 i) j)
                               (matrix-get decoded (sub1 i) (sub1 j))))]))
    
    (define (predict)
      (for ([i size])
        (for ([j size])
          (define prediction (get-pred i j))
          (define error/Q (quantize (- (matrix-get original-matrix i j) prediction)))
          (matrix-set ep/Q i j error/Q)
          (matrix-set decoded i j (normalize (+ prediction (dequantize error/Q)))))))
    
    (define/public (get-ep/Q)
      (predict) ep/Q)))


;-----------------------------------------TESTING----------------------------------------


(define original
  (vector (vector 7 5 2 0)
          (vector 2 11 1 0)
          (vector 15 15 15 0)
          (vector 1 4 14 14)))

(define tester (new coder%
                    [original-matrix original]
                    [size 4] [range 15] [k 2]
                    [predictor A+B-C]))

(send tester get-ep/Q)