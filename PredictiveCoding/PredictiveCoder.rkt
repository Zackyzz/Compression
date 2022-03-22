#lang racket
(provide (all-defined-out))

(define SIZE 256)

(define (->128 A B C) 128)
(define (A A B C) A)
(define (B A B C) B)
(define (C A B C) C)
(define (A+B-C A B C) (+ A B (- C)))
(define (A+B/2-C/2 A B C) (+ A (quotient (- B C) 2)))
(define (B+A/2-C/2 A B C) (+ B (quotient (- A C) 2)))
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
    
    (define original original-matrix)
    (define ep (for/vector ([i size]) (make-vector size)))
    (define ep/Q (for/vector ([i size]) (make-vector size)))
    (define decoded (for/vector ([i size]) (make-vector size)))

    (define/public (get-matrices)
      (predict)
      (vector original ep ep/Q decoded))
    
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
          (define error (- (matrix-get original i j) prediction))
          (matrix-set ep i j error)
          (matrix-set ep/Q i j (quantize error))
          (matrix-set decoded i j (normalize (+ prediction (dequantize (quantize error))))))))))

;-----------------------------------------FUNCTIONS----------------------------------------

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (get-histogram matrix [scaling-factor 1])
  (define (vector-increment vec i)
    (vector-set! vec i (add1 (vector-ref vec i))))
  (define flattened (apply vector-append (vector->list matrix)))
  (define temp (make-vector 511))
  (for ([i flattened])
    (vector-increment temp (+ 255 i)))
  (vector-map vector
              (for/vector ([i (in-range (- 255) 256)]) i)
              (vector-map (λ(x) (* x scaling-factor)) temp)))

#|(define original
  (vector (vector 7 5 2 0)
          (vector 2 11 1 0)
          (vector 15 15 15 0)
          (vector 1 4 14 14)))

(define tester (new coder%
                    [original-matrix original]
                    [size 4] [range 15] [k 2]
                    [predictor A+B-C]))

(send tester get-matrices)|#