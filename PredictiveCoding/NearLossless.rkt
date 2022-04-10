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

(define predictors-list
  (list ->128 A B C A+B-C A+B/2-C/2 B+A/2-C/2 A/2+B/2 jpeg-ls))

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
      (process-matrices)
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
         (predictor (matrix-get decoded i (sub1 j))
                    (matrix-get decoded (sub1 i) j)
                    (matrix-get decoded (sub1 i) (sub1 j)))]))
    
    (define (process-matrices)
      (for ([i size])
        (for ([j size])
          (define prediction (normalize (get-pred i j)))
          (define error (- (matrix-get original i j) prediction))
          (define error/Q (quantize error))
          (matrix-set ep i j error)
          (matrix-set ep/Q i j error/Q)
          (matrix-set decoded i j (normalize (+ prediction (dequantize error/Q)))))))))

(define decoder%
  (class object%
    (super-new)
    (init compressed-matrix)
    (init size)
    (init range)
    (init k)
    (init predictor)
    
    (define (matrix-get matrix i j)
      (vector-ref (vector-ref matrix i) j))
    (define (matrix-set matrix i j val)
      (vector-set! (vector-ref matrix i) j val))
    
    (define ep/Q compressed-matrix)
    (define epd (for/vector ([i size]) (make-vector size)))
    (define decoded (for/vector ([i size]) (make-vector size)))

    (define/public (get-matrices)
      (process-matrices)
      (vector epd ep/Q decoded))
    
    (define (normalize x)
      (cond
        [(< x 0) 0]
        [(> x range) range]
        [else x]))
    
    (define (dequantize x) (* x (add1 (* 2 k))))
    
    (define (get-pred i j)
      (cond
        [(= i j 0) (add1 (quotient range 2))]
        [(= i 0) (matrix-get decoded i (sub1 j))]
        [(= j 0) (matrix-get decoded (sub1 i) j)]
        [else
         (predictor (matrix-get decoded i (sub1 j))
                    (matrix-get decoded (sub1 i) j)
                    (matrix-get decoded (sub1 i) (sub1 j)))]))
    
    (define (process-matrices)
      (for ([i size])
        (for ([j size])
          (define prediction (normalize (get-pred i j)))
          (define dq-error (dequantize (matrix-get ep/Q i j)))
          (matrix-set epd i j dq-error)
          (matrix-set decoded i j (normalize (+ prediction dq-error))))))))

;------------------------------------FUNCTIONS------------------------------------

(define (flatten-matrix matrix)
  (apply vector-append (vector->list matrix)))

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (get-histogram matrix [scaling-factor 1])
  (define (vector-increment vec i)
    (vector-set! vec i (add1 (vector-ref vec i))))
  (define temp (make-vector 511))
  (for ([i (flatten-matrix matrix)])
    (vector-increment temp (+ 255 i)))
  (vector-map vector
              (for/vector ([i (in-range (- 255) 256)]) i)
              (vector-map (λ(x) (* x scaling-factor)) temp)))

(define (matrix->bytes matrix scale)
  (define (make-pixel4 pixel scale)
    (define (normalize pixel)
      (define x (+ 128 (* pixel scale)))
      (exact-floor (cond [(< x 0) 0] [(> x 255) 255] [else x])))
    (list 255 (normalize pixel) (normalize pixel) (normalize pixel)))
  (list->bytes
   (apply append (map (curryr make-pixel4 scale) (vector->list (flatten-matrix matrix))))))

(define (decoded->bytes matrix)
  (list->bytes
   (apply append (map (λ(x) (list 255 x x x)) (vector->list (flatten-matrix matrix))))))

;---------------------------------------------------------------------------------

#|(define original
  (vector (vector 7 5 2 0)
          (vector 2 11 1 0)
          (vector 15 15 15 0)
          (vector 1 4 14 14)))

(define coder (new coder%
                   [original-matrix original]
                   [size 4] [range 15] [k 2]
                   [predictor A+B-C]))

(define matrices (send coder get-matrices))
(printf "Coder:\n")
matrices

(define quantized-matrix (vector-ref matrices 2))

(define decoder (new decoder%
                     [compressed-matrix quantized-matrix]
                     [size 4] [range 15] [k 2]
                     [predictor A+B-C]))
(printf "Decoder:\n")
(send decoder get-matrices)|#