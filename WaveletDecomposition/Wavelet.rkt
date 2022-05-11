#lang racket
(provide (all-defined-out))

(define SIZE 512)
(define n 9)
(define padd (quotient n 2))

(define AL '(0.026748757411 -0.016864118443 -0.078223266529 0.266864118443 0.602949018236 0.266864118443  -0.078223266529 -0.016864118443 0.026748757411))
(define AH '(0.000000000000 0.091271763114 -0.057543526229 -0.591271763114 1.115087052457 -0.591271763114 -0.057543526229 0.091271763114 0.000000000000))
(define SL '(0.000000000000 -0.091271763114 -0.057543526229 0.591271763114 1.115087052457 0.591271763114 -0.057543526229 -0.091271763114 0.000000000000))
(define SH '(0.026748757411 0.016864118443 -0.078223266529 -0.266864118443 0.602949018236 -0.266864118443 -0.078223266529 0.016864118443 0.026748757411))
;(define x '(1 2 3 4 5 6 7 8 9 9 9 9 9 9 3 2 7 5 2 8 2 55 2 7 3 1 6 9 1 3 2 66))


(define (padd-sequence lst)
  (append (reverse (take (rest lst) padd))
          lst
          (rest (reverse (drop lst (- (length lst) (+ 1 padd)))))))

(define (convolution signal filter)
  (let loop ([padded-sequence (padd-sequence signal)])
    (if (< (length padded-sequence) n)
        empty
        (cons (apply + (map * (take padded-sequence n) filter))
              (loop (rest padded-sequence))))))

(define (downsample lst proc)
  (define clone (list->vector lst))
  (for/list ([i (length lst)] #:when (proc i)) (vector-ref clone i)))

(define (upsample lst proc)
  (if (equal? proc even?)
      (apply append (map list lst (make-list (length lst) 0)))
      (apply append (map list (make-list (length lst) 0) lst))))

(define (analysis signal)
  (append (downsample (convolution signal AL) even?)
          (downsample (convolution signal AH) odd?)))

(define (synthesis signal)
  (define len (quotient (length signal) 2))
  (define low (upsample (take signal len) even?))
  (define high (upsample (drop signal len) odd?))
  (map (λ(x y) (+ x y))
       (convolution low SL) (convolution high SH)))

(define (analyse-matrix matrix)
  (for/vector ([i matrix])
    (list->vector (analysis (vector->list i)))))

(define (synthetize-matrix matrix)
  (for/vector ([i matrix])
    (list->vector (synthesis (vector->list i)))))

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (flatten-matrix matrix)
  (apply vector-append (vector->list matrix)))

(define (get-columns matrix size)
  (for/vector ([i size])
    (for/vector ([j size])
      (matrix-get matrix j i))))

(define (get-lines matrix size)
  (for/vector ([i size])
    (for/vector ([j size])
      (matrix-get matrix i j))))

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set matrix i j val)
  (define my-row (vector-ref matrix i))
  (vector-set! my-row j val)
  (vector-set! matrix i my-row))

(define (matrix->bytes original [scale 1] [offset 0] [x SIZE] [y SIZE] [level 0])
  (define temp
    (for/vector ([i SIZE])
      (for/vector ([j SIZE])
        (if (and (< i (quotient SIZE (expt 2 level))) (< j (quotient SIZE (expt 2 level))))
            (if (and (< i y) (< j x))
                (exact-round (matrix-get original i j))
                (exact-round (+ offset (* scale (matrix-get original i j)))))
            (exact-round (+ offset (* scale (matrix-get original i j))))))))
  
  (define (make-rgb-pixel pixel)
    (define (normalize pixel)
      (cond [(< pixel 0) 0] [(> pixel 255) 255] [else pixel]))
    (list 255 (normalize pixel) (normalize pixel) (normalize pixel)))
  (list->bytes (apply append (map make-rgb-pixel (vector->list (flatten-matrix temp))))))
