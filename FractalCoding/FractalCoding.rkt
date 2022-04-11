#lang racket
(provide (all-defined-out))

(define SIZE 512)
(define n 64.0)

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set matrix i j val)
  (vector-set! (vector-ref matrix i) j val))

(define (flatten-matrix matrix)
  (apply vector-append (vector->list matrix)))

(define (matrix->bytes matrix)
  (list->bytes (apply append (map (λ(x) (list 255 x x x)) (vector->list (flatten-matrix matrix))))))

;--------------------------------------ENCODING--------------------------------------------

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
  (apply
   append
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
       (list (vector->list (apply vector-append (vector->list block))) sum sum^2)))))

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
        (for/list ([i 8])
          (list (vector->list (apply vector-append (vector->list (make-isometry block i))))
                sum sum^2)))))))

(define (search-range lrange domains)
  (define range (first lrange))
  (define sum-r (second lrange))
  (define sum-r^2 (third lrange))
  (define index 0)
  (define error 10000000)
  (define S 0)
  (define O 0)
  (for ([i domains] [j (in-naturals)])
    (define domain (first i))
    (define sum-d (second i))
    (define sum-d^2 (third i))
    (define sum-rd (apply + (map * range domain)))
    (define denom-s (- (* n sum-d^2) (sqr sum-d)))
    (define s
      (if (= 0 denom-s)
          0
          (/ (- (* n sum-rd) (* sum-r sum-d)) denom-s)))
    (define o
      (if (= 0 denom-s)
          (/ sum-r n)
          (/ (- sum-r (* s sum-d)) n)))
    (define Error (/ (+ sum-r^2
                        (* s (+ (* s sum-d^2) (- (* 2 sum-rd)) (* 2 o sum-d)))
                        (* o (- (* o n) (* 2 sum-r))))
                     n))
    (when (< Error error)
      (set! error Error)
      (set! index j)
      (set! S s)
      (set! O o)))
  (list error index S O))

;---------------------------------------DECODING----------------------------------------

(define (get-decoding-domains matrix [nr 63] [size 16] [step 8])
  (list->vector
   (apply
    append
    (for/list ([i (in-range 0 (* nr step) step)])
      (apply
       append
       (for/list ([j (in-range 0 (* nr step) step)])
         (define block
           (for/vector ([a (in-range i (+ i size) 2)])
             (for/vector ([b (in-range j (+ j size) 2)])
               (quotient (+ (matrix-get matrix a b)
                            (matrix-get matrix a (add1 b))
                            (matrix-get matrix (add1 a) b)
                            (matrix-get matrix (add1 a) (add1 b)))
                         4))))
         (for/list ([i 8]) (make-isometry block i))))))))

(define (normalize x)
  (set! x (exact-round x))
  (cond [(< x 0) 0] [(> x 255) 255] [else x]))

(define (decode founds new-domains)
  (for/vector ([i founds])
    (define domain (vector-ref new-domains (second i)))
    (define s (third i))
    (define o (fourth i))
    (for/vector ([i 8])
      (for/vector ([j 8])
        (normalize (+ o (* s (matrix-get domain i j))))))))

(define (blocks->image-matrix blocks)
  (define new-matrix (for/vector ([i SIZE]) (make-vector SIZE)))
  (for ([block blocks] [index (in-naturals)])
    (define row (quotient index 64))
    (define column (remainder index 64))
    (for ([i 8])
      (for ([j 8])
        (matrix-set new-matrix (+ (* 8 row) i) (+ (* 8 column) j) (matrix-get block i j)))))
  new-matrix)

;----------------------------------------------------------------------------------------
                
(define (save-founds founds path)
  (display-lines-to-file
   (map (λ(x) (string-join (map number->string x))) founds) path #:exists 'replace))

(define (read-founds path)
  (map (λ(x) (map string->number (string-split x))) (file->lines path)))

(define (PSNR original decoded)
  (set! original (vector->list (flatten-matrix original)))
  (set! decoded (vector->list (flatten-matrix decoded)))
  (* 10 (log
         (/ (* SIZE SIZE (sqr (apply max original)))
            (apply + (map (λ(x y) (sqr (- x y))) original decoded)))
         10)))
