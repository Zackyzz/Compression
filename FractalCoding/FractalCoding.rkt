#lang racket
(provide (all-defined-out))

(define SIZE 512)
(define n 64.0)
(define max-s (arithmetic-shift 1 5))
(define max-o (arithmetic-shift 1 7))

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

(define (get-block matrix x y size [zoom 10])
  (define new-matrix (for/vector ([i (* size zoom)]) (make-vector (* size zoom))))
  (for/vector ([i size])
    (for/vector ([j size])
      (define pixel (matrix-get matrix (+ (* 8 x) i) (+ (* 8 y) j)))
      (for ([a zoom])
        (for ([b zoom])
          (matrix-set new-matrix (+ (* zoom i) a) (+ (* zoom j) b) pixel)))))
  new-matrix)

(define (whiterize matrix x y size)
  (define new-matrix matrix)
  (for ([i size])
    (for ([j size])
      (when (or (= 0 (remainder i (sub1 size))) (= 0 (remainder j (sub1 size))))
        (matrix-set new-matrix (+ (* 8 x) i) (+ (* 8 y) j) 255))))
  new-matrix)

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

(define (limit n max)
  (cond [(< n 0) 0] [(>= n max) (- max 1)] [else n]))

(define (quantize-s s)
  (limit (exact-floor (+ 0.5 (/ (* max-s (+ s 1)) 2.0))) max-s))

(define (dequantize-s quantized-s)
  (- (/ (* quantized-s 2.0) max-s) 1))

(define (quantize-o o s)
  (limit (exact-floor (+ 0.5 (/ (* o (sub1 max-o)) (* 255.0 (add1 (abs s)))))) max-o))

(define (dequantize-o quantized-o s)
  (/ (* quantized-o (add1 (abs s)) 255.0) (sub1 max-o)))

(define (search-range lrange domains)
  (define range (first lrange))
  (define sum-r (second lrange))
  (define sum-r^2 (third lrange))
  (let loop ([error (expt 2 30)] [index 0] [S 0] [O 0] [domains domains] [it 0])
    [cond
      [(empty? domains) (list index S O)]
      [else
       (define domain (caar domains))
       (define sum-d (cadar domains))
       (define sum-d^2 (caddar domains))
       (define sum-rd (apply + (map * range domain)))
       (define denom-s (- (* n sum-d^2) (sqr sum-d)))
       (define s (if (= 0 denom-s) 0
                     (/ (- (* n sum-rd) (* sum-r sum-d)) denom-s)))
       (define quantized-s (quantize-s s))
       (set! s (dequantize-s quantized-s))
       (define o (/ (- sum-r (* s sum-d)) n))
       (when (> s 0) (set! o (+ o (* s 255.0))))
       (define quantized-o (quantize-o o s))
       (set! o (dequantize-o quantized-o s))
       (when (> s 0) (set! o (- o (* s 255.0))))
       (define Error (+ sum-r^2
                        (* s (+ (* s sum-d^2) (- (* 2 sum-rd)) (* 2 o sum-d)))
                        (* o (- (* o n) (* 2 sum-r)))))
       (if (< Error error)
           (loop Error it quantized-s quantized-o (rest domains) (add1 it))
           (loop error index S O (rest domains) (add1 it)))]]))

(define (search-ranges ranges domains [p-gauge #f])
  (for/list ([i ranges])
    (when p-gauge (send p-gauge set-value (add1 (send p-gauge get-value))))
    (search-range i domains)))

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
    (define domain (vector-ref new-domains (first i)))
    (define s (dequantize-s (second i)))
    (define o (dequantize-o (third i) s))
    (when (> s 0) (set! o (- o (* s 255.0))))
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

(define (MAE original decoded)
  (set! original (vector->list (flatten-matrix original)))
  (set! decoded (vector->list (flatten-matrix decoded)))
  (/ (apply + (map (λ(x y) (abs (- x y))) original decoded)) (sqr 512.0)))