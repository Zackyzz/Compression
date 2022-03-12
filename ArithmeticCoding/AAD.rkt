#lang racket
(require "../helpers/bitwr.rkt")
(provide arithmetic-decode)

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

(define (get-interval model index low high size)
  (define interval (add1 (- high low)))
  (list (+ low (quotient (* (vector-ref model index) interval) (vector-ref model size)))
        (sub1 (+ low (quotient (* (vector-ref model (add1 index)) interval) (vector-ref model size))))))

(define (update-model model index size)
  (for ([i (in-range (add1 index) (add1 size))])
    (vector-set! model i (add1 (vector-ref model i))))
  (when (>= (vector-ref model size) (<< 11..1 (- 2)))
    (define temp
      (for/vector ([i size])
        (quotient (add1 (- (vector-ref model (add1 i)) (vector-ref model i))) 2)))
    (for ([i (in-range 1 (add1 size))])
      (vector-set! model i (+ (vector-ref model (sub1 i)) (vector-ref temp (sub1 i))))))
  model)

(define (first-shift low high)
  (list (& (<< low 1) 11..1)
        (& (|| (<< high 1) 1) 11..1)))

(define (second-shift low high)
  (list (& (& (<< low 1) (sub1 10..0)) 11..1)
        (& (|| (<< high 1) (add1 10..0)) 11..1)))
  
;-------------------------------------------------------------------------

(define (process-symbol interval value bit-reader)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (process-symbol (first-shift low high)
                     (& (|| (<< value 1) (send bit-reader read-bit)) 11..1)
                     bit-reader)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (process-symbol (second-shift low high)
                     (& (|| (& value 10..0)
                            (& (<< value 1) (sub1 10..0))
                            (send bit-reader read-bit)) 11..1)
                     bit-reader)]
    [else (list (list low high) value)]))

(define (get-symbol low high value model size)
  (define count (quotient (sub1 (* (add1 (- value low)) (vector-ref model size)))
                          (add1 (- high low))))
  (let loop ([index 1])
    (cond
      [(> (vector-ref model index) count) (sub1 index)]
      [else (loop (add1 index))])))
  
(define (arithmetic-decode input-file output-file size [model (build-vector (add1 size) values)])
  (define bit-reader (new bit-reader% [path input-file]))
  (define bit-writer (new bit-writer% [path output-file]))
  (let loop ([low 0] [high 11..1] [value (send bit-reader read-bits nr-bits)] [model model])
    (define symbol (get-symbol low high value model size))
    (cond
      [(= (sub1 size) symbol) #t]
      [else
       (send bit-writer write-bits symbol 8)
       (define temp (process-symbol (get-interval model symbol low high size) value bit-reader))
       (loop (caar temp) (cadar temp) (cadr temp) (update-model model symbol size))]))
  (send bit-reader close-file)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(define (main)
  (define original-file "files/testw.txt")
  (define encoded-file "files/encoded.txt")
  (define decoded-file "files/decoded.txt")

  (time (arithmetic-decode encoded-file decoded-file SIZE))
  (printf "~a -> ~a -> ~a\n" (file-size original-file) (file-size encoded-file) (file-size decoded-file))
  (equal? (file->string original-file) (file->string decoded-file)))