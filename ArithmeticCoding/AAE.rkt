#lang racket
(require "../helpers/bitwr.rkt")
(provide arithmetic-encode)

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

(define (get-interval model index low high size)
  (define total-sum (vector-ref model size))
  (define interval (add1 (- high low)))
  (list (+ low (quotient (* (vector-ref model index) interval) total-sum))
        (sub1 (+ low (quotient (* (vector-ref model (add1 index)) interval) total-sum)))))

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

(define (output-bit+storage bit storage bit-writer)
  (send bit-writer write-bit bit)
  (for ([i storage]) (send bit-writer write-bit (^ 1 bit))))

(define (process-symbol interval storage bit-writer)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (output-bit+storage (<< low (- 1 nr-bits)) storage bit-writer)
     (process-symbol (first-shift low high) 0 bit-writer)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (process-symbol (second-shift low high) (+ 1 storage) bit-writer)]
    [else (list (list low high) storage)]))

(define (arithmetic-encode input-file output-file size [model (build-vector (add1 size) values)])
  (define in (open-input-file input-file))
  (define bit-writer (new bit-writer% [path output-file]))
  (let loop ([low 0] [high 11..1] [storage 0] [model model])
    (define input (read-byte in))
    (cond
      [(eof-object? input)
       (define temp (process-symbol (get-interval model (sub1 size) low high size) storage bit-writer))
       (output-bit+storage (if (< (caar temp) (<< 10..0 (- 1))) 0 1) (+ 1 (cadr temp)) bit-writer)
       (send bit-writer write-bits 0 (- nr-bits 2))]
      [else
       (define temp (process-symbol (get-interval model input low high size) storage bit-writer))
       (loop (caar temp) (cadar temp) (cadr temp) (update-model model input size))]))
  (close-input-port in)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(define (main)
  (define original-file "files/testw.txt")
  (define encoded-file "files/encoded.txt")

  (time (arithmetic-encode original-file encoded-file SIZE))
  (printf "~a bytes -> ~a bytes\n" (file-size original-file) (file-size encoded-file))
  (printf "Compression Ratio: ~a" (exact->inexact (/ (file-size original-file) (file-size encoded-file)))))