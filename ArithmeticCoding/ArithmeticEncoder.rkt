#lang racket
(require "../helpers/bitwr.rkt")
(provide (all-defined-out))

(define original-file "files/testw.txt")
(define encoded-file "files/encoded.txt")

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

;-------------------------------------------------------------------------

(define (get-frequencies file [len SIZE])
  (define in (open-input-file file))
  (define sums (make-vector (add1 len) 0))
  (let loop ([counts (make-vector len 0)])
    (define input (read-byte in))
    (cond
      [(eof-object? input)
       (vector-set! counts (sub1 len) 1)
       (close-input-port in)
       (for ([i (in-range 1 (add1 len))])
         (vector-set! sums i (+ (vector-ref counts (sub1 i)) (vector-ref sums (sub1 i)))))
       sums]
      [else (vector-set! counts input (add1 (vector-ref counts input))) (loop counts)])))

(define (get-interval model index low high)
  (define total-sum (vector-ref model SIZE))
  (define interval (add1 (- high low)))
  (list (+ low (quotient (* (vector-ref model index) interval) total-sum))
        (sub1 (+ low (quotient (* (vector-ref model (add1 index)) interval) total-sum)))))

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

(define (arithmetic-encode input-file out-file [model (make-vector (add1 SIZE) 1)])
  (define in (open-input-file input-file))
  (define bit-writer (new bit-writer% [path out-file]))
  (let loop ([low 0] [high 11..1] [storage 0])
    (define input (read-byte in))
    (cond
      [(eof-object? input)
       (define temp (process-symbol (get-interval model (sub1 SIZE) low high) storage bit-writer))
       (output-bit+storage (if (< (caar temp) (<< 10..0 (- 1))) 0 1) (+ 1 (cadr temp)) bit-writer)
       (send bit-writer write-bits 0 (- nr-bits 2))]
      [else
       (define temp (process-symbol (get-interval model input low high) storage bit-writer))
       (loop (caar temp) (cadar temp) (cadr temp))]))
  (close-input-port in)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(time (arithmetic-encode original-file encoded-file (get-frequencies original-file)))
(printf "~a bytes -> ~a bytes\n" (file-size original-file) (file-size encoded-file))
(printf "Compression Ratio: ~a" (exact->inexact (/ (file-size original-file) (file-size encoded-file))))