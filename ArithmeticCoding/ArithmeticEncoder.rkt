#lang racket
(require "../helpers/bitwr.rkt")

(define input-file "test.txt")
(define output-file "output.txt")

(define << arithmetic-shift)
(define || bitwise-ior)
(define & bitwise-and)

(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))
(define 01..0 (<< 1 (- nr-bits 2)))
(define 11..0 (|| 10..0 01..0))

(define bit-writer (new bit-writer% [path output-file]))

(define (get-frequencies file [len 257])
  (define in (open-input-file file))
  (vector->list
   (let loop ([counts (make-vector len 0)])
     (define input (read-byte in))
     (cond
       [(eof-object? input)
        (vector-set! counts (sub1 len) 1)
        (close-input-port in) counts]
       [else (vector-set! counts input (add1 (vector-ref counts input))) (loop counts)]))))

(define (get-interval index counts)
  (define count (list-ref counts index))
  (define sum (apply + (take counts index)))
  (define total-sum (apply + counts))
  (list sum (+ sum count) total-sum))

(define (encode-symbol low high storage)
  (cond
    [(< high 10..0)
     (send bit-writer write-bit 0)
     (for ([i storage])
       (send bit-writer write-bit 1))
     (encode-symbol (& (<< low 1) 11..1)
                    (& (|| (<< high 1) 1) 11..1)
                    0)]
    [(>= low 10..0)
     (send bit-writer write-bit 1)
     (for ([i storage])
       (send bit-writer write-bit 0))
     (encode-symbol (& (<< low 1) 11..1)
                    (& (|| (<< high 1) 1) 11..1)
                    0)]
    [(and (>= low 01..0) (< high 11..0))
     (encode-symbol (& (<< low 1) (sub1 10..0))
                    (|| (<< high 1) (add1 10..0))
                    (+ 1 storage))]
    [else (list low high storage)]))

(define (arithmetic-coding file [counts (make-list 257 1)])
  (define in (open-input-file file))
  (let loop ([low 0] [high 11..1] [storage 0])
    (printf "~a ~a ~a \n" low high storage)
    (define input (read-byte in))
    (cond
      [(eof-object? input) #t]
      [else
       (define range (+ (- high low) 1))
       (define interval (get-interval input counts))
       (define lh (encode-symbol
                   (+ low (quotient (* (first interval) range) (third interval)))
                   (sub1 (+ low (quotient (* (second interval) range) (third interval))))
                   storage))
       (loop (first lh) (second lh) (third lh))]))
  (close-input-port in)
  (send bit-writer close-file))

(time (arithmetic-coding input-file))

(file-size input-file)
(file-size output-file)
