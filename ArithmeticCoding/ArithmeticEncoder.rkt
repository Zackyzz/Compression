#lang racket
(require "../helpers/bitwr.rkt" racket/format)

(define input-file "test.txt")
(define output-file "output.txt")

(define << arithmetic-shift)
(define || bitwise-ior)
(define & bitwise-and)
(define ^ bitwise-xor)

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))
(define 01..0 (<< 1 (- nr-bits 2)))
(define 11..0 (|| 10..0 01..0))

(define (binarize val [len 32]) (~r val #:base 2 #:min-width len #:pad-string "0"))

;-------------------------------------------------------------------------

;return a list with the bytes frequencies from file (last one is eof)
(define (get-frequencies file [len SIZE])
  (define in (open-input-file file))
  (vector->list
   (let loop ([counts (make-vector len 0)])
     (define input (read-byte in))
     (cond
       [(eof-object? input)
        (vector-set! counts (sub1 len) 1)
        (close-input-port in) counts]
       [else (vector-set! counts input (add1 (vector-ref counts input))) (loop counts)]))))

(define (output-bit+storage bit storage bit-writer)
  (send bit-writer write-bit bit)
  (for ([i storage]) (send bit-writer write-bit (^ 1 bit))))

;return low and high for a specific symbol
(define (get-interval index counts low high)
  (define partial-sum (apply + (take counts index)))
  (define total-sum (apply + counts))
  (define interval (+ (- high low) 1))
  (list (+ low (quotient (* partial-sum interval) total-sum))
        (sub1 (+ low (quotient (* (+ (list-ref counts index) partial-sum) interval) total-sum)))))

;-------------------------------------------------------------------------

;output bits based on the symbol interval
(define (encode-symbol interval storage bit-writer)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (output-bit+storage (<< low (- 1 nr-bits)) storage bit-writer)
     (encode-symbol (list (& (<< low 1) 11..1)
                          (& (|| (<< high 1) 1) 11..1))
                    0 bit-writer)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (encode-symbol (list (& (& (<< low 1) (sub1 10..0)) 11..1)
                          (& (|| (<< high 1) (add1 10..0)) 11..1))
                    (+ 1 storage) bit-writer)]
    [else (list (list low high) storage)]))

;-------------------------------------------------------------------------

(define (arithmetic-encode file [counts (make-list SIZE 1)])
  (define in (open-input-file file))
  (define bit-writer (new bit-writer% [path output-file]))
  (let loop ([low 0] [high 11..1] [storage 0])
    (define input (read-byte in))
    (cond
      [(eof-object? input)
       (encode-symbol (get-interval (sub1 SIZE) counts low high) storage bit-writer)]
      [else
       (define lh (encode-symbol
                   (get-interval input counts low high)
                   storage bit-writer))
       (loop (caar lh) (cadar lh) (cadr lh))]))
  (close-input-port in)
  (send bit-writer close-file))

(time (arithmetic-encode input-file (get-frequencies input-file)))
(file-size input-file)
(file-size output-file)