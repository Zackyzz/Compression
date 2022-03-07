#lang racket
(require "../helpers/bitwr.rkt" racket/format)

(define preinput "test.txt")
(define sizef (file-size preinput))
(define input-file "encoded.txt")
(define output-file "decoded.txt")

(define << arithmetic-shift)
(define || bitwise-ior)
(define & bitwise-and)
(define ^ bitwise-xor)

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

(define (binarize val [len 32]) (~r val #:base 2 #:min-width len #:pad-string "0"))

;-------------------------------------------------------------------------

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

(define (get-interval index counts low high)
  (define partial-sum (apply + (take counts index)))
  (define total-sum (apply + counts))
  (define range (+ (- high low) 1))
  (list (+ low (quotient (* partial-sum range) total-sum))
        (sub1 (+ low (quotient (* (+ (list-ref counts index) partial-sum) range) total-sum)))))

(define (first-shift low high)
  (list (& (<< low 1) 11..1)
        (& (|| (<< high 1) 1) 11..1)))

(define (second-shift low high)
  (list (& (& (<< low 1) (sub1 10..0)) 11..1)
        (& (|| (<< high 1) (add1 10..0)) 11..1)))
  
;-------------------------------------------------------------------------

(define (arithmetic-decode file [counts (make-list SIZE 1)])
  (define bit-reader (new bit-reader% [path input-file]))
  (define bit-writer (new bit-writer% [path output-file]))
  (define n 0)
  (let loop ([low 0] [high 11..1] [value (send bit-reader read-bits nr-bits)])
    (define count (quotient
                   (sub1 (* (add1 (- value low)) (apply + counts)))
                   (add1 (- high low))))
    (define symb
      (let get-sym ([sym 1])
        (cond
          [(> (apply + (take counts sym)) count) (sub1 sym)]
          [else (get-sym (add1 sym))])))
    (cond
      [(= n sizef) "done"]
      [(= 256 symb) "done"]
      [else
       (set! n (add1 n))
       (send bit-writer write-bits symb 8)
       (define interval (get-interval symb counts low high))
       (let inner ([lh interval] [val value])
         (define low (first lh))
         (define high (second lh))
         (cond
           [(= 0 (<< (^ low high) (- 1 nr-bits)))
            (inner (first-shift low high)
                   (& (|| (<< val 1) (send bit-reader read-bit)) 11..1))]
           [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
            (define bit (send bit-reader read-bit))
            (inner (second-shift low high)
                   (& (|| (& (<< val 1) (sub1 10..0)) (& val 10..0) bit) 11..1))]
           [else (loop (first lh) (second lh) val)]))]))
  (send bit-reader close-file)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(time (arithmetic-decode input-file (get-frequencies preinput)))
(file-size preinput)
(file-size input-file)
(file-size output-file)
(equal? (file->string preinput) (file->string output-file))