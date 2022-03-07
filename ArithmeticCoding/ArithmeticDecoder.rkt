#lang racket
(require "../helpers/bitwr.rkt")

(define original-file "test.txt")
(define encoded-file "encoded.txt")
(define decoded-file "decoded.txt")

(define << arithmetic-shift)
(define || bitwise-ior)
(define & bitwise-and)
(define ^ bitwise-xor)

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

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

(define (get-symbol low high value counts)
  (define count (quotient (sub1 (* (add1 (- value low)) (apply + counts)))
                          (add1 (- high low))))
  (let loop ([index 1])
    (cond
      [(> (apply + (take counts index)) count) (sub1 index)]
      [else (loop (add1 index))])))

(define (prepare-symbol interval value bit-reader)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (prepare-symbol (first-shift low high)
                     (& (|| (<< value 1) (send bit-reader read-bit)) 11..1)
                     bit-reader)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (prepare-symbol (second-shift low high)
                     (& (|| (& value 10..0)
                            (& (<< value 1) (sub1 10..0))
                            (send bit-reader read-bit)) 11..1)
                     bit-reader)]
    [else (list (list low high) value)]))

(define (arithmetic-decode file [counts (make-list SIZE 1)])
  (define bit-reader (new bit-reader% [path encoded-file]))
  (define bit-writer (new bit-writer% [path decoded-file]))
  (let loop ([low 0] [high 11..1] [value (send bit-reader read-bits nr-bits)])
    (define symbol (get-symbol low high value counts))
    (cond
      [(= (sub1 SIZE) symbol)
       (printf "~a ~a\n" (send bit-reader get-counter) (* 8 (file-size encoded-file))) #t]
      [else
       (send bit-writer write-bits symbol 8)
       (define params (prepare-symbol (get-interval symbol counts low high) value bit-reader))
       (loop (caar params) (cadar params) (cadr params))]))
  (send bit-reader close-file)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(time (arithmetic-decode encoded-file (get-frequencies original-file)))
(printf "~a -> ~a -> ~a\n" (file-size original-file) (file-size encoded-file) (file-size decoded-file))
(equal? (file->string original-file) (file->string decoded-file))