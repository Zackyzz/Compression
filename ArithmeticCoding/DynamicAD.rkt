#lang racket
(require "../helpers/bitwr.rkt")

(define original-file "files/testw.txt")
(define encoded-file "files/encoded.txt")
(define decoded-file "files/decoded.txt")

(define SIZE 257)
(define nr-bits 32)
(define 11..1 (- (<< 1 nr-bits) 1))
(define 10..0 (<< 1 (- nr-bits 1)))

;-------------------------------------------------------------------------

(define (get-interval model index low high)
  (define interval (add1 (- high low)))
  (list (+ low (quotient (* (vector-ref model index) interval) (vector-ref model SIZE)))
        (sub1 (+ low (quotient (* (vector-ref model (add1 index)) interval) (vector-ref model SIZE))))))

(define (update-model model index)
  (for ([i (in-range (add1 index) (add1 SIZE))])
    (vector-set! model i (add1 (vector-ref model i))))
  (when (>= (vector-ref model SIZE) (<< 11..1 (- 2)))
    (define temp
      (for/vector ([i SIZE])
        (quotient (add1 (- (vector-ref model (add1 i)) (vector-ref model i))) 2)))
    (for ([i (in-range 1 (add1 SIZE))])
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

(define (get-symbol low high value model)
  (define count (quotient (sub1 (* (add1 (- value low)) (vector-ref model SIZE)))
                          (add1 (- high low))))
  (let loop ([index 1])
    (cond
      [(> (vector-ref model index) count) (sub1 index)]
      [else (loop (add1 index))])))
  
(define (arithmetic-decode file)
  (define bit-reader (new bit-reader% [path encoded-file]))
  (define bit-writer (new bit-writer% [path decoded-file]))
  (let loop ([low 0] [high 11..1] [value (send bit-reader read-bits nr-bits)]
                     [model (build-vector (add1 SIZE) values)])
    (define symbol (get-symbol low high value model))
    (cond
      [(= (sub1 SIZE) symbol)
       (printf "~a ~a\n" (send bit-reader get-counter) (* 8 (file-size encoded-file))) #t]
      [else
       (send bit-writer write-bits symbol 8)
       (define temp (process-symbol (get-interval model symbol low high) value bit-reader))
       (loop (caar temp) (cadar temp) (cadr temp) (update-model model symbol))]))
  (send bit-reader close-file)
  (send bit-writer close-file))

;-------------------------------------------------------------------------

(time (arithmetic-decode encoded-file))
(printf "~a -> ~a -> ~a\n" (file-size original-file) (file-size encoded-file) (file-size decoded-file))
(equal? (file->string original-file) (file->string decoded-file))