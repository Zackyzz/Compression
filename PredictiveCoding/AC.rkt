#lang racket
(require "../helpers/bitwr.rkt")
(provide arithmetic-encode arithmetic-decode)

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
  model)

(define (first-shift low high)
  (list (& (<< low 1) 11..1)
        (& (|| (<< high 1) 1) 11..1)))

(define (second-shift low high)
  (list (& (& (<< low 1) (sub1 10..0)) 11..1)
        (& (|| (<< high 1) (add1 10..0)) 11..1)))
  
;-------------------------------ENCODE------------------------------

(define (output-bit+storage bit storage bit-writer)
  (send bit-writer write-bit bit)
  (for ([i storage]) (send bit-writer write-bit (^ 1 bit))))

(define (code-symbol interval storage bit-writer)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (output-bit+storage (<< low (- 1 nr-bits)) storage bit-writer)
     (code-symbol (first-shift low high) 0 bit-writer)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (code-symbol (second-shift low high) (+ 1 storage) bit-writer)]
    [else (list (list low high) storage)]))

(define (arithmetic-encode input-matrix header output-file [size 512] [model (build-vector (add1 size) values)])
  (define bit-writer (new bit-writer% [path output-file]))
  (send bit-writer write-bits (first header) 4)
  (send bit-writer write-bits (second header) 4)
  (send bit-writer write-bits 2 2)
  (let loop ([low 0] [high 11..1] [storage 0] [model model] [matrix input-matrix])
    (cond
      [(empty? matrix)
       (define temp (code-symbol (get-interval model (sub1 size) low high size) storage bit-writer))
       (output-bit+storage (if (< (caar temp) (<< 10..0 (- 1))) 0 1) (+ 1 (cadr temp)) bit-writer)
       (send bit-writer write-bits 0 (- nr-bits 2))]
      [else
       (define temp (code-symbol (get-interval model (first matrix) low high size) storage bit-writer))
       (loop (caar temp) (cadar temp) (cadr temp) (update-model model (first matrix) size) (rest matrix))]))
  (send bit-writer close-file))

;-------------------------------DECODE------------------------------

(define (decode-symbol interval value bit-reader)
  (define low (car interval))
  (define high (cadr interval))
  (cond
    [(= 0 (<< (^ low high) (- 1 nr-bits)))
     (decode-symbol (first-shift low high)
                    (& (|| (<< value 1) (send bit-reader read-bit)) 11..1)
                    bit-reader)]
    [(and (= #b01 (<< low (- 2 nr-bits))) (= #b10 (<< high (- 2 nr-bits))))
     (decode-symbol (second-shift low high)
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
  
(define (arithmetic-decode bit-reader [size 512] [model (build-vector (add1 size) values)])
  (let loop ([low 0] [high 11..1] [value (send bit-reader read-bits nr-bits)] [model model] [matrix empty])
    (define symbol (get-symbol low high value model size))
    (cond
      [(= (sub1 size) symbol)
       (send bit-reader close-file) (reverse matrix)]
      [else
       (define temp (decode-symbol (get-interval model symbol low high size) value bit-reader))
       (loop (caar temp) (cadar temp) (cadr temp) (update-model model symbol size) (cons symbol matrix))])))
