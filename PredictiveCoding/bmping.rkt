#lang racket/gui
(require plot math/matrix)

(define SIZE 256)

(define frame
  (new frame%
       [label "BMP"]
       [x 500] [y 250]
       [width 600] [height 500]))

(define main-panel
  (new horizontal-panel%
       [parent frame]))

(send frame show #t)

;--------------------------ENCODE--------------------------

(define input-panel
  (new vertical-panel%
       [parent main-panel]))

(define input-bitmap (make-bitmap SIZE SIZE))
(define input-dc (send input-bitmap make-dc))
(send input-dc set-background (make-color 0 0 0))
(send input-dc clear)

(define input-canvas
  (new canvas%
       [parent input-panel]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap input-bitmap 20 20))]))

(define input-buffer (make-bytes (* SIZE SIZE 4)))
(define load-button
  (new button%
       [parent input-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file))
          (when path
            (set! input-bitmap (read-bitmap path))
            (send input-canvas on-paint)
            (send input-bitmap get-argb-pixels 0 0 SIZE SIZE input-buffer)))]))

;--------------------------HISTOGRAM--------------------------

(define (get-frequency buffer)
  (let ((red-pixels
         (for/list ([i (in-range 1 (* SIZE SIZE 4) 4)])
           (bytes-ref buffer i))))
    (map list
         (build-list SIZE values)
         (for/list ([i SIZE])
           (count (λ(x) (= i x)) red-pixels)))))

(define (plot-histogram pixels dc)
  (plot/dc (discrete-histogram pixels
                               #:y-max 1000 #:add-ticks? #f)
           dc 20 20 SIZE SIZE))

(define histogram-panel
  (new vertical-panel%
       [parent main-panel]))
  
(define histogram-canvas
  (new canvas%
       [parent histogram-panel]
       [paint-callback
        (λ(canvas dc)
          (plot-histogram (get-frequency input-buffer) dc))]))

(define refresh-button
  (new button%
       [parent histogram-panel]
       [label "Refresh"]
       [callback
        (λ (button event)
          (plot-histogram (get-frequency input-buffer)
                          (send histogram-canvas get-dc)))]))

;--------------------------Functions--------------------------

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

(define (matrix-get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (matrix-set matrix i j val)
  (vector-set! (vector-ref matrix i) j val))

#|
(require "../helpers/bitwr.rkt")

(define br (new bit-reader% [path "Peppers.bmp"]))

(define header
  (for/list ([i 1078])
    (send br read-bits 8)))

(define pixels
  (for/list ([i (* 256 256)])
    (send br read-bits 8)))

(take pixels 256)|#