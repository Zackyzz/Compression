#lang racket/gui
(require "Wavelet.rkt")

(define frame
  (new frame%
       [label "Wavelet Decomposition"]
       [x 50] [y 100]
       [width 1400] [height 620]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------INPUT PANEL----------------------------------------

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
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap input-bitmap 20 20))]))

(define image-name #f)
(define input-buffer (make-bytes (* SIZE SIZE 4)))
(define 3vt-matrix #f)
(define original-matrix #f)

(define load-button-input
  (new button%
       [parent input-panel]
       [label "Load image"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../WaveletDecomposition/utils" #f #f null))
          (when path
            (set! image-name (last (string-split (path->string path) "\\")))
            (set! input-bitmap (read-bitmap path))
            (send input-canvas on-paint)
            (send input-bitmap get-argb-pixels 0 0 SIZE SIZE input-buffer)
            (set! 3vt-matrix (get-matrix input-buffer))
            (set! original-matrix (get-matrix input-buffer))))]))

;------------------------------------WAVELET PANEL----------------------------------------

(define 3vt-panel
  (new vertical-panel%
       [parent main-panel]))

(define 3vt-bitmap (make-bitmap SIZE SIZE))
(define 3vt-dc (send 3vt-bitmap make-dc))
(send 3vt-dc set-background (make-color 0 0 0))
(send 3vt-dc clear)

(define 3vt-canvas
  (new canvas%
       [parent 3vt-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap 3vt-bitmap 20 20))]))

;------------------------------------BUTTONS PANEL----------------------------------------

(define buttons-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define scale-field
  (new text-field%
       [parent buttons-panel]
       [label "s: "]
       [init-value "1"]))

(define offset-field
  (new text-field%
       [parent buttons-panel]
       [label "o: "]
       [init-value "0"]))

(define x-field
  (new text-field%
       [parent buttons-panel]
       [label "x: "]
       [init-value "0"]))

(define y-field
  (new text-field%
       [parent buttons-panel]
       [label "y: "]
       [init-value "0"]))

(define 3vt-error
  (new button%
       [parent buttons-panel]
       [label "Chebyshev distance:"]
       [callback
        (λ (button event)
          (when (and original-matrix 3vt-matrix)
            (define original (vector->list (flatten-matrix original-matrix)))
            (define decoded (vector->list (flatten-matrix 3vt-matrix)))
            (send min-error set-value (number->string (apply min (map - original decoded))))
            (send max-error set-value (number->string (apply max (map - original decoded))))))]))

(define min-error
  (new text-field%
       [parent buttons-panel]
       [label "Min:"]
       [init-value ""]))

(define max-error
  (new text-field%
       [parent buttons-panel]
       [label "Max:"]
       [init-value ""]))

;----------------------------------------------------------------------------

(define analysis-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define negatron
  (new button%
       [parent analysis-panel]
       [label "qqq"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 512))
          (send y-field set-value (number->string 512))
          (define qq (get-matrix input-buffer))
          
          (define analysis-h1 (analyse-matrix qq))
          
          (define analysis-v1 (transpose (analyse-matrix (transpose analysis-h1))))
          (set! qq analysis-v1)
          
          (define analysis-h2 (analyse-matrix analysis-v1))

          (define analysis-v2 (transpose (analyse-matrix (transpose analysis-h2))))
          
          (define sv2 (transpose (synthetize-matrix (transpose analysis-v2))))

          (define sh2 (synthetize-matrix sv2))
          
          (for ([i 256])
            (for ([j 256])
              (matrix-set qq i j (matrix-get sh2 i j))))

          (define sv1 (transpose (synthetize-matrix (transpose qq))))
          
          (define sh1 (synthetize-matrix sv1))
          (set! qq sh1)

          (set! 3vt-matrix qq)

          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix 
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))