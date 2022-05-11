#lang racket/gui
(require "Wavelet.rkt" binaryio)

(define frame
  (new frame%
       [label "Wavelet Decomposition"]
       [x 250] [y 150]
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
            (set! 3vt-matrix (get-matrix input-buffer))))]))

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

(define temp #f)

(define load-3vt
  (new button%
       [parent 3vt-panel]
       [label "Load wavelet"]
       [callback
        (λ (button event)
          (define file-name (get-file #f #f "../WaveletDecomposition/utils"))
          (define in (open-input-file file-name))
          (when file-name
            (set! temp
                  (for/vector ([i (* 512 512)])
                    (read-float 8 in)))
            (close-input-port in)))]))

(define save-3vt
  (new button%
       [parent 3vt-panel]
       [label "Save wavelet"]
       [callback
        (λ (button event)
          (when image-name
            (define out (open-output-file (string-append "utils/" image-name ".3vt") #:exists 'replace))
            (when (and out 3vt-matrix)
              (for ([i (flatten-matrix 3vt-matrix)])
                (write-float i 8 out))
              (close-output-port out))))]))

;------------------------------------BUTTONS PANEL----------------------------------------

(define buttons-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define scale-field
  (new text-field%
       [parent buttons-panel]
       [label "s: "]
       [init-value "5.7"]))

(define offset-field
  (new text-field%
       [parent buttons-panel]
       [label "o: "]
       [init-value "128"]))

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
          (when 3vt-matrix
            (define original (vector->list (flatten-matrix (get-matrix input-buffer))))
            (define 3vt (vector->list (vector-map exact-round (flatten-matrix 3vt-matrix))))
            (send min-error set-value (number->string (apply min (map - original 3vt))))
            (send max-error set-value (number->string (apply max (map - original 3vt))))))]))

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

;------------------------------------ANALYSIS PANEL----------------------------------------

(define analysis-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define analyse-h1
  (new button%
       [parent analysis-panel]
       [label "AnH1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 512))
          (define ah1 (analyse-matrix 3vt-matrix))
          (set! 3vt-matrix ah1)
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define analyse-v1
  (new button%
       [parent analysis-panel]
       [label "AnV1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 256))
          (define av1 (get-columns (analyse-matrix (get-columns 3vt-matrix 512)) 512))
          (set! 3vt-matrix av1)
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define analyse-h2
  (new button%
       [parent analysis-panel]
       [label "AnH2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 256))
          (define ah2 (analyse-matrix (get-lines 3vt-matrix 256)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get ah2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define analyse-v2
  (new button%
       [parent analysis-panel]
       [label "AnV2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 128))
          (define av2 (get-columns (analyse-matrix (get-columns 3vt-matrix 256)) 256))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get av2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

;------------------------------------SYNTHESIS PANEL----------------------------------------

(define synthesis-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define synthesis-v2
  (new button%
       [parent synthesis-panel]
       [label "SyV2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 256))
          (define sv2 (get-columns (synthetize-matrix (get-columns 3vt-matrix 256)) 256))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get sv2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define synthesis-h2
  (new button%
       [parent synthesis-panel]
       [label "SyH2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 256))
          (define sh2 (synthetize-matrix (get-lines 3vt-matrix 256)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get sh2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define synthesis-v1
  (new button%
       [parent synthesis-panel]
       [label "SyV1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 512))
          (define sv1 (get-columns (synthetize-matrix (get-columns 3vt-matrix 512)) 512))
          (set! 3vt-matrix sv1)
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define synthesis-h1
  (new button%
       [parent synthesis-panel]
       [label "SyH1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 512))
          (send y-field set-value (number->string 512))
          (define sh1 (synthetize-matrix 3vt-matrix))
          (set! 3vt-matrix sh1)
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))