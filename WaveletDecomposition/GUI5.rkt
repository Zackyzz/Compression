#lang racket/gui
(require "Wavelet.rkt")

(define frame
  (new frame%
       [label "Wavelet Decomposition"]
       [x 350] [y 150]
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

(define 3vt-name #f)
(define 3vt-buffer (make-bytes (* SIZE SIZE 4)))
(define load-button-3vt
  (new button%
       [parent 3vt-panel]
       [label "Load wavelet"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../WaveletDecomposition/utils" #f #f null '(("3vt" "*.3vt"))))
          (when path
            (set! 3vt-name (last (string-split (path->string path) "\\")))
            (set! 3vt-bitmap (read-bitmap path))
            (send 3vt-bitmap get-argb-pixels 0 0 SIZE SIZE 3vt-buffer)
            (set! 3vt-matrix (get-matrix 3vt-buffer))
            (send 3vt-canvas on-paint)))]))

(define 3vt-save
  (new button%
       [parent 3vt-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when (and image-name analysis)
            (with-output-to-file (string-append "utils/" image-name ".3vt") #:exists 'replace
              (λ() (for ([i (flatten-matrix analysis)])
                     (printf "~a\n" i))))))]))

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
          (when (and original-matrix 3vt-matrix)
            (define original (vector->list (flatten-matrix original-matrix)))
            (define decoded (map exact-round (vector->list (flatten-matrix 3vt-matrix))))
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

;------------------------------------ANALYSIS PANEL----------------------------------------

(define analysis-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define analysis-h1 #f)

(define analyse-h1
  (new button%
       [parent analysis-panel]
       [label "AnH1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 512))
          (set! analysis-h1 (analyse-matrix 3vt-matrix))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get analysis-h1 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis-h1
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define analysis-v1 #f)

(define analyse-v1
  (new button%
       [parent analysis-panel]
       [label "AnV1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 256))
          (set! analysis-v1 (transpose (analyse-matrix (transpose analysis-h1))))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get analysis-v1 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis-v1
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define analysis-h2 #f)

(define analyse-h2
  (new button%
       [parent analysis-panel]
       [label "AnH2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 256))
          (set! analysis-h2 (analyse-matrix (vector-take analysis-v1 256)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis-h2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis-h2
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define analysis-v2 #f)

(define analyse-v2
  (new button%
       [parent analysis-panel]
       [label "AnV2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 128))
          (set! analysis-v2 (transpose (analyse-matrix (vector-take (transpose analysis-h2) 256))))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis-v2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis-v2
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

(define syn-v2 #f)

(define synthesis-v2
  (new button%
       [parent synthesis-panel]
       [label "SyV2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 128))
          (send y-field set-value (number->string 256))
          (set! syn-v2 (transpose (synthetize-matrix (vector-take (transpose 3vt-matrix) 256))))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get syn-v2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix syn-v2
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define syn-h2 #f)

(define synthesis-h2
  (new button%
       [parent synthesis-panel]
       [label "SyH2"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 256))
          (set! syn-h2 (synthetize-matrix (vector-take 3vt-matrix 256)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get syn-h2 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix syn-h2
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define syn-v1 #f)

(define synthesis-v1
  (new button%
       [parent synthesis-panel]
       [label "SyV1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 512))
          (set! syn-v1 (transpose (synthetize-matrix (vector-take (transpose 3vt-matrix) 512))))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get syn-v1 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix syn-v1
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))

(define syn-h1 #f)

(define synthesis-h1
  (new button%
       [parent synthesis-panel]
       [label "SyH1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 512))
          (send y-field set-value (number->string 512))
          (set! syn-h1 (synthetize-matrix (vector-take 3vt-matrix 512)))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get syn-h1 i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix syn-h1
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))