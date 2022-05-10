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

;------------------------------------ANALYSIS PANEL----------------------------------------

(define analysis-panel
  (new vertical-panel%
       [parent main-panel]
       [vert-margin 20]))

(define analysis #f)

(define analyse-h1
  (new button%
       [parent analysis-panel]
       [label "AnH1"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 256))
          (send y-field set-value (number->string 512))
          (set! analysis (analyse-matrix 3vt-matrix))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (transpose (analyse-matrix (transpose 3vt-matrix))))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (analyse-matrix (vector-take 3vt-matrix 256)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (transpose (analyse-matrix (vector-take (transpose 3vt-matrix) 256))))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               1))
          (send 3vt-canvas on-paint))]))

(define analyse-h3
  (new button%
       [parent analysis-panel]
       [label "AnH3"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 64))
          (send y-field set-value (number->string 128))
          (set! analysis (analyse-matrix (vector-take analysis 128)))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               2))
          (send 3vt-canvas on-paint))]))

(define analyse-v3
  (new button%
       [parent analysis-panel]
       [label "AnV3"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 64))
          (send y-field set-value (number->string 64))
          (set! analysis (transpose (analyse-matrix (vector-take (transpose analysis) 128))))
          (for ([i 128])
            (for ([j 128])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               2))
          (send 3vt-canvas on-paint))]))

(define analyse-h4
  (new button%
       [parent analysis-panel]
       [label "AnH4"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 32))
          (send y-field set-value (number->string 64))
          (set! analysis (analyse-matrix (vector-take analysis 64)))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               3))
          (send 3vt-canvas on-paint))]))

(define analyse-v4
  (new button%
       [parent analysis-panel]
       [label "AnV4"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 32))
          (send y-field set-value (number->string 32))
          (set! analysis (transpose (analyse-matrix (vector-take (transpose analysis) 64))))
          (for ([i 64])
            (for ([j 64])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               3))
          (send 3vt-canvas on-paint))]))

(define analyse-h5
  (new button%
       [parent analysis-panel]
       [label "AnH5"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 16))
          (send y-field set-value (number->string 32))
          (set! analysis (analyse-matrix (vector-take analysis 32)))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               4))
          (send 3vt-canvas on-paint))]))

(define analyse-v5
  (new button%
       [parent analysis-panel]
       [label "AnV5"]
       [callback
        (λ (button event)
          (send x-field set-value (number->string 16))
          (send y-field set-value (number->string 16))
          (set! analysis (transpose (analyse-matrix (vector-take (transpose analysis) 32))))
          (for ([i 32])
            (for ([j 32])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               4))
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
          (set! analysis (transpose (synthetize-matrix (vector-take (transpose 3vt-matrix) 256))))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (synthetize-matrix (vector-take 3vt-matrix 512)))
          (for ([i 256])
            (for ([j 256])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (transpose (synthetize-matrix (vector-take (transpose 3vt-matrix) 512))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
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
          (set! analysis (synthetize-matrix (vector-take analysis 512)))
          (for ([i 512])
            (for ([j 512])
              (matrix-set 3vt-matrix i j (matrix-get analysis i j))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                (matrix->bytes 3vt-matrix analysis
                               (string->number (send scale-field get-value))
                               (string->number (send offset-field get-value))
                               (string->number (send x-field get-value))
                               (string->number (send y-field get-value))
                               0))
          (send 3vt-canvas on-paint))]))