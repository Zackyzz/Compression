#lang racket/gui
(require "Wavelet.rkt" binaryio)

(define frame
  (new frame%
       [label "Wavelet Decomposition"]
       [x 40] [y 100]
       [width 1450] [height 625]))

(send frame show #t)

(define main-panel (new horizontal-panel% [parent frame]))

;------------------------------------INPUT PANEL----------------------------------------

(define input-panel (new vertical-panel% [parent main-panel]))

(define input-bitmap (make-bitmap SIZE SIZE))
(define input-dc (send input-bitmap make-dc))
(send input-dc set-background (make-color 0 0 0))
(send input-dc clear)

(define input-canvas
  (new canvas%
       [parent input-panel] [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap input-bitmap 20 20))]))

(define image-name #f)
(define input-buffer (make-bytes (* SIZE SIZE 4)))
(define 3vt-matrix #f)

(define load-button-input
  (new button%
       [parent input-panel] [label "Load image"]
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

(define 3vt-panel (new vertical-panel% [parent main-panel]))

(define 3vt-bitmap (make-bitmap SIZE SIZE))
(define 3vt-dc (send 3vt-bitmap make-dc))
(send 3vt-dc set-background (make-color 0 0 0))
(send 3vt-dc clear)

(define 3vt-canvas
  (new canvas%
       [parent 3vt-panel] [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap 3vt-bitmap 20 20))]))

(define load-3vt
  (new button%
       [parent 3vt-panel] [label "Load wavelet"]
       [callback
        (λ (button event)
          (define file-name (get-file #f #f "../WaveletDecomposition/utils" #f #f null '(("wvt" "*.3vt"))))
          (define in (open-input-file file-name))
          (when file-name
            (set! 3vt-matrix
                  (for/vector ([i 512])
                    (for/vector ([i 512])
                      (read-float 8 in))))
            (close-input-port in)))]))

(define save-3vt
  (new button%
       [parent 3vt-panel] [label "Save wavelet"]
       [callback
        (λ (button event)
          (when image-name
            (define out (open-output-file (string-append "utils/" image-name ".3vt") #:exists 'replace))
            (when (and out 3vt-matrix)
              (for ([i (flatten-matrix 3vt-matrix)])
                (write-float i 8 out))
              (close-output-port out))))]))

;------------------------------------BUTTONS PANEL----------------------------------------

(define buttons-panel (new vertical-panel% [parent main-panel] [vert-margin 20]))
(define scale-field (new text-field% [parent buttons-panel] [label "s: "] [init-value "5.7"]))
(define offset-field (new text-field% [parent buttons-panel] [label "o: "] [init-value "128"]))
(define x-field (new text-field% [parent buttons-panel] [label "x: "] [init-value "0"]))
(define y-field (new text-field% [parent buttons-panel] [label "y: "] [init-value "0"]))

(define 3vt-refresh
  (new button%
       [parent buttons-panel] [label "Refresh Wavelet:"] [horiz-margin 10]
       [callback
        (λ (button event)
          (when 3vt-matrix
            (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (matrix->bytes 3vt-matrix
                                 (string->number (send scale-field get-value))
                                 (string->number (send offset-field get-value))
                                 (string->number (send x-field get-value))
                                 (string->number (send y-field get-value))))
            (send 3vt-canvas on-paint)))]))

(define 3vt-error
  (new button%
       [parent buttons-panel] [label "Chebyshev distance:"] [horiz-margin 20]
       [callback
        (λ (button event)
          (when 3vt-matrix
            (define original (vector->list (flatten-matrix (get-matrix input-buffer))))
            (define 3vt (vector->list (vector-map exact-round (flatten-matrix 3vt-matrix))))
            (send min-error set-value (number->string (apply min (map - original 3vt))))
            (send max-error set-value (number->string (apply max (map - original 3vt))))))]))

(define min-error (new text-field% [parent buttons-panel] [label "Min:"] [init-value ""]))
(define max-error (new text-field% [parent buttons-panel] [label "Max:"] [init-value ""]))

;------------------------------------ANALYSIS + SYNTHETIS PANELS----------------------------------------

(define analysis-panel (new vertical-panel% [parent main-panel] [vert-margin 20]))
(define synthesis-panel (new vertical-panel% [parent main-panel] [vert-margin 20]))

(define analysis-buttons
  (for/vector ([i 14])
    (new button%
         [parent analysis-panel]
         [label (string-append "An" (if (even? i) "H" "V") (number->string (+ 1 (quotient i 2))))]
         [callback
          (λ (button event)
            (define size (quotient SIZE (expt 2 (quotient i 2))))
            (send x-field set-value (number->string (quotient size 2)))
            (send y-field set-value (number->string (if (odd? i) (quotient size 2) size)))
            (if (even? i)
                (overwrite 3vt-matrix (make analysis (modify 3vt-matrix size #f)))
                (overwrite 3vt-matrix (modify (make analysis (modify 3vt-matrix size)) size)))
            (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (matrix->bytes 3vt-matrix
                                 (string->number (send scale-field get-value))
                                 (string->number (send offset-field get-value))
                                 (string->number (send x-field get-value))
                                 (string->number (send y-field get-value))))
            (send 3vt-canvas on-paint))])))

(define synthesis-buttons
  (for/vector ([i 14])
    (new button%
         [parent synthesis-panel]
         [label (string-append "Sy" (if (even? i) "H" "V") (number->string (+ 1 (quotient i 2))))]
         [callback
          (λ (button event)
            (define size (quotient SIZE (expt 2 (quotient i 2))))
            (send x-field set-value (number->string (if (even? i) size (quotient size 2))))
            (send y-field set-value (number->string size))
            (if (even? i)
                (overwrite 3vt-matrix (make synthesis (modify 3vt-matrix size #f)))
                (overwrite 3vt-matrix (modify (make synthesis (modify 3vt-matrix size)) size)))
            (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (matrix->bytes 3vt-matrix
                                 (string->number (send scale-field get-value))
                                 (string->number (send offset-field get-value))
                                 (string->number (send x-field get-value))
                                 (string->number (send y-field get-value))))
            (send 3vt-canvas on-paint))])))

(define analysis-button
  (new button%
       [parent buttons-panel]
       [label "Analysis"]
       [callback
        (λ (button event)
          (for ([i (* 2 (string->number (send level-field get-value)))])
            (send (vector-ref analysis-buttons i) command 'button)))]))

(define synthesis-button
  (new button%
       [parent buttons-panel]
       [label "Synthetis"]
       [callback
        (λ (button event)
          (define limit (sub1 (vector-length synthesis-buttons)))
          (for ([i (* 2 (string->number (send level-field get-value)))])
            (send (vector-ref synthesis-buttons (- limit i)) command 'button)))]))

(define level-field (new text-field% [parent buttons-panel] [label "Level: "] [init-value "5"]))