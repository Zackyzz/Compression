#lang racket/gui
(require plot "../helpers/bitwr.rkt" "NearLossless.rkt" "AC.rkt")

(define frame
  (new frame%
       [label "Near-Lossless Predictive Coding"]
       [x 250] [y 250]
       [width 1450] [height 525]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------ENCODE PANEL------------------------------------

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
(define original-matrix (get-matrix input-buffer))
(define load-button
  (new button%
       [parent input-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../PredictiveCoding/utils" #f #f null '(("bmp" "*.bmp"))))
          (when path
            (set! image-name (last (string-split (path->string path) "\\")))
            (set! input-bitmap (read-bitmap path))
            (send input-canvas on-paint)
            (send input-bitmap get-argb-pixels 0 0 SIZE SIZE input-buffer)
            (set! original-matrix (get-matrix input-buffer))))]))

(define matrices #f)
(define encode-button
  (new button%
       [parent input-panel]
       [label "Encode"]
       [callback
        (λ (button event)
          (when image-name
            (define coder
              (new coder%
                   [original-matrix original-matrix]
                   [size SIZE] [range 255] [k (send k get-value)]
                   [predictor (list-ref predictors-list (send predictors get-selection))]))
            (set! matrices (send coder get-matrices))
            (set! quantized-values (flatten-matrix (vector-ref matrices 2)))))]))


(define predictors
  (new choice%
       [parent input-panel]
       [label ""]
       [choices (list "->128" "A" "B" "C" "A+B-C" "A+B/2-C/2"
                      "B+A/2-C/2" "A/2+B/2" "jpeg-ls")]))

(define k
  (new slider%
       [parent input-panel]
       [label "k"]
       [horiz-margin 50]
       [min-value 0]
       [max-value 15]
       [init-value 2]))

(define writer #f)
(define quantized-values #f)
(define save-fixed
  (new button%
       [parent input-panel]
       [label "Save Fixed"]
       [callback
        (λ (button event)
          (when quantized-values
            (define save-name (string-append "utils/" image-name ".k" (number->string (send k get-value))
                                             "p" (number->string (send predictors get-selection)) "F.nl"))
            (set! writer (new bit-writer% [path save-name]))
            (send writer write-bits (send k get-value) 4)
            (send writer write-bits (send predictors get-selection) 4)
            (send writer write-bits 0 2)
            (for ([i quantized-values]) (send writer write-bits (+ 255 i) 9))
            (send writer write-bits 0 7)
            (send writer close-file)))]))

(define save-table
  (new button%
       [parent input-panel]
       [label "Save JPEG"]
       [callback
        (λ (button event)
          (when quantized-values
            (define save-name (string-append "utils/" image-name ".k" (number->string (send k get-value))
                                             "p" (number->string (send predictors get-selection)) "T.nl"))
            (set! writer (new bit-writer% [path save-name]))
            (send writer write-bits (send k get-value) 4)
            (send writer write-bits (send predictors get-selection) 4)
            (send writer write-bits 1 2)
            (for ([i quantized-values])
              (cond
                [(= i 0) (send writer write-bit 0)]
                [else
                 (define len (add1 (exact-floor (log (abs i) 2))))
                 (send writer write-bits (- (expt 2 (add1 len)) 2) (add1 len))
                 (send writer write-bits (if (positive? i) i (+ i (sub1 (expt 2 len)))) len)]))
            (send writer write-bits 0 7)
            (send writer close-file)))]))

(define save-arithmetic
  (new button%
       [parent input-panel]
       [label "Save Arithmetic"]
       [callback
        (λ (button event)
          (when quantized-values
            (define save-name (string-append "utils/" image-name ".k" (number->string (send k get-value))
                                             "p" (number->string (send predictors get-selection)) "A.nl"))
            (arithmetic-encode (map (λ(x) (+ x 255)) (vector->list quantized-values))
                               (list (send k get-value) (send predictors get-selection)) save-name)))]))

;------------------------------------TAB PANEL------------------------------------

(define tab-panel
  (new tab-panel%
       [parent main-panel] [choices (list "Coder" "Decoder")]
       [callback
        (λ (tp e)
          (case (send tp get-selection)
            [(0) (send tp change-children (λ(x) (list coder-panel)))]
            [(1) (send tp change-children (λ(x) (list decoder-panel)))]))]))

(define coder-panel
  (new horizontal-panel%
       [parent tab-panel]))

(define decoder-panel
  (new horizontal-panel%
       [parent tab-panel]))

(send tab-panel change-children (λ(x) (list coder-panel)))

;------------------------------------CODER HISTOGRAM PANEL------------------------------------

(define histogram-panel
  (new vertical-panel%
       [parent coder-panel]))

(define show-plot #f)
(define histogram-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (and (send event button-down? 'left) show-plot)
        (plot-new-window? #t)
        (plot
         (discrete-histogram
          (get-histogram
           (vector-ref matrices (send histogram-choices get-selection))
           (string->number (send histogram-scale get-value)))
          #:y-max 100 #:gap 0)
         #:x-label #f #:y-label #f)
        (set! show-plot #f)))))

(define histogram-pixels (get-histogram original-matrix))
(define histogram-canvas
  (new histogram-canvas%
       [parent histogram-panel]
       [min-width (* 2 SIZE)]
       [paint-callback
        (λ(canvas dc)
          (plot/dc (discrete-histogram histogram-pixels #:y-max 100 #:add-ticks? #f #:gap 0)
                   dc 0 20 (* 2 SIZE) SIZE #:x-label "0" #:y-label #f))]))

(define refresh-histogram
  (new button%
       [parent histogram-panel]
       [label "Refresh Histogram (C)"]
       [callback
        (λ (button event)
          (when matrices
            (set! show-plot #t)
            (set! histogram-pixels (get-histogram
                                    (vector-ref matrices (send histogram-choices get-selection))
                                    (string->number (send histogram-scale get-value))))
            (send histogram-canvas on-paint)))]))

(define histogram-choices
  (new choice%
       [parent histogram-panel]
       [label ""]
       [choices (list "Original" "Error Pred" "Error Pred Q" "Decoded")]))

(define histogram-scale
  (new text-field%
       [parent histogram-panel]
       [label ""]
       [horiz-margin 225]
       [init-value "0.02"]))

;------------------------------------CODER ERROR PANEL------------------------------------

(define error-panel
  (new vertical-panel%
       [parent coder-panel]))

(define error-bitmap (make-bitmap SIZE SIZE))
(define error-dc (send error-bitmap make-dc))
(send error-dc set-background (make-color 0 0 0))
(send error-dc clear)

(define error-canvas
  (new canvas%
       [parent error-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap error-bitmap 20 20))]))

(define refresh-error-image
  (new button%
       [parent error-panel]
       [label "Refresh Error Image (C)"]
       [callback
        (λ (button event)
          (when matrices
            (send error-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (matrix->bytes (vector-ref matrices (add1 (send error-choices get-selection)))
                                 (string->number (send error-scale get-value)))))
          (send error-canvas on-paint))]))

(define error-choices
  (new radio-box%
       [parent error-panel]
       [label ""]
       [choices (list "Error prediction" "Q Error prediction")]))

(define error-scale
  (new text-field%
       [parent error-panel]
       [label ""]
       [horiz-margin 100]
       [init-value "7.5"]))
          
;------------------------------------DECODE PANEL------------------------------------

(define decode-panel
  (new vertical-panel%
       [parent main-panel]))

(define decode-bitmap (make-bitmap SIZE SIZE))
(define decode-dc (send decode-bitmap make-dc))
(send decode-dc set-background (make-color 0 0 0))
(send decode-dc clear)

(define decoder-canvas
  (new canvas%
       [parent decode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap decode-bitmap 20 20))]))

(define d/reader #f)
(define d/quantized-matrix #f)
(define d/k #f)
(define d/p #f)
(define d/s #f)
(define d/image-name #f)

(define d/load-button
  (new button%
       [parent decode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../PredictiveCoding/utils" #f #f null '(("nl" "*.nl"))))
          (when path
            (set! d/image-name (last (string-split (path->string path) "\\")))
            (set! d/reader (new bit-reader% [path path]))
            (set! d/k (send d/reader read-bits 4))
            (set! d/p (send d/reader read-bits 4))
            (set! d/s (send d/reader read-bits 2))
            (cond
              [(= 0 d/s)
               (set! d/quantized-matrix
                     (for/vector ([i SIZE])
                       (for/vector ([i SIZE])
                         (- (send d/reader read-bits 9) 255))))
               (send d/reader close-file)]
              [(= 1 d/s)
               (set! d/quantized-matrix
                     (for/vector ([i SIZE])
                       (for/vector ([i SIZE])
                         (cond
                           [(= 0 (send d/reader read-bit)) 0]
                           [else
                            (define len
                              (let loop ([it 1])
                                (if (= 0 (send d/reader read-bit))
                                    it
                                    (loop (add1 it)))))
                            (define val (send d/reader read-bits len))
                            (if (< val (expt 2 (sub1 len)))
                                (- val (sub1 (expt 2 len)))
                                val)]))))
               (send d/reader close-file)]
              [(= 2 d/s)
               (define temp-matrix (list->vector (arithmetic-decode d/reader)))
               (set! d/quantized-matrix
                     (for/vector ([i SIZE])
                       (for/vector ([j SIZE])
                         (- (vector-ref temp-matrix (+ (* i SIZE) j)) 255))))])))]))

(define d/matrices #f)
(define decode-button
  (new button%
       [parent decode-panel]
       [label "Decode"]
       [callback
        (λ (button event)
          (when d/quantized-matrix
            (define decoder
              (new decoder%
                   [compressed-matrix d/quantized-matrix]
                   [size SIZE] [range 255] [k d/k]
                   [predictor (list-ref predictors-list d/p)]))
            (set! d/matrices (send decoder get-matrices))
            (send decode-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (decoded->bytes (vector-ref d/matrices 2)))
            (send decoder-canvas on-paint)))]))

(define d/save-button
  (new button%
       [parent decode-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when d/image-name
            (send decode-bitmap save-file (string-append "utils/" d/image-name ".bmp") 'bmp)))]))

(define d/error-button
  (new button%
       [parent decode-panel]
       [label "Chebyshev distance:"]
       [callback
        (λ (button event)
          (when (and matrices d/matrices)
            (define original (vector->list (flatten-matrix (vector-ref matrices 0))))
            (define decoded (vector->list (flatten-matrix (vector-ref d/matrices 2))))
            (send min-error set-value (number->string (apply min (map - original decoded))))
            (send max-error set-value (number->string (apply max (map - original decoded))))))]))

(define min-error
  (new text-field%
       [parent decode-panel]
       [label "Min:"]
       [horiz-margin 50]
       [init-value ""]))

(define max-error
  (new text-field%
       [parent decode-panel]
       [label "Max:"]
       [horiz-margin 50]
       [init-value ""]))

;------------------------------------DECODER HISTOGRAM PANEL------------------------------------

(define d/histogram-panel
  (new vertical-panel%
       [parent decoder-panel]))

(define d/histogram-pixels (get-histogram original-matrix))
(define d/histogram-canvas
  (new histogram-canvas%
       [parent d/histogram-panel]
       [min-width (* 2 SIZE)]
       [paint-callback
        (λ(canvas dc)
          (plot/dc (discrete-histogram d/histogram-pixels #:y-max 100 #:add-ticks? #f #:gap 0)
                   dc 0 20 (* 2 SIZE) SIZE #:x-label "0" #:y-label #f))]))

(define d/refresh-histogram
  (new button%
       [parent d/histogram-panel]
       [label "Refresh Histogram (D)"]
       [callback
        (λ (button event)
          (when d/matrices
            (set! show-plot #t)
            (set! d/histogram-pixels (get-histogram
                                      (vector-ref d/matrices (send d/histogram-choices get-selection))
                                      (string->number (send d/histogram-scale get-value))))
            (send d/histogram-canvas on-paint)))]))

(define d/histogram-choices
  (new choice%
       [parent d/histogram-panel]
       [label ""]
       [choices (list "Error Pred" "Error Pred Q" "Decoded")]))

(define d/histogram-scale
  (new text-field%
       [parent d/histogram-panel]
       [label ""]
       [horiz-margin 225]
       [init-value "0.02"]))

;------------------------------------DECODER ERROR PANEL------------------------------------

(define d/error-panel
  (new vertical-panel%
       [parent decoder-panel]))

(define d/error-bitmap (make-bitmap SIZE SIZE))
(define d/error-dc (send d/error-bitmap make-dc))
(send d/error-dc set-background (make-color 0 0 0))
(send d/error-dc clear)

(define d/error-canvas
  (new canvas%
       [parent d/error-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap d/error-bitmap 20 20))]))

(define d/refresh-error-image
  (new button%
       [parent d/error-panel]
       [label "Refresh Error Image (D)"]
       [callback
        (λ (button event)
          (when d/matrices
            (send d/error-bitmap set-argb-pixels 0 0 SIZE SIZE
                  (matrix->bytes (vector-ref d/matrices (send d/error-choices get-selection))
                                 (string->number (send d/error-scale get-value)))))
          (send d/error-canvas on-paint))]))

(define d/error-choices
  (new radio-box%
       [parent d/error-panel]
       [label ""]
       [choices (list "Error prediction" "Q Error prediction")]))

(define d/error-scale
  (new text-field%
       [parent d/error-panel]
       [label ""]
       [horiz-margin 100]
       [init-value "7.5"]))
