#lang racket/gui
(require "Wavelet.rkt")

(define SIZE 512)

(define frame
  (new frame%
       [label "Wavelet Decomposition"]
       [x 350] [y 200]
       [width 1150] [height 720]))

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

(define (get-matrix buffer)
  (for/vector ([i SIZE])
    (for/vector ([j (in-range (add1 (* i 4 SIZE)) (add1 (* (add1 i) 4 SIZE)) 4)])
      (bytes-ref buffer j))))

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

(define (flatten-matrix matrix)
  (apply vector-append (vector->list matrix)))

(define (normalize x)
  (set! x (exact-round x))
  (cond [(< x 0) 0] [(> x 255) 255] [else x]))

(define (matrix->bytes matrix)
  (list->bytes (apply append (map (λ(x) (list 255 (normalize x) (normalize x) (normalize x)))
                                  (vector->list (flatten-matrix matrix))))))

(define analyse-h1
  (new button%
       [parent 3vt-panel]
       [label "AnH1"]
       [callback
        (λ (button event)
          (set! 3vt-matrix (time (analyse-matrix 3vt-matrix)))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE (time (matrix->bytes 3vt-matrix)))
          (send 3vt-canvas on-paint))]))

(define (transpose matrix)
  (apply vector-map vector (vector->list matrix)))

(define analyse-v1
  (new button%
       [parent 3vt-panel]
       [label "AnV1"]
       [callback
        (λ (button event)
          (set! 3vt-matrix (time (transpose (analyse-matrix (transpose 3vt-matrix)))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE (time (matrix->bytes 3vt-matrix)))
          (send 3vt-canvas on-paint))]))

(define synthesis-h1
  (new button%
       [parent 3vt-panel]
       [label "SyH1"]
       [callback
        (λ (button event)
          (set! 3vt-matrix (time (synthetize-matrix 3vt-matrix)))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE (time (matrix->bytes 3vt-matrix)))
          (send 3vt-canvas on-paint))]))

(define synthesis-v1
  (new button%
       [parent 3vt-panel]
       [label "SyV1"]
       [callback
        (λ (button event)
          (set! 3vt-matrix (time (transpose (synthetize-matrix (transpose 3vt-matrix)))))
          (send 3vt-bitmap set-argb-pixels 0 0 SIZE SIZE (time (matrix->bytes 3vt-matrix)))
          (send 3vt-canvas on-paint))]))