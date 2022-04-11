#lang racket/gui
(require "../helpers/bitwr.rkt" "FractalCoding.rkt")

(define frame
  (new frame%
       [label "Fractal Coding"]
       [x 300] [y 150]
       [height 648]))

(send frame show #t)

(define main-panel
  (new horizontal-panel%
       [parent frame]))

;------------------------------------ENCODE PANEL----------------------------------------

(define encode-panel
  (new vertical-panel%
       [parent main-panel]))

(define encode-bitmap (make-bitmap SIZE SIZE))
(define encode-dc (send encode-bitmap make-dc))
(send encode-dc set-background (make-color 0 0 0))
(send encode-dc clear)

(define encode-canvas
  (new canvas%
       [parent encode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap encode-bitmap 0 0))]))

(define image-name #f)
(define encode-buffer (make-bytes (* SIZE SIZE 4)))
(define original-matrix (get-matrix encode-buffer))
(define load-button-encode
  (new button%
       [parent encode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../FractalCoding/utils" #f #f null '(("bmp" "*.bmp"))))
          (when path
            (set! image-name (last (string-split (path->string path) "\\")))
            (set! encode-bitmap (read-bitmap path))
            (send encode-canvas on-paint)
            (send encode-bitmap get-argb-pixels 0 0 SIZE SIZE encode-buffer)
            (set! original-matrix (get-matrix encode-buffer))
            (set! ranges (get-ranges original-matrix))
            (set! domains (get-domains original-matrix))))]))

(define ranges (get-ranges original-matrix))
(define domains (get-domains original-matrix))

;------------------------------------DECODE PANEL----------------------------------------

(define decode-panel
  (new vertical-panel%
       [parent main-panel]))

(define decode-bitmap (make-bitmap SIZE SIZE))
(define decode-dc (send decode-bitmap make-dc))
(send decode-dc set-background (make-color 0 0 0))
(send decode-dc clear)

(define decode-canvas
  (new canvas%
       [parent decode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap decode-bitmap 0 0))]))

(define load-button-decode
  (new button%
       [parent decode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file))
          (when path
            (set! decode-bitmap (read-bitmap path))
            (send decode-canvas on-paint)))]))

;------------------------------------TEST PANEL----------------------------------------

(define test-panel
  (new vertical-panel%
       [parent main-panel]))

(define test-bitmap (make-bitmap SIZE SIZE))
(define test-dc (send test-bitmap make-dc))
(send test-dc set-background (make-color 0 0 0))
(send test-dc clear)

(define test-canvas
  (new canvas%
       [parent test-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap test-bitmap 0 0))]))

(define new-matrix original-matrix)
(define iso-button
  (new button%
       [parent test-panel]
       [label "Iso"]
       [callback
        (λ (button event)
          (define founds (read-founds))
          (define ds (get-decoding-domains new-matrix))
          (define blocks (decode founds ds))
          (set! new-matrix (blocks->image-matrix blocks))
          (send test-bitmap set-argb-pixels 0 0 SIZE SIZE (matrix->bytes new-matrix))
          (send test-canvas on-paint))]))
