#lang racket/gui
(require "FractalCoding.rkt")

(define frame
  (new frame%
       [label "Fractal Coding"]
       [x 300] [y 150]
       [width 1124] [height 710]))

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
          (send dc draw-bitmap encode-bitmap 20 20))]))

(define ranges #f)
(define domains #f)
(define image-name #f)
(define encode-buffer (make-bytes (* SIZE SIZE 4)))
(define original-matrix #f)

(define gauge-process
  (new gauge%
       [parent encode-panel]
       [label ""]
       [range 2048]
       [horiz-margin 100]))

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
            (send gauge-process set-value 0)
            (send encode-bitmap get-argb-pixels 0 0 SIZE SIZE encode-buffer)
            (set! original-matrix (get-matrix encode-buffer))
            (set! ranges (get-ranges original-matrix))
            (set! domains (get-domains original-matrix))))]))

(define founds #f)
(define process-button
  (new button%
       [parent encode-panel]
       [label "Process"]
       [callback
        (λ (button event)
          (when (and ranges domains)
            (set! founds
                  (time
                   (let ([f (future (λ() (search-ranges (drop ranges 2048) domains)))])
                     (apply append (list (search-ranges (take ranges 2048) domains #t gauge-process)
                                         (touch f))))))))]))
                     
(define save-button
  (new button%
       [parent encode-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when founds
            (save-founds founds (string-append image-name ".txt"))))]))

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
          (send dc draw-bitmap decode-bitmap 20 20))]))

(define decode-buffer (make-bytes (* SIZE SIZE 4)))
(define new-matrix (get-matrix decode-buffer))
(define load-button-decode
  (new button%
       [parent decode-panel]
       [label "Load Original"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../FractalCoding/utils" #f #f null '(("bmp" "*.bmp"))))
          (when path
            (set! decode-bitmap (read-bitmap path))
            (send decode-bitmap get-argb-pixels 0 0 SIZE SIZE decode-buffer)
            (set! new-matrix (get-matrix decode-buffer))
            (send decode-canvas on-paint)))]))

(define d/image-name #f)
(define load-founds
  (new button%
       [parent decode-panel]
       [label "Preinitialize"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../FractalCoding" #f #f null '(("txt" "*.txt"))))
          (when path
            (set! d/image-name (last (string-split (path->string path) "\\")))
            (set! founds (read-founds path))))]))

(define decode-button
  (new button%
       [parent decode-panel]
       [label "Decode"]
       [callback
        (λ (button event)
          (when founds
            (define blocks (decode founds (get-decoding-domains new-matrix)))
            (set! new-matrix (blocks->image-matrix blocks))
            (when original-matrix
              (send psnr-field set-value (number->string (PSNR original-matrix new-matrix))))
            (send decode-bitmap set-argb-pixels 0 0 SIZE SIZE (matrix->bytes new-matrix))
            (send decode-canvas on-paint)))]))

(define d/save-button
  (new button%
       [parent decode-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when d/image-name
            (send decode-bitmap save-file (string-append "utils/" d/image-name ".bmp") 'bmp)))]))

(define psnr-field
  (new text-field%
       [parent decode-panel]
       [label "PSNR:"]
       [horiz-margin 200]
       [init-value ""]))
