#lang racket/gui
(require "FC.rkt" "../helpers/bitwr.rkt" plot)

(define frame
  (new frame%
       [label "Fractal Coding"]
       [x 100] [y 50]
       [width 1372] [height 730]))

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

(define encode-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (and (send event button-down? 'left) original-matrix founds)
        (define range-x (quotient (- (send event get-y) 20) 8))
        (define range-y (quotient (- (send event get-x) 20) 8))
        (when (and (>= range-x 0) (< range-x 64) (>= range-y 0) (< range-y 64))
          (set! original-matrix (get-matrix encode-buffer))
          (define r-matrix (get-block original-matrix range-x range-y 8))
          (send range-bitmap set-argb-pixels 0 0 80 80 (matrix->bytes r-matrix))
          (send range-canvas on-paint)
          
          (define details (list-ref founds (+ (* range-x 8) range-y)))
          (define iso (remainder (first details) 8))
          (define domain-xy (quotient (first details) 8))
          (define domain-x (quotient domain-xy 63))
          (define domain-y (remainder domain-xy 63))
          (send xd-field set-value (number->string domain-x))
          (send yd-field set-value (number->string domain-y))
          (send iso-field set-value (number->string iso))
          (send s-field set-value (number->string (second details)))
          (send o-field set-value (number->string (third details)))
          
          (define d-matrix (get-block original-matrix domain-x domain-y 16))
          (send domain-bitmap set-argb-pixels 0 0 160 160 (matrix->bytes d-matrix))
          (send domain-canvas on-paint)
          (set! original-matrix (whiterize original-matrix range-x range-y 8))
          (set! original-matrix (whiterize original-matrix domain-x domain-y 16))
          (send encode-bitmap set-argb-pixels 0 0 SIZE SIZE (matrix->bytes original-matrix))
          (send encode-canvas on-paint))))))

(define encode-canvas
  (new encode-canvas%
       [parent encode-panel]
       [min-width SIZE]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap encode-bitmap 20 20))]))

(define gauge-process
  (new gauge%
       [parent encode-panel]
       [label ""]
       [range 1024]
       [horiz-margin 100]))

(define ranges #f)
(define domains #f)
(define image-name #f)
(define encode-buffer (make-bytes (* SIZE SIZE 4)))
(define original-matrix #f)

(define load-button-encode
  (new button%
       [parent encode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../FC/utils" #f #f null '(("bmp" "*.bmp"))))
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
                   (let ([f (future (λ() (search-ranges (take (drop ranges 1024) 1024) domains)))]
                         [g (future (λ() (search-ranges (take (drop ranges 2048) 1024) domains)))]
                         [h (future (λ() (search-ranges (take (drop ranges 3072) 1024) domains)))])
                     (apply append (list (search-ranges (take ranges 1024) domains gauge-process)
                                         (touch f) (touch g) (touch h))))))))]))

(define save-button
  (new button%
       [parent encode-panel]
       [label "Save"]
       [callback
        (λ (button event)
          (when founds
            (define writer (new bit-writer% [path (string-append image-name ".fc")]))
            (for ([i founds])
              (send writer write-bits (first i) 15)
              (send writer write-bits (second i) 5)
              (send writer write-bits (third i) 7))
            (send writer write-bits 0 7)
            (send writer close-file)))]))

;------------------------------------CLICK PANEL----------------------------------------

(define middle-panel
  (new vertical-panel%
       [parent main-panel]))

(define click-panel
  (new horizontal-panel%
       [parent middle-panel]))

(define details-panel
  (new vertical-panel%
       [parent middle-panel]))

(define fill-panel
  (new vertical-panel%
       [parent middle-panel]))

(define range-bitmap (make-bitmap 80 80))
(define range-dc (send range-bitmap make-dc))
(send range-dc set-background (make-color 0 0 0))
(send range-dc clear)

(define range-canvas
  (new canvas%
       [parent click-panel]
       [min-width 80]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap range-bitmap 20 20))]))

(define domain-bitmap (make-bitmap 160 160))
(define domain-dc (send domain-bitmap make-dc))
(send domain-dc set-background (make-color 0 0 0))
(send domain-dc clear)

(define domain-canvas
  (new canvas%
       [parent click-panel]
       [min-width 160]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap domain-bitmap 20 20))]))

(define xd-field
  (new text-field%
       [parent details-panel]
       [label "Xd:"]
       [horiz-margin 60]
       [init-value ""]))

(define yd-field
  (new text-field%
       [parent details-panel]
       [label "Yd:"]
       [horiz-margin 60]
       [init-value ""]))

(define iso-field
  (new text-field%
       [parent details-panel]
       [label "ISO:"]
       [horiz-margin 60]
       [init-value ""]))

(define s-field
  (new text-field%
       [parent details-panel]
       [label "S:"]
       [horiz-margin 60]
       [init-value ""]))

(define o-field
  (new text-field%
       [parent details-panel]
       [label "O:"]
       [horiz-margin 60]
       [init-value ""]))



;------------------------------------DECODE PANEL----------------------------------------

(define decode-panel
  (new vertical-panel%
       [parent main-panel]))

(define decode-bitmap (make-bitmap SIZE SIZE))
(define decode-dc (send decode-bitmap make-dc))
(send decode-dc set-background (make-color 0 0 0))
(send decode-dc clear)

(define decode-canvas%
  (class canvas%
    (super-new)
    (define/override (on-event event)
      (when (and (send event button-down? 'left) new-matrix founds)
        (define range-x (quotient (- (send event get-y) 20) 8))
        (define range-y (quotient (- (send event get-x) 20) 8))
        (when (and (>= range-x 0) (< range-x SIZE) (>= range-y 0) (< range-y SIZE))
          (define r-matrix (get-block new-matrix range-x range-y 8))
          (send range-bitmap set-argb-pixels 0 0 80 80 (matrix->bytes r-matrix))
          (send range-canvas on-paint)
          
          (define details (list-ref founds (+ (* range-x 8) range-y)))
          (define iso (remainder (first details) 8))
          (define domain-xy (quotient (first details) 8))
          (define domain-x (quotient domain-xy 63))
          (define domain-y (remainder domain-xy 63))
          (send xd-field set-value
                (string-join
                 (map number->string
                      (list domain-x domain-y iso (second details) (third details)))))
          
          (define d-matrix (get-block new-matrix domain-x domain-y 16))
          (send domain-bitmap set-argb-pixels 0 0 160 160 (matrix->bytes d-matrix))
          (send domain-canvas on-paint))))))

(define decode-canvas
  (new decode-canvas%
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
          (define path (get-file #f #f "../FC/utils" #f #f null '(("bmp" "*.bmp"))))
          (when path
            (set! decode-bitmap (read-bitmap path))
            (send decode-bitmap get-argb-pixels 0 0 SIZE SIZE decode-buffer)
            (set! new-matrix (get-matrix decode-buffer))
            (send decode-canvas on-paint)))]))

(define d/image-name #f)
(define load-founds
  (new button%
       [parent decode-panel]
       [label "Initialize"]
       [callback
        (λ (button event)
          (define path (get-file #f #f "../FC" #f #f null '(("fc" "*.fc"))))
          (when path
            (define reader (new bit-reader% [path path]))
            (set! founds
                  (for/list ([i (sqr 64)])
                    (define domain (send reader read-bits 15))
                    (define s (dequantize-s (send reader read-bits 5)))
                    (define o (dequantize-o (send reader read-bits 7) s))
                    (when (> s 0) (set! o (- o (* s 255.0))))
                    (list domain s o)))
            (send reader close-file)))]))

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
              (send psnr-field set-value (number->string (PSNR original-matrix new-matrix)))
              (send mae-field set-value (number->string (MAE original-matrix new-matrix))))
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
       [horiz-margin 150]
       [init-value ""]))

(define mae-field
  (new text-field%
       [parent decode-panel]
       [label "MAE: "]
       [horiz-margin 150]
       [init-value ""]))

(define (get-histogram original decoded [scaling-factor 1])
  (set! original (vector->list (flatten-matrix original)))
  (set! decoded (vector->list (flatten-matrix decoded)))
  (define (vector-increment vec i)
    (vector-set! vec i (add1 (vector-ref vec i))))
  (define temp (make-vector 256))
  (for ([i (map (λ(x y) (abs (- x y))) original decoded)])
    (vector-increment temp i))
  (vector-map vector
              (make-vector 256)
              (vector-map (λ(x) (* x scaling-factor)) temp)))

(define (show-histogram)
  (plot-new-window? #t)
  (plot
   (discrete-histogram
    (get-histogram original-matrix new-matrix)
    #:y-max 65536 #:add-ticks? #t)
   #:x-label #f #:y-label #f))

(define histogram-button
  (new button%
       [parent decode-panel]
       [label "Histogram"]
       [callback
        (λ (button event)
          (show-histogram))]))