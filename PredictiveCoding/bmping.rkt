#lang racket/gui

(define bm (make-bitmap 256 256))

(define dc (send bm make-dc))
(send dc set-background (make-object color% 0 0 0))
(send dc clear)

(define buffer (make-bytes (* 256 256 4)))
(send dc get-argb-pixels 0 0 255 255 buffer)

#|
(define aquamarine (make-color 100 255 100))
(for ([i 255])
  (send dc set-pixel i i aquamarine))
|#

(define frame
  (new frame%
       [label "BMP"]
       [x 300] [y 100]
       [width 600] [height 600]))

(define hp
  (new horizontal-panel%
       [parent frame]))

(define encode-panel
  (new vertical-panel%
       [parent hp]))

(define canvas
  (new canvas%
       [parent encode-panel]
       [paint-callback
        (λ (canvas dc)
          (send dc draw-bitmap bm 0 0))]))

(define load-button
  (new button%
       [parent encode-panel]
       [label "Load file"]
       [callback
        (λ (button event)
          (define path (get-file))
          (when path
            (set! bm (read-bitmap path))
            (send canvas on-paint)))]))

(send frame show #t)

(define gray-pixels
  (time (for/vector ([i (in-range 1 (* 256 256 4) 4)])
          (bytes-ref buffer i))))

(vector-length gray-pixels)