#lang racket/gui
(require "bitwr.rkt")

(define br #f)
(define bw #f)

(define frame (new frame%
                   [label "Bit Reader & Writer"]
                   [x 500]
                   [y 100]
                   [width 1000]
                   [height 500]))

(define load-ibutton (new button%
                          [parent frame] [label "Load Input File"]
                          [callback (λ(button event)
                                      (set! br (new bit-reader% [path "test.txt"])))]))

(define load-obutton (new button%
                          [parent frame] [label "Load Output File"]
                          [callback (λ(button event)
                                      (set! bw (new bit-writer% [path "testo.txt"])))]))

(define display (new editor-canvas% [parent frame]))

(define start (new button%
                   [parent frame] [label "Start"]
                   [callback (λ(button event)
                               (test-file (* 8 (file-size "test.txt"))))]))

(define text (new text%))
(send display set-editor text)

(define (test-file file-length)
  (let loop ([nbr file-length])
    (cond
      [(= nbr 0) (send bw write-bits 0 7)]
      [else
       (define nb (random 1 256))
       (when (> nb nbr) (set! nb nbr))
       (define val (send br read-bits nb))
       (send bw write-bits val nb)
       (send text insert
             (string-append "Random = " (number->string nb)
                            " -> Value = " (number->string val)
                            " -> Binarized = " (binarize val nb)
                            " -> Write Buffer = " (number->string (send bw get-buffer))
                            " -> Binarized = " (binarize (send bw get-buffer)) "\n"))
       (loop (- nbr nb))])))


(define verify (new button%
                    [parent frame] [label "Verify"]
                    [callback (λ(button event)
                                (send text insert (if (clean-up) "Ok" "Not ok")))]))

(define (clean-up)
  (send br close-file)
  (send bw close-file)
  (equal? (file->string "test.txt") (file->string "testo.txt")))

(send frame show #t)
