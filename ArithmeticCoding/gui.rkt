#lang racket/gui
(require racket/format)
(require "ArithmeticEncoder.rkt")

(define (binarize val [len 8])
  (~r val #:base 2 #:min-width len #:pad-string "0"))

(arithmetic-encode "files/testw.txt" "test_out.txt")