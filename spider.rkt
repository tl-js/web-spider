#lang plai
(require net/url)
(require 2htdp/batch-io)

(define q (box '()))
(define title-holder (box ""))

(define crawled (make-hash))

(define (push elem)
  (set-box! q (append (unbox q) (list elem))))

(define (pop)
  (local [(define front (car (unbox q)))]
    (set-box! q (cdr (unbox q))) front))

(define (full?)
  (if (equal? 0 (length (unbox q))) #f #t))
