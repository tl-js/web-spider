#lang plai
(require net/url)
(require 2htdp/batch-io)
(define test "http://people.eecs.ku.edu/~cratnapa/sample.html")

;; Exclude these characters from the source
(define specialChars '(#\" #\' #\% #\& #\, #\; #\@ #\\ #\] #\^ #\` #\/ #\newline))

;; Queue data structure
(define q (box '()))

;;Box for Titles
(define t (box ""))

;; Push to the queue
(define (push x)
  (set-box! q (append (unbox q) (list x))))

;; Pop from the front of the queue
(define (pop)
  (begin0
    (car (unbox q))
    (set-box! q (cdr (unbox q)))))

;; Returns true if the queue has at least 1 element
(define (full?)
  (not (empty? (unbox q))))

;; Brings HTML source from a given page
(define (fetch-source url)
(port->string (get-pure-port (string->url url))))


;; Adds spaces
(define (add-spaces chars)
  (if (empty? chars)
      '()
      (if (equal? (car chars) #\>)
        (append (cons (car chars) '(#\space) ) (add-spaces (cdr chars)))
        (if (equal? (car chars) #\<)
            (cons #\space (cons (car chars) (add-spaces (cdr chars))))      
            (cons (car chars) (add-spaces (cdr chars)))))))

;; Breaks string into tokens (#\space #\tab)
;; Also used to split url into pieces ( #\" #\')
(define (tokenize-string str a b)
  (define in (open-input-string str))
  (let recur ((out (open-output-string)))
    (define c (read-char in))
    (cond ((eof-object? c)
           (list (get-output-string out)))
          ((or  (char=? c a) (char=? c b))
           (cons (get-output-string out)
                 (recur (open-output-string))))
          (else (write-char c out) (recur out)))))

;; Tokenize - x is the URL
(define (tokens x)
  (tokenize-string (list->string(add-spaces (string->list (fetch-source x)))) #\space #\tab))

;; Extracts Title from the URL
(define (title x)
  (if (empty? x)
      "Untitled"
      (if (equal? (car x) "<title>")
          (title-helper (cdr x))
          (title (cdr x)))))

(define (title-helper x)
  (if (empty? x)
      "error"
      (if (equal? (car x) "</title>")
          (begin0
            (unbox t)
            (set-box! t ""))
          (begin
            (set-box! t (string-append (string-append (unbox t) " ") (car x)))
            (title-helper (cdr x))))))

;; Extracts Title from the URL
(define (title x)
  (if (empty? x)
      "Untitled"
      (if (equal? (car x) "<title>")
          (title-helper (cdr x))
          (title (cdr x)))))

(define (title-helper x)
  (if (empty? x)
      "error"
      (if (equal? (car x) "</title>")
          (begin0
            (unbox t)
            (set-box! t ""))
          (begin
            (set-box! t (string-append (string-append (unbox t) " ") (car x)))
            (title-helper (cdr x))))))
