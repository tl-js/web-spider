#lang plai
(require net/url)
(require 2htdp/batch-io)

(define-type Page
  (orphan)
  (info (url string?) (parent Page?)))

;; Special Characters to be excluded from the words
(define specialChars '(#\" #\' #\% #\& #\, #\; #\@ #\\ #\] #\^ #\` #\/ #\newline))


(define q (box '()))
(define title-holder (box ""))
(define page-count (box 0))

(define crawled (make-hash))
(define downloaded-pages (make-hash))

;; Push to the end of the queue
(define (push elem)
  (set-box! q (append (unbox q) (list elem))))

;; Pop from the front of the queue
(define (pop)
  (local [(define front (car (unbox q)))]
    (set-box! q (cdr (unbox q))) front))

;; Returns false if the queue empty
(define (full?)
  (if (equal? 0 (length (unbox q))) #f #t))

;; Adds URL to the hashtable
(define (remember url parent)
  (hash-set! crawled url parent))

;; Returns True if already in memory
(define (crawled? url)
  (hash-has-key? crawled url))

;; Adds a title to the hashtable
(define (downloaded title stat)
  (hash-set! downloaded-pages title stat))


;; Crawl unrestricted
(define (crawl limit)
  (crawl-helper limit ""))

;; Crawl with restriction
(define (crawl! limit restriction)
  (crawl-helper limit restriction))


;; Crawl with exception handling
(define (crawl-helper limit restriction)
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln (string-append "Error : " (exn-message exn)))
                               (displayln "Skipping this page & continuing.\n")
                               (crawl-helper limit restriction))])
    (fetch limit restriction)))

;; Begins crawling the seeded URLs
(define (fetch limit restriction)
  (if (< (unbox page-count) limit)
      (if (full?)
          (local [(define url (pop))
                  (define page (port->string (get-pure-port (string->url url))))
                  (define title (get-title (read-content page)))]
            (begin
              ;; Exclude pages with obscure titles
              (if (not (or (string-contains? title "Moved Permanently")
                           (string-contains? title "No title")
                           (string-contains? title "Not Found")
                           (string-contains? title "not found")
                           (string-contains? title "302 Found")
                           (string-contains? title "403 Forbidden")
                           (string-contains? title "403 forbidden")
                           (string-contains? title "404")
                           (string-contains? title "Document Moved")
                           (string-contains? title "object moved")
                           (string-contains? title "Object Moved")
                           (string-contains? title "Object moved")
                           
                           ;; The following are common social networks to be avoided!
                           (string-contains? title "Twitter")
                           (string-contains? title "Bitly")
                           (string-contains? title "Instagram")
                           (string-contains? title "Facebook")))
                  (begin
                    (write-file (make-name title (unbox page-count)) page)
                    (downloaded title
                                (info url
                                        (if (equal? 1 (hash-ref downloaded-pages (unbox title-holder) [(lambda() 1)]))
                                            (orphan)
                                            (hash-ref downloaded-pages (unbox title-holder)))))
                    (extract-urls (read-content page) restriction))
                (set-box! page-count (- (unbox page-count) 1 )))
              (begin (set-box! page-count (+ 1 (unbox page-count))) (fetch limit restriction))))
          "Queue is empty")
      "Reached Limit"))

;; Produces a file name
(define (make-name title count)
  (if (non-empty-string? (remove-spaces (clean-up title)))
      (clean-up (string-append (number->string count) (string-append " " (string-append title ".html"))))
      (string-append (number->string count) ".html")))

;; Removes special characters
(define (clean-up str)
    (list->string (remove* specialChars (string->list str))))

;; Takes a list of URLs and seeds them
(define (seed pages)
  (if (null? pages)
      "Finished seeding"
      (begin (push (car pages)) (seed (cdr pages)))))

;; Reads a page and returns a list of tokens
(define (read-content page)
  (tokenize-string (list->string (tag-spacer (string->list page))) #\space #\tab))

;; Extract a title from the page
(define (get-title lst)
  (cond
    [(null? lst) "No title"]
    [(equal? "<title>" (car lst)) (begin (set-box! title-holder "")(title-read (cdr lst) 0))]
    [else (get-title (cdr lst))]))

;; Helper for title extraction
(define (title-read lst counter)
 (cond
   [(> counter 8) (unbox title-holder)]
   [(null? lst) (error 'title-reader "Title never ended!")]
   [(equal? "</title>" (car lst)) (unbox title-holder)]
   [else (begin (set-box! title-holder (string-append (unbox title-holder) (string-append (car lst) " "))) (title-read (cdr lst) (+ 1 counter)))]))

;; Extracts URLs from a list of tokens
(define (extract-urls lst restriction)
  (cond
    [(null? lst) "Finished searching document."]
    [(and (url? (car lst)) (not (null? (cdr (tokenize-string (remove-spaces (car lst)) #\" #\'))))) (local [(define url (car (cdr (tokenize-string (remove-spaces (car lst)) #\" #\'))))]
                                                     (begin
                                                       (if (valid? url restriction)
                                                           (if (not (crawled? url))
                                                               (begin
                                                                 (push url) ; Add it to the frontier
                                                                 (remember url (unbox title-holder))) ; Remember as already seen with current title as the parent
                                                               (set-box! title-holder (unbox title-holder))) ;; This is a useless statement
                                                           (set-box! title-holder (unbox title-holder)))     ;; This is a useless statement
                                                       (extract-urls (cdr lst) restriction)))]
    [else (extract-urls (cdr lst) restriction)]))


;; Returns true if the string is contains a url
(define (url? str)
  (equal? (car (tokenize-string (remove-spaces str) #\" #\')) "href="))

;; Returns true if the string is a url
(define (valid? str restriction)
  (and (string-contains? str restriction)(< (length (string->list str)) 60) (> (length (string->list str)) 4) (equal? "http" (substring str 0 4))))

;; Accepts a string and removes spaces
(define (remove-spaces str)
  (list->string (remove-space-helper (string->list str))))

;; Accepts a list of characters and removes spaces
(define (remove-space-helper chars)
  (cond
    [(null? chars) '()]
    [(equal? (car chars) #\space) (remove-space-helper (cdr chars))]
    [else (cons (car chars) (remove-space-helper (cdr chars)))]))

;; Separates HTML tags
(define (tag-spacer chars)
  (if (null? chars)
      '()
      (if (equal? (car chars) #\<)
                  (cons #\space (cons (car chars) (tag-spacer (cdr chars))))
                  (if (equal? (car chars) #\>)
                      (cons (car chars) (cons #\space (tag-spacer (cdr chars))))
                      (cons (car chars) (tag-spacer (cdr chars)))))))

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

;; Test seed
(define test '("http://www.imdb.com/list/ls052058062/" "http://www.ku.edu"))