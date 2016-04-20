#lang plai


(define months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILE READING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-files (map path->string (directory-list "data/")))
(define (documents xs)
  (cond
    [(null? xs) '()]
    [else (if (or (equal? (car xs) ".DS_Store") (equal? (car xs) "spider.rkt")) (documents (cdr xs)) (cons (car xs) (documents (cdr xs))))]))

(define universe (documents all-files))

(define (read-file file-name)
  (file->lines (string-append "data/" file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HTML PROCESSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Accepts a document name and returns a list of tokens using the following functions.
(define (tokenize name)
  (flatten (map de-tag (map trim (map tokenize-string (map list->string (map tag-spacer (read-file name))))))))
;--------------------------------------------------------------------------------------------------------------
;; Adds a space before and after '<' and '>' to separate tags
(define (tag-spacer x)
  (let ((chars (string->list x)))
    (cond
      [(null? chars) '()]
      [(or (eq? (car chars) #\<) (eq? (car chars) #\>)) (cons #\space (cons (car chars) (cons #\space (tag-spacer (list->string (cdr chars))))))]
      [else (cons (car chars) (tag-spacer (list->string (cdr chars))))])))

;; Creates tokens from a string
(define (tokenize-string str)
  (define in (open-input-string str))
  (let recur ((out (open-output-string)))
    (define c (read-char in))
    (cond ((eof-object? c)
           (list (get-output-string out)))
          ((or  (char=? c #\space) (char=? c #\tab))
           (cons (get-output-string out)
                 (recur (open-output-string))))
          (else (write-char c out) (recur out)))))

;; Removes Style and Script
(define (trim xs)
  (cond
    [(null? xs) '()]
    [(not (or (string-contains? (car xs) "< style >") (string-contains? (car xs) "< script >"))) (cons (car xs) (trim (cdr xs)))]
    [else
     (let ((disjunct (or (is-member "< /style >" xs) (is-member "< /script >" xs))))
       (if (equal? disjunct #f)
           (rev-trim (cdr xs))
           (trim disjunct)))]))

;; Helper for Style and Script Remover
(define (rev-trim xs)
  (cond
    [(null? xs) '()]
    [(or (string-contains? (car xs) "< style >") (string-contains? (car xs) "< script >")) (cons (car xs) (rev-trim (cdr xs)))]
    [else
     (let ((disjunct (or (is-member "< /script >" xs)  (is-member "< /style >" xs))))
       (if (equal? disjunct #f)
           (trim (cdr xs))
           (rev-trim disjunct)))]))

;; Removes all the HTML tags
(define (de-tag list-of-tokens)
  (cond
    [(null? list-of-tokens) '()]
    [(not (equal? "<" (car list-of-tokens))) (cons (car list-of-tokens) (de-tag (cdr list-of-tokens)))]
    [else (de-tag (is-member ">" list-of-tokens))]))

(define (is-member x xs)
  (cond
    [(null? xs) #f]
    [(equal? (car xs) x) (cdr xs)]
    [else (is-member x (cdr xs))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Information extraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-str tokens)
  (cond
    [(null? tokens) ""]
    [(not (non-empty-string? (car tokens))) (make-str (cdr tokens))]
    [else (string-append (car tokens) " " (make-str (cdr tokens)))]))

(define (sentencize-doc str)
  (define in (open-input-string str))
  (let recur ((out (open-output-string)))
    (define c (read-char in))
    (cond ((eof-object? c)
           (list (get-output-string out)))
          ((or  (char=? c #\.) (char=? c #\?) (char=? c #\!) (char=? c #\:))
           (cons (get-output-string out)
                 (recur (open-output-string))))
          (else (write-char c out) (recur out)))))


(define sentence (box ""))

(define (sentencize tokens lst)
  (cond
    [(null? tokens) lst]
    [(sent-end? (car tokens))
        ;(not (non-empty-string? (car tokens))))
     (cons (string-append (unbox sentence) " ") lst)]
    [else (begin
            (set-box! sentence (string-append (unbox sentence) " " (car tokens)))
            (sentencize (cdr tokens) lst))]))

;; Returns True if the last character is a (.)
(define (sent-end? word)
  (local [(define chars (string->list word))]
  (cond
    [(null? chars) #f]
    [(null? (cdr chars)) (equal? (car chars) #\.)]
    [else (sent-end? (list->string (cdr chars)))])))

(sentencize-doc (make-str (tokenize "sample.html")))