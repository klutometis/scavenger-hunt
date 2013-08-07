#!/usr/bin/env chicken-scheme

(use alist-lib
     call-with-query
     debug
     html-parser
     http-client
     irregex
     matchable
     srfi-95
     sxml-transforms
     sxpath)

(require-library htmlprag)
(import (only htmlprag write-shtml-as-html))

(define (node-children node)
  ((sxpath '((*not* @))) node))
    
(define (node-attributes node)
  ((sxpath '(@)) node))

(define (call-with-children-attributes tag f)
  (f (node-children tag) (node-attributes tag)))

(define-syntax translate
  (ir-macro-transformer
   (lambda (expression inject compare)
     `(lambda ,(inject 'tag)
        (let ((,(inject 'children) (node-children ,(inject 'tag)))
              (,(inject 'attributes) (node-attributes ,(inject 'tag))))
          ,@(cdr expression))))))

(define (worksheets spreadsheet-id)
  (with-input-from-request
   (format "https://spreadsheets.google.com/feeds/worksheets/~a/public/basic"
           spreadsheet-id)
   #f
   html->sxml))

;; (debug (worksheets "0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE"))

(define (column+row column-row)
  (let ((column-row (irregex-match '(seq (submatch-named column (+ alpha))
                                         (submatch-named row (+ num)))
                                   column-row)))
    (let ((column (irregex-match-substring column-row 'column))
          (row (irregex-match-substring column-row 'row)))
      (values column row))))

(define (parse-worksheet url)
  (let ((worksheet
         (with-input-from-request
          url
          #f
          html->sxml)))
    (car
     (pre-post-order
      worksheet
      `((*TOP* . ,(translate children))
        (feed . ,(translate children))
        (entry *preorder* .
               ,(translate
                 (let ((column-row (car ((sxpath '(title *text*)) tag)))
                       (content (car ((sxpath '(content *text*)) tag))))
                   (call-with-values (lambda () (column+row column-row))
                     (lambda (column row)
                       (list (string->symbol column)
                             (string->number row)
                             content))))))
        (*default* . ,(translate '())))))))

(define (worksheet-transpose worksheet)
  (let ((transposition (make-hash-table)))
    (for-each
        (match-lambda
            ((column row content)
             (hash-table-update!/default
              transposition
              row
              (lambda (row)
                (alist-cons column content row))
              '())))
      worksheet)
    (map cdr (sort (hash-table->alist transposition) < car))))

(define (worksheet->alists worksheet)
  (let ((worksheet (worksheet-transpose worksheet)))
    (let ((header (car worksheet))
          (worksheet (cdr worksheet)))
      (map (lambda (row)
             (alist-map
              (lambda (column content)
                (cons (string->symbol
                       (alist-ref/default header column "*none*"))
                      content))
              row))
           worksheet))))

(define twilio-write write-shtml-as-html)

(define (twilio-response response)
  `(Response ,response))

(define (twilio-write-sms sms)
  (twilio-write (twilio-response `(Sms ,sms))))

(define hunt-worksheet
  (make-parameter
   "https://spreadsheets.google.com/feeds/cells/0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE/od6/public/basic"))

(define teams-worksheet
  (make-parameter
   "https://spreadsheets.google.com/feeds/cells/0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE/od7/public/basic"))

(define edit-worksheet
  (make-parameter
   "https://docs.google.com/spreadsheet/ccc?key=0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE&usp=sharing"))

(define teams (make-parameter #f))

(define hunt (make-parameter #f))

(define progress (make-parameter #f))

(define (normalize-phone phone)
  (format "+1~a" (irregex-replace/all '(seq (~ num)) phone)))

(define (make-teams)
  (let ((teams (worksheet->alists (parse-worksheet (teams-worksheet)))))
    (map (lambda (team)
           (alist-map
            (lambda (key value)
              (if (eq? key 'phone-number)
                  (cons key (normalize-phone value))
                  (cons key value)))
            team))
         teams)))

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (match (query-any query 'path-info)
     ("/start"
      (progress (make-hash-table))
      (teams (make-teams))
      (hunt (worksheet->alists (parse-worksheet (hunt-worksheet))))
      (for-each (lambda (team)
                  (debug (alist-ref/default team 'phone-number #f)))
        (teams))
      (display-content-type-&c. 'html)
      (write-shtml-as-html
       `(html
         (head (title "Scavenger Hunt"))
         (body (p "Race started! "
                  (a (@ (href "..")) "Return")
                  ".")))))
     ("/sms"
      (display-content-type-&c. 'xml)
      (twilio-write-sms "Harro!"))
     (_ (display-content-type-&c. 'html)
        (write-shtml-as-html
         `(html
           (head (title "Scavenger Hunt"))
           (body (p (a (@ (href "start")) "Start")
                    " the scavenger hunt or "
                    (a (@ (href ,(edit-worksheet))) "edit")
                    " it."))))))))
