#!/usr/bin/env chicken-scheme

(use alist-lib
     call-with-query
     debug
     define-record-and-printer
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

(define start (make-parameter #f))

(define (normalize-phone phone)
  (format "+1~a" (irregex-replace/all '(seq (~ num)) phone)))

(define-record-and-printer player
  team
  name
  phone)

(define (make-teams)
  (let ((teams (make-hash-table)))
    (for-each (lambda (team)
                (let ((phone (normalize-phone
                              (alist-ref/default team 'phone-number "000-000-0000"))))
                  (hash-table-set!
                   teams
                   phone
                   (make-player
                    (alist-ref/default team 'team-name #f)
                    (alist-ref/default team 'participant #f)
                    phone))))
      (worksheet->alists (parse-worksheet (teams-worksheet))))
    teams))

(define-record-and-printer stage
  clue
  secret
  next)

(define (hunt-start hunt-sheet)
  (alist-ref (car hunt-sheet) 'stage))

(define (make-hunt)
  (let* ((hunt-sheet (worksheet->alists (parse-worksheet (hunt-worksheet))))
         (start (hunt-start hunt-sheet)))
    (let ((hunt (make-hash-table)))
      (for-each (lambda (hunt-row)
                  (hash-table-set!
                   hunt
                   (alist-ref/default hunt-row 'stage #f)
                   (make-stage
                    (alist-ref/default hunt-row 'clue #f)
                    (alist-ref/default hunt-row 'secret #f)
                    (alist-ref/default hunt-row 'next-stage #f))))
        hunt-sheet)
      (values start hunt))))

(define-record-and-printer finished)
(define finished (make-finished))

(define started? (make-parameter #f))

(define (start!)
  (progress (make-hash-table))
  (teams (make-teams))
  (call-with-values (lambda () (make-hunt))
    (lambda (made-start made-hunt)
      (start made-start)
      (hunt made-hunt)))
  (let ((clue (stage-clue (hash-table-ref (hunt) (start)))))
    (hash-table-walk (teams)
      (lambda (phone player)
        (twilio-send-sms phone clue))))
  (started? #t))

(define twilio-sid (make-parameter (get-environment-variable "TWILIO_SID")))

(define twilio-auth (make-parameter (get-environment-variable "TWILIO_AUTH")))

(define twilio-from (make-parameter (get-environment-variable "TWILIO_FROM")))

(define (twilio-send-sms to body)
   (with-input-from-request
    (format "https://~a:~a@api.twilio.com/2010-04-01/Accounts/~a/SMS/Messages"
            (twilio-sid)
            (twilio-auth)
            (twilio-sid))
    `((From . ,(twilio-from))
      (To . ,to)
      (Body . ,body))
    void))

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (match (query-any query 'path-info)
     ("/start"
      (start!)
      (display-content-type-&c. 'html)
      (write-shtml-as-html
       `(html
         (head (title "Scavenger Hunt"))
         (body (p "Race started! "
                  (a (@ (href "..")) "Return")
                  ".")))))
     ("/sms"
      (unless (started?) (start!))
      (display-content-type-&c. 'xml)
      (let* ((phone (query-any query 'From))
             (stage-name (hash-table-ref/default (progress) phone (start)))
             (stage (hash-table-ref/default (hunt) stage-name finished)))
        (if (finished? stage)
            (twilio-write-sms "You've already finished!")
            (let ((guess (query-any query 'Body)))
              (if (string=? (string-downcase (stage-secret stage))
                            (string-downcase guess))
                  (let ((stage-next-name (stage-next stage)))
                    (if stage-next-name
                        (let ((stage-next (hash-table-ref/default
                                           (hunt)
                                           stage-next-name
                                           #f)))
                          (if stage-next
                              (begin
                                (hash-table-set! (progress) phone stage-next-name)
                                (twilio-write-sms (stage-clue stage-next)))
                              (begin
                                (hash-table-set! (progress) phone finished)
                                (twilio-write-sms "You're in no-man's land!"))))
                        (begin
                          (hash-table-set! (progress) phone finished)
                          (twilio-write-sms "Congratulations, you've finished!"))))
                  (twilio-write-sms "Nope; try again!"))))))
     (_ (display-content-type-&c. 'html)
        (write-shtml-as-html
         `(html
           (head (title "Scavenger Hunt"))
           (body (p (a (@ (href "start")) "Start")
                    " the scavenger hunt or "
                    (a (@ (href ,(edit-worksheet))) "edit")
                    " it."))))))))
