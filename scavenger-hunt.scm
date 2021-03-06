#!/usr/bin/env chicken-scheme

(use alist-lib
     call-with-query
     debug
     define-record-and-printer
     html-parser
     http-client
     irregex
     matchable
     random-bsd
     srfi-13
     srfi-95
     sxml-transforms
     sxpath
     twilio)

(require-library htmlprag)
(import (only htmlprag write-shtml-as-html))

(require-library aima-csp)
(import (only aima-csp shuffle))

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

(define hunt-worksheet
  (make-parameter
   (get-environment-variable "HUNT_WORKSHEET")))

(define teams-worksheet
  (make-parameter
   (get-environment-variable "TEAMS_WORKSHEET")))

(define edit-worksheet
  (make-parameter
   (get-environment-variable "EDIT_WORKSHEET")))

(define teams (make-parameter #f))

(define hunt (make-parameter #f))

(define-record-and-printer game-state
  stage-name
  starting-points
  start-time
  penalty)

(define (make-shuffled-game-state starting-points)
  (make-game-state #f (shuffle starting-points) (current-seconds) 0))

(define progress (make-parameter #f))

(define starting-points (make-parameter #f))

(define (normalize-phone phone)
  (format "+1~a" (irregex-replace/all '(seq (~ num)) phone)))

(define-record-and-printer player
  team
  name
  phone
  email)

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
                    phone
                    (alist-ref/default team 'email #f)))))
      (worksheet->alists (parse-worksheet (teams-worksheet))))
    teams))

(define-record-and-printer stage
  clue
  secret
  next
  if-wrong)

(define (hunt-start hunt-sheet)
  (alist-ref (car hunt-sheet) 'stage))

;;; Symbols generated by gensym appear to be uninterned.
(define (intern symbol)
  (##sys#intern-symbol (##sys#symbol->string symbol)))

(define (make-hunt)
  (let ((hunt (make-hash-table))
        (hunt-sheet (worksheet->alists (parse-worksheet (hunt-worksheet)))))
    (let iter ((hunt-sheet hunt-sheet)
               (previous-sequence #f)
               (previous-subsequence #f)
               (previous-stage-name #f)
               (starting-points '())
               (subsequence-names '()))
      (if (null? hunt-sheet)
          (values hunt starting-points)
          (let ((hunt-row (car hunt-sheet)))
            (let ((sequence (alist-ref/default hunt-row 'sequence #f))
                  (subsequence (alist-ref/default hunt-row 'subsequence #f))
                  (clue (alist-ref/default hunt-row 'clue #f))
                  (secret (alist-ref/default hunt-row 'secret #f))
                  (stage-name (intern (gensym))))
              (let ((stage
                     (make-stage
                      clue
                      secret
                      #f
                      #f)))
                (hash-table-set! hunt stage-name stage)
                (if (and previous-sequence
                         previous-stage-name
                         (string=? sequence previous-sequence))
                    (if previous-subsequence
                        (if (and subsequence
                                 (string=? subsequence previous-subsequence))
                            (let ((previous-stage
                                   (hash-table-ref hunt previous-stage-name)))
                              (stage-if-wrong-set! previous-stage stage-name)
                              (iter (cdr hunt-sheet)
                                    sequence
                                    subsequence
                                    stage-name
                                    starting-points
                                    (cons previous-stage-name subsequence-names)))
                            (begin
                              (for-each
                                  (lambda (subsequence-name)
                                    (stage-next-set!
                                     (hash-table-ref
                                      hunt
                                      subsequence-name)
                                     stage-name))
                                (cons previous-stage-name subsequence-names))
                              (iter (cdr hunt-sheet)
                                    sequence
                                    subsequence
                                    stage-name
                                    starting-points
                                    '())))
                        (let ((previous-stage
                               (hash-table-ref hunt previous-stage-name)))
                          (stage-next-set! previous-stage stage-name)
                          (iter (cdr hunt-sheet)
                                sequence
                                subsequence
                                stage-name
                                starting-points
                                '()))) 
                    (if (null? subsequence-names)
                        (iter (cdr hunt-sheet)
                              sequence
                              subsequence
                              stage-name
                              (cons stage-name starting-points)
                              '())
                        (iter (cdr hunt-sheet)
                              sequence
                              subsequence
                              stage-name
                              (cons stage-name starting-points)
                              '()))))))))))

(define-record-and-printer finished)
(define finished (make-finished))

(define started? (make-parameter #f))

(define (start! send)
  (progress (make-hash-table))
  (teams (make-teams))
  (call-with-values (lambda () (make-hunt))
    (lambda (this-hunt these-starting-points) 
      (hunt this-hunt)
      (starting-points these-starting-points)))
  (unless (null? (starting-points))
    (hash-table-walk (teams)
      (lambda (phone player)
        (let* ((game-state (make-shuffled-game-state (starting-points)))
               (starting-points (game-state-starting-points game-state))
               (stage-name (car starting-points))
               (stage (hash-table-ref (hunt) stage-name))
               (clue (stage-clue stage)))
          (game-state-stage-name-set! game-state stage-name)
          (game-state-starting-points-set! game-state (cdr starting-points))
          (hash-table-set! (progress) phone game-state)
          (send phone clue)))))
  (started? #t))

(define (play player guess write)
  ;; Cover the case of the unexpected player. Teamless? Who cares
  ;; about the team lookup, at this point. At some point: parity
  ;; between phone, email, facebook.
  (let* ((game-state (hash-table-ref/default
                      (progress)
                      player
                      (make-game-state #f '() (current-seconds) 0)))
         (stage (hash-table-ref/default (hunt) (game-state-stage-name game-state) finished)))
    (if (finished? stage)
        (write "You've already finished!")
        (if (string=? (string-trim-both (string-downcase (stage-secret stage)))
                      (string-trim-both (string-downcase guess)))
            (let ((stage-next-name (stage-next stage)))
              (if stage-next-name
                  (let ((stage-next (hash-table-ref (hunt) stage-next-name)))
                    (begin
                      (game-state-stage-name-set! game-state stage-next-name)
                      (write (stage-clue stage-next))))
                  (let ((starting-points (game-state-starting-points game-state)))
                    (if (null? starting-points)
                        (begin
                          (game-state-stage-name-set! game-state '*finished*)
                          (write "Congratulations, you've finished!"))
                        (let* ((stage-next-name (car starting-points))
                               (stage-next (hash-table-ref (hunt) stage-next-name)))
                          (game-state-starting-points-set!
                           game-state
                           (cdr starting-points))
                          (game-state-stage-name-set! game-state stage-next-name)
                          (write (stage-clue stage-next)))))))
            (begin
              (game-state-penalty-set!
               game-state
               (+ (game-state-penalty game-state) 60))
              (let ((if-wrong-name (stage-if-wrong stage)))
                (if if-wrong-name
                    (let ((if-wrong (hash-table-ref (hunt) if-wrong-name)))
                      (game-state-stage-name-set! game-state if-wrong-name)
                      (write (stage-clue if-wrong)))
                    (write "Nope; try again!"))))))))

(define (twilio-write-sms text)
  (twilio-write (twilio-response (twilio-sms text))))

;; (parameterize ((hunt-worksheet "https://spreadsheets.google.com/feeds/cells/0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE/od6/public/basic")
;;                (teams-worksheet "https://spreadsheets.google.com/feeds/cells/0AnvJq9OyBeoUdGJ3SXpHZE8xUzZocWQ4c1ZCcndXNUE/od7/public/basic"))
;;   (let ((clue (make-parameter #f)))
;;     (start! (lambda (recipient message) (debug message) (clue message)))
;;     ;; (debug (hash-table->alist (hunt))
;;     ;;        (hash-table->alist (teams)))
;;     (let ((answers (make-hash-table)))
;;       (hash-table-walk (hunt)
;;         (lambda (stage-name stage)
;;           (hash-table-set! answers (stage-clue stage) (stage-secret stage))))
;;       (do ()
;;           ((string=? (clue) "Congratulations, you've finished!")
;;            (play "+16268172836"
;;                  "Just once more"
;;                  (lambda (message) (debug message))))
;;         (let ((response (if (< (random-real) 0.25)
;;                             (hash-table-ref answers (clue))
;;                             "A wrong answer")))
;;           (debug response)
;;           (play "+16268172836" response
;;                 (lambda (message) (debug message)
;;                    (unless (string=? message "Nope; try again!")
;;                      (clue message))))))
;;       (debug (hash-table->alist (progress))))))

(call-with-dynamic-fastcgi-query
 (lambda (query)
   (match (query-any query 'path-info)
     ("/start"
      (start! twilio-send-sms)
      (display-content-type-&c. 'html)
      (write-shtml-as-html
       `(html
         (head (title "Scavenger Hunt"))
         (body (p "Race started!")))))
     ("/sms"
      (unless (started?) (start! twilio-send-sms))
      (display-content-type-&c. 'xml)
      (let ((phone (query-any query 'From))
            (body (query-any query 'Body)))
        (play phone body twilio-write-sms)))
     ("/status"
      (unless (started?) (start! twilio-send-sms))
      (display-status-&c.)
      (pp (hash-table->alist (progress))))
     (_ (display-content-type-&c. 'html)
        (write-shtml-as-html
         `(html
           (head (title "Scavenger Hunt"))
           (body (p (a (@ (href "start")) "Start")
                    " the scavenger hunt or "
                    (a (@ (href ,(edit-worksheet))) "edit")
                    " it."))))))))
