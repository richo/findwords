#!/usr/bin/env csi -ss

(use utils)
(use srfi-13)
(use srfi-69)

(define hash-table-depth 2)

(define word-list-file
  (let ((environment-file (get-environment-variable "WORDLIST_FILE")))
    (cond (environment-file environment-file)
          (else "/usr/share/dict/words"))))
; Read all words into memory
(define word-list
  (with-input-from-file word-list-file
    (lambda () (port-fold (lambda (line hash)
                            (let* ((first-two (substring line 0 1)))
                              (if (hash-table-exists? hash first-two)
                                  (hash-table-set! hash first-two (append (hash-table-ref hash first-two) (list line)))
                                  (hash-table-set! hash first-two (list line)))))
      (make-hash-table)
      read-line))))

(define find-matches
  (lambda (token return)
    (define process-token (lambda (tokens remaining)
                     (map (lambda (word)
                            (cond ((string=? word remaining)
                                    (call-with-current-continuation
                                    (lambda (cc) (return (cons (append tokens (list word)) cc)))))
                                  ((string-prefix? word remaining)
                                    (process-token (append tokens (list word))
                                             (substring remaining (string-length word) (string-length remaining)))))
                            )
                          word-list)))
    (process-token (list) token)
    ;; Stub out the values our continuation passes back
    (cons '() '())
    ))

(define display=
  (lambda (maxr)
    (display "=")
    (if (> maxr 1) (display= (- maxr 1)))))

(define display-header
  (lambda (title)
    (display (string-upcase title))
    (newline)
    (display= (string-length title))
    (newline)))


(define main
  (lambda (argv)
    (map
      (lambda (token)
        (display-header token)
        (let* ((_tmp (call-with-current-continuation (lambda (cc) (find-matches token cc))))
               (match (car _tmp))
               (progress (cdr _tmp)))
        (display (string-intersperse
                   match
                   "|"
                   ))
        (newline)
        (if (procedure? progress) (progress (list)))
        ))
        argv)))
