#!/usr/bin/env csi -ss

(use utils)
(use srfi-13)

(define word-list-file
  (let ((environment-file (get-environment-variable "WORDLIST_FILE")))
    (cond (environment-file environment-file)
          (else "/usr/share/dict/words"))))
; Read all words into memory
(define word-list
  (read-lines word-list-file))

(define find-matches
  (lambda (token return)
    (define process-token (lambda (tokens remaining)
                     (map (lambda (word)
                            (if (string=? word remaining)
                              (call-with-current-continuation
                                (lambda (cc) (return (cons (append tokens (list word)) cc)))))
                            (if (string-prefix? word remaining)
                                    (process-token (append tokens (list word))
                                             (substring remaining (string-length word) (string-length remaining))))
                            )
                          word-list)))
    (process-token (list) token)
    ;; Stub out the values our continuation passes back
    (cons '() '())
    ))

(define display=
  (lambda (maxr)
    (display "=")
    (if (> maxr 0) (display= (- maxr 1)))))

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
