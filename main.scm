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



(define main
  (lambda (argv)
    (map
      (lambda (token)
        (let* ((_tmp (call-with-current-continuation (lambda (cc) (find-matches token cc))))
               (match (car _tmp))
               (progress (cdr _tmp)))
        (display (string-intersperse
                   match
                   "|"
                   ))
        (display "\n")
        (if (procedure? progress) (progress (list)))
        ))
        argv)))
