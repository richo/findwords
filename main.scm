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
                                      (return (append tokens (list word))))
                            (if (string-prefix? word remaining)
                                    (process-token (append tokens (list word))
                                             (substring remaining (string-length word) (string-length remaining))))
                            )
                          word-list)))
    (process-token (list) token)
    ;; Catch the case where we don't find anything return an empty list
    '()
    ))



(define main
  (lambda (argv)
    (map
      (lambda (token)
        (display (string-intersperse
                   (call-with-current-continuation (lambda (cc) (find-matches token cc)))
                   "|"
                   ))
        (display "\n"))
        argv)))
