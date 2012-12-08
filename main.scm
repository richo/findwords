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


(define real-find-matches
  (lambda (exit tokens current-token)
    (let* ((first (string-ref current-token 0))
           (new-tokens (append tokens (list first))))
      (cond ((= (string-length current-token) 0) (exit (list new-tokens "")))
            (else (real-find-matches exit new-tokens (substring current-token 1 (string-length current-token)))))
    )))

(define find-matches
  (lambda (token)
    (let ((process (lambda (current-values)
      (let* ((_tmp (call-with-current-continuation (lambda (cc)
                                                     (real-find-matches cc (car current-values) (cdr current-values)))))
             (tokens (car _tmp))
             (remaining (cdr _tmp)))
        (cond ((= (string-length remaining) 0) tokens)
              (else (process current-values)))))))
      (process (list '() token)))))

(define main
  (lambda (argv)
    (map (lambda (token) (display (string-append (find-matches token) "\n"))) argv)))
