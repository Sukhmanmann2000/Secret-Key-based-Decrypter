#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in algo: "dict-closure.rkt"))

(provide secret-word-enumeration
         secret-word-enumeration-modified)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (define (exploit-dictionary dict count-candidate counted-word)
    (cond [(= (car count-candidate) 2) (begin (displayln "swe: completed key not found") key-after-dictionary-closure)]
          [(null? dict) (if (= (car count-candidate) 1) (begin (displayln "swe: completed key found!")
                                                               (begin (displayln (string-downcase (string-append "swe: secret word is " (cdr count-candidate))))
                                                                                                              (utils:encryption-key (cdr count-candidate))))
                            (begin (displayln "swe: no consistent candidates, key failed!") #f))]
          [(equal? (car dict) counted-word) (exploit-dictionary (cdr dict) count-candidate counted-word)]
          [(not (= (length (string->list (car dict))) 6)) (exploit-dictionary (cdr dict) count-candidate counted-word)]
          [(utils:is-monoalphabetic? (generate-sub (car dict)) key-after-dictionary-closure) (begin (displayln (string-append "swe: potential consistent candidate: (" (string-downcase (car dict)) ")"))
                                                                                                    (exploit-dictionary (cdr dict) (cons (+ 1 (car count-candidate)) (car dict)) (car dict)))]
          [else (exploit-dictionary (cdr dict) count-candidate counted-word)]))
  (exploit-dictionary utils:dictionary (cons 0 '()) ""))

(define (generate-sub str)
  (map cons (build-list (string-length str) (lambda (x) (integer->char (+ x 65)))) (string->list (string-downcase str))))

(define (secret-word-enumeration-modified key-after-dictionary-closure) ;; Returns a key or false (#f)
  (define (exploit-dictionary dict count-candidate counted-word)
    (cond [(= (car count-candidate) 2) (begin (displayln "swe: completed key not found") key-after-dictionary-closure)]
          [(null? dict) (if (= (car count-candidate) 1) (begin (displayln "swe: completed key found!")
                                                               (begin (displayln (string-downcase (string-append "swe: secret word is " (cdr count-candidate))))
                                                                                                              (utils:encryption-key (cdr count-candidate))))
                            (begin (displayln "swe: no consistent candidates, key failed!") #f))]
          [(equal? (car dict) counted-word) (exploit-dictionary (cdr dict) count-candidate counted-word)]
          [(not (= (length (string->list (car dict))) 6)) (exploit-dictionary (cdr dict) count-candidate counted-word)]
          [(utils:is-monoalphabetic? (generate-sub (car dict)) key-after-dictionary-closure) (if (not (list? (algo:dictionary-closure-complete (utils:encryption-key (car dict)))))
                                                                                                 (exploit-dictionary (cdr dict)count-candidate counted-word)
                                                                                                 (begin (displayln (string-append "swe: potential consistent candidates: (" (string-downcase (car dict)) ")"))
                                                                                                        (exploit-dictionary (cdr dict) (cons (+ 1 (car count-candidate)) (car dict)) (car dict))))]
          [else (exploit-dictionary (cdr dict) count-candidate counted-word)]))
  (exploit-dictionary utils:dictionary (cons 0 '()) ""))
