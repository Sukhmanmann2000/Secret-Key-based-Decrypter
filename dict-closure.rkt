#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         racket/format
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure
         dictionary-closure-complete
         binary-map
         search)
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key [key-number 0])
  (define decrypted-word-list (map (lambda (word) (utils:decrypt key word)) utils:cipher-word-list))
  (define (key-modifier decrypted-words)
    (cond [(null? decrypted-words) key]
          [(andmap char-upper-case? (string->list (car decrypted-words))) (if (list? (member (car decrypted-words) utils:dictionary))
                                                                              (begin (displayln (string-append (car decrypted-words) " : skipping this one"))
                                                                                     (key-modifier (cdr decrypted-words)))
                                                                              #f)]
          [else (let ([search-val (search-dictionary (car decrypted-words) key)])
                  (cond [(= (car search-val) 1) (let ([modified-key (utils:add-substitution (cadr search-val) key)])
                                                  (begin (displayln (string-append (car decrypted-words) " : Unique Match (" (cddr search-val) ")"))
                                                         (printf (string-append "~nDC*:~n@@@+ " (~a key-number) "~n")) (utils:show-key modified-key) (printf "@@@-~n")
                                                         (printf "~nSTARTING AT THE BEGINNING OF WORD-LIST~n")
                                                         (dictionary-closure modified-key key-number)))]
                        [(= (car search-val) 2) (begin (displayln (string-append (car decrypted-words) " : Multiple Matches (" (cadr search-val) " " (cddr search-val) "...)"))
                                                       (key-modifier (cdr decrypted-words)))]
                        [else (begin (displayln (string-append (car decrypted-words) " : No match")) #f)]))]))
  (key-modifier decrypted-word-list))

(define (search-dictionary word key)
  (define (search-helper word-list dict count key-list counted-word)
    (cond [(null? dict) (cons count (cons key-list counted-word))]
          [(equal? counted-word (car dict)) (search-helper word-list (cdr dict) count key-list counted-word)]
          [(not (= (length word-list) (length (string->list (car dict))))) (search-helper word-list (cdr dict) count key-list counted-word)]
          [(not (coincide-uppercase? word-list (string->list (car dict)))) (search-helper word-list (cdr dict) count key-list counted-word)]
          [else (let ([poss-sub (consistent-mapping? word-list (string->list (car dict)) key)])
                  (if (list? poss-sub) (if (= count 0) (search-helper word-list (cdr dict) (+ count 1) poss-sub (car dict))
                                           (cons 2 (cons counted-word (car dict))))
                      (search-helper word-list (cdr dict) count key-list counted-word)))]))
  (search-helper (string->list word) utils:dictionary 0 '() '()))

(define (coincide-uppercase? word-list dict-word)
  (andmap (lambda (x) (char=? (list-ref word-list x) (list-ref dict-word x))) (indexes-where word-list char-upper-case?))) 

(define (consistent-mapping? word-list dict-word key)
  (define key-list (filter (lambda (x) (char-lower-case? (cdr x))) (remove-duplicates (zip dict-word word-list))))
  (cond [(not (utils:is-monoalphabetic? key-list key)) #f]
        [else key-list]))

(define (search element lst)
  (list? (member element lst)))

(define (binary-map f l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (f (car l1) (car l2)) (binary-map f (cdr l1) (cdr l2)))))

(define (zip l1 l2)
  (binary-map cons l1 l2))

(define (dictionary-closure-complete key)
  (define decrypted-word-list (map (lambda (word) (utils:decrypt key word)) utils:cipher-word-list))
  (define (dict-upcase decrypted-words)
    (cond [(null? decrypted-words) key]
          [(not (list? (member (car decrypted-words) utils:dictionary))) #f]
          [else (dict-upcase (cdr decrypted-words))]))
  (dict-upcase decrypted-word-list))