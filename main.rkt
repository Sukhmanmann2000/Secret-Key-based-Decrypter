#lang racket

;; You can require more modules of your choice.
;; For example, list-comprehensions!
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in strat: "strategies.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in  algo: "dict-closure.rkt")
         (prefix-in  algo: "secret-word-enumeration.rkt"))

(provide crack-cipher
         crack-hard-cipher ;; Optional, no extra credits for this :)
         )

;; TIPS
;; ----
;; 0. Please read the README. It will help you navigate these modules.
;; 1. Take a look at utilities in "utils.rkt" to ensure you don't re-invent the wheel!
;; 2. You can create a key quickly using `utils:encryption-key`
;; 3. You can create a random permutation key quickly using `utils:encryption-key-hard`
;; 4. Please make use of `utils:plaintext`, `utils:ciphertext` and `utils:cipher-word-list`.
;; 5. DO NOT ATTEMPT TO SPLIT THE TEXT INTO WORDS ON YOUR OWN.

;; CAUTION
;; =======
;; Your submission must operate on `utils:ciphertext` and
;; `utils:cipher-word-list`. We will be setting these variables forcibly while
;; automatically grading your submission. PLEASE DO NOT HARDCODE SOME OTHER
;; VARIABLE INSIDE ANY OF YOUR FUNCTIONS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The "Key"                                                                        ;;
;; ========                                                                         ;;
;;                                                                                  ;;
;; Is represented by a list of 26 lower-case chars (including underscore). Example: ;;
;; ```                                                                              ;;
;; '(#\w #\i #\s #\d #\o #\m #\n #\p #\q #\r #\t #\u #\v #\x #\y #\z #\a #\b #\c #\e #\f #\g #\h #\j #\k #\l) ;;
;; ```                                                                              ;;
;;                                                                                  ;;
;; Make sure you know how to interpret this. The first character replaces "A" from  ;;
;; the plaintext, the next replaces "B", and so on. An underscore implies that we   ;;
;; don't know what replaces the character at that position in the alphabet. It is   ;;
;; important to place only underscores and lower-case characters in this list.      ;;
;;                                                                                  ;;
;; Here, "A" -> "w", "B" -> "i", "C" -> "s", ..., "Z" -> "l"                        ;;
;;                                                                                  ;;
;; Use `utils:show-key` to conveniently display the key.                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define plaintext (utils:read-plaintext "samples/text-2-sherlockholmes.txt"))
;; (define ciphertext (utils:read-ciphertext "encrypted/02.txt"))
;; (define ciphertext (encrypt (encryotion-key "wisdom") plaintext))
;; (define cipher-word-list utils:cipher-word-list)

;; The initial BLANK key
(define key (build-list 26 (lambda (_) #\_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; crack-cipher                                                                     ;;
;; ============                                                                     ;;
;; The main driver function!                                                        ;;
;;                                                                                  ;;
;; Formal arguments:                                                                ;;
;; `strategies` is a list of functions from the `strat` namespace. The              ;;
;;              strategies should be explored by `crack-cipher` in this order.      ;;
;; `key`        is the (initial) encryption key.                                    ;;
;;                                                                                  ;;
;; Implicit arguments:                                                              ;;
;; `ciphertext` is the encrypted text read from a file. It must be in lower         ;;
;;              case. See `utils:read-ciphertext`.                                  ;;
;; `cipher-word-list` is the list of words in the cipher text. You just have to     ;;
;;              use the pre-computed list in `utils:cipher-word-list`               ;;
;;                                                                                  ;;
;; Behaviour of `crack-cipher`                                                      ;;
;; ---------------------------                                                      ;;
;;                                                                                  ;;
;; `crack-cipher` needs to emulate a (dumb) head first dive into the search space   ;;
;; of the encryption key.                                                           ;;
;;                                                                                  ;;
;; It sufficies (for this assignment) to explore (each) strategy's substitution     ;;
;; choices and pick the first one that seems promising and jump to the next         ;;
;; strategy (since we have, in some sense, used up whatever this strategy could     ;;
;; tell us).                                                                        ;;
;;                                                                                  ;;
;; The most general approach is to revisit all remaining substitutions and explore  ;;
;; them as well, such an exploration is called a Depth First Search.                ;;
;;                                                                                  ;;
;; If the substitution of a particular strategy is found to be bad, we pick it's    ;;
;; next substitution. If we are unlucky, all substitutions could get exhausted, and ;;
;; then we must ditch this strategy and proceed to the next one. But the moment we  ;;
;; get lucky, we short-circuit all the remaining substitutions and proceed to the   ;;
;; next strategy (if necessary).                                                    ;;
;;                                                                                  ;;
;; We judge a substitution as "bad" using dictionary-closure and                    ;;
;; secret-word-enumeration. You are free to create your own criterions to judge bad ;;
;; vs good substitutions. Of course these two steps help greatly in leap-frogging   ;;
;; to the complete key! The smarter they are, the better.                           ;;
;;                                                                                  ;;
;; Take a moment to pin down the termination criterion for your algorithm. How do   ;;
;; you detect errors (early) or eventual completion?                                ;;
;;                                                                                  ;;
;; A word to the wise: When building the substitution list in the strategy          ;;
;; function, if unsure about correctness of a substitution it is better to keep it  ;;
;; than throw it away. You can filter out bad keys with more confidence than        ;;
;; filtering out substitutions.                                                     ;;
;;                                                                                  ;;
;; EDIT in v1.1                                                                     ;;
;; ------------                                                                     ;;
;; The function must return a list of keys, each key being a list of 26             ;;
;; lower-case chars representing the key. If you are unable to completely           ;;
;; determine the key, return a partial key. A trivial partial key is a list         ;;
;; of 26 underscores.                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define key-number 0)
(define (crack-cipher strategies key) ;; Returns list of encryption keys.
  ;; make use of `utils:ciphertext` and `utils:cipher-word-list`
  ;; DISPLAY A KEY AS SOON AS YOU FIND IT USING (show-key key)
  (define (crack-outer-helper strats curr-key list-of-keys)
    (cond [(null? strats) (if (null? list-of-keys) (begin (displayln "*******STARTING DICTIONARY CLOSURE ON GIVEN KEY*******")
                                                          (let ([last-straw (complete-if-single (algo:dictionary-closure curr-key))])
                                                            (cond [(not (list? last-straw)) (list key)]
                                                                  [else (let ([swe-last-straw (algo:secret-word-enumeration-modified last-straw)])
                                                                          (if (not (list? swe-last-straw)) (list curr-key)
                                                                              (list swe-last-straw)))])))
                              (begin (printf "Number of possible keys: ") (display key-number) (newline) (displayln "******LIST OF POSSIBLE KEYS IS:*******") list-of-keys))]
          [else (begin (printf "~n~nCURRENT STRATEGY: ") (displayln (car strats))
                       (define (crack-inner-helper strategy-subs-list key-list)
                         (cond [(null? strategy-subs-list) (crack-outer-helper (cdr strats) curr-key key-list)]
                               [else (let* ([strat-modified-key (begin (printf "~n~nCURRENT SUBSTITUTION: ~n") (displayln (car strategy-subs-list)) (printf "~n")
                                                                       (printf "Modified key:~n")
                                                                       (utils:show-key (utils:add-substitution (car strategy-subs-list) curr-key))
                                                                       (utils:add-substitution (car strategy-subs-list) curr-key))]
                                            [dict-closure-modified-key (begin (printf (string-append "~n@@@+ " (~a key-number) "~n"))
                                                                              (utils:show-key strat-modified-key) (printf "@@@-~n")
                                                                              (algo:dictionary-closure strat-modified-key key-number))])
                                       (cond [(not (list? dict-closure-modified-key)) (crack-inner-helper (cdr strategy-subs-list) key-list)]
                                             [else (let ([swe-modified-key (algo:secret-word-enumeration-modified dict-closure-modified-key)])
                                                     (cond [(not (list? swe-modified-key)) (crack-inner-helper (cdr strategy-subs-list) key-list)]
                                                           [else (cond [(not (complete-key? swe-modified-key)) (let ([poss-com (complete-helper (cdr strats) swe-modified-key)])
                                                                                                                 (cond [(not (complete-key? poss-com)) (crack-inner-helper (cdr strategy-subs-list) key-list)]
                                                                                                                       [(list? (member poss-com key-list)) (crack-inner-helper (cdr strategy-subs-list) key-list)]
                                                                                                                       [else (crack-inner-helper (cdr strategy-subs-list) (append key-list (list poss-com)))]))]
                                                                       [(member swe-modified-key list-of-keys) (crack-inner-helper (cdr strategy-subs-list) key-list)]
                                                                       [else (begin (set! key-number (add1 key-number)) (newline) (newline) (displayln "*******POSSIBLE KEY FOUND!!!*******")
                                                                                    (displayln (string-append "@@@+ " (~a key-number)))
                                                                                    (utils:show-key swe-modified-key)
                                                                                    (displayln "@@@- ")
                                                                                    (crack-inner-helper (cdr strategy-subs-list) (append key-list (list swe-modified-key))))])]))]))]))
                       (crack-inner-helper ((car strats) curr-key) list-of-keys))]))
  (crack-outer-helper strategies key '()))

(define (complete-helper strats key)
  (define (inner-helper subs-list m-key)
    (cond [(complete-key? m-key) m-key]
          [(null? subs-list) (complete-helper (cdr strats) m-key)]
          [(not (utils:is-monoalphabetic? (car subs-list) m-key)) (inner-helper (cdr subs-list) m-key)]
          [else (let ([dc-mod-key (algo:dictionary-closure (utils:add-substitution (car subs-list) m-key))])
                  (cond [(not (list? dc-mod-key)) (inner-helper (cdr subs-list) m-key)]
                        [else (let ([swe-mod-key (algo:secret-word-enumeration-modified dc-mod-key)])
                                (cond [(not (list? swe-mod-key)) (inner-helper (cdr subs-list) m-key)]
                                      [else (inner-helper (cdr subs-list) swe-mod-key)]))]))]))
  (if (null? strats) key (inner-helper ((car strats) key) key)))

(define (complete-if-single key)
  (cond [(or (not (list? key)) (list? (member #\_ (remove #\_ key)))) key]
        [else (let ([req-subs (car (remove* key (build-list 26 (lambda (x) (integer->char (+ x 97))))))])
                (append (takef key (lambda (x) (not (equal? x #\_)))) (list req-subs) (cdr (member #\_ key))))]))

(define (complete-key? key)
  (not (list? (member #\_ key))))

                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Optional task                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Behaviour is exactly as `crack-cipher` except that you cannot use
;; `secret-word-enumeration` because there is no secret word in the key.
(define (crack-hard-cipher strategies key) ;; Returns a list of encryption keys.
  ;; make use of `utils:ciphertext` and `utils:cipher-word-list`
  ;; DISPLAY A KEY AS SOON AS YOU FIND IT USING (show-key key)
  (define (crack-hard-outer-helper strats curr-key list-of-keys)
    (cond [(null? strats) (cond [(null? list-of-keys) (begin (displayln "*******STARTING DICTIONARY CLOSURE ON GIVEN KEY*******")
                                                             (let ([last-straw (fill-gaps (algo:dictionary-closure curr-key))])
                                                               (if (list? last-straw) (list last-straw)
                                                                   (list curr-key))))]
                                [else (begin (displayln "******LIST OF POSSIBLE KEYS IS:*******") (remove-duplicates list-of-keys))])]
          [else (begin (printf "~n~nCURRENT STRATEGY: ") (displayln (car strats))
                       (define (crack-hard-inner-helper strategy-subs-list key-list)
                         (cond [(null? strategy-subs-list) (crack-hard-outer-helper (cdr strats) curr-key key-list)]
                               [else (let* ([strat-modified-key (begin (printf "~n~nCURRENT SUBSTITUTION: ~n") (displayln (car strategy-subs-list)) (newline)
                                                                       (utils:add-substitution (car strategy-subs-list) curr-key))]
                                            [dict-closure-modified-keys (begin (printf (string-append "~n@@@+ " (~a key-number) "~n")) (utils:show-key strat-modified-key) (printf "@@@-~n")
                                                                              (fill-gaps (algo:dictionary-closure strat-modified-key key-number)))])
                                       (cond [(or (not (list? dict-closure-modified-keys)) (null? dict-closure-modified-keys))
                                              (crack-hard-inner-helper (cdr strategy-subs-list) key-list)]
                                             [(not (list? (car dict-closure-modified-keys))) (crack-hard-outer-helper (cdr strats) dict-closure-modified-keys key-list)]
                                             [(andmap (lambda (x) (member x list-of-keys)) dict-closure-modified-keys)
                                              (crack-hard-inner-helper (cdr strategy-subs-list) key-list)]
                                             [else (begin (map (lambda (x) (begin (displayln "*******POSSIBLE KEY FOUND!!!*******")
                                                                           (set! key-number (add1 key-number))
                                                                           (displayln (string-append "@@@+ " (~a key-number)))
                                                                           (utils:show-key x) (displayln "@@@-"))) dict-closure-modified-keys)
                                                   (crack-hard-inner-helper (cdr strategy-subs-list) (append key-list dict-closure-modified-keys)))]))]))
                       (crack-hard-inner-helper ((car strats) curr-key) list-of-keys))]))
  (crack-hard-outer-helper strategies key '()))

(define (fill-gaps key)
  (cond [(not (list? key)) key]
        [(> (strat:find-frequency #\_ key) 5) key]
        [else (let* ([rem-cipher-letters (remove* key (build-list 26 (lambda (x) (integer->char (+ x 97)))))]
                     [rem-plain-letters (map (lambda (x) (integer->char (+ x 65))) (indexes-of key #\_))]
                     [all-possible-subs (map (lambda (x) (map cons rem-plain-letters x)) (strat:permute rem-cipher-letters))])
                (filter list? (map (lambda (x) (algo:dictionary-closure-complete (utils:add-substitution x key))) all-possible-subs)))]))