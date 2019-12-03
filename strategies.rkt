#lang racket

;; You can require more modules of your choice.
(require racket/list
         "list-comprehension.rkt"
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in dictc: "dict-closure.rkt")
         (prefix-in swe: "secret-word-enumeration.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         common-words-double
         common-words-triple
         common-words-quadruple
         bigrams
         common-initial-letters
         common-final-letters
         common-words-triple
         trigrams
         common-double-letters
         quadgrams
         find-frequency
         cipher-letters-list
         permute
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (strategy key strategy-substitution-list)
  (filter (lambda (x) (utils:is-monoalphabetic? x key)) (remove* '(()) strategy-substitution-list)))

(define (etai key)
  (strategy key etai-substitution-list))

(define (find-frequency element lst)
  (foldr (lambda (x y) (if (equal? x element) (+ 1 y) y)) 0 lst))

(define empty-key (make-list 26 #\_))

(define (permute l)
  (cond [(null? l) '(())]
        [else (define (g x) (define (h y) (cons x y))
                  (map h (permute (remove x l))))
  (append* (map g l))]))

(define (zip-all l lstoflst)
  (map (lambda (x) (dictc:binary-map cons l x)) lstoflst))

(define (first-k k lst)
  (if (or (null? lst) (= k 0)) '()
      (cons (car lst) (first-k (- k 1) (cdr lst)))))

(define (convert-each-char l)
  (foldr (lambda (x y) (append (string->list x) y)) '() l))

(define (permute-k k l)
  (cond [(or (= k 0) (null? l)) '(())]
        [else (define (g x) (define (h y) (cons x y))
                  (map h (permute-k (- k 1) (remove x l))))
  (append* (map g l))]))

(define cipher-letters-list (stats:cipher-monograms utils:ciphertext))
(define top-five (first-k 5 cipher-letters-list))
(define single-letter-cipher-words-list (filter char-alphabetic? (convert-each-char (stats:cipher-common-words-single utils:cipher-word-list))))
(define non-single-lettered-candidates (remove* single-letter-cipher-words-list top-five))
(define candidate-neighbourhood-list (stats:zip-sort (append* (map (lambda (element) (filter (lambda (y) (equal? (car y) element)) (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both))) non-single-lettered-candidates))))
(define remaining-list (remove (car (last candidate-neighbourhood-list)) non-single-lettered-candidates))
    
(define etai-substitution-list
  (let ([tmp-lst (permute-k (- 4 (length single-letter-cipher-words-list)) (append (list (car remaining-list) (car (last candidate-neighbourhood-list))) (cdr remaining-list)))])
    (cond [(= (length single-letter-cipher-words-list) 1) (zip-all (list #\E #\T #\A #\I) (append* (map (lambda (x) (cons (append x single-letter-cipher-words-list) (list (append (first-k 2 x) single-letter-cipher-words-list (list (last x)))))) tmp-lst)))]
          [(= (length single-letter-cipher-words-list) 2) (zip-all (list #\E #\T #\A #\I) (append* (map (lambda (x) (cons (append x single-letter-cipher-words-list) (list (append x (reverse single-letter-cipher-words-list))))) tmp-lst)))]
          [else (zip-all (list #\E #\T #\A #\I) tmp-lst)])))

(define (word-zip char-list1 char-list2)
    (dictc:binary-map cons (string->list char-list1) (string->list char-list2)))

(define (word-mapping lofl1 l2)
   (map (lambda (elementlst) (remove-duplicates (append* (dictc:binary-map word-zip elementlst l2)))) lofl1))

(define (filter-monoalphabetic l)
  (filter (lambda (x) (utils:is-monoalphabetic? x empty-key)) l))

(define top-three-double-cipher-words (first-k 3 (stats:cipher-common-words-double utils:cipher-word-list)))
(define top-ten-double-plain-words (first-k 10 utils:plain-common-words-double))
(define common-words-double-substitution-list (if (null? top-three-double-cipher-words) '()
                                                  (remove-duplicates (first-k 25 (filter-monoalphabetic (word-mapping (permute-k 3 top-ten-double-plain-words) top-three-double-cipher-words))))))

(define (common-words-double key)
  (strategy key common-words-double-substitution-list))

(define top-three-triple-cipher-words (first-k 3 (stats:cipher-common-words-triple utils:cipher-word-list)))
(define top-ten-triple-plain-words (first-k 10 utils:plain-common-words-triple))
(define common-words-triple-substitution-list (if (null? top-three-triple-cipher-words) '()
                                                  (remove-duplicates (first-k 20 (filter-monoalphabetic (word-mapping (permute-k 3 top-ten-triple-plain-words) top-three-triple-cipher-words))))))

(define (common-words-triple key)
  (strategy key common-words-triple-substitution-list))

(define most-frequent-quadruple-cipher-word (stats:cipher-common-words-quadruple utils:cipher-word-list))
(define top-ten-quadruple-plain-words (first-k 10 utils:plain-common-words-quadruple))
(define common-words-quadruple-substitution-list (if (null? most-frequent-quadruple-cipher-word) '()
                                                     (remove-duplicates (filter-monoalphabetic (word-mapping (permute-k 1 top-ten-quadruple-plain-words) most-frequent-quadruple-cipher-word)))))

(define (common-words-quadruple key)
  (strategy key common-words-quadruple-substitution-list))

(define top-three-cipher-bigrams (first-k 3 (stats:cipher-bigrams utils:cipher-word-list)))
(define top-ten-plain-bigrams (first-k 10 utils:plain-bigrams))
(define bigrams-substitution-list (if (null? top-three-cipher-bigrams) '()
                                      (remove-duplicates (filter-monoalphabetic (word-mapping (permute-k 3 top-ten-plain-bigrams) top-three-cipher-bigrams)))))

(define (bigrams key)
  (strategy key bigrams-substitution-list))

(define top-two-cipher-trigrams (first-k 2 (stats:cipher-trigrams utils:cipher-word-list)))
(define top-ten-plain-trigrams (first-k 10 utils:plain-trigrams))
(define trigrams-substitution-list (if (null? top-two-cipher-trigrams) '()
                                       (filter-monoalphabetic (word-mapping (permute-k 2 top-ten-plain-trigrams) top-two-cipher-trigrams))))

(define (trigrams key)
  (strategy key trigrams-substitution-list))

(define top-five-cipher-quadgrams (first-k 5 (stats:cipher-quadgrams utils:cipher-word-list)))
(define top-five-plain-quadgrams (first-k 5 utils:plain-quadgrams))
(define quadgrams-substitution-list (if (null? top-five-cipher-quadgrams) '()
                                        (lc (word-zip x y) : x <- top-five-plain-quadgrams y <- top-five-cipher-quadgrams @(utils:is-monoalphabetic?
                                                                                                                            (remove-duplicates (word-zip x y)) empty-key))))
(define (quadgrams key)
  (strategy key quadgrams-substitution-list))

(define top-five-cipher-initial-letters (first-k 5 (stats:cipher-common-initial-letters utils:cipher-word-list)))
(define top-three-plain-initial-letters (first-k 3 utils:plain-common-initial-letters))
(define common-initial-letters-substitution-list (if (null? top-five-cipher-initial-letters) '()
                                                     (first-k 20 (zip-all (remove* (list #\space) (convert-each-char top-three-plain-initial-letters)) (permute-k 3 top-five-cipher-initial-letters)))))

(define (common-initial-letters key)
  (strategy key common-initial-letters-substitution-list))

(define top-five-cipher-final-letters (first-k 5 (stats:cipher-common-final-letters utils:cipher-word-list)))
(define top-three-plain-final-letters (first-k 3 utils:plain-common-final-letters))
(define common-final-letters-substitution-list (if (null? top-five-cipher-final-letters) '()
                                                   (first-k 20 (zip-all (remove* (list #\space) (convert-each-char top-three-plain-final-letters)) (permute-k 3 top-five-cipher-final-letters)))))

(define (common-final-letters key)
  (strategy key common-final-letters-substitution-list))

(define (convert-each-singly-char l)
  (define (f x y) (cons (car (string->list x)) y))
  (foldr f '() l))

(define top-two-plain-double-letters (first-k 2 utils:plain-common-double-letters))
(define common-double-letters-substitution-list (if (null? top-two-plain-double-letters) '()
                                                    (zip-all (convert-each-singly-char top-two-plain-double-letters) (permute-k 2 (stats:cipher-common-double-letters utils:cipher-word-list)))))

(define (common-double-letters key)
  (strategy key common-double-letters-substitution-list))
;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai
                          common-words-double
                          common-words-triple
                          common-words-quadruple
                          bigrams
                          trigrams
                          quadgrams
                          common-initial-letters
                          common-final-letters))
                          ;common-double-letters))


