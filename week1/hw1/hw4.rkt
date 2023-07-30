#lang racket
(provide (all-defined-out))

;; 1.
;; Integer Integer Integer -> List
;; Produces list from low to high
(define (sequence low high stride)
  (cond [(< high low) '()]
        [#t
         (cons low
               (sequence (+ low stride) high stride))]))


;; 2.
;; (Listof String) String -> Listof String
;; Appends suffix to each list item using map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;; 3.
;; (Listof Number) Number -> Number
;; returns val at index i of i = remainder of n / list-length
(define (list-nth-mod xs n)
  (cond [(null? xs)   (error "list-nth-mod: empty list")]
        [(negative? n)(error "list-nth-mod: negative number")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


;; 4.
;; Stream Natural -> (Listof x)
(define (stream-for-n-steps s n)
  (cond [(zero? n) empty]
        [#t
         (letrec ([st (s)])
           (cons (car st)
                 (stream-for-n-steps (cdr st) (sub1 n))))]))


;; 5.
;; _ -> Stream
;; Creates stream of numbers, divisibles by 5 are negated
(define funny-number-stream
  (letrec
      ([f(lambda (x)
           (cons (if (zero? (remainder x 5))
                     (- x)
                     x)
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


;; 6.
;; _ -> Stream
;; ele of stream alternate between string "dan.jpg" and "dog.jpg"
;; thunk that when called produces pair ("dan.jpg" . (thunk: (n_1: "dog.jpg") . ...)
(define dan-then-dog
  (local [(define (acc p)
            (lambda ()
              (cons (car p)
                    (acc (cons (cdr p) (car p))))))]
    (acc (cons "dan.jpg" "dog.jpg"))))


;; 7.
;; Stream -> Stream
;; map-stream (cons 0 v)
(define (stream-add-zero s)
  (lambda () (local [(define inner (s))]
               (cons (cons 0 (car inner))
                     (stream-add-zero (cdr inner))))))


;; 8.
;; (Listof X) (Listof Y) -> Stream
;; interleaves xs and ys in a continuous fashion
(define (cycle-lists xs ys)
  (letrec
      ([f (lambda (n)
            (lambda ()(cons (cons (list-nth-mod xs n)
                                  (list-nth-mod ys n))
                            (f (+ n 1)))))])
    (f 0)))


;; 9.
;; X (Pairof Vector) -> (Pairof X)
;; takes a value and a list of pairs and returns the first pair in the list where
;; the car of the pair is equal? to v. It returns #f if no pair has such car
;; uses vector-length, vector-ref and equal?
(define (vector-assoc v vec)
  (cond [(empty? vec) #f]
        [else
         (letrec ([f (lambda (n)
                       (cond [(equal? (vector-length vec) n) #f]
                             [else
                              (let ([vr (vector-ref vec n)])
                                (if (and (pair? vr)
                                         (equal? (car vr) v))
                                    vr
                                    (f (+ n 1))))]))])
           (f 0))]))


;; 10.
;; (Vector Pair) Number -> Func
;; assoc with n-element cache of recent results, cache is a racket vector of length n
(define (cached-assoc vec n)
  (letrec([memo (make-vector n #f)] ;; cache starts empty
          [i 0]
          [f (lambda (mv)
               (let ([ans (vector-assoc mv memo)]) ;; calls helper to check the cache for the answer
                 (if (not (false? ans))
                     ans
                     (let ([new-ans (assoc mv vec)])
                       (begin
                         (vector-set! memo i new-ans)
                         (set! i (if (= (+ i 1) n) 0 (+ i 1)))
                         new-ans)))))])
    f))


;; 11. ======== Challenge Problem ========
;; Integer Integer -> Proc | #t
;; (While-less e1:v1, e2:v2_n; v1 < v2
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([l e1])
       (letrec ([loop (lambda ()
                        (local [(define v2 e2)]
                        (if (or (not (number? v2))(< l v2))
                            #t
                            (loop))))])
         (loop)))]))