;; Programming Languages, Homework 5
#lang racket
(provide (all-defined-out))
(struct var       (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int       (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add       (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun       (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call      (funexp actual)       #:transparent) ;; function call
(struct mlet      (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair     (e1 e2)     #:transparent) ;; make a new pair
(struct fst       (e)    #:transparent) ;; get first part of a pair
(struct snd       (e)    #:transparent) ;; get second part of a pair
(struct aunit     ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit   (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct closure   (env fun) #:transparent) ;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
define-syntax let*
;; Racket-List -> Mupl-List
;; takes a Racket list and makes a MUPL list
(define (racketlist->mupllist lst)
  (cond [(empty? lst) (aunit)]
        [else
           (apair (car lst)
                  (racketlist->mupllist (cdr lst)))]))

;; MUPL-List -> Racket-List
;; takes a MUPL list and makes it a racket list
(define (mupllist->racketlist lst)
  (cond [(aunit? lst) '()]
        [else
         (cons (apair-e1 lst)
               (mupllist->racketlist (apair-e2 lst)))]))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
                ;(("x" .... = "x" then in (cdr("x" . (int 5))) , return (int 1)
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL fst applied to non-pair")))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isaunit? e)
         (let([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1)  (int 0)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1)(int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater type mismatch")))]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [extended-env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) extended-env))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [a (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let ([cfn (closure-fun c)])
                 (eval-under-env (fun-body cfn)
                                 (let ([extended-env (cons (cons (fun-formal cfn) a)
                                                           (closure-env c))])
                                   (if (fun-nameopt cfn)
                                       (cons (cons (fun-nameopt cfn) c) extended-env)
                                       extended-env))))
               (error (format "call-funexp ~e is not a closure" (call-funexp e)))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
;; (int 1) (int 2) (int 3)
(define (ifaunit e1 e2 e3)
  (if (isaunit? e1)
      (eval-under-env e2 '())
      (eval-under-env e3 '())))
;; (s1.e1)...(si.ei)

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
