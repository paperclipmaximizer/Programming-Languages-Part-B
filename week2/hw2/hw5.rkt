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

(define (racketlist->mupllist lst)
  (cond [(empty? lst) (aunit)]
        [else
         (apair (car lst)
                (racketlist->mupllist (cdr lst)))]))

(define (mupllist->racketlist lst)
  (cond [(aunit? lst) empty]
        [else
         (cons (apair-e1 lst)
               (mupllist->racketlist (apair-e2 lst)))]))

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (eval-under-env e env)
  (cond [(var? e) (envlookup env (var-string e))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
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
               (error "MUPL snd applied to non-pair")))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [a (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let ([cfn (closure-fun c)])
                 (eval-under-env (fun-body cfn)
                  (let ([extended-env
                         (cons (cons (fun-formal cfn) a)
                               (closure-env c))])
                    (if (fun-nameopt cfn)
                        (cons
                         (cons (fun-nameopt cfn) c) extended-env)
                        extended-env))))
               (error "call-funexp is not a closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))
        
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0)
       e2
       e3))

(define (mlet* lstlst e2)
  (cond [(empty? lstlst) e2]
        [else
         (mlet (caar lstlst)
               (cdar lstlst)
               (mlet* (cdr lstlst) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
    (mlet "_y" e2
      (ifgreater (var "_y")(var "_x")
                  e4
                 (ifgreater (var "_x") (var "_y")
                             e4
                             e3)))))

(define mupl-map
  (fun "mupl-map" "mupl-fun"
       (fun "aux" "mupl-list"
       (ifaunit (var "mupl-list") 
                (aunit)
                (apair (call (var "mupl-fun") (fst (var "mupl-list")))
                       (call (var "aux") (snd (var "mupl-list"))))))))


(define mupl-mapAddN 
  (mlet "map" mupl-map
    (fun "mupl-mapAddN" "i"
       (fun #f "mupl-list"
          (call (call (var "map")
                      (fun #f "x"
                          (add (var "x")
                               (var "i"))))
                (var "mupl-list"))))))