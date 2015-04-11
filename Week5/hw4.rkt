#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence(+ low stride) high stride))
      null
      );;if
  );;sequence

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))


(define (funny-number-stream)
  (define (helper n) (cons (if (= 0 (remainder n 5)) (- n) n)
                           (lambda () (helper(+ n 1)))))
  (helper 1))

(define (dan-then-dog)
  (define (helper x) (cons x (lambda () helper (if (eq? "dan.jpg" x) "dog.jpg" "dan.jpg"))))
  (helper "dan.jpg")
  )

(define (stream-add-zero s)
  (define (helper n) (cons
                 (cons 0 (car (n)))
                 (lambda () (helper (cdr (n))))))
  (lambda () (helper s)))


 (define (cycle-lists xs ys)
   (define (helper n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (helper (+ n 1)))))
   (lambda () (helper 0))
   )

 
(define (vector-assoc v vec)
  (define (helper n)
    (if (>= n (vector-length vec)) #f
        (let ([thisv (vector-ref vec n)])
          (cond [(not (pair? thisv)) (helper (+ 1 n))]
                [(equal? v (car thisv)) thisv]
                [else (helper (+ n 1))])))
         );else
  (helper 0)
  )
