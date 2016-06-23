#lang racket/base

;; A tuple of size N combines N values in order, where a non-tuple
;; value counts as a type of size 1.

(provide (rename-out [create-tuple tuple])
         tuple-join
         tuple-ref
         tuple-size)

;; Implement a multi-value tuple `α_1 * ... α_n` as a list, but wrap
;; it in a structure to make tuples distinct from lists. Implement as
;; single-value tuple as the value itself.

(struct tuple (list)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc tuple port mode)
           (write-string (if mode "(tuple " "<") port)
           (define l (tuple-list tuple))
           (define recur
             (case mode
               [(#t) write]
               [(#f) display]
               [else (lambda (p port) (print p port mode))]))
           (unless (null? l)
             (recur (car l) port)
             (for-each (lambda (e)
                         (write-string (if mode " " ", ") port)
                         (recur e port))
                       (cdr l)))
           (write-string (if mode ")" ">") port))])

(define create-tuple
  (case-lambda
    [(v) v]
    [vs (tuple vs)]))
(define (tuple-join l r)
  (tuple (append (if (tuple? l)
                     (tuple-list l)
                     (list l))
                 (if (tuple? r)
                     (tuple-list r)
                     (list r)))))
(define (tuple-ref t i) (list-ref (tuple-list t) i))
(define (tuple-size t) (length (tuple-list t)))
