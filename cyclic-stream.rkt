#lang racket/base
(require racket/list
         racket/generic
         racket/stream
         "tuple.rkt")

;; A cyclic stream of period N typically generates the same set of
;; elements (not not necessarily in the same order) for every N items.
;;
;; We represent simple cyclic streams, whcih just repeat the same N
;; elements, as a list of length N. For example, and α-factor is
;; represented as an α-list, which counts as an α-stream.
;;
;; We represent more general cyclic streams as `cyclic-stream` value,
;; which combines a period with a stream. Instances of `cyclic-stream`
;; count as instances of α-stream, of course.

(provide factor
         
         lift
         
         repeat
         sequence
         randomize
         randomize/replacement
         
         zip
         cross
         latin-square)

;; ----------------------------------------

(struct cyclic-stream (period stream)
        ;; Make a cyclic stream implement the general stream API:
        #:methods gen:stream
        [(define/generic super-stream-empty? stream-empty?)
         (define/generic super-stream-first stream-first)
         (define/generic super-stream-rest stream-rest)
         (define (stream-empty? s)
           (super-stream-empty? (cyclic-stream-stream s)))
         (define (stream-first s)
           (super-stream-first (cyclic-stream-stream s)))
         (define (stream-rest s)
           (cyclic-stream
            (cyclic-stream-period s)
            (super-stream-rest (cyclic-stream-stream s))))])

(define (stream-period s)
  (cond
   [(list? s)
    (length s)]
   [else
    (cyclic-stream-period s)]))

(define (to-stream s)
  (cond
   [(list? s) (for/stream ([e (in-cycle (in-list s))])
                e)]
   [else s]))

(define (stream-take s n)
  (for/list ([e (in-stream s)]
             [i (in-range n)])
    e))
  
(define (stream-append* str get-rest-str)
  (let loop ([str str])
    (if (stream-empty? str)
        (get-rest-str)
        (stream-cons (stream-first str)
                     (loop (stream-rest str))))))

(module+ test
  (require rackunit)
  
  (define 1-2-3...
    (cyclic-stream 3
                   (for/stream ([i (in-cycle (in-range 3))])
                     (add1 i))))
  (define a-b-b-a...
    (cyclic-stream 2
                   (for/stream ([e (in-cycle (in-list '(a b b a)))])
                     e)))
  
  (check-equal? (stream-take 1-2-3... 6)
                '(1 2 3 1 2 3))
  
  (check-equal? (stream-period 1-2-3...)
                3)
  (check-equal? (stream-period a-b-b-a...)
                2)
  (check-equal? (stream-period '(1 2 3 4))
                4))

;; ----------------------------------------
;; Implement a factor as just a list. Map operations on lists to other
;; lists.

;; factor : α-level ... α-level -> α-factor
(define (factor . levels)
  levels)


;; lift : (α -> β) α-stream -> β-stream
(define (lift f str)
  (cond
   [(list? str)
    (map f str)]
   [else
    (cyclic-stream (stream-period str)
                   (for/stream ([e (in-stream str)])
                     (f e)))]))

(module+ test
  (check-equal? (lift add1 '(1 2 3))
                '(2 3 4))
  (check-equal? (stream-take (lift add1 1-2-3...)
                             7)
                '(2 3 4 2 3 4 2)))

;; repeat : nat α-stream -> α-stream
(define (repeat n str)
  (cond
   [(list? str)
    (for*/list ([e (in-list str)]
                [i (in-range n)])
      e)]
   [(stream? str)
    (define new-s
      (for*/stream ([e (in-stream str)]
                    [i (in-range n)])
        e))
    (cyclic-stream (* n (stream-period str))
                   new-s)]))

(module+ test
  (check-equal? (repeat 2 '(1 2 3))
                '(1 1 2 2 3 3))
  (check-equal? (stream-take
                 (repeat 3 1-2-3...)
                 12)
                '(1 1 1 2 2 2 3 3 3 1 1 1))
  (check-equal? (stream-period (repeat 3 1-2-3...))
                9))


;; sequence : α-stream -> α-stream
(define (sequence . strs)
  (cond
   [(andmap list? strs)
    (apply append strs)]
   [else
    (define new-s
      (let loop ([strs strs])
        (stream-append*
         (for/stream ([str (in-list strs)]
                      #:when #t
                      [e (in-stream str)]
                      [i (in-range (stream-period str))])
           e)
         (lambda () (loop (for/list ([str (in-list strs)])
                       (stream-tail str (stream-period str))))))))
    (cyclic-stream (apply + (map stream-period strs))
                   new-s)]))

(module+ test
  (check-equal? (sequence '(a b c) '(x y z))
                '(a b c x y z))
  (check-equal? (stream-take (sequence '(a b c) 1-2-3...)
                             7)
                '(a b c 1 2 3 1))
  (check-equal? (stream-take (sequence a-b-b-a... 1-2-3...)
                             12)
                '(a b 1 2 3 b a 1 2 3 a b))
  (check-equal? (stream-period (sequence a-b-b-a... 1-2-3...))
                5))


;; rotate : α-stream -> α-stream
(define (rotate given-i str)
  (define period (stream-period str))
  (define i (modulo given-i period))
  (cond
   [(zero? i) str]
   [(list? str) (append (list-tail str i)
                        (take str i))]
   [else
    (define new-str
      (let loop ([str str])
        (define init-elems (stream-take str i))
        (define rest-str (stream-tail str i))
        (stream-append*
         (stream-append
          (stream-take rest-str (- period i))
          init-elems)
         (lambda () (loop (stream-tail rest-str (- period i)))))))
    (cyclic-stream period
                   new-str)]))

(module+ test
  (check-equal? (rotate 1 '(a b c))
                '(b c a))
  (check-equal? (rotate 2 '(a b c))
                '(c a b))
  (check-equal? (rotate 5 '(a b c))
                '(c a b))
  
  (check-equal? (stream-take (rotate 2 1-2-3...)
                             6)
                '(3 1 2 3 1 2)))


;; randomize : α-stream -> α-stream
(define (randomize str)
  (cond
   [(list? str)
    (cyclic-stream (length str)
                   (for*/stream ([i (in-naturals)]
                                 [e (in-list (shuffle str))])
                     e))]
   [else
    (define period (stream-period str))
    (define new-str
      (let loop ([str str])
        (define elems (stream-take str period))
        (stream-append*
         (shuffle elems)
         (lambda () (loop (stream-tail str period))))))
    (cyclic-stream period
                   new-str)]))

(module+ test
  (define (check-randomize str elems)
    ;; Each set of N has all elements, and eventually
    ;; we cover all possible orders; this test fails will very low
    ;; probability, since we try at most N! * 1000 iterations.
    (define N (length elems))
    (define (! n) (if (zero? n) 1 (* n (! (sub1 n)))))
    (define combos (! N))
    (check-true
     (let loop ([str (randomize str)] [ht (hash)] [count 0])
       (cond
        [(= count (* combos 1000)) (error "failed to find all orders" ht)]
        [else
         (define group (stream-take str N))
         (define new-ht (hash-set ht group #t))
         (and (for/and ([e (in-list elems)])
                (member e group))
              (or (= combos (hash-count ht))
                  (loop (stream-tail str N) new-ht (add1 count))))]))))
  
  (check-randomize '(1 2 3) '(1 2 3))
  (check-randomize 1-2-3... '(1 2 3))
  (check-randomize a-b-b-a... '(a b)))


;; randomize/replacement : α-stream -> α-stream
(define (randomize/replacement str)
  (cond
   [(list? str)
    (define len (length str))
    (for/stream ([i (in-naturals)])
      (list-ref str (random len)))]
   [else
    (define period (stream-period str))
    (define new-str
      (let loop ([str str])
        (define elems (stream-take str period))
        (stream-cons (list-ref elems (random period))
                     (loop (stream-tail str period)))))
    (cyclic-stream period new-str)]))

(module+ test
  (define (check-randomize/replacement str elems)
    ;; After 10000 iterations, distribution of each element should be
    ;; close. This test fails with low probability.
    (define counts
      (for/fold ([counts (hash)]) ([e (in-stream (randomize/replacement str))]
                                   [i (in-range 10000)])
        (hash-update counts e add1 0)))
    (define expected (/ 10000 (length elems)))
    (check-true
     (for/and ([e (in-list elems)])
       (define c (hash-ref counts e))
       (<= (* 0.9 expected) c (* 1.1 expected)))))
  
  (check-randomize/replacement '(1 2 3) '(1 2 3))
  (check-randomize/replacement 1-2-3... '(1 2 3))
  (check-randomize/replacement a-b-b-a... '(a b)))

;; ----------------------------------------

;; zip : α_1-stream ... α_n-stream -> (α_1 * ... α_n)-stream
(define (zip #:period [given-period #f]
             str . rest-strs)
  (cond
   [(null? rest-strs) str]
   [else
    (define rest-zip (apply zip rest-strs
                            #:period given-period))
    (cond
     [(and (list? str)
           (list? rest-zip))
      (define period
        (or given-period (lcm (length str) (length rest-zip))))
      (for/list ([i (in-range period)]
                 [e (in-cycle (in-list str))]
                 [r (in-cycle (in-list rest-zip))])
        (tuple-join e r))]
     [else
      (define new-s
        (for/stream ([e (in-cycle (in-stream str))]
                     [r (in-cycle (in-stream rest-zip))])
          (tuple-join e r)))
      (cyclic-stream (or given-period
                         (lcm (stream-period str)
                              (stream-period rest-zip)))
                     new-s)])]))

(module+ test
  (check-equal? (zip '(1 2 3) '(a b))
                (list (tuple 1 'a) (tuple 2 'b) (tuple 3 'a)
                      (tuple 1 'b) (tuple 2 'a) (tuple 3 'b)))
  (check-equal? (stream-take (zip 1-2-3... a-b-b-a...)
                             9)
                (list (tuple 1 'a) (tuple 2 'b) (tuple 3 'b)
                      (tuple 1 'a) (tuple 2 'a) (tuple 3 'b)
                      (tuple 1 'b) (tuple 2 'a) (tuple 3 'a)))
  (check-equal? (stream-period (zip 1-2-3... a-b-b-a...))
                6))
                

;; cross : α_1-stream α_n-stream -> (α_1 * ... α_n)-stream
(define cross
  (case-lambda
    [(str) str]
    [(str . rest)
     (define rest-cross (apply cross rest))
     (cond
      [(and (list? str)
            (list? rest-cross))
       (for*/list ([e (in-list str)]
                   [r (in-list rest-cross)])
         (tuple-join e r))]
      [else
       (define period (stream-period str))
       (define rest-period (stream-period rest-cross))
       (define new-s
         (let loop ([str (to-stream str)] [rest-cross (to-stream rest-cross)])
           (stream-append*
            (for/stream ([e (in-stream str)]
                         [i (in-range period)]
                         #:when #t
                         [e-rest (in-stream rest-cross)]
                         [i-rest (in-range rest-period)])
              (tuple-join e e-rest))
            (lambda ()
              (loop (stream-tail str period)
                    (stream-tail rest-cross rest-period))))))
       (cyclic-stream (* period rest-period)
                           new-s)])]))

(module+ test
  (check-equal? (cross '(1 2 3) '(a b))
                (list (tuple 1 'a) (tuple 1 'b)
                      (tuple 2 'a) (tuple 2 'b)
                      (tuple 3 'a) (tuple 3 'b)))
  (check-equal? (stream-take (cross 1-2-3... '(a b))
                             6)
                (list (tuple 1 'a) (tuple 1 'b)
                      (tuple 2 'a) (tuple 2 'b)
                      (tuple 3 'a) (tuple 3 'b)))
  (check-equal? (stream-take (cross 1-2-3... a-b-b-a...)
                             9)
                (list (tuple 1 'a) (tuple 1 'b)
                      (tuple 2 'a) (tuple 2 'b)
                      (tuple 3 'a) (tuple 3 'b)
                      (tuple 1 'b) (tuple 1 'a)
                      (tuple 2 'b)))
  (check-equal? (stream-period (cross 1-2-3... a-b-b-a...))
                6))

;; latin-square : α_1-stream ... α_n-stream -> ((α_1 * ... α_n)-stream)-list
(define latin-square
  (case-lambda
    [(str) (list str)]
    [(str . strs)
     (define num-zips (stream-period str))
     (define new-period (apply * (map stream-period strs)))
     (for/list ([i (in-range num-zips)])
       (apply zip #:period new-period
              (rotate i str)
              strs))]))

(module+ test
  (check-equal? (latin-square '(1 2) '(a b))
                (list
                 (list (tuple 1 'a) (tuple 2 'b))
                 (list (tuple 2 'a) (tuple 1 'b))))
  (check-equal? (latin-square '(1 2) '(a b c))
                (list
                 (list (tuple 1 'a) (tuple 2 'b) (tuple 1 'c))
                 (list (tuple 2 'a) (tuple 1 'b) (tuple 2 'c))))
  (check-equal? (latin-square (cross '(1 2) '(x y)) '(a b c))
                (list
                 (list (tuple 1 'x 'a) (tuple 1 'y 'b) (tuple 2 'x 'c))
                 (list (tuple 1 'y 'a) (tuple 2 'x 'b) (tuple 2 'y 'c))
                 (list (tuple 2 'x 'a) (tuple 2 'y 'b) (tuple 1 'x 'c))
                 (list (tuple 2 'y 'a) (tuple 1 'x 'b) (tuple 1 'y 'c))))
  (check-equal? (latin-square '(1 2) '(x y) '(a b c))
                (list
                 (list (tuple 1 'x 'a) (tuple 2 'y 'b) (tuple 1 'x 'c)
                       (tuple 2 'y 'a) (tuple 1 'x 'b) (tuple 2 'y 'c))
                 (list (tuple 2 'x 'a) (tuple 1 'y 'b) (tuple 2 'x 'c)
                       (tuple 1 'y 'a) (tuple 2 'x 'b) (tuple 1 'y 'c)))))

;; ----------------------------------------

;; permutations : α-stream -> α-stream-stream
(define (permutations str)
  (cond
   [(list? str)
    (define len (length str))
    (cond
     [(= 1 len)
      (list str)]
     [else
      (define a (first str))
      (define rest-permutations (permutations (rest str)))
      (for*/list ([i (in-range len)]
                  [l (in-list rest-permutations)])
        (append (take l i)
                (list a)
                (drop l i)))])]
   [else
    (define period (stream-period str))
    (define new-str
      (let loop ([str str])
        (define elems (stream-take str period))
        (stream-append*
         (permutations elems)
         (lambda () (loop (stream-tail str period))))))
    (cyclic-stream (factorial period)
                   new-str)]))

(module+ test
  (check-equal? (permutations '(1 2 3))
                (list
                 '(1 2 3) '(1 3 2)
                 '(2 1 3) '(3 1 2)
                 '(2 3 1) '(3 2 1)))
  (check-equal? (stream-take (permutations 1-2-3...) 8)
                (list
                 '(1 2 3) '(1 3 2)
                 '(2 1 3) '(3 1 2)
                 '(2 3 1) '(3 2 1)
                 '(1 2 3) '(1 3 2)))
  (check-equal? (stream-period (permutations '(1 2 3)))
                6))

;; I've written `factorial` as an example or benchmark many, many
;; times --- but this is the first time I've seen it in a real
;; program!
(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))

;; ----------------------------------------

;; flatten : α-stream-stream [integer] -> α-stream
;;  If a period is not given, assumes that every α-stream in the
;;  α-stream-stream has the same period
;;
;;  Since we have an infinite stream of infinite streams, the
;;  `explore-mode` argument determines which dimension to explore:
;;
;;    'inner : flattening uses only the first period of elements from
;;             the α-stream-stream, working through all elements of
;;             each of those sub-streams
;;
;;    'outer : flatten takes the first period of elements from the
;;             α-stream-stream, works through only the first period of
;;             element from each sub-stream, and then continues with
;;             the next period of the α-stream-stream
(define (flatten str-of-str [period (stream-period (stream-first str-of-str))]
                 #:explore-mode [explore-mode 'inner])
  (cond
   [(and (list? str-of-str)
         (= 1 (length str-of-str))
         (list? (first str-of-str))
         (= period (length (first str-of-str))))
    (first str-of-str)]
   [else
    (define strs-period (stream-period str-of-str))
    (define flattened-str
      (let loop ([str-of-str str-of-str])
        (stream-append*
         (for/stream ([str (in-cycle (in-stream str-of-str))]
                      [j (in-range strs-period)]
                      #:when #t
                      [e (in-cycle (in-stream str))]
                      [i (in-range period)])
           e)
         (lambda ()
           (loop (case explore-mode
                   [(inner)
                    (for/list ([str (in-stream str-of-str)]
                               [j (in-range strs-period)])
                      (if (list? str)
                          str
                          (stream-tail str period)))]
                   [(outer)
                    (if (list? str-of-str)
                        str-of-str
                        (stream-tail str-of-str strs-period))]))))))
    (cyclic-stream period
                   flattened-str)]))

(module+ test
  (check-equal? (flatten '((x y)))
                '(x y))
  (check-equal? (stream-take (flatten '((1 2 3) (a b c))) 9)
                '(1 2 3 a b c 1 2 3))
  (check-equal? (stream-take (flatten 
                              (cyclic-stream
                               2
                               (for/stream ([str (in-cycle
                                                  (list '(a b c) 1-2-3...))])
                                 str)))
                             9)
                '(a b c 1 2 3 a b c))
  (check-equal? (stream-take (flatten (list '(1 2) a-b-b-a...))
                             8)
                '(1 2 a b 1 2 b a))
  (check-equal? (stream-take (flatten (list '(1 2) a-b-b-a...)
                                      #:explore-mode 'outer)
                             8)
                '(1 2 a b 1 2 a b)))

;; ----------------------------------------

;; alternate : α-stream ... α-stream -> α-stream
(define (alternate str . rest-strs)
  (define strs (cons str rest-strs))
  (define period (apply lcm (map stream-period strs)))
  (flatten strs period #:explore-mode 'inner))

(module+ test
  (check-equal? (alternate '(1 2 3))
                '(1 2 3))
  (check-equal? (stream-take (alternate '(1 2 3) '(a b c))
                             9)
                '(1 2 3 a b c 1 2 3))
  (check-equal? (stream-take (alternate 1-2-3... '(a b c))
                             9)
                '(1 2 3 a b c 1 2 3))
  (check-equal? (stream-take (alternate 1-2-3... a-b-b-a...)
                             24)
                '(1 2 3 1 2 3
                  a b b a a b
                  1 2 3 1 2 3
                  b a a b b a)))

;; ----------------------------------------

;; Helper to roughly demonstrate what a stream produces:
(define (demo label v)
  (printf "~a:\n" label)
  (define (show-stream str)
    (define period (stream-period str))
    (for/fold ([str str]) ([i (if (list? str) 1 3)])
      (for ([e (in-stream str)]
            [i (in-range period)])
        (if (cyclic-stream? e)
            (show-stream e)
            (printf " ~a" e)))
      (printf "\n")
      (stream-tail str period))
    (printf "\n"))
  (show-stream v))

;; ----------------------------------------

(module+ main
  (define groups (list 1 2 3))

  (define colors (factor "red" "green" "blue"))
  (define words (factor "apple" "banana" "coconut"))
  
  (demo "Randomization of full crossing"
        (randomize (cross colors words)))

  (demo "Match group with a Latin Square color colors*words"
        (zip groups
             (latin-square colors words)))

  (demo "Cross colors and words with blocked-random for colors"
        (cross (randomize colors) (randomize words)))

  (demo "Select color by group, combine with random words"
        (lift
         (lambda (g+c)
           (lift (lambda (word) (tuple-join g+c word))
                 (randomize words)))
         (zip groups
              colors))))

;; New operations: permutations, permutations-in-random-order, stream-flatten
;; Terminology: balance
