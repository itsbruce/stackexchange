; http://codereview.stackexchange.com/questions/79244/
; Version closer to OP's
(define (square n) (* n n))
(define (largest2 a b c) (list (max a b) (max (min a b) c)))
(define (square_of_larger_numbs a b c)
  (apply + (map square (largest2 a b c)))
)
; Denser version
(define (square_of_larger_numbs a b c)
  (apply + (map (lambda (n) (* n n))
    (list (max a b) (max (min a b) c))))
)
