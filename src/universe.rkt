#lang racket

;; A module to create the Board
(provide initialize display-moves)
(require 2htdp/image)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define size 150)
(define box (rectangle size size 'outline 'black))
(define 3-boxes (beside box box box))
(define display-text (text "Hello" 24 "orange"))
(define text-area (rectangle (* 3 size) (/ size 3)  'outline 'white))
(define ttt-brd (above 3-boxes 3-boxes 3-boxes text-area))


(define (display-moves brd x y me)
  (local (;; Nat Str -> Bool
          (define (on-sq? n)
            (and (<= (abs (- (first (gridsq n)) x))(/ size 2))
                 (<= (abs (- (second (gridsq n)) y)) (/ size 2))
                 (string=? me "button-down")))
          ;; Nat -> Board
          ;; makes a new board
          (define (create-board n)
            (list 'c
                  (second brd)
                  (append (third brd) (list n))
                  (remove n (fourth brd)))))
    (cond
      [(on-sq? 1)(create-board 1)]
      [(on-sq? 2)(create-board 2)]
      [(on-sq? 3)(create-board 3)]
      [(on-sq? 4)(create-board 4)]
      [(on-sq? 5)(create-board 5)]
      [(on-sq? 6)(create-board 6)]
      [(on-sq? 7)(create-board 7)]
      [(on-sq? 8)(create-board 8)]
      [(on-sq? 9)(create-board 9)]
      [else brd])))

(define (initialize brd)
  (place-image
   (text (cond[(symbol=? 'p (first brd))  "Your Turn"]
              [else "Computer thinking"])
         (floor (/ size 6)) 'green)
   (floor (* 1.5 size))
   (floor (+ (* 3.15 size) ))
   (foldr p-moves
          (foldr c-moves ttt-brd (second brd))
          (third brd))))


;; Nat Img -> Img
(define (c-moves n im)
  (place-image (text "O" (/ size 2) 'blue)
               (first (gridsq n))
               (second (gridsq n))
               im))

;; Num Img -> Img
(define (p-moves n im)
  (place-image (text "X" (/ size 2) 'red)
               (first (gridsq n))
               (second (gridsq n))
               im))


;; Nat -> (listof Num Num)
(define (gridsq n)
  (cond
    [(= n 1)(grid 1 1)]
    [(= n 2)(grid 2 1)]
    [(= n 3)(grid 3 1)]
    [(= n 4)(grid 1 2)]
    [(= n 5)(grid 2 2)]
    [(= n 6)(grid 3 2)]
    [(= n 7)(grid 1 3)]
    [(= n 8)(grid 2 3)]
    [(= n 9)(grid 3 3)]))


(define (grid x y)
  (list (* size (- x .5)) (* size (- y .5))))

