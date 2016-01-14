#lang racket
;; A Module for basic functions [description of module]

(provide player-turn? game-over? win? evaluate register-move poss-brds depth)

;; (player-turn? turn) produces true if it is player's turn and false otherwise
;; player-turn?: Sym -> Bool

;; (game-over? board) produces true if game is over, false otherwise
;; game-over?: Board -> Bool

;; (win? moves) checks if 'moves' is a winning combination
;; win?: (listof Nat) -> Bool

;; (evaluate board) consumes a Board and produces +10 if computer wins,
;; -10 if player wins, and 0 if it is a draw...
;; evaluate: Board -> Nat

;; (register-move move board) registers 'move' in brd and produces the
;; board after the move has been made
;; register-move: Nat Board -> Board

;; (poss-brds board) produces a list of Boards that are one move from
;; the current Board
;; poss-brds: Board -> (listof Board)

;; (depth lst n) produces a list of Scores after subtracting n for each move
;; further
;; depth: (listof Any) -> (listof Num)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IMPLEMENTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Board is a (list Sym (listof Nat) (listof Nat) (listof Nat))
;; first -> Who's turn it is to play
;; second -> computer's moves
;; third -> player's moves
;; fourth -> remaining moves

;; Produces true if x is in lst and false otherwise
;; member?: X (listof Any) -> Bool
(define (member? x lst) (not (false? (member x lst)))) 

;; Defining the win-states
(define win-state-1 (list 1 2 3))
(define win-state-2 (list 4 5 6)) 
(define win-state-3 (list 7 8 9))
(define win-state-4 (list 1 4 7)) 
(define win-state-5 (list 2 5 8)) 
(define win-state-6 (list 3 6 9)) 
(define win-state-7 (list 1 5 9)) 
(define win-state-8 (list 3 5 7)) 
(define win-states (list win-state-1 win-state-2 win-state-3 win-state-4
                         win-state-5 win-state-6 win-state-7 win-state-8))

;; see interface above [no further info required]
(define (player-turn? turn)
  (if (symbol=? turn 'p) true false))

;; see interface above [no further info required]
(define (game-over? brd)
  (or(empty? (fourth brd))
     (win? (second brd))
     (win? (third brd))))

;; see interface above [no further info required]
(define (win? moves)
  (ormap (lambda (win-state)
           (andmap (lambda (ele-in-ws) (member? ele-in-ws moves)) win-state))
         win-states))

;; see interface above [no further info required]
(define (evaluate board)
  (cond[(win? (third board)) -10]
       [(win? (second board)) +10]
       [else 0]))

;; see interface above [no further info required]
(define (register-move move brd)
  (if (player-turn? (first brd))
      (list 'c
            (second brd)
            (append (third brd) (list move))
            (remove move (fourth brd)))
      (list 'p
            (append (second brd) (list move))
            (third brd)
            (remove move (fourth brd)))))

;; see interface above [no further info required]
(define (poss-brds board)
  (local[(define (each-rem board rem-lst)
           (cond[(empty? rem-lst) '()]
                [else (append (list (register-move (first rem-lst) board))
                              (each-rem board (rest rem-lst)))]))]
    (each-rem board (fourth board))))

;; see interface above [no further info required]
(define (depth lst n)
  (cond [(number? lst) (list (- lst n))]
        [(empty? lst) '()]
        [else (append (depth (first lst) (+ n .1))
                      (depth (rest lst) n))]))