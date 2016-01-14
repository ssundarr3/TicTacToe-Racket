#lang racket



(require "definitions.rkt")
(require "universe.rkt")
(require 2htdp/universe)
(provide main)

;; A Board is a (list Sym (listof Nat))
;; first -> Who's turn it is to play
;; second -> computer's moves
;; third -> player's moves
;; fourth -> remaining moves



(define brd-test-1 (list 'c '(1 3) '(5 9 2) '(4 6 7 8)))
(define brd-test-2 (list 'c '(2 5 7) '(1 3 4 6)  '(5 8 9)))
(define brd-test-3 (list 'c '() '() '(1 2 3 4 5 6 7 8 9)))
(define brd-test-6 (list 'c '(7 8) '(2 6 9) '(1 3 4 5)))



(define (best-move brd)
  (local[(define (f-1) (depth list-scores .1))
         (define list-scores
           (map best-move (poss-brds brd)))
         (define argmax/min
           (if (player-turn? (first brd)) argmin argmax))]
    (cond
      [(game-over? brd) (evaluate brd)]
      [else (argmax/min (lambda (x) x) (f-1))])))

(define (c-turn brd)
  (cond [(game-over? brd) brd]
        [(not (player-turn? (first brd) ))    
         (argmax best-move (poss-brds brd))]
        [else brd]))

(define (main brd)
  (big-bang brd
            (to-draw initialize)
            (on-mouse display-moves)
            (on-tick c-turn)
            (stop-when game-over?)))
