#lang racket

#|
1) Download all files
2) Run ttt.rkt
3) In the interactions window, enter one of the following: 
(main player-first) ;;for player to play first
(main comp-first)   ;; for computer to play first
|#

(define player-first (list 'p '() '() '(1 2 3 4 5 6 7 8 9)))
(define comp-first (list 'c '() '() '(1 2 3 4 5 6 7 8 9)))

(require "./src/ttt.rkt")

#|To Add...
Continuously play games
keep track of games lost, tied and won ...
Themes?
Clock? for time played, time to think per move ???
wait message (computer thinking..., play now/ your turn...)
Web?
On same spot and on computer's prev spots, disallow.. Also  when comp thinking?)
learn, impl ab pruning? (equivalent tree or worse => don't go??, <-1 => no good ??)
|#

