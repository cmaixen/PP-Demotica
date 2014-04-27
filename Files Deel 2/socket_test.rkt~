#lang racket

(define in '())
(define out '())
(define lst (tcp-listen 6666))
(let-values ([(pi po) (tcp-accept lst)])
  (set! in pi)
  (set! out po))

; 1. 
(write "Hello!" out)
(flush-output out)

; 2.
(define (server)
  (write '(begin 
            (write ID out)
            (newline out)
            (flush-output out))
         out)
  (newline out)
  (flush-output out)
  (read in))

; (server) ; run this in the REPL, then run (client)