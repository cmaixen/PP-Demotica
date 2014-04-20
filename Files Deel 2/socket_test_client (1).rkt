#lang racket

(define in '())
(define out '())
(let-values ([(pi po) (tcp-connect "localhost" 6666)])
  (set! in pi)
  (set! out po))

(define ID 'stewart)

; 1.
(read in)

; 2. 
(define (client)
  (let ((exp (read in)))
    (display "I have received and will evaluate: ") (display exp) (newline)
    (eval exp)))
; (client) ; run this in the REPL, after the (server)