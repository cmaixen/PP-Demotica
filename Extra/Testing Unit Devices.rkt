#lang racket
(#%require "Utillities.rkt")
(#%require "Devices.rkt")


;Testing Unit

(display "Testing Unit")
(newline)
(display "------------")
(newline)
;defining object

(define room-tempsensor (make-tempsensor "woonkamer" "1234"))



(define (out) (send room-tempsensor 'get-device-output-port))
(define( in ) (send room-tempsensor 'get-device-input-port))


;(display "Valid Commands")
;(newline )


(write '(get name) (out))
(read (in))

(write '(get temp) (out)) 
(read (in))

(write '(get serial) (out)) 
(read (in))

(newline )
(display "fout prefix")
(newline )

(write '(gett serial) (out)) 
(read (in))

(newline )
(display "fout gevraagde" )
(newline )

(write '(get seriaaaal) (out)) 
(read (in))

(newline )
(display "2X read of 2X write is toegelaten, moet nog iets aan gedaaan worden!" )  
(newline )

(write '(get serial) (out)) 
(write '(get serial) (out)) 

(read (in))
(read (in))
