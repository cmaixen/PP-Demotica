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

(display "Valid Commands")


(write '(get name) (send room-tempsensor 'get-device-port))
(read (send room-tempsensor 'get-device-port))

(write '(get temp) (send room-tempsensor 'get-device-port) )
(read (send room-tempsensor 'get-device-port))

(write '(get serial) (send room-tempsensor 'get-device-port) )
(read (send room-tempsensor 'get-device-port))

(newline)
(display "fout prefix")
(newline)

(write '(gett serial) (send room-tempsensor 'get-device-port) )
(read (send room-tempsensor 'get-device-port))

(newline)
(display "fout gevraagde")
(newline)

(write '(get seriaaaal) (send room-tempsensor 'get-device-port) )
(read (send room-tempsensor 'get-device-port))

(newline)
(display "Error bij 2X read of 2X write")  
(newline)

(write '(get serial) (send room-tempsensor 'get-device-port) )
(write '(get serial) (send room-tempsensor 'get-device-port) )

(read (send room-tempsensor 'get-device-port))
(read (send room-tempsensor 'get-device-port))

