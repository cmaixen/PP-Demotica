#lang racket


(#%require "Utillities.rkt")




;je moet je devices simuleren voor simulatie


(define (tempsensor name serialnumber)
  (let-values ([(input output) (make-pipe)]  ;aanmaken van outputfile bij het aanmaken van het object
               [(status) 'output] 
               [(current-temp) 25] ) ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
    
    (define (get-temp) 
      (write `(ACK (TEMP ,current-temp)) output))
    
    (define (get-name)
      (write `(ACK (NAME  ,name)) output))
    
    (define (get-serial) 
      (write `(ACK (SERIAL ,serialnumber)) output))
    
    
    (define (generate-answer commando)
      (let( (prefix (car commando))  ;controle of commando geldig is
            (asked (cadr commando))) ;wat er gevraagd is
        (if (eq? 'get prefix)
            (case asked
              ((name) (get-name))
              ((temp) (get-temp))
              ((serial) (get-serial))
              (else (write `(NACK , commando) output)))
            (write `(NACK , commando) output))))
   
    
    
    ;bij elke oproep van my-device-port wordt de variable port verandert in een iput/ouputport
    
    (define (my-device-port)
      (if (eq? status 'output)
          (begin (set! status 'input)
                 output)
          
          (begin (set! status 'output)
                 (generate-answer (read input)) ;van de moment je een read vraagt aan het object, gaat het object zijn antwoord genereren.
                 input)))
    
    
    
    (define (internal-dispatching message)
      (display "congrats you're in the internal dispatching"))
    ;(case message
    ;  ((name) get-name)
    ;  ((serial) get-serialnumber)
    ; ((temp)  get-temp)))
    
    (define (dispatch message)
      (case message
        ((get-device-port) my-device-port)
        (else (error 'tempsensor "unknown message ~a" message))))
    
    
    dispatch))




;Testing Unit

(display "Testing Unit")
(newline)
(display "------------")
(newline)
;defining object

(define room-tempsensor (tempsensor "woonkamer" "1234"))

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

