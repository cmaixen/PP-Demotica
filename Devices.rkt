#lang racket
(#%require "Utillities.rkt")


;je moet je devices simuleren voor simulatie


(define (tempsensor name serialnumber port-adress)
  (let ((port (open-output-file serialnumber #:exists 'truncate))  ;aanmaken van outputfile bij het aanmaken van het object
                                                                   ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
                                                                   ;voor je leest van je port
        
        (status 'output)  ;nodig om te weten of je als port een output of input port moet teruggeven
        (change #f))      ;nodig om eerste keer schrijven te overbruggen (zie "my-device-port")
                          ;je moet omschakeling zien de overbruggen van inputfile naar outputfile voor de eerste keer.
    (define current-temp 25)
    
    (define get-name name)
    (define get-serialnumber serialnumber)
    (define get-portadress port-adress)
    (define (get-status) status)
    
    (define (get-temp) 
      (begin 
        (display "the current temparature is ")
        (display current-temp)
        (display " degrees")))
    
    (define (device-input-port)
      (begin
        (close-output-port port)
        (set! port (open-input-file serialnumber))
        port))
      
      (define (device-output-port)
        
      port)
      
    
    ;bij elke oproep van my-device-port wordt de variable port verandert in een iput/ouputport
    
      (define (my-device-port)      
           (if (eq? status 'output)
              (begin (set! status 'input)
                     (if change                                        ;overbrugging bij eerste keer oproepen outputport
                         (begin 
                         (close-input-port port)
                         (set! port (open-output-file serialnumber))
                         (set! change #t))
                         'nothing)
                     (device-output-port))
              (begin (set! status 'output)
                     (device-input-port))))
      
      

      (define (get message)
        (case message
          ((name) get-name)
          ((serial) get-serialnumber)
          ((temp)  get-temp)))
      
      (define (dispatch message)
        (case message
            ((status) get-status)
          ((get-device-port) my-device-port)
          (else (error 'tempsensor "unknown message ~a" message))))
      
      dispatch))
  
  
  
  ;
  (define room-tempsensor (tempsensor "woonkamer" "1234" "tempsensor-woonkamer"))
  
  ;(send room-tempsensor  'get 'name)
  
  ;(send room-tempsensor  '(get name))
  
  
  ;testing met read
  

  
  (

  
  (send room-tempsensor 'status)

 (write '(get bat) (send room-tempsensor 'get-device-port))
 (read (send room-tempsensor 'get-device-port))
 
  