#lang racket


(#%require "Utillities.rkt")




;je moet je devices simuleren voor simulatie


(define (tempsensor name serialnumber port-adress)
  (let-values ([(input output) (make-pipe)]  ;aanmaken van outputfile bij het aanmaken van het object
        ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
        ;voor je leest van je port
        
      [(status) 'output];nodig om te weten of je als port een output of input port moet teruggeven
      [(change) #f]     ;nodig om eerste keer schrijven te overbruggen (zie "my-device-port")
       [(written) #f])           ;je moet omschakeling zien de overbruggen van inputfile naar outputfile voor de eerste keer.
    (define current-temp 25)
    
    (define (get-name) (begin
                           ;in produce-answer  lees je de file 
                         (write '(name  name) output)))
                         
    (define get-serialnumber serialnumber)
    (define get-portadress port-adress)
    (define (get-status) status)
    
    (define (get-temp) 
      (begin 
        (display "the current temparature is ")
        (display current-temp)
        (display " degrees")))
  
   (define (generate-answer commando)
     (let( (prefix (car commando))
           (needed (cadr commando)))
       (if (eq? 'get prefix)
           (case needed
             ((name) (get-name))
             (else (display "needed not defined")))
           (display "prefix not valid"))))
       
     
    

    
    
    ;bij elke oproep van my-device-port wordt de variable port verandert in een iput/ouputport
    
    (define (my-device-port)
      (if (eq? status 'output)
          (begin (set! status 'input)
                 output)
                 
          (begin (set! status 'output)
                 (generate-answer (read input))
                 input)))
    
    
    
    (define (internal-dispatching message)
      (display "congrats you're in the internal dispatching"))
    ;(case message
    ;  ((name) get-name)
    ;  ((serial) get-serialnumber)
    ; ((temp)  get-temp)))
    
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








(write '(get name) (send room-tempsensor 'get-device-port))
(read (send room-tempsensor 'get-device-port))

