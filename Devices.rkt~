#lang racket
(#%require "Utillities.rkt")
(#%provide make-device)


(define (make-device type name serialnumber)
  (let ((giventype (string->symbol type)))
  (case giventype
    ((Temperaturesensor) (make-tempsensor name serialnumber))
    ((Lightswitch) (make-lightswitch name serialnumber))
    (else (error 'devicemaker "Type ~a not recognized!" type)))))



;Tempsensor Object
;-----------------

;Simuleert een temperatuursensor

(define (make-tempsensor name serialnumber)
  (let-values ([(input output) (make-pipe)]  ;aanmaken van outputfile bij het aanmaken van het object
               [(written-counter) #f] 
               [(current-temp) 25]   ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
               [(status) "on"]
               [(type) "Temperatuursensor"])
    
    
    (define (get-name-sim)
      name)
    
    (define (get-status)
      `(ACK (STATUS ,status)))
      
    (define (get-temp) 
      `(ACK (TEMP ,current-temp)))
    
    (define (get-name)
      `(ACK (NAME  ,name)))

(define (get-type)
  `(ACK (TYPE ,type)))
    
    (define (get-serial) 
      `(ACK (SERIAL ,serialnumber)))
    
    
    (define (generate-answer)
      (let*((commando (read input))
            (prefix (car commando))  ;controle of commando geldig is
            (asked (cadr commando)))
        ;wat er gevraagd is
        (if (eq? 'get prefix)
            (case asked
              ((name) (get-name))
              ((temp) (get-temp))
              ((serial) (get-serial))
              ((status) (get-status))
              ((type) (get-type))
              (else `(NACK , commando)))
            `(NACK , commando))))
   
    (define (change-wc!) (if written-counter 
                             (set! written-counter #f)
                             (set! written-counter #t)))
    
    ;bij elke oproep van my-device-port wordt de variable port verandert in een iput/ouputport

    (define (get-device-output-port)
     output)
    

    
    (define (get-device-input-port)
      (make-input-port
       'name 
      (lambda (ignore)
      (let-values (((in out) (make-pipe))
                   ((answer) 'none))
      (set! answer (generate-answer))
        (write answer out)
        in))
      #f
      (lambda () 'closed)))
  
    
    
    (define (dispatch message)
      (case message
        ((get-name-sim) get-name-sim)
        ((get-device-output-port) get-device-output-port)
        ((get-device-input-port) get-device-input-port)
        (else (error 'tempsensor "unknown message ~a" message))))
    
    
    dispatch))

(define (make-lightswitch name serialnumber)
  (let-values ([(input output) (make-pipe)]  ;aanmaken van outputfile bij het aanmaken van het object
               [(written-counter) #f] 
               [(current-status) "Switched on"]   ;je mag er vanuit gaan dat je eerst een outputport hebt aangezien je eerst wil schrijven 
               [(status) "on"]
               [(type) "Lightswitch"])
    
    
    (define (get-name-sim)
      name)
    
    (define (get-status)
      `(ACK (STATUS ,status)))
      
    (define (get-current-status) 
      `(ACK (TEMP ,current-status)))
    
    (define (get-name)
      `(ACK (NAME  ,name)))

(define (get-type)
  `(ACK (TYPE ,type)))
    
    (define (get-serial) 
      `(ACK (SERIAL ,serialnumber)))
    
    
    (define (generate-answer)
      (let*((commando (read input))
            (prefix (car commando))  ;controle of commando geldig is
            (asked (cadr commando)))
        ;wat er gevraagd is
        (if (eq? 'get prefix)
            (case asked
              ((name) (get-name))
              ((current-status) (get-current-status))
              ((serial) (get-serial))
              ((status) (get-status))
              ((type) (get-type))
              (else `(NACK , commando)))
            `(NACK , commando))))
   
    (define (change-wc!) (if written-counter 
                             (set! written-counter #f)
                             (set! written-counter #t)))
    
    ;bij elke oproep van my-device-port wordt de variable port verandert in een iput/ouputport

    (define (get-device-output-port)
     output)
    

    
    (define (get-device-input-port)
      (make-input-port
       'name 
      (lambda (ignore)
      (let-values (((in out) (make-pipe))
                   ((answer) 'none))
      (set! answer (generate-answer))
        (write answer out)
        in))
      #f
      (lambda () 'closed)))
  
    
    
    (define (dispatch message)
      (case message
        ((get-name-sim) get-name-sim)
        ((get-device-output-port) get-device-output-port)
        ((get-device-input-port) get-device-input-port)
        (else (error 'tempsensor "unknown message ~a" message))))
    
    
    dispatch))


