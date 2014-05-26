#lang R5RS

(#%require racket/base)
(#%require "Utillities.rkt")
(#%require "xbee_utils.rkt")
(#%provide (all-defined))

(define (make-energysensor)
  (let ((64-adress  #(0 19 162 0 64 148 36 184))
        (16-adress #(0 0))
        (BRI "321x")
        (DBRI "1231x")
        (TEM "27,5°C")
        (DTEM "1988mA")
        (HUM "65%")
        (DHUM "5%")
        (name "MultiSensor")
        (PRES "1015.46hPa")
        (DPRES "12hPa")
        (BAT "OK")
        (UBAT "3.85V")
        (HITEM "22.0")
        (LOTEM "18.0")
        (LOBRI "12")
        (HIBRI "74")
        (TXT "1800")
        (MSI "120"))
        
        (define (get_name) 
          name)
        
        (define (get_64-adress)
          64-adress)
        
        (define (get_16-adress)
          16-adress)
        
        (define(get-command message)
          (vector->string (vector-loop 1 3 message)))
        
         
        ;voorbereiden van antwoord van getframe
        ;geeft string terug
        (define (prepare_getframe)
         (process_answer_as_frame
          (append_string "BRI=" BRI "\n"
                  "DBRI=" DBRI "\n"
                  "TEM=" TEM "\n"
                  "DTEM=" DTEM  "\n"
                  "HUM=" HUM "\n"
                  "DHUM=" DHUM  "\n"
                  "PRES=" PRES  "\n"
                  "DPRES=" DPRES  "\n"
                  "BAT=" BAT "\n"
                  "DHUM=" UBAT  "\n")))
        
        
    ;voer het set commando uit
        (define (execute_set message)
          (let ((3_letter_command (vector->string (vector-loop 5 7 message))) ;SET + spatie is voorafgaand is vector
                (5_letter_command (vector->string (vector-loop 5 9 message)))
                (nack #f))
            (display 3_letter_command)
           (newline)
            (cond ((equal? 3_letter_command "TXT")
                   (let ((new_val (vector-loop 9 (vector-length message) message)))
                     (display (vector->string new_val))
                     (set! TXT (vector->list new_val))))
                   ((equal? 3_letter_command "MSI")
                    (let ((new_val (vector-loop 9 (vector-length message) message)))
                      (display (vector->string new_val))
                      (set! MSI new_val)))
                   ((equal? 5_letter_command "LOTEM")
                    (let ((new_val (vector-loop 11 (vector-length message) message)))
                      (display (vector->string new_val))
                      (set! LOTEM new_val)))
                   ((equal? 5_letter_command "HITEM")
                    (let ((new_val (vector-loop 11 (vector-length message) message))) 
                      (display (vector->string new_val))
                      (set! HITEM new_val)))
                   ((equal? 5_letter_command "LOBRI")
                    (let ((new_val (vector-loop 11 (vector-length message) message))) 
                      (display (vector->string new_val))
                      (set! LOBRI new_val)))
                   ((equal? 5_letter_command "HIBRI")
                    (let ((new_val ((vector-loop 11 (vector-length message) message))) )
                      (display (vector->string new_val))
                      (set! HIBRI new_val)))
                   (else (set! nack #t)))
            (if (not nack)
         (process_answer_as_frame (append_string "ack " (vector->string message)))
          (process_answer_as_frame (append_string "nack " (vector->string message))))
            ))
            

        ;message is hier een string en maakt het finale answerframe en stuurt een pair met een statusframe en answerframe naar xbee
        ;xbee gaat deze frames pushen op de buffer en deze buffer wordt dan gepushed op de steward. 
        (define (process_answer_as_frame message)
         (let ((receive_frame (make_frame Transmit_answer 64-adress 16-adress message-acknowledged message "\n\n"))
               (status_frame  (make_frame Transmit_status 16-adress transmit_retry_counter delivery_status_succes discovery_status_succes "\n" "\n")))
                  (cons status_frame receive_frame)))
     
           
           
        (define (handle-request message)
          (let ((command (get-command message)))
            (cond ((equal? command "GET")
                   (prepare_getframe))
            ((equal? command "SET")
             (execute_set message))
            (else (display "command not recognized")))))
        
        
        (define (dispatch message)
          (case message
            ((get_64-adress) get_64-adress)
            ((get_name) get_name)
            ((get_16-adress) get_16-adress)
            ((request) handle-request)
            (else (display "EnergySensor: unknown message")
                  (newline))))
        
        dispatch)
    )
  

  


  
