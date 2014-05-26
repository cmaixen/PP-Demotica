#lang R5RS
(#%require racket/base)
(#%require "Utillities.rkt")
(#%require "xbee_utils.rkt")
(#%provide (all-defined))

(define (make-powerplug)
  (let ((64-adress  #(0 19 162 0 64 155 139 45))
        (16-adress #(0 0))
        (POW "on")
        (FREQ "49.8125Hz")
        (VRMS "227V")
        (IRMS "1988mA")
        (LOAD "443W")
        (WORK "0.046kWh")
              (name "PowerPlug"))
        
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
          (append_string "POW=" POW "\n"
                  "FREQ=" FREQ "\n"
                  "VRMS=" VRMS "\n"
                  "IRMS=" IRMS  "\n"
                  "LOAD=" LOAD "\n"
                  "WORK=" WORK  "\n")))
        
        
        (define (execute_set message)
          (let ((3_letter_command (vector->string (vector-loop 5 7 message))) ;SET + spatie is voorafgaand is vector
                (4_letter_command (vector->string (vector-loop 5 8 message)))
                (nack #f))
            (display 3_letter_command)
            (cond ((equal? 3_letter_command "POW")
                   (let ((new_val (vector-loop 9 (vector-length message) message)))
                     (display (vector->string new_val))
                     (set! POW (vector->list new_val))))
                   ((equal? 4_letter_command "FREQ")
                    (let ((new_val (vector-loop 10 (vector-length message) message)))
                      (display (vector->string new_val))
                      (set! FREQ new_val)))
                   ((equal? 4_letter_command "VRMS")
                    (let ((new_val (vector-loop 10 (vector-length message) message)))
                      (display (vector->string new_val))
                      (set! VRMS new_val)))
                   ((equal? 4_letter_command "IRMS")
                    (let ((new_val (vector-loop 10 (vector-length message) message))) 
                      (display (vector->string new_val))
                      (set! IRMS new_val)))
                   ((equal? 4_letter_command "LOAD")
                    (let ((new_val (vector-loop 10 (vector-length message) message))) 
                      (display (vector->string new_val))
                      (set! LOAD new_val)))
                   ((equal? 4_letter_command "WORK")
                    (let ((new_val ((vector-loop 10 (vector-length message) message))) )
                      (display (vector->string new_val))
                      (set! WORK new_val)))
                   (else (display "invalid command")
                         (newline)))
            (if (not nack)
         (process_answer_as_frame (append_string "ack " (vector->string message)))
          (process_answer_as_frame (append_string "nack " (vector->string message))))))
        
        
        ;OPSTELLEN VAN ANTWOORD FRAME MOET NOG GEBUEREN + TESTINGnee kga ni
        (define (handle-request message)
          (let ((command (get-command message)))
            (cond ((equal? command "GET")
                   (prepare_getframe))
            ((equal? command "SET")
             (execute_set message))
            (else (display "command not recognized")))))
        
       ;message is hier een string en maakt het finale answerframe en stuurt een pair met een statusframe en answerframe naar xbee
        ;xbee gaat deze frames pushen op de buffer en deze buffer wordt dan gepushed op de steward. 
        (define (process_answer_as_frame message)
         (let ((receive_frame (make_frame Transmit_answer 64-adress 16-adress message-acknowledged message "\n\n"))
               (status_frame  (make_frame Transmit_status 16-adress transmit_retry_counter delivery_status_succes discovery_status_succes "\n" "\n")))
       (display   (cons status_frame receive_frame))
                  (cons status_frame receive_frame)))
        
        (define (dispatch message)
          (case message
            ((get_64-adress) get_64-adress)
            ((get_name) get_name)
            ((get_16-adress) get_16-adress)
            ((request) handle-request)
            (else (display "Powerplug unknown message")
                  (newline))))
        
        dispatch)
    )
  

  