;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Domotica Energie Monitoring Systeem                  *-*-
;-*-*                        Yannick Merckx                           *-*-
;-*-* Programmeerproject 2013-2014 2de Bachelor Computerwetenschappen *-*-
;-*-*           Student van Vrije Universiteit Brussel                *-*-
;-*-*                                                                 *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Beschrijving: ADT Steward ; Argumenten: Majordomo-object, logsystem-object , steward-table (naam van table waar men de informatie voor de devices kan vinden) (string?)
; Output: Steward object
; Messages:

; - add-device:  argumenten: geen
;                output: void

; - delete_device: argumenten: geen
;                  output: void

; - get-location: argumenten: geen
;                 output: locatie van de steward (string?)


; - open-output-device: argumenten: de naam van het toestel (string?)
;          output: outputport van toestel

; - open-input-device: argumenten: de naam van het toestel (string?)
;                 output: inputport van het toestel

; - list_devices_names: argumenten: 1) no argument
;                                   2) optional argument: type (string?) 
;                        output: 1) pair (lijst met namen van de toestellen) j
;                                2) pair (lijst met de namen van de toestellen van het gegeven type)

; - list_devices_status: argumenten: 1) no argument
;                                   2) optional argument: type  (string?) 
;                        output: 1) pair (lijst met statussen van de toestellen)
;                                2) pair (lijst met de statussen van de toestellen van het gegeven type)

; - list_devices_type: argumenten: 1) no argument
;                                  2) optional argument: type  (string?) 
;                      output: 1) pair (lijst met types van de toestellen)
;                                2) pair (lijst met types van de toestellen van het gegeven type)

; - list_devices_com_adress: argumenten: 1) no argument
;                                        2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met communicatieadressen van de toestellen)
;                                     2) pair (lijst met communicatieadressen van de toestellen van het gegeven type)

; - list_devices_serial:  argumenten: 1) no argument
;                                     2) optional argument: type  (string?) 
;                          output:    1) pair (lijst met serienummers van de toestellen)
;                                     2) pair (lijst met serienummers van de toestellen van het gegeven type)

; -list_devices_location:    argumenten: 1) no argument
;                                        2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met locaties van de toestellen)
;                                     2) pair (lijst met locaties van de toestellen van het gegeven type)

; -  list_devices_mesurement: argumenten: 1) no argument
;                                         2) optional argument: type  (string?) 
;                             output: 1) pair (lijst met metingen van de toestellen)
;                                     2) pair (lijst met metingen van de toestellen van het gegeven type)
;
; - list_devices_mesurement_with_value:  argumenten: 1) no argument
;                                                    2) optional argument: type  (string?) 
;                                        output: 1) pair (lijst met metingen met eenheden van de toestellen)
;                                                2) pair (lijst met metingen met eenheden van de toestellen van het gegeven type)
; - change-mesurement: argumenten: naam van het toestel (string?) , value (de waarde waar de status naar moet worden aangepast) (string?)
;          output: void

; - change-status: argumenten: naam van het toestel (string?) , value (de waarde waar de status naar moet worden aangepast) (bool)
;                 output: void

;
; Commentaar: list_devices_location en list_devices_type hebben nog de mogelijkheid om performanter geschreven te worden, voorlopig blijven ze zo staan vanwege vereenvoudiiging. 
;              Verder aanpassing hieraan zal in de tweede fase nog volgen.



#lang racket
(require db)
(require unstable/contract)
(#%require "Utillities.rkt")
(#%require "Devices.rkt")
(#%require "Logsystem.rkt")
(#%require "Constants.rkt")
(provide make-steward)




;parameters logsystem location , maj vallen weg

(define (make-steward location port)
  (let (
        (list-of-objects '())
        (location location)
        (already_initialized #f)
        
        )
    
    ;Private Hulpfunctie: parse-answer
    ;zorgt ervoor dat je het de eigelijke informatie dat je nodig hebt terug krijgt en anders false geeft
    
    (define (parse-answer lst)
      (let ((ACK-sign (car lst))
            (commando (cadr lst)))
        (if (eq? ACK-sign 'NACK)
            (error 'device "Commando ~a not acknowledged!" commando )
            (let*((result (cadr lst))
                  (resulttype (car result))
                  (resultval (cadr result)))
              resultval))))
    
    
    
    (define (get-location)
      location)
    
    
    ;teruggeven van inputport van gegeven toestel
    (define (open-output-device name)
      (look-up-device name 'get-device-output-port))
    
    ;teruggeven van inputport van gegeven toestel
    (define (open-input-device name)
      (look-up-device name 'get-device-input-port))
    
    ;communiceren met database voor informatie
    ;-----------------------------------------
    ;mag weg allemaal
    ; INFO FROM MAJORDOMO
    ; <--------------------
    ;generaliserende functie
    
    
    ;communiceren met devices voor informatie
    ;----------------------------------------
    
    ;generaliserende functie
    
    (define  (list_devices_info_from_device command . type)
      (define (loop-all lst result)
        (if (empty? lst)
            result 
            (let* ((first-obj (car lst))
                   (output (send first-obj 'get-device-output-port))
                   (input (send first-obj 'get-device-input-port)))
              (write `(get ,command) output)
              (loop-all (cdr lst) (cons (parse-answer (read input)) result)))))
      
      (define (loop-spec lst result)
        (if (empty? lst)
            result 
            (let* ((first-obj (car lst))
                   (output (send first-obj 'get-device-output-port))
                   (input (send first-obj 'get-device-input-port)))
              (write `(get type) output)
              (let ((answer (parse-answer (read input))))
                (newline)
                (display "loopspec:   ")
                (display command)
                (newline)
                (if (equal? answer (car type))
                    (begin (write `(get ,command) output)
                           (loop-spec (cdr lst) (cons (parse-answer (read input)) result)))
                    (loop-spec (cdr lst) result))))))
     
               
      (if (null? type)
          (loop-all (reverse list-of-objects) '())
          (loop-spec (reverse list-of-objects) '())))
    
    
    (define (list_devices_status . type)
      (if (null? type)
          (list_devices_info_from_device 'status)
          (list_devices_info_from_device 'status (car type))))
    
    
    (define (list_devices_location . type)
      (if (null? type)
          (list_devices_info_from_device 'location)
          (list_devices_info_from_device 'location (car type))))
    
    
    (define (list_devices_mesurement . type)
      (if (null? type)
          (list_devices_info_from_device 'mesurement)
          (list_devices_info_from_device 'mesurement (car type))))
    
    
    (define (list_devices_mesurement_with_value . type)
      (if (null? type)
          (list_devices_info_from_device 'mesurement_w_v)
          (list_devices_info_from_device 'mesurement_w_v (car type))))
    
    
    ;voegt een gegeven device toe aan de deviceslist
    
    (define (update-devices type name serialnumber status mesurement)
      (set! list-of-objects  (cons (make-device type name serialnumber location status mesurement) list-of-objects))
      (newline)
      (display list-of-objects)
      (newline)
      'updated-devices)
    
    ;je bekijkt de naam als primary key, je let er ook op dat er geen dubbele serial in staat 
    
    ;procedure bepaalt welke standaardwaarde er moet gegeven  worden
    (define (mesurement_default type)
      (if (equal? type temperaturesensor_type)
          temperaturesensor_default_value
          lightswitch_default_value))
    
    ;Moet nu via Majordomo Database
    (define (add-device type name serialnumber com-adress)
      (update-devices type name serialnumber status_default_value (mesurement_default type) ))
    
    ;verwijderen vvan een toestel
    (define (delete_device device-name)
      (set! list-of-objects (filter (lambda (x) (not (equal? device-name (send x 'get-name-sim)))) list-of-objects))
      'device-deleted)
    
    
    ;naam verkrijgen kan nog via protocol van poorten gaan, maar aangezien we hier toch aan het simuleren zijn, speelt dit op de moment geen rol
    ;In de latere fase wordt dit nog aangepast
    (define (look-up-device name arg) 
      (define (loop lst)
        (if (empty? lst)
            (begin 
              (error 'steward  "the device ~a doesn't exist!" name)
              #f)
            (let* ((first-object (car lst))
                   (name-object (send first-object 'get-name-sim)))
              (if (equal? name-object  name )
                  (send first-object arg)
                  (loop (cdr lst))))))
      (loop list-of-objects))
    
    
    ;geeft object terug van de gegeven naam
    (define (get-device name) 
      (define (loop lst)
        (if (empty? lst)
            (begin 
              (error 'steward  "the device ~a doesn't exist!" name)
              #f)
            (let* ((first-object (car lst))
                   (name-object (send first-object 'get-name-sim)))
              (display name)
              (if (equal? name-object  name )
                  first-object
                  (loop (cdr lst))))))
      (loop list-of-objects))
    
    
    ;veranderen van de meting van het gegeven toestel
    (define (change-mesurement name value)
      (let* ((first-obj (get-device name))
             (output (send first-obj 'get-device-output-port))
             (input (send first-obj 'get-device-input-port)))
        (write `(set mesurement ,value) output)
        (read input)
        ))
    
    ;veranderen van de status van het gegeven toestel
    (define (change-status name value)
      (let* ((first-obj (get-device name))
             (output (send first-obj 'get-device-output-port))
             (input (send first-obj 'get-device-input-port))) 
        (if value
            (write `(set status "on") output)
            (write `(set status "off") output))
        (read input)
        ))
    
    
    ;initiliazatie
    ;aanmaken van lijst met device-objecten
    (define (initialize device_list)
      (define (loop lst)
        (if (empty? lst)
            'done
            (let* ((devicevector (car lst))
                   (device_name (vector-ref devicevector device_table_name_column))
                   (device_serial (vector-ref devicevector device_table_serial_column))
                   (device_comadress (vector-ref devicevector device_table_com_adress_column))  
                   (device_type (vector-ref devicevector device_table_type_column))
                   (device_status (vector-ref devicevector device_table_status_column))
                   (device_mesurement (string->number (vector-ref devicevector device_table_mesurement_column))))
              (set! list-of-objects (cons (make-device device_type device_name device_serial location device_status device_mesurement) list-of-objects))
              (loop (cdr lst)))))
      (if (not already_initialized)
          (begin 
      (loop device_list)
      (set! already_initialized #t)
      'done)
          'already_initialized))
    
    
    
    ;DISPATCHER over TCP/IP
    (define (dispatch message out)
      (let* ((command (car message))
             (arguments (cdr message))
             (answer (generate-answer command arguments)))
        (begin 
          (write answer out)
          (newline out)
          (flush-output out))
        ))
    
    ;Returns '(ACK ...) of '(NACK ...)
    (define (generate-answer command arguments)
      (display command)
      (display arguments)
      (newline)
      ;argumenten zitten in de lijst in de volgorde van de argumenten van
      (cond ((equal? command 'add-device)
             (let ((type (car arguments))
                   (name (cadr arguments))
                   (serialnumber (caddr arguments))
                   (com-adress (cadddr arguments)))
               (command_acknowledged (add-device type name serialnumber com-adress))
               ))
            ((equal? command 'get-location)
             ;needs to be done
             (command_acknowledged 'get-location))
            ((equal? command 'set-location)
            ;needs to be done
             (command_acknowledged 'set-location))
            ((equal? command 'open-output-device)
             (let ((name (car arguments)))
             (command_acknowledged  (open-output-device name))))
            ((equal? command 'open-input-device)
             (let ((name (car arguments)))
               (command_acknowledged open-input-device name)))
            ((equal? command 'list_devices_status)
             (if (empty? arguments)
               (command_acknowledged (list_devices_status))
                (command_acknowledged (list_devices_status (car arguments)))))
            ((equal? command 'list_devices_location)
             (if (empty? arguments)
               (command_acknowledged (list_devices_location))
              (command_acknowledged   (list_devices_location (car arguments)))))
            ((equal? command 'list_devices_mesurement)
             (if (empty? arguments)
               (command_acknowledged  (list_devices_mesurement))
             ( command_acknowledged   (list_devices_mesurement (car arguments)))))
            ((equal? command 'list_devices_mesurement_with_value)
             (if (empty? arguments)
                 (command_acknowledged (list_devices_mesurement_with_value))
                (command_acknowledged (list_devices_mesurement_with_value (car arguments)))))
            ((equal? command 'delete_device)
             (let ((device-name (car arguments)))
               (command_acknowledged (delete_device device-name))))
            ((equal? command 'change-mesurement)
             (let ((name (car arguments))
                   (value (cadr arguments)))
               (command_acknowledged (change-mesurement name value))))
            ((equal? command 'change-status)
             (let ((name (car arguments))
                   (value (cadr arguments)))
               (command_acknowledged( change-status name value))))
            ((equal? command' initialize-steward)
             (let ((devicelist (car arguments)))
              (command_acknowledged (initialize devicelist))))
            (else (invalid_command)))) 
    
    
    ;boodschap bij ongeldog commando
    (define (invalid_command)
      '(NACK "Invalid Command"))
    
    (define (server listener)
      (let ((in '())
            (out '()))
        (let-values ([(pi po) (tcp-accept listener)])
          (set! in pi)
          (set! out po)
          (write "===>> Connection Established! <<===" out)
          (newline out)
          (flush-output out)
          (dispatch (read in)  out))))
    
    ;oneindige loop die zorgt dat je steward blijft gaan
    (define (infinite-loop procedure listener)
      (display "Steward avaible")
      (newline)
      (procedure listener)
      (infinite-loop procedure listener))
    
    
    (infinite-loop server (tcp-listen port 10 #t))
    
    ;initialization of Steward
    ;Het genereren van de objectlijst
    ;gebeurt nu vanuit de steward
))

(define (command_acknowledged reply)
  `(ACK ,reply))