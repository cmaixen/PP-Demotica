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

; Beschrijving: ADT Majordomo ; Argumenten: geen
;               
;
; Output: majordomo object
;
; Messages:
;
; - start: argumenten: geen
;          output: void

; - get-steward: argumenten: naam (de naam van de steward dat je wil)(string?)
;          output: steward-object

; - get-listrooms: argumenten: geen
;          output: (listof string?  ...) (lijst met al de verschillende kamers waar een steward staat)

; - get-listdevices: argumenten: geen
;          output: (listof string?  ...) (lijst met al de verschillende types van toestellen)

; - get-list_w_stewards: argumenten: geen
;          output: (listof steward-object ...) (lijst met alle stewards in)

; - get-current-steward: argumenten: geen
;          output: steward-object (geeft huidige actuele steward terug)

; - set-current-steward: argumenten: steward-object
;          output: void (zet de huidige actuele steward op de gegeven steward)
;
; Commentaar: geen

#lang racket
(#%require  "Utillities.rkt")
(#%require "GUI.rkt") 
(#%require "Steward.rkt")
(require db)
(#%require "Logsystem.rkt")
(#%require "Constants.rkt")
(require unstable/contract)
(#%provide make-majordomo)

(define  (make-majordomo) 
  (let* ((db (sqlite3-connect  #:database db-pad
                               #:mode 'read/write))
         (list_w_stewards '())
         (current-steward 'NULL) ;pair (naam . (serveradress .  steward-databasse)) serveradress = (ip . port)
         (current-listbox 'NULL)      
         (logsystem (make-logsystem))
         (listrooms (list_neutralizer (query-rows db "select location from the_stewards") steward_table_locationcolummn))
         (listdevices (list lightswitch_type temperaturesensor_type)))
    
    
    ;starten van programma
    (define (start)
      (send (make-gui dispatch logsystem) 'start))
    
    
    
    ;initializatiefunctie
    
    ;maakt een interne lijst met steward objecten aan
    ;bij het aanmaken kijkt hij in de database welke stewards er zijn aangemaakt
    
    ;---------------------------------------------------------------------------------------------------------------------------------------------
    ;| VERANDERING: Ipv objecten gaat majordomo nu info sturen om steward te initialiseren en worden serveradress opgeslagen in  de stewardlijst |
    ;---------------------------------------------------------------------------------------------------------------------------------------------
    
    (define  (initialize_majordomo)
      (define (loop lst)
        (if (empty? lst)
            'done  
            (let* ((devicevector (car lst))
                   (location (vector-ref devicevector steward_table_locationcolummn))
                   (steward-database (vector-ref devicevector steward_table_steward_database_column)))
              (set! list_w_stewards (cons (make-steward dispatch location logsystem steward-database) list_w_stewards))
              
              (loop (cdr lst)))))
      (loop (query-rows db "select * from the_stewards")))
    
    
    ;geeft de steward terug op de overeenkomstige locatie 
    ;iedere steward valt te identificeren aan zijn locatie! dus algemeen wordt de locatie als de naam/identiteit van de steward genomen.
    
    (define (get-steward locationname)
      (define (search lst)
        (if (empty? lst)
            (error 'search "steward -> ~a not found" locationname)
            (let* ((first_steward (car lst))
                   (location (send first_steward 'get-location)))
              (if (equal? location locationname)
                  first_steward
                  (search (cdr lst))))))
      (search list_w_stewards))
    
    
    
    ;ACCESSOREN
    ;----------
    ;geeft de huidige steward waar je mee aan het werken ben terug
    (define (get-current-steward)
      current-steward)
    
    ;geeft de databaconnectie door
    (define (get-db)
      db)
    
    ;geeft een lijst terug met al de verschillende kamers
    (define (get-listrooms)
      listrooms)
    
    
    ;geeft een lijst terug met al de verschillende types van devices. !LIGT VAST!
    (define (get-listdevices)
      listdevices)
    
    
    
    ;geeft de lijst met stewardobjecten terug
    (define (get-list_w_stewards)
      list_w_stewards)
    
    
    ;MUTATOREN
    ;---------
    
    
    ;de huidige steward veranderen in de gegeven
    (define (set-current-steward new-steward)
      (set! current-steward new-steward))
    
    ;verwacht lijst met nummers
    (define (calculate-average-from-numberlist list)
      (define (loop lst result counter)
        (if (empty? lst)
            (/ result  counter)
            (loop (cdr lst) (+ result (string->number (car lst))) (+ counter 1))))
      (loop list 0 0))
    
    ;INFORMATIE KRIJGEN VAN HUIDIGE STEWARD(wordt gebruikt door GUI)
    ;---------------------------------------------------------------
    
    ;MUTATOREN STEWARD
    ;-----------------
    (define location "testlocation")
    (define (delete_device_current_steward device-name)
      (let ((steward-database (cddr current-steward))
            (steward-adress (cdar current-steward)))
        (;uit database verwijderen
         (query-exec db (format "DELETE from ~a where name = $1" steward-database) device-name)
         ;NOT IMPLEMENTED YET
         (send-over-tcp `(delete_device ,device-name))
         ;update logfile
         ;location moet er nog bij
         (send logsystem 'delete-device device-name location))))              
    
    
    (define (change_mesurement_current_steward name value)
      (let ((steward-database (cddr current-steward)))
        ;in database veranderen
        ((query-exec db (format "update ~a set mesurement = $1 where name = $2" steward-database)  value name)
         ;NOT IMPLEMETED YET
         (send-over-tcp `(change-mesurement ,value))
         ;update logfile
         (send logsystem 'change-mesurement location name value))))
    
    (define (change_status_current_device name value)
      (let ((steward-database (cddr current-steward)))
        (if value
            (begin
              (send-over-tcp `(change-status status "on"))
              (query-exec db (format "update ~a set status = $1 where name = $2" steward-database)  "on" name))
            (begin
              (send-over-tcp `(change-status status "off"))
              (query-exec db (format "update ~a set status = $1 where name = $2" steward-database)  "off" name)))
        (if value
            (send logsystem 'device-on location name)
            (send logsystem 'device-off location name))))
    
    (define (add-device type name serialnumber com-adress)
      (let ((steward-database (cddr current-steward)))
        ;apparaat in database toevoegen 
        ;je mag geen veld oplaten!
        (cond ((not (and (non-empty-string? name)  (non-empty-string? serialnumber) (non-empty-string? com-adress)))
               (error 'steward "Not everything is filled in!"))
              ((query-maybe-row db (format "select name from ~a where name = $1" steward-database) name);we namen de lamp als primary key
               (error 'steward "You can not add two devices with the same name!"))
              ((query-maybe-value db (format "select serialnumber from ~a where serialnumber = $1" steward-database) serialnumber)
               (error 'steward "serialnumber must be unique!"))
              ((query-maybe-value db (format "select comadresse from ~a where comadresse = $1" steward-database) com-adress)
               (error 'steward "communicationadress must be unique!"))
              (else
               (query-exec db (format "insert into ~a values ($1, $2 , $3 , $4, $5, $6)" steward-database) name serialnumber com-adress type status_default_value (mesurement_default type))
               ;NOT IMPLEMENTED YET
               (send-over-tcp '(update-devices ,type ,name ,serialnumber ,status_default_value ,(mesurement_default type) ))
               (send logsystem 'add-device name location)))))
    
    ;ACCESSOREN STEWARD
    ;------------------
    
    ;algemene functie
    (define (list_devices_info_from_db  position . type)
      (let ((steward-database (cddr current-steward)))
        (if (empty? (car type))
            (list_neutralizer (query-rows db (format "select * from ~a" steward-database)) position)
            (list_neutralizer (query-rows db  (format "select * from ~a where type = $1" steward-database) (caar type)) position))))
    
    (define (list_devices_names_current_steward . type)
      (list_devices_info_from_db  device_table_name_column  type))
    
    (define (list_devices_serial_current_steward . type)
      (list_devices_info_from_db  device_table_serial_column  type))
    
    (define (list_devices_com_adress_current_steward . type)
      (list_devices_info_from_db  device_table_com_adress_column  type))
    
    (define (list_devices_type_current_steward . type)
      (list_devices_info_from_db  device_table_type_column  type))
    
    ;onrechtstreeks via steward aan devices info vragen
    ;NOT IMPLMENTED YET
    (define (list_devices_status . type)
      (send logsystem 'status-update location)
      (if (null? type)
          (send-over-tcp `(list_devices_info_from_device 'status))
          (send-over-tcp `(list_devices_info_from_device 'status ,(car type)))))
    
    
    (define (list_devices_location . type)
      (if (null? type)
          (send-over-tcp `(list_devices_info_from_device 'location))
          (send-over-tcp `(list_devices_info_from_device 'location ,(car type)))))
    
    
    (define (list_devices_mesurement . type)
      (if (null? type)
          (send-over-tcp `(list_devices_info_from_device 'mesurement))
          (send-over-tcp `(list_devices_info_from_device 'mesurement (car type)))))
    
    
    (define (list_devices_mesurement_with_value . type)
      (if (null? type)
          (send-over-tcp `(list_devices_info_from_device 'mesurement_w_v))
          (send-over-tcp `(list_devices_info_from_device 'mesurement_w_v ,(car type)))))
    
    (define (dispatch message)
      (case message
        ((start) start)
        ((get-steward) get-steward)
        ((get-listrooms) get-listrooms)
        ((get-listdevices) get-listdevices)
        ((get-db) get-db)
        ((get-list_w_stewards) get-list_w_stewards)
        ((get-current-steward) get-current-steward)
        ((set-current-steward) set-current-steward)
        (else (error 'Majordomo "unknown message ~a" message
                     
                     ))))
    (initialize_majordomo)
    
    ;thread die om het uur informatie wegschrijft naar de logfile wat het gemiddelde verbruik is
    (thread    (lambda () 
                 (do ([i 1 (+ i 1)])
                   ((= i 0) (print "done"))
                   
                   (sleep light-graph-sleep)
                   (print "thread called")
                   (let* ((file light-graph-file)
                          (input (open-input-file file))
                          (old-lst-data (read input))
                          (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards lightswitch_type 'list_devices_mesurement)))
                          (new-data-list (cons new-value old-lst-data)))
                     (close-input-port input)
                     (let ((output (open-output-file file #:exists 'can-update)))
                       (write new-data-list output)
                       (close-output-port output))))))
    
    
    ;thread die om het uur informatie wegschrijft naar de logfile wat het gemiddelde verbruik is
    (thread    (lambda () 
                 (do ([i 1 (+ i 1)])
                   ((= i 0) (print "done"))
                   
                   (sleep temp-graph-sleep)
                   (print "thread2 called")
                   (let* ((file temp-graph-file)
                          (input (open-input-file file))
                          (old-lst-data (read input))
                          (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards temperaturesensor_type 'list_devices_mesurement)))
                          (new-data-list (cons new-value old-lst-data)))
                     (close-input-port input)
                     (let ((output (open-output-file file #:exists 'can-update)))
                       (write new-data-list output)
                       (close-output-port output))))))
    
;SENDING OVER TCP/IP
;RETURN: ANY
(define (send-over-tcp message)
  (let* ((in '())
         (out '()))
    ; (server-adress (cdar current-steward))
    ;   (ip (car server-adress))
    ;  (port (cdr server-adress)))
   
    (let-values ([(pi po) (tcp-connect "localhost" 6664)])
      (display "Connecting with ip ....")
      (newline)
      (set! in pi)
      (set! out po))
(display (read in))
    (write message
           out)
   (flush-output out)
    
    (newline)
    
 (read in)))
    
    dispatch))


    
    
    


;(send-over-tcp '(add-device "test"))

