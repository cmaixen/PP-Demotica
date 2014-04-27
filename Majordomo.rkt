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
(require db)
(#%require "Steward.rkt")
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
    
   
        
    ;SENDING OVER TCP/IP
    ;RETURN: ANY
    (define (send-over-tcp message steward . type)
      (let* ((in '())
             (out '())
             (ip (get_ip steward))
             (port (get_port steward)))
        (let-values ([(pi po) (tcp-connect ip port)])
          (display "Connecting with ip ....")
          (newline)
          (set! in pi)
          (set! out po))
        (display (read in))
        (write message
               out)
        (flush-output out)
        (newline)    
        (acknowledged (read in))))
    
    
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
                   (steward-database (vector-ref devicevector steward_table_steward_database_column))
                   (ip (vector-ref devicevector steward_table_ip_column))
                   (port (vector-ref devicevector steward_table_port_column)) 
                   (serveradress (cons ip port)))                
              (set! list_w_stewards (cons (make_steward_info location ip port steward-database) list_w_stewards))
              (loop (cdr lst)))))
      (loop (query-rows db "select * from the_stewards")))
    
    (define (make_steward_info location ip port steward_database)
      (let ((new_steward (cons location (cons (cons ip port) steward_database)))
           (stewardinfo (query-rows db (format "select * from ~a" steward_database))))
        (display location)
      ;  (send-over-tcp `(initialize-steward ,new_steward))
        (display (send-over-tcp `(initialize-steward ,stewardinfo) new_steward))
        new_steward))
    
    
    ;geeft de steward terug op de overeenkomstige locatie 
    ;iedere steward valt te identificeren aan zijn locatie! dus algemeen wordt de locatie als de naam/identiteit van de steward genomen.
    
    (define (get-steward locationname)
      (define (search lst)
        (if (empty? lst)
            (error 'search "steward -> ~a not found" locationname)
            (let* ((first_steward (car lst))
                   (location (get_location first_steward)))
              (if (equal? location locationname)
                  first_steward
                  (search (cdr lst))))))
      (search list_w_stewards))
    
    
    
    ;ACCESSOREN
    ;----------
    ;geeft de huidige steward waar je mee aan het werken ben terug
    (define (get-current-steward)
      current-steward)
    
    ;geeft locatie/naam van de steward. locatie en naam zijn hetrzelfde
    (define (get_location steward)
      (car steward))
    
 
    
    ;geeft database (de naam ervan) terug
    (define (get_steward_db steward)
      (cddr steward))
    
    
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
    (define (set-current-steward steward)
     (set! current-steward  steward))
    
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
    
    
    (define (delete_device device-name steward)
      (let ((steward-database (get_steward_db steward))
            (location (get_location steward)))
        ;uit database verwijderen
         (query-exec db (format "DELETE from ~a where name = $1" steward-database) device-name)

         (send-over-tcp `(delete_device ,device-name) steward)
         ;update logfile
         ;location moet er nog bij
         (send logsystem 'delete-device device-name location)))              
    
    
    (define (change_mesurement_current_steward name value)
      (let ((steward-database (get_steward_db current-steward))
            (location (get_location current-steward)))
        ;in database veranderen
        (query-exec db (format "update ~a set mesurement = $1 where name = $2" steward-database)  value name)
         (send-over-tcp `(change-mesurement ,name ,value) current-steward)
         ;update logfile
         (send logsystem 'change-mesurement location name value)))
    
    (define (change_status_current_device name value)
      (let ((steward-database (get_steward_db current-steward))
            (location (get_location current-steward)))
        (if value
            (begin
              (send-over-tcp `(change-status ,name "on") current-steward)
              (query-exec db (format "update ~a set status = $1 where name = $2" steward-database)  "on" name))
            (begin
              (send-over-tcp `(change-status ,name "off") current-steward)
              (query-exec db (format "update ~a set status = $1 where name = $2" steward-database)  "off" name)))
        (if value
            (send logsystem 'device-on location name)
            (send logsystem 'device-off location name))))
    
    (define (add-device type name serialnumber com-adress)
      (let ((steward-database (get_steward_db current-steward))
            (location (get_location current-steward)))
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
     
               (send-over-tcp `(add-device ,type ,name ,serialnumber ,com-adress) current-steward)
               (send logsystem 'add-device name location)))))
    
    ;ACCESSOREN STEWARD
    ;------------------
    
    ;algemene functie
    (define (list_devices_info_from_db  position . type)
      (let ((steward-database (get_steward_db current-steward)))
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
  
    (define (list_devices_status . type)
      (let ((location (get_location current-steward)))
      (send logsystem 'status-update location)
      
      (if (null? type)
          (send-over-tcp '(list_devices_status) current-steward)
          (send-over-tcp `(list_devices_status ,(car type)) current-steward))))
    
    
    (define (list_devices_location . type)
      (if (null? type)
          (send-over-tcp '(list_devices_location) current-steward)
          (send-over-tcp `(list_devices_location ,(car type)) current-steward)))
    
    
    (define (list_devices_mesurement . type)
      (if (null? type)
          (send-over-tcp '(list_devices_mesurement) current-steward)
          (send-over-tcp `(list_devices_mesurement ,(car type)) current-steward)))
    
    
    (define (list_devices_mesurement_with_value . type)
      (if (null? type)
          (send-over-tcp '(list_devices_mesurement_with_value) current-steward)
          (send-over-tcp `(list_devices_mesurement_with_value ,(car type)) current-steward)))
    
    (define (initialize-steward-list stewardlst)
      (define (loop lst)
        (if (empty? lst)
            (begin
              (display "steward-list is initialized")
              (newline))
            (let* ((steward (car lst))
                   (steward-database (cddr steward))
                   (stewardinfo (query-rows db (format "select * from ~a" steward-database))))
              (set! current-steward steward)
              ;send-over-tcp neemt altijd current steward om er nr toe te zenden, KUNNNEN WE NOG VERANDEREN
              (send-over-tcp '(initialize-steward stewardinfo))
              (loop (cdr lst)))))
      (loop stewardlst))
    

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
                          (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards lightswitch_type 'list_devices_mesurement dispatch)))
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
                          (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards temperaturesensor_type 'list_devices_mesurement dispatch)))
                          (new-data-list (cons new-value old-lst-data)))
                     (close-input-port input)
                     (let ((output (open-output-file file #:exists 'can-update)))
                       (write new-data-list output)
                       (close-output-port output))))))

    
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
        ((delete_device) delete_device)
        ((list_devices_names) list_devices_names_current_steward)
        ((list_devices_serial) list_devices_serial_current_steward)
        ((list_devices_com_adress) list_devices_com_adress_current_steward)
        ((list_devices_type) list_devices_type_current_steward)
        ((list_devices_status) list_devices_status)
        ((list_devices_location) list_devices_location)
        ((list_devices_mesurement) list_devices_mesurement)
        ((list_devices_mesurement_with_value) list_devices_mesurement_with_value)
        ((add-device) add-device)
        ((change-mesurement) change_mesurement_current_steward)
        ((change-status) change_status_current_device)
        (else (error 'Majordomo "unknown message ~a" message))))
    
    dispatch))





;TO DO represent steward in list and database and make connection => needs to be done! 
;GUI needs to be working


;(send-over-tcp '(add-device "test"))

