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
                   (steward-database (vector-ref devicevector steward_table_steward_database_column)) ;naam van database waar stewardinfor zich bevind
                   (ip (vector-ref devicevector steward_table_ip_column))
                   (port (vector-ref devicevector steward_table_port_column)) 
                   (serveradress (cons ip port)))                
              (set! list_w_stewards (cons (make_steward_info location ip port steward-database) list_w_stewards))
              (loop (cdr lst)))))
      (loop (query-rows db "select * from the_stewards")))
    
    (define (initialize_2)
      (let* (  (location "Livingroom")
               (steward-database "Livingroom") ;naam van database waar stewardinfor zich bevind
               (ip "192.168.1.131")
               (port 6666) 
               (serveradress (cons ip port)))
 (set! list_w_stewards (list  (make_steward_info location ip port steward-database)))))
    
    (define (make_steward_info location ip port steward_database)
      (let ((new_steward (cons location (cons (cons ip port) steward_database)))
           )
     (check_online_device  (send-over-tcp 'initialize new_steward))
        (display new_steward)
        new_steward))
    
    
    (define (check_online_device lst_with_discovered_devices)
      'done)
    
    
    ;geeft de steward terug op de overeenkomstige locatie 
    ;iedere steward valt te identificeren aan zijn locatie! dus algemeen wordt de locatie als de naam/identiteit van de steward genomen.
    (define (get-steward locationname)
      
      (define (search lst)
        (if (empty? lst)
            (error 'search "steward -> ~a not found" locationname)
            (let* ((first_steward (car lst))
                   (location (get_location first_steward)))
               (display location)
              (display locationname)
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
    
 
   ;initializeren van de steward
   ; (initialize_majordomo)
    (initialize_2)
    
    (print "thread1 started")
    ;thread die om het uur informatie wegschrijft naar de logfile wat het gemiddelde verbruik is
   ; (thread    (lambda () 
   ;              (do ([i 1 (+ i 1)])
   ;                ((= i 0) (print "done"))
   ;                (sleep light-graph-sleep)
   ;                
   ;                (let* ((file light-graph-file)
   ;                       (input (open-input-file file))
   ;                       (old-lst-data (read input))
   ;                       (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards lightswitch_type 'list_devices_mesurement dispatch)))
   ;                       (new-data-list (cons new-value old-lst-data)))
   ;                  (close-input-port input)
   ;                  (let ((output (open-output-file file #:exists 'can-update)))
   ;                    (write new-data-list output)
   ;                    (close-output-port output))))))
    
    
    ;thread die om het uur informatie wegschrijft naar de logfile wat het gemiddelde verbruik is
    ;(print "thread2 started")
    ;(thread    (lambda () 
    ;             (do ([i 1 (+ i 1)])
    ;               ((= i 0) (print "done"))
    ;               
    ;               (sleep temp-graph-sleep)
    ;       
    ;               (let* ((file temp-graph-file)
    ;                      (input (open-input-file file))
    ;                      (old-lst-data (read input))
    ;                      (new-value (calculate-average-from-numberlist (list_devices_spectype list_w_stewards temperaturesensor_type 'list_devices_mesurement dispatch)))
    ;                      (new-data-list (cons new-value old-lst-data)))
    ;                 (close-input-port input)
    ;                 (let ((output (open-output-file file #:exists 'can-update)))
    ;                   (write new-data-list output)
    ;                   (close-output-port output))))))
    
    ;commando sturen naar steward en dan xbee
    ;bv: (send-over-tcp "SET POW=ON" EnergySensor_LivingRoom) 
    (define (request message device_name)
      (send-over-tcp message current-steward device_name))
    

    
        (define (dispatch message)
      (case message
        ((start) start)
        ((get-steward) get-steward)
        ((get-listrooms) get-listrooms)
        ((get-listdevices) get-listdevices)
        ((get-db) get-db)
        ((request) request)
        ((get-list_w_stewards) get-list_w_stewards)
        ((get-current-steward) get-current-steward)
        ((set-current-steward) set-current-steward)
        (else (error 'Majordomo "unknown message ~a" message))))
    
    dispatch))



(define test (make-majordomo))

(send test 'set-current-steward (send test 'get-steward "Livingroom"))


(send test 'request "SET POW=ON" "ZBS110V2120895")

;TO DO represent steward in list and database and make connection => needs to be done! 
;GUI needs to be working


;(send-over-tcp '(add-device "test"))

