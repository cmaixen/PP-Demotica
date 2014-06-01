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


#lang racket
(#%require  "Utillities.rkt")
(#%require "GUI.rkt") 
(#%require "xbee_utils.rkt")
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
         (current-steward 'NULL) ;pair (naam . (serveradress .  steward-database)) serveradress = (ip . port)
         (current-device 'NULL)
         (current-user 'NULL)
         (logsystem (make-logsystem))
         (listrooms (list_neutralizer (query-rows db "select location from the_stewards") steward_table_locationcolummn)))
    
    
    ;starten van programma
    (define (start)
      (send (make-gui dispatch logsystem) 'start))
    
    
    
    ;initializatiefunctie
    
    ;maakt een interne lijst met stewards aan
    ;de stewards worden voorgesteld als het volgend paar (naam . (serveradress .  steward-database)) met serveradress = (ip . port)
    ;bij het aanmaken kijkt hij in de database welke stewards er zijn aangemaakt
    
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
    
    
    ;maakt een steward-pair met de gegegeven info.
    ;een stewardpair wordt voorgesteld door (naam . (serveradress .  steward-database)) met serveradress = (ip . port)
    (define (make_steward_info location ip port steward_database)
      (let ((new_steward (cons location (cons (cons ip port) steward_database)))
            )
        (check_online_device  (send-over-tcp 'initialize new_steward) steward_database)
        new_steward))
    
    
      ;returns boolean
    ;kijkt of het gegeven element zich in de lijst bevindt
    (define (check_if_in_list item lst)
      (define (loop lst)
        (cond ((empty? lst) #f)
              ((equal? item (car lst)) #t)
              (else (loop (cdr lst)))))
      (loop lst))
    

    
    ;gaat kijken welke toestellen er in de database zitten en geeft een lijst terug met paren
    ;bv '(("toestel1" #(serveradress)) ("toestel2" #(serveradress)))
    
    (define (generate_old_device_lst database)
      (let* ((lst (query-rows db (format "select * from ~a" database))))
        (define (loop lst result)
          (if (empty? lst)
              result
              (let*((device (car lst))
                    (device-name (vector-ref device 0))
                    (device-adress   (bytes->vector (vector-ref device 1))))         
                (loop (cdr lst) (append result (list (list device-name device-adress)))))))
        (loop lst '())))
    
    
     ;is een functie
    ;geeft een lijst terug met de namen van de toestellen van de huidige steward
    (define (get_list_devicesnames_steward steward)
      (let ((list_of_devices (send-over-tcp 'list_of_devices steward)))
        (define (loop lst result)
          (if (empty? lst) 
              result
              (loop (cdr lst) (append result (list (caar lst))))))
        (loop list_of_devices '())))
    
    ;is functie
    ;geeft lijst met namen van toestellen in databases 
    (define (get_list_databasedevices steward)
      (let* ((steward_db (get_steward_db steward))
             (devices (query-rows db (format "select device_name from ~a" steward_db))))
        (define (loop lst result)
          (if (empty? lst) 
              result
              (loop (cdr lst) (append result (list (vector-ref (car lst) 0))))))
        (loop devices '())))
    

    (define offlinecounter_default 0)
    ;update van database
    ;kijkt of het device online is en gaat dan de database updaten met de online/offline info.
    ;je geeft de lijst met de toestellen van de database mee en de lijst met de momenteel gevonden toestellen door de steward
    
    (define (check_online_device lst_with_discovered_devices database)
      (let((old_lst (generate_old_device_lst database)))
        
        (define (loop_check_online lst)
          (if (empty? lst)
              'done
              (let* ((device (car lst))
                     (device_name (car device))
                     (device_adress(vector->bytes (cadr device))))
                
                (if (check_if_in_list (car lst) old_lst)
                    ;als het al in de lijst zit , zet toestel op online
                    (let ((offlinecounter (vector-ref (query-row db (format "select * from ~a  where device_name = $1" database) device_name) 3)))
                      (query-exec db (format "delete from ~a  where device_name = $1" database) device_name)
                            (query-exec db (format "insert into ~a values ($1, $2 , $3, $4)" database) device_name device_adress online offlinecounter_default ))
                    ;het zit er nog niet in dan moet het wel een nieuw toestel zijn en voegen we deze toe
                    (query-exec db (format "insert into ~a values ($1, $2 , $3, $4)" database) device_name device_adress online ))
                (loop_check_online (cdr lst)))
                ))
        
        
        (define (loop_check_offline lst)
          (if (empty? lst)
              'done
              (let* ((device (car lst))
                     (device_name (car device))
                     (device_adress(vector->bytes(cadr device))))
     
                (if (check_if_in_list (car lst) lst_with_discovered_devices)
                    'done
                    ;als het niet in de lijst zit, zet toestel op offline
                    (let ((offlinecounter (vector-ref (query-row db (format "select * from ~a  where device_name = $1" database) device_name) 3)))
                      (query-exec db (format "delete from ~a  where device_name = $1" database) device_name)
                            (query-exec db (format "insert into ~a values ($1, $2 , $3, $4)" database) device_name device_adress offline offlinecounter ))
                    
                    )
                (loop_check_online (cdr lst)))))
        
        
        ;controleert of alle devices in database online zijn 
        (loop_check_online lst_with_discovered_devices)
        (loop_check_offline old_lst)
        
        ))
    
    
    ;geeft de productID van een toestel terug op een dev_answer
    (define (get-productID name)
  (let* ((answer (request "DEV\n" name))
     (vector (string->vector answer))
    (productID (vector-loop productID_offset productID_end vector)))
        (vector->string productID)))
    
    ;geeft het type terug van de steward
    ;gebaseerd op de current steward
    (define (get_type name)
        (get-productID name))
    
    
        
    (define (add-device name com-adress)
      ;apparaat in database toevoegen 
      ;je mag geen veld oplaten!
      (let ((steward-database (get_steward_db current-steward))
            (location (get_location current-steward)))
      (cond ((not (and (non-empty-string? name)))
             "FAIL: Not everything is filled in!")
             ((query-maybe-row db (format "select device_name from ~a where device_name = $1" steward-database ) name)
              "FAIL: Device with the same name is already added!")
            ((= (vector-length com-adress) 16)
             "FAIL: invalid com-adress")
            (else
             (query-exec db (format "insert into ~a values ($1, $2 , $3 , $4)" steward-database) name (vector->bytes com-adress) offline offlinecounter_default )
             (send logsystem 'add-device name location)
             "Device added succesfully!"))

        
           ))

    
    ;ACCESSOREN
    ;----------
    ;geeft de huidige steward waar je mee aan het werken ben terug
    (define (get-current-steward)
      (car current-steward))
        
    ;geeft lijst me offline toestellen terug
    ;geeft locatie/naam van de steward. locatie en naam zijn hetrzelfde
    (define (get_location steward)
      (car steward))
    
    ;geeft database (de naam ervan) terug
    (define (get_steward_db steward)
      (cddr steward))
    
    ;geeft een lijst terug met al de verschillende kamers
    (define (get-listrooms)
      listrooms)
    
    ;geeft de lijst met stewardobjecten terug
    (define (get-list_w_stewards)
      list_w_stewards)
    

    
    ;geeft lijst me offline toestellen terug
    (define (get_offline_devices steward)
      (let ((online_list  (get_list_devicesnames_steward steward))
            (offline_devices (get_list_databasedevices steward)))
        
        (define (loop lst onlinelst result)
          (cond ((empty? lst) 
                 result)
                ((check_if_in_list (car lst) onlinelst)
                 (loop (cdr lst) onlinelst result))
                (else   (loop (cdr lst) onlinelst (append result (list (car lst)))))))
        (loop offline_devices online_list '())))
    
        ;geeft het huidige device waar je op aan het werken ben terug
    (define (get-current-device)
      current-device)
    
        (define (get-password  name)
      (query-value db "select Password from user_system where Username = $1"  name))
    
    
    
    ;MUTATOREN
    ;---------
    
    ;de huidige steward veranderen in de gegeven
    (define (set-current-steward name-steward)
      (define (loop lst )
        (cond ((empty? lst) (error 'majarodomo "Steward does not exist!"))
              ((equal? (caar lst) name-steward ) (set! current-steward  (car lst)))
              (else (loop (cdr lst)))))
      (loop list_w_stewards))
    
    
    ;huidige toestel bijhouden
    (define (set-current-device device-name)
      (set! current-device device-name))
    
    
    
    ;verwacht lijst met nummers
    (define (calculate-average-from-numberlist list)
      (define (loop lst result counter)
        (if (empty? lst)
            (/ result  counter)
            (loop (cdr lst) (+ result (string->number (car lst))) (+ counter 1))))
      (loop list 0 0)) 
    
    (define (set-current-user name)
      (set! current-user name))
    
    
     ;FUNCTIONALITEITEN
    ;------------------
    
     ;commando sturen naar steward en dan xbee
    ;stuurt request naar naar huidige steward voor het huidige toestel 
    (define (request message name)
      (send-over-tcp message current-steward name))
    

    ;kijk of current-user de toestemming heeft om aanpassing te doen
    (define (check-authorized)
      ;tegen hacking
      (if (equal? current-user 'NULL)
          #f
          (let ((permissionlevel (vector-ref (query-row db "select Permissionlevel from user_system where username = $1" current-user) 0)))
            (if (= permissionlevel admin_level)
                #t
                #f))))
    
    
    
    
     ;Username check
    (define (user_check name)
      (query-maybe-value db "select Username from user_system where Username = $1"  name))
    

    
    ;initializeren van de steward
    (initialize_majordomo)
    
    
    ;gemiddelde beschikbaarheid van de toestellen in %
    ; wanneer een toestel lang niet beschikbaar is geeft het een melding
    
   (define  (monitor-devices steward)
    
     (define (loop lst stewarddb)
       
       (if (empty? lst)
           (let ((online (length (get_list_devicesnames_steward steward)))
                 (total (length (get_list_databasedevices steward))))
             (* (/ online total) 100))
    (begin (display (car lst))
           (let* ((device (query-row db (format "select * from ~a where device_name = $1" stewarddb) (car lst)))
                 (devicename (vector-ref device 0))
                 (deviceadress (vector-ref device 1))
                 (devicestatus (vector-ref device 2))
                 (deviceofflinecounter (vector-ref device 3)))
               (query-exec db (format "delete from ~a  where device_name = $1" stewarddb) devicename)
           (query-exec db (format "insert into ~a values ($1, $2 , $3 , $4)" stewarddb)  devicename deviceadress devicestatus (+ deviceofflinecounter 1))
             (loop (cdr lst) stewarddb)))))
  (send-over-tcp 'update steward)

      (loop (get_offline_devices steward) (get_steward_db steward)))
           
           
(define (thread-loop)
  (define (loop lst filelst)
    (if (empty? lst)
        'done
         (let* ((steward  (car lst))
                (file (car filelst))
                          (input (open-input-file (format "Sources/~a.txt" file)))
                          (old-lst-data (read input))
                          (new-value (monitor-devices steward))
                          (new-data-list (cons new-value old-lst-data)))
                     (close-input-port input)
                     (let ((output (open-output-file (format "Sources/~a.txt" file) #:exists 'can-update)))
                       (write new-data-list output)
                       (close-output-port output))
           (loop (cdr lst) (cdr filelst)))))
  (loop list_w_stewards listrooms))
        
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
    
    (print "thread2 started")
    
    (thread    (lambda () 
                    (
                 (do ([i 1 (+ i 1)])
                   ((= i 0) (print "done"))
                   
                   (sleep 5)
         (thread-loop)))))
    
    
   
   ;geeft boolean terug   
      (define (offlinelimit? steward-name)
    (let* ((steward (get-steward steward-name))
           (stewarddb (get_steward_db steward))
          (offlinelimits (query-rows db (format  "select device_name from ~a where offlinecounter > $1" stewarddb) offlinecounterlimit)))
      (not (empty? offlinelimits))))
    
    
    ;return string
         (define (get_checkup_devices steward-name)
    (let* ((steward (get-steward steward-name))
           (stewarddb (get_steward_db steward))
          (offlinelimits (query-rows db (format  "select device_name from ~a where offlinecounter > $1" stewarddb) offlinecounterlimit)))
      (define (loop lst result)
        (if (empty? lst) 
            result
            (loop (cdr lst) (format "~a ~a" result (vector-ref (car lst) 0)))))
      (loop offlinelimits "")))
    
    (define (dispatch message)
      (case message
        ((start) start)
        ((get-listrooms) get-listrooms)
        ((request) request)
        ((get-current-steward) get-current-steward)
        ((set-current-steward) set-current-steward)
        ((set-current-device) set-current-device)
        ((get-current-device) get-current-device)
        ((get-type) get_type)
        ((get_offline_devices) get_offline_devices)
        ((set-current-user) set-current-user)
        ((check-authorized) check-authorized)
        ((get-password) get-password)
        ((user_check) user_check)
        ((offlinelimit?) offlinelimit?)
        ((get_checkup_devices) get_checkup_devices)
        ((get_list_devicesnames_steward) get_list_devicesnames_steward)
        ((get-steward) get-steward)
        ((add-device) add-device)
        (else (error 'Majordomo "unknown message ~a" message))))
    dispatch))



(define test (make-majordomo))
(send test 'start)

    ;geeft het steward-pair terug op de overeenkomstige locatie 
    ;iedere steward valt te identificeren aan zijn locatie! dus algemeen wordt de locatie als de naam/identiteit van de steward genomen.
    


;(send test 'set-current-steward (send test 'get-steward "Livingroom"))
;(send test 'request "GET\n" "PowerPlug")
;(send test 'request "SET POW=ON\n" "PowerPlug")



;TO DO represent steward in list and database and make connection => needs to be done! 
;GUI needs to be working





;TO DO
;threads
;commando's inbouwen
;inloggegevens
;graphes
;verslag