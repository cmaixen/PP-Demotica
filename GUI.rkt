#lang racket

;GUI

(require racket/gui/base)
(require db)
(#%require (prefix util: "Utillities.rkt"))
(#%require "Steward.rkt")
(#%require "Logsystem.rkt")

(provide make-gui)

(define db-pad "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/pp_db.sqlite")
(define (make-gui) 
  (let ((db (sqlite3-connect  #:database db-pad
                              #:mode 'read/write))
        (list_w_stewards '())
        (current-steward 'NULL)
        (current-listbox 'NULL)      
        (logsystem (make-logsystem)))
    
    
    ;starten van de GUI
    
    (define (start)
      (send dialog show #t))
    
    
    ;initialization
    ;--------------
    
    ;maakt een interne lijst met steward objecten aan
    ;bij het aanmaken kijkt hij in de database welke stewards er zijn aangemaakt
    
    (define  (initialize_gui)
      (define (loop lst)
        (if (empty? lst)
            (display "initialization done")
            (let* ((devicevector (car lst))
                   (location (vector-ref devicevector 0)))
              (set! list_w_stewards (cons (make-steward location logsystem) list_w_stewards))
              
              (loop (cdr lst)))))
      (loop (query-rows db "select * from the_stewards")))
    
    
    (initialize_gui)
    
    
    ;geeft de steward terug op de overeenkomstige locatie
    
    (define (get-steward locationname)
      (define (search lst)
        (if (empty? lst)
            (error 'search "steward -> ~a not found" locationname)
            (let* ((first_steward (car lst))
                   (location (util:send first_steward 'get-location)))
              (if (equal? location locationname)
                  first_steward
                  (search (cdr lst))))))
      (search list_w_stewards))
    
    
    
    
    ;maakt lijst aan met alle locaties van de stewards
    
    (define listrooms (util:list_neutralizer (query-rows db "select location from the_stewards") 0))
    
    ;maakt een lijst aan van al de verschillende devicetypes. !LIGT VAST!
    
    (define listdevices (list "Lightswitch" "Temperaturesensor"))
    
    
    ;Definieren van het hoofdframe
    
    (define mainframe (new frame% [label "Log in"]
                           [width 800]
                           [height 600]))
    
    ;tabpanel
    
    (define tab-panel
      (new tab-panel%	 
           [choices (list "Rooms" "Devices" "Map" "Logs" "Userinformation")]	 
           [parent mainframe]
           [callback (lambda (tab-panel click)
                       (case (send tab-panel get-selection)
                         ((0) (send tab-panel change-children (lambda (x) (list tab1))))
                         ((1) (send tab-panel change-children (lambda (x) (list tab2))))
                         ((2) (send tab-panel change-children (lambda (x) (list tab3))))
                         ((3) 	 (update-log)	 	 	
                                     (send tab-panel change-children (lambda (x) (list tab4))) )
                         (else (send tab-panel change-children (lambda (x) (list list-box-bathroom))))))]))
    
    
    
    
    
    ; LOGIN
    ;-------
    
    ;Username check
    (define (user_check name)
      (query-maybe-value db "select Username from user_system where Username = $1"  name))
    
    (define (get-password  name)
      (query-value db "select Password from user_system where Username = $1"  name))
    
    ;test of username aanwezig is en of passwoord dan correct is.
    ;lazy and is hier ideaal vanwege het feit dat het eerst gaat controleren of de username bestaat
    
    (define (check_login name password)
      (let ((result (and (user_check name) (equal? password (get-password name)))))
        (if result
            (util:send logsystem 'succesfull-login name)
            (util:send logsystem 'failed-login name))
        result
      ))
    
    
    
    ; Create a dialog
    (define dialog (new dialog% [parent mainframe] 
                        [label "Dialog"]))
    
    
    
    
    (define dialog_panel (new vertical-panel% [parent dialog]
                              [min-width 100]
                              [alignment '(center center)]
                              [min-height 40]))
    
    (define Button_panel (new horizontal-panel% [parent dialog]
                              [alignment '(center center)]
                              [min-width 100] 
                              [min-height 20 ]))
    
    ; Add a text field to the dialog
    
    (define message (new message%	 
                         [label "Welcome"]	 
                         [parent dialog_panel]
                         [enabled #t]	   	 	 
                         [min-width 100]	 
                         [min-height 20]	 
                         [auto-resize #t]))
    
    
    
    ;Textfields login
    
    
    (define username_input (new text-field% [parent dialog_panel] [label "Username"]))
    (define password_input (new text-field% [parent dialog_panel] [style '(single password)] [label "Password "]))
    
    
    ; Add Cancel and Ok buttons to the horizontal panel
    
    (define button-login (new button% [parent  Button_panel ] [label "Login"] [callback (lambda (button-cancel click)
                                                                                          (print (user_check (send (send username_input get-editor) get-text)))
                                                                                          (if (check_login (send (send username_input get-editor) get-text) (send (send password_input get-editor) get-text) )
                                                                                              (begin (send dialog show #f) 
                                                                                                     (send mainframe set-label "Welcome to your personal Electricity Home Monitoring System"))
                                                                                              (send message set-label "Wrong Username/Password"))
                                                                                          
                                                                                          )]))
    
    (define button-cancel (new button% [parent  Button_panel ] [label "Cancel"] [callback
                                                                                 (lambda (button-cancel click)
                                                                                   (send dialog show #f)
                                                                                   (send mainframe on-exit)
                                                                                   
                                                                                   )]))
    
    
    
    
    
    
    
    
    ;TAB 1
    ;-----
    
    ;genereren van de  buttons voor de  roomtab
    (define (buttongenerator lst destination tab)
      (cond ( (empty? lst)
              (display "done"))
            ((not (equal? (util:list_size lst) (util:list_size destination))) (error 'buttongenerator-roomtab "labellist and destination list  are not of the same size"))
            
            (else (let ((name (car lst))
                        (dest (car destination)))
                    (new button% [parent tab] [label name]
                         [vert-margin 20]	
                         [horiz-margin 20]	 
                         [min-width 20]	 
                         [min-height 20]
                         [callback (lambda (button click) (send tab-panel change-children (lambda (x) (list dest add_button delete_button)))
                                     (set! current-listbox dest)
                                     (set! current-steward (get-steward name))
                                     (update-list-box dest))])
                    (buttongenerator (cdr lst) (cdr destination) tab)))))
    
    ;genereren van list-boxen voor tab1
    
    (define (list-box-tab1 string) (new list-box%
                                   (label string)
                                   (parent tab-panel)
                                   (choices (util:send (get-steward string) 'list_devices_names))
                                   (style (list 'single
                                                'column-headers))
                                   (columns (list "Name"
                                                  "Type"
                                                  "Status"
                                                  "Serial"
                                                  "Last Update"
                                                  ))))
    
    
    ;de verschillende list-boxen
    
    (define list-box-bathroom (list-box-tab1 "Bathroom"))
    (define list-box-bedroom (list-box-tab1 "Bedroom"))
    (define list-box-livingroom (list-box-tab1 "Livingroom"))
    
    
    (define tab1 (new vertical-panel%
                      [parent tab-panel]
                      [spacing 10]	 
                      [alignment (list 'center 'center)]
                      ))
    
    ;destination list moet nog automatisch gegenereerd worden 
    
    (buttongenerator listrooms (list list-box-livingroom list-box-bathroom list-box-bedroom) tab1)
    
    
    (define add_button (new button% [parent tab-panel ] [label "Add"] [callback (lambda (button click) (send add-dialog show #t))]))
    
    ;ADD-Dialog
    
    (define add-dialog (new dialog% [parent mainframe] 
                            [label "Add-Dialog"]))
    
    
    (define add-dialog_panel (new vertical-panel% [parent add-dialog]
                                  [min-width 400]
                                  [alignment '(center center)]
                                  [min-height 40]))
    
    (define name_device (new text-field% [parent add-dialog_panel] [label "Name"]))
    (define type (new choice% [parent add-dialog_panel] [label "Type"] [choices listdevices] ))
    (define serial (new text-field% [parent add-dialog_panel] [label "Serialnumber"]))
    (define com-adress (new text-field% [parent add-dialog_panel] [label "COM-adres"]))
    
    (define add_button-dialog (new button% [parent add-dialog ] [label "Add"] [callback (lambda (button click) (send add-dialog show #f)
                                                                                          (util:send current-steward 'add-device
                                                                                                    (determine-choice listdevices (send type get-selection))
                                                                                                     (send (send name_device get-editor) get-text)
                                                                                                     (send (send serial get-editor) get-text)
                                                                                                     (send (send com-adress get-editor) get-text))
                                                                                          (update-list-box current-listbox))]))
    
    (define delete_button (new button% [parent tab-panel ] [label "Delete"] [callback (lambda (button click)
                                                                                        
                                                                                        (util:send current-steward 'delete_device (send current-listbox get-string-selection))
                                                                                        
                                                                                        (update-list-box current-listbox))]))
    ;beslissen welke keuze je moet teruggeven bij een keuzeobject
    
    ;algmene functie 
   (define (determine-choice lst selection)
     (define (loop lst counter)
       (let ((choice (car lst)))
         (if (= counter selection)
             choice
             (loop (cdr lst) (+ counter 1)))))
     (if (< (util:list_size lst) selection)
         (error 'determine-choice-function "Selection is out of range of the list!")
         (loop lst 0)))
    

    
    ;listbox updater voor tab 1
    
    (define (update-list-box lst-box)
      (let ((namelst (util:send current-steward 'list_devices_names))
            (typelst (util:send current-steward 'list_devices_type))
            (statuslst (util:send current-steward 'list_devices_status))
            (seriallst (util:send current-steward 'list_devices_serial))
            (com-adresslst (util:send current-steward 'list_devices_com_adress)))
        (util:send current-steward 'get-location)
        (send lst-box set namelst typelst statuslst seriallst com-adresslst)))
    
    
    
    ;TAB 2 / DEVICES
    ;---------------
    
    ;Locatie moet nog geregeld worden het best is dat dit ook in een database wordt bijgehouden
    ;in overweging moet er de status bijgehouden worden in database zodat het altijd mogelijk is om "offline" de status de zien hierbij wordt ook de "timestamp interessanter
    ;personalized  info over ieder object => eigen logfile  ; meer info : wie allemaal toegang totaal energiegebruik , gemiddelde blabla, soort van profielbeschrijving
    ;database met logfile adressen zodanig dat elke object eraan kan.
    (define tab2 (new vertical-panel%
                      [parent tab-panel]
                      [alignment (list 'center 'center)]
                      ))
    
  
   
   
    
   ;!!!! HOGERE ORDE FUNCTIE IS HIER MOGELIJK! MOET NOG AANGEPAST WORDEN!!!!!!
    
        (define (list-box-tab2 string type) 
          (new list-box%
                                   (label string)
                                   (parent tab-panel)
                                   (choices '())
                                   (style (list 'single
                                                'column-headers))
                                   (columns (list "Name"
                                                  "Status"
                                                  "Location"
                                                  "Serial"
                                                  "Last Update"
                                                  ))))
      ;creeren van listboxen
    
    (define list-box-lightswitch (list-box-tab2 "Lightswitch" "Lightswitch" ))
    (define list-box-tempsensor (list-box-tab2 "Temperaturesensor" "Temperaturesensor"  ))
    
    
     ;creeeren van knoppen tab2
    
        (define (buttongenerator-tab2 lst destination tab)
      (cond ( (empty? lst)
              (display "done"))
            ((not (equal? (util:list_size lst) (util:list_size destination))) (error 'buttongenerator-roomtab "labellist and destination list  are not of the same size"))
            
            (else (let ((name (car lst))
                        (dest (car destination)))
                    (new button% [parent tab] [label name]
                         [vert-margin 20]	
                         [horiz-margin 20]	 
                         [min-width 20]	 
                         [min-height 20]
                         [callback (lambda (button click) (send tab-panel change-children (lambda (x) (list dest add_button delete_button)))
                                     (set! current-listbox dest)
                              (set! current-steward (get-steward "Bathroom"))
                                     (update-list-box-tab2 dest name))])
                    (buttongenerator (cdr lst) (cdr destination) tab)))))
    
        (define (update-list-box-tab2 lst-box type)
      (let ((namelst (util:send current-steward 'list_devices_names type ))
            
            (statuslst (util:send current-steward 'list_devices_status type))
            (seriallst (util:send current-steward 'list_devices_serial type))
            (com-adresslst (util:send current-steward 'list_devices_com_adress type)))
        (display namelst)
        (display statuslst)
        (display seriallst)
        (display com-adresslst)
        
        (send lst-box set namelst statuslst statuslst seriallst com-adresslst)))
    
    
                    
    
    (buttongenerator-tab2 listdevices (list list-box-lightswitch list-box-tempsensor) tab2)
    
    ;TAB 3 / MAP
    ;-----------
    
    
    (define tab3 (new vertical-panel% 
                      [parent tab-panel]
                      [alignment (list 'center 'center)]
                      ))
    
    
    
    
    ;  (make-object bitmap% "/Users/yannickmerckx/Desktop/white-house-floor-plans.jpg")
    ;  (new bitmap-dc% [bitmap object]) ))
    
    
    ;plattegrond
    
    (define plattegrond (make-object bitmap% "/Users/yannickmerckx/Desktop/white-house-floor-plans.jpg"))
    (define plat-dc
      (new bitmap-dc% [bitmap plattegrond])) 
    
    
    
    
    
    ;definieren van Bitmap
    (define object (make-object bitmap% 
                     
                     100
                     20
                     
                     ))
    
    (define object-dc
      (new bitmap-dc% [bitmap object]) )
    
    (send object-dc set-scale 1 1)
    (send object-dc set-text-foreground "black")
    (send object-dc draw-text "press this button" 0 0)
    (send object-dc set-smoothing 'aligned)
    
    ;TAB 4 / LOGS
    ;------------
    
    (define tab4 (new vertical-panel% 
                      [parent tab-panel]
                      [alignment (list 'center 'center)]
                      
                      ))
    
    
    ;aanmaken van tekst object
    
    (define t (new text% ))
 ;  (send t find-string (file->string  "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/Logfile.txt"))
 ;   (send t load-file  "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/Logfile.txt")
    ;aanmaken van editor-panel
    
    (define log-panel (new editor-canvas%	 
                           [parent tab4]
                           (editor t)))
  
    
    ;update van de log door gewoon de file terug in te laden
    (define (update-log) 
     (send t load-file  "/Users/yannickmerckx/Dropbox/Unif/2de bachelor/Programmeerproject 2/PP-Demotica/Logfile.txt")
        )
  
    ;logfile inloaden in textobject dat gebonden is aan editorcanvas
    
    
    
    
    (define Update_button (new button% [parent tab-panel ] [label "Delete"] [callback (lambda (button click)
                                                                                        (logsystem 'Update-all)
                                                                                        (update-list-box current-listbox))]))
    
    ;TAB 5 / USERINFORMATION
    ;----------------------
    
    
    (send tab-panel change-children (lambda (x) (list tab1)))
    
    (define (dispatch message)
      (case message 
        ((start) start)
        (else (error 'GUI "unknown message ~a" message))))
    
    
    dispatch
    ))







;TESTINIG
;-------

(util:send (make-gui) 'start)
(define db (sqlite3-connect  #:database db-pad
                             #:mode 'read/write))


