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

; Beschrijving: ADT GUI_tab2 ; Argumenten: Majordomo-object, GUI-object
;               Het ADT is een voorgeschreven panel met een bepaalde workflow
;
; Output: gui_tab2 object
;
; Messages:
;
; - get-tab2: argumenten: geen
;          output: panel%
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(#%require "GUI_dialogmaker.rkt")
(#%require "Constants.rkt")

(provide make_GUI_tab2)


(define (make_GUI_tab2 majordomo GUI)
  (let ((mainframe (util:send GUI 'get-mainframe))
        (tab-panel (util:send GUI 'get-tabpanel))
        (listdevices (util:send majordomo 'get-listdevices))
        (list_w_locations (util:send majordomo 'get-listrooms)))
    
    
    ;ACCESSOR
    ;--------
    
    (define (get-tab2)
      tab2)
    
    
    ;SETUP
    ;-----
    
    ;verticaal paneel in tabpaneel
    (define tab2 (new vertical-panel%
                      [parent tab-panel]
                      [alignment (list 'center 'center)]
                      ))
    
    ;"add"-knop toevoegen aan paneel dat add-dialoog doet verschijnen
    (define add_button (new button% [parent tab-panel ] [label "Add"] [callback (lambda (button click) (util:send add-dialog-object 'show-dialog))]))
    
    
    
    ;"delete"-knop toevoegen aan paneel dat een toestel kan verwijdere, triggert ook systeem om dit uit te voeren en update listbox
    (define delete_button (new button% [parent tab-panel ] [label "Delete"] [callback (lambda (button click)
                                                                                        
                                                                                        (let* ((current-listbox (util:send GUI 'get-current-listbox))
                                                                                               (current-selection (send current-listbox get-selection))
                                                                                               (datalist (send current-listbox get-data current-selection))
                                                                                               (current-location (caddr datalist))
                                                                                               (current-device (car datalist))
                                                                                               
                                                                                               (type-device (send current-listbox get-label )))
                                                                                          
                                                                                          (display current-location)
                                                                                          
                                                                                     
                                                                                          (util:send majordomo 'delete_device current-device (util:send majordomo 'get-steward current-location) )
                                                                                          
                                                                                          (update-list-box-spectype current-listbox type-device  (util:send majordomo 'get-list_w_stewards) majordomo)))]))
    
    ;ADD-Dialog
    ;----------
    
    
    
    (define add-dialog-object (make-dialog GUI "Add-dialog"))
    (define add-dialog (util:send add-dialog-object 'get-dialog))
    
    ;toevoegen van tekstvelden aan dialoog
    
    ;tekstveld voor naam
    (define name_device (new text-field% [parent add-dialog] [label "Name"]))
    
    ;tekstveld voor locatie
    (define location (new choice% [parent add-dialog] [label "Location"] [choices list_w_locations] ))
    
    ;tekstveld voor serial
    (define serial (new text-field% [parent add-dialog] [label "Serialnumber"]))
    
    ;tekstveld voor het com-adress
    (define com-adress (new text-field% [parent add-dialog] [label "COM-adres"]))
    
    ;"add"-knop in het "add"-dialoog dat het systeem triggert om ingegeven toestel toe te voegen en update listbox
    (define add_button-dialog (new button% [parent add-dialog ] [label "Add"] [callback (lambda (button click) (util:send add-dialog-object 'hide-dialog)
                                                                                          (util:send majordomo 'set-current-steward (util:send majordomo 'get-steward (send location get-string-selection)))
                                                                                          (util:send majordomo 'add-device
                                                                                                     ;type
                                                                                                     (send (util:send GUI 'get-current-listbox) get-label)
                                                                                                     ;name
                                                                                                     (send (send name_device get-editor) get-text)
                                                                                                     ;serial
                                                                                                     (send (send serial get-editor) get-text)
                                                                                                     ;com-adress
                                                                                                     (send (send com-adress get-editor) get-text))
                                                                                          
                                                                                          (update-list-box-spectype (util:send GUI 'get-current-listbox) (send (util:send GUI 'get-current-listbox) get-label) (util:send majordomo 'get-list_w_stewards) majordomo))]))
    
   
    ;ADJUSTMENT DIALOG TEMPERATURE
    ;-----------------------------
    
    (define adjust-dialog-object (make-dialog GUI "Adjustment-Dialog"))
    (define adjust-dialog (util:send adjust-dialog-object 'get-dialog))
    
    
    
    ;selecteer de juiste slider die moeten worden weergegeven
    (define (get-right-slider type)
      (cond ((equal? type lightswitch_type) slider-light)
            ((equal? type temperaturesensor_type) slider-temp)
            (else (error 'get-right-slider "unknown type, no valid slider found!"))))
    
    ;"adjust"-knop toevoegen aan paneel dat add-dialoog doet verschijnen
    (define adjust_button (new button% [parent tab-panel ] [label "Adjust"] [callback (lambda (button click)
                                                                                        (let* ((slidertype (send (util:send GUI 'get-current-listbox) get-label))
                                                                                               (correct-slider (get-right-slider slidertype)))
                                                                                          (send adjust-dialog change-children (lambda (x)(list correct-slider statusbox confirm_button-dialog cancel_button-dialog)))
                                                                                          
                                                                                          (util:send adjust-dialog-object 'show-dialog)))]))
    
    
    ;temperatuurslider
    (define slider-temp (new slider% [parent adjust-dialog] [label "°C"] [min-value min_value_tempslider]	 
                             [max-value max_value_tempslider]
                             [min-width min_width_dialog]	 
                             [min-height min_height_dialog]))
    ;lichtslider
    (define slider-light (new slider% [parent adjust-dialog] [label "Watt"] [min-value min_value_ligthslider]	 
                              [max-value max_value_ligthslider]
                              [min-width min_width_dialog]	 
                              [min-height min_height_dialog]))
    
    ;bevestigingsknop
    (define confirm_button-dialog (new button% [parent adjust-dialog ] 
                                       [label "Confirm"] 
                                       [callback (lambda (button click)
                                                   (let* ((current-listbox (util:send GUI 'get-current-listbox))
                                                          (current-selection (send current-listbox get-selection))
                                                          (datalist (send current-listbox get-data current-selection))
                                                          (current-location (caddr datalist))
                                                          (current-steward (util:send majordomo 'get-steward current-location))
                                                          (current-device-name (car datalist))
                                                          (current-type (send current-listbox get-label))
                                                          (slider (get-right-slider current-type))
                                                          (new-value (send slider get-value))
                                                          (statusvalue (not (send statusbox get-value))))
                                                     
                                                     ;change-mesurement en change-status werken met de current-steward.
                                                     
                                                   (util:send majordomo 'set-current-steward current-steward)
                                                     (if statusvalue
                                                         (begin (util:send majordomo 'change-status current-device-name statusvalue)
                                                                (util:send majordomo 'change-mesurement current-device-name new-value))
                                                         (begin
                                                           (util:send majordomo 'change-mesurement current-device-name device_off_mesurement)
                                                           (util:send majordomo 'change-status current-device-name statusvalue)
                                                           ))
                                                     
                                                     (update-list-box-spectype (util:send GUI 'get-current-listbox) (send (util:send GUI 'get-current-listbox) get-label) (util:send majordomo 'get-list_w_stewards) majordomo)
                                                     (util:send adjust-dialog-object 'hide-dialog)))]))
    
    (define cancel_button-dialog (new button% [parent adjust-dialog]
                                      [label "Cancel"]
                                      [callback (lambda (button click) (util:send adjust-dialog-object 'hide-dialog))]))
    
    ;statusbox toevoegen
    
    (define statusbox (new check-box%	 
                           [label "Turn Device Off"]	 
                           [parent adjust-dialog]
                           [value #f]))
    
    ;creeren van listboxen
    
    (define list-box-lightswitch (list-box-tab lightswitch_type (list "Name" "Status" "Location" "Serial" "Usage (Watt)") tab-panel))
    (define list-box-tempsensor (list-box-tab temperaturesensor_type (list "Name" "Status" "Location" "Serial" "Current Temperature (°C)" ) tab-panel))
    
    
    ;creeeren van knoppen tab2
    
    (define (buttongenerator-tab2 lst destination tab)
      (cond ( (empty? lst)
              (display "done"))
            ((not (equal? (util:list_size lst) (util:list_size destination))) (error 'buttongenerator-roomtab "labellist and destination list  are not of the same size"))
            
            (else (let ((name (car lst))
                        (dest (car destination)))
                    (new button% [parent tab] [label name]
                         [vert-margin standaardmargin_button]	
                         [horiz-margin standaardmargin_button]	 
                         [min-width standaardmargin_button]	 
                         [min-height standaardmargin_button]
                         [callback (lambda (button click) (send tab-panel change-children (lambda (x) (list dest add_button delete_button adjust_button)))
                                     (util:send GUI 'set-current-listbox dest)
                                     ;  (util:send GUI 'set-current-steward (util:send GUI 'get-steward "Bathroom"))
                                     (update-list-box-spectype dest name (util:send majordomo 'get-list_w_stewards) majordomo))])
                    (buttongenerator-tab2 (cdr lst) (cdr destination) tab)))))
    
    
    
    
    
    (buttongenerator-tab2 listdevices (list list-box-lightswitch list-box-tempsensor) tab2)
    
    
    (define (dispatch message)
      (case message
        ((get-tab2) get-tab2)
        (else (error 'tab2 "unknown message ~a" message))))
    
    dispatch))

