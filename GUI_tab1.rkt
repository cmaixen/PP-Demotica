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

; Beschrijving: ADT GUI_tab1 ; Argumenten: Majordomo-object, GUI-object
;               Het ADT is een voorgeschreven panel met een bepaalde workflow
;
; Output: gui_tab1 object
;
; Messages:
;
; - get-tab1: argumenten: geen
;          output: panel%
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(provide make_GUI_tab1)


(define (make_GUI_tab1 majordomo GUI)
  (let ((tab-panel (util:send GUI 'get-tabpanel)))
    
    ;ACCESSOREN
    ;----------
    
    (define (get-tab1)
      tab1)
    
    
    ;SETUP
    ;-----
    
    ;verticaal paneel in tabpaneel
    (define tab1 (new vertical-panel%
                      [parent tab-panel]
                      [spacing standard_spacing]	 
                      [alignment (list 'center 'center)]
                      ))
    
    
    ;"add"-knop toevoegen aan paneel dat add-dialoog doet verschijnen
    (define add_button (new button% [parent tab-panel ] [label "Add"] [callback (lambda (button click) (send add-dialog show #t))]))
    
    
    ;"delete"-knop toevoegen aan paneel dat een toestel kan verwijdere, triggert ook systeem om dit uit te voeren en update listbox
    (define delete_button (new button% [parent tab-panel] [label "Delete"] [callback (lambda (button click)
                                                                                       (util:send (util:send majordomo 'get-current-steward) 'delete_device (send (util:send GUI 'get-current-listbox) get-string-selection))
                                                                                       (update-list-box (util:send GUI 'get-current-listbox) (util:send majordomo 'get-current-steward)))]))
    ;genereren van de knoppen voor iedere kamer
    ;aan ieder knop is gelinkt aan een listbox die alle toestel van in die kamer bevat
    ;dit gedrag is kenmerkend voor tab1
    
    (define (buttongenerator lst destination tab)
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
                         [callback (lambda (button click) (send tab-panel change-children (lambda (x) (list dest add_button delete_button)))
                                     (util:send GUI 'set-current-listbox dest)
                                     (util:send majordomo 'set-current-steward (util:send majordomo 'get-steward name))
                                     (update-list-box  (util:send GUI 'get-current-listbox) (util:send majordomo 'get-current-steward)))])
                    (buttongenerator (cdr lst) (cdr destination) tab)))))
    
    
    
    
    ;de verschillende list-boxen voor iedere kamer
    
    ;listbox voor de badkamer
    (define list-box-bathroom (list-box-tab "Bathroom" (list "Name" "Type" "Status"  "Serial" "Mesurement" ) tab-panel))
    
    ;listbox voor de slaapkamer
    (define list-box-bedroom (list-box-tab "Bedroom" (list "Name"  "Type" "Status" "Serial" "Mesurement" ) tab-panel))
    
    ;listbox voor de leefruimte
    (define list-box-livingroom (list-box-tab "Livingroom" (list "Name" "Status" "Type" "Serial" "Mesurement" ) tab-panel))
    
    
   ;ADD-Dialog
   ;----------
    
    ;dialoog   
    (define add-dialog (new dialog% [parent (util:send GUI 'get-mainframe)] 
                            [label "Add-Dialog"]))
    
    
    ;verticaal paneel voor dialoog
    (define add-dialog_panel (new vertical-panel% [parent add-dialog]
                                  [min-width min_width_dialog]
                                  [alignment '(center center)]
                                  [min-height min_height_dialog]))
    
    ;toevoegen van de tekstvelden aan dialoog
    
    ;tekstveld voor naam
    (define name_device (new text-field% [parent add-dialog_panel] [label "Name"]))
    
    ;tekstveld voor type
    (define type (new choice% [parent add-dialog_panel] [label "Type"] [choices (util:send majordomo 'get-listdevices)] ))
    
    ;tekstveld voor serial
    (define serial (new text-field% [parent add-dialog_panel] [label "Serialnumber"]))
    
    ;tekstveld voor com-adres
    (define com-adress (new text-field% [parent add-dialog_panel] [label "COM-adres"]))
    
    ;"add"-knop in het "add"-dialoog dat het systeem triggert om ingegeven toestel toe te voegen en update listbox
    (define add_button-dialog (new button% [parent add-dialog ] [label "Add"] [callback (lambda (button click) (send add-dialog show #f)
                                                                                          (util:send (util:send majordomo 'get-current-steward) 'add-device
                                                                                                     (determine-choice (util:send majordomo 'get-listdevices) (send type get-selection))
                                                                                                     (send (send name_device get-editor) get-text)
                                                                                                     (send (send serial get-editor) get-text)
                                                                                                     (send (send com-adress get-editor) get-text))
                                                                                          (update-list-box (util:send GUI 'get-current-listbox)  (util:send majordomo 'get-current-steward)))]))
    
   
    
    ;genereren van knoppen
    
    (buttongenerator (util:send majordomo 'get-listrooms) (list list-box-livingroom list-box-bathroom list-box-bedroom) tab1)
    
    
    (define (dispatch message)
      (case message 
        ((get-tab1) get-tab1)
        (else (error 'tab1 "unknown message ~a" message))))
    
    
    dispatch
    ))