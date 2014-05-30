#lang racket
(require racket/gui/base)
(require racket/draw)


(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(#%require "GUI_tab1-3-mulitisensor.rkt")
(#%require "GUI_tab1-3-powerplug.rkt")
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(provide make_GUI_tab1-1)


(define (make_GUI_tab1-1 majordomo GUI)
  (let ((tab-panel (util:send GUI 'get-tabpanel))
        (mainframe (util:send GUI 'get-mainframe)))
    
    
    
    ;ACCESSOREN
    ;----------
        ;eerste venster dat je te zien krijgt bij het selecteren tab1
    (define tab1-1 (new vertical-panel%
                        [parent tab-panel]
                        [spacing standard_spacing]	 
                        [alignment (list 'center 'center)]
                        ))  
     
    (define (get-tab1)
      tab1-1)
    
  
    
    ;2de venster dat je te zien krijgt na het selecteren van een kamer
    (define tab1-2 (new vertical-panel%
                        [parent tab-panel]
                        [spacing standard_spacing]	 
                        [alignment (list 'center 'center)]
                        ))

    
       ;wanneer er toestellen worden toegevoegd moeten deze uitgebreid worden.
  (define (decide-type name)
    (let ((type (util:send majordomo 'get-type name)))
      (newline)
      (display "found type :")
      (display type)
      (newline)
    (if (equal? type "ZBS-110") ;komt overeen met het product id van de powerplug
       (util:send (make_GUI_tab1-3-powerplug majordomo dispatch name) 'get-tab)
      (util:send (make_GUI_tab1-3-multisensor majordomo dispatch name) 'get-tab))))
    
 ;genereren van juiste linking van knoppen van toestel naar toestel overzicht
    (define (buttongenerator-devices lst tab)
      (cond ( (empty? lst)
              'done)
            (else (let* ((name (car lst))
                        (type-panel (decide-type name)))
                    (new button% 
                         [parent tab] 
                         [label name]
                         [vert-margin standaardmargin_button]	
                         [horiz-margin standaardmargin_button]	 
                         [min-width standaardmargin_button]	 
                         [min-height standaardmargin_button]
                         [callback (lambda (button click)
                                     ;zet huidig toestel op het geselecteerde toestel
                                     (util:send majordomo 'set-current-device name)
                                     (send tab-panel change-children (lambda (x) (list type-panel))))])
                    (buttongenerator-devices (cdr lst) tab)))))
    
  
    
    ;genereren van de knoppen voor iedere kamer
    ;aan ieder knop is er event gelinkt dat wijst naar een canvas dat  alle toestel van in die kamer weergeeft 
    ;verder kan ja dan kijken naar de precieze specificaties van een toestel
    ;dit gedrag is kenmerkend voor tab1
    (define (buttongenerator-stewards lst tab)
      (cond ((empty? lst)
              (display "done"))
            (else (let* ((name (car lst))
                        (linked-tab (new vertical-panel%
                        [parent tab-panel]
                        [spacing standard_spacing]	 
                        [alignment (list 'center 'center)]
                        ))
                        (list_devices_steward (begin
                                               (util:send majordomo 'set-current-steward name)
                                               (util:send majordomo 'get_list_devicesnames_steward))))
                    
                    ;zet current-steward juist 
                     (util:send majordomo 'set-current-steward name)
                    ;voegt de nodige koppen aan aangemaakt paneel                
                    (buttongenerator-devices list_devices_steward linked-tab)
                                               
                    (new button% [parent tab] [label name]
                         [vert-margin standaardmargin_button]	
                         [horiz-margin standaardmargin_button]	 
                         [min-width standaardmargin_button]	 
                         [min-height standaardmargin_button]
                         [callback (lambda (button click)
                                     (send tab-panel change-children (lambda (x) (list linked-tab)))
                                     (util:send majordomo 'set-current-steward name))])
                    
                    (buttongenerator-stewards (cdr lst) tab)))))
  
    (define (get-tabpanel)
     tab-panel)
    
    (define (get-mainframe)
      mainframe)
    
  
      (define (dispatch message)
      (case message 
        ((get-tab1) get-tab1)
        ((get-tabpanel) get-tabpanel)
        ((get-mainframe) get-mainframe)
        (else (error 'tab1-1  "unknown message ~a" message))))
    
    
 
    ;genereren van de kamers
    (buttongenerator-stewards (util:send majordomo 'get-listrooms) tab1-1  )
    
    
    ;volgende pagina die je te zien krijgt bij het kiezen van een kamer
 ;   (buttongenerator-devices (util:send majordomo 'get_list_devicesnames_steward) tab1-2)
    
    
        (send tab-panel change-children (lambda (x) (list tab1-1)))
    
    

    dispatch
    ))










