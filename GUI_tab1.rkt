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

; Beschrijving: ADT GUI_tab3 ; Argumenten: Majordomo-object, GUI-object
;               Het ADT is een voorgeschreven panel met een bepaalde workflow
;
; Output: gui_tab3 object
;
; Messages:
;
; - get-tab3: argumenten: geen
;          output: panel%
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(provide make_GUI_tab3)


(define (make_GUI_tab3 majordomo GUI logsystem)
  (let ((tab-panel (util:send GUI 'get-tabpanel)))
    
    ;ACCESSOREN
    ;----------
    
    (define (get-tab3)
      tab3)
    
    
    ;SETUP
    ;-----
    
    ;verticaal paneel in tabpaneel
    (define tab3 (new vertical-panel%
                      [parent tab-panel]
                      [spacing standard_spacing]	 
                      [alignment (list 'center 'center)]
                      ))
    
    
    

    
    
    (define (buttongenerator-stewards lst tab)
      (cond ((empty? lst)
          'done)
            (else (let* ((name (car lst))
                         
                         (linked-tab (new vertical-panel%
                                          [parent tab-panel]
                                          [spacing standard_spacing]	 
                                          [alignment (list 'center 'center)]
                                          ))
                         (message     (new message% [parent linked-tab]
                                          [label "You can add a device here:"]))
                         (name_device (new text-field% [parent linked-tab] [label "Name"]))
                         (com-adress  (new text-field% [parent linked-tab] [label "COM-adres"])))
                    
                     (new button% [parent linked-tab ] [label "Add"] [callback (lambda (button click) (new message% [parent linked-tab]   [label (util:send majordomo 'add-device
                                                                                                                                  (send (send name_device get-editor) get-text)
                                                                                                                        (list->vector    (map char->integer  (string->list (split-string-in-new-string (send (send com-adress get-editor) get-text) #\space)))))]
                                                                                                                         ))])
                    
                    (new button% [parent tab] [label name]
                         [vert-margin standaardmargin_button]	
                         [horiz-margin standaardmargin_button]	 
                         [min-width standaardmargin_button]	 
                         [min-height standaardmargin_button]
                         [callback (lambda (button click)
                                     (send tab-panel change-children (lambda (x) (list linked-tab)))
                                     (util:send majordomo 'set-current-steward name)
                                
                                     (if (not (util:send majordomo 'check-authorized))
                                           (begin (send linked-tab change-children (lambda (x)  '()))
                                           (new message% [parent linked-tab] 
                   [label (make-object bitmap% "no-permission.png" 'png/alpha)]
                   [stretchable-width #t]	 
                   [stretchable-height #t]	 
                   [auto-resize #t]
                   ))
                                    'oke)
                                     )] )
                    (buttongenerator-stewards (cdr lst) tab)))))
    
    
    
    
    ;ADD-Dialog
    ;----------
     
    
    
    ;genereren van knoppen
    ;aanmaken vna panel
    (buttongenerator-stewards (util:send majordomo 'get-listrooms) tab3)
    
    
    (define (dispatch message)
      (case message 
        ((get-tab3) get-tab3)
        (else (error 'tab3 "unknown message ~a" message))))
    
    
    dispatch
    ))