#lang racket
(require racket/gui/base)
(require racket/draw)
(require "GUI_utillities.rkt")
(require "Constants.rkt")
(require racket/gui/base)
(#%require (prefix util: "Utillities.rkt"))
(provide make_GUI_tab1-3-multisensor)


(define (make_GUI_tab1-3-multisensor majordomo GUI)
  (let ((tab-panel (util:send GUI 'get-tabpanel))
        (mainframe (util:send GUI 'get-mainframe)))
    
    
    
    ;ACCESSOREN
    ;----------
    
    (define (get-tab)
      tab1-3)

    
    ;hoofdcontainer van de tab
    (define tab1-3 (new vertical-panel%
                        [parent tab-panel]
                        [alignment (list 'center 'center)]))
    
    
    ;opbouw van de tab
    
    ;---------------------------------------
    ;                |                     |
    ;                |  device_info_panel  |
    ;   image_panel  |---------------------|
    ;                |  change_panel       |
    ;                |                     |
    ;--------------------------------------|
    ;                                      |
    ;           editor_panel               |
    ;                                      |
    ;---------------------------------------
    
    
    ;container bovenste helft
    (define tab_info (new horizontal-panel%
                          [parent tab1-3]	 
                          [alignment (list 'center 'top)]
                          ))
    
    ;editor voor de onderste helft van het scherm
    (new editor-canvas%	 
         [parent tab1-3]
         )
    
    
    ;linkerbovenhoek van het canvas 
    (define image_tab (new vertical-panel%
                           [parent tab_info]
                           [spacing 0]	 
                           [style (list 'border)]
                           [vert-margin 0]
                           [alignment (list 'center 'top)]
                           ))
    
    
    ;Afbeelding voor in linkerbovenhoek    
    (define l (new message% [parent image_tab] 
                   [label (make-object bitmap% "multisensor.png" 'png/alpha)]
                   [stretchable-width #t]	 
                   [stretchable-height #t]	 
                   [auto-resize #t]
                   )) 
    
    
    ;rechterbovenhoek van het canvas
    (define device_info_canvas (new vertical-panel%
                                    [parent tab_info]
                                    [spacing 0]                           
                                    [alignment (list 'left 'top)]
                                    ))
    
    ;bovenste helft van linkerbovenhoek
    (define device_info_canvas_info (new vertical-panel%
                                         [parent device_info_canvas]
                                         [spacing 0]
                                         [style (list 'border)]
                                         
                                         [alignment (list 'left 'top)]
                                         ))
    
    
    ;afbeelden van de info in het infovenster
    (define (create_info_panel  panel list_of_strings)
      (define (loop lst)
        (  if (empty? lst)
              'done
              (let ((message (car lst)))
                (new message% [parent panel] 
                     [label message]
                     )
                (loop (cdr lst)))))
      (loop list_of_strings))
    
    
    
    ;onderhleft van de rechterbovenhoek
    (define change_panel (new vertical-panel%
                              [parent device_info_canvas]
                              [alignment (list 'left 'center)]
                              [style (list 'border)]
                              ))
    
    ;geeft een status box weer die het toestel aan of uitzet
    (define statusbox (new check-box%	 
                           [label "Turn Device Off"]	 
                           [parent change_panel]
                           [vert-margin 20]
                           
                           [value #f]))
    
    ;paneel dat de slider en de choices bij elkaar houd
    (define setter_panel (new horizontal-panel%
                              [parent change_panel]
                              [spacing 0]
                              
                              [vert-margin 20]
                              [alignment (list 'left 'center)]
                              ))

        
    ;return the right slider
    (define (get-right-slider choice)
     ( cond ((equal? choice "TXT")
            TXT-slider)
      ((equal? choice "MSI")
       MSI-slider)
      ((equal? choice "LOTEM")
       LOTEM-slider)
      ((equal? choice "HITTEM")
       HITTEM-slider)
      ((equal? choice "LOBRI")
       LOBRI-slider)
   (else HIBRI-slider)))
    
    
    ;stelt een choicebox op met daarin al de keuzes
    
(define choice
      (new choice%  (label "Choice")
           (parent setter_panel)
           (choices (list "TXT" "MSI" "LOTEM" "HITTEM" "LOBRI" "HIBRI"))
           [callback (lambda (c e) (send setter_panel change-children (lambda (x) (list choice (get-right-slider c)))))]))
    
    
 
    
    ;verschillende sliders
    ;---------------------
    
    (define TXT-slider (new slider%
                             (label "")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    
    (define MSI-slider (new slider%
                             (label "seconds")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    (define LOTEM-slider (new slider%
                             (label "°c")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    (define HITTEM-slider (new slider%
                             (label "°c")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 100)
                             (init-value 42)))
    
    (define LOBRI-slider (new slider%
                             (label "Lux")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 1)
                             (init-value 0)))
    
    (define HIBRI-slider (new slider%
                             (label "Lux")
                             (parent setter_panel)
                             (min-value 0)
                             (max-value 1)
                             (init-value 0)))
    
    
    ;enkel choice box instellen, pas bij selectie komt de slider  
    (send setter_panel change-children (lambda (x) (list choice)))
    
    
    (create_info_panel device_info_canvas_info (list "POW=on" "FREQ=49.8125Hz" "VRMS=227V" "IRMS=1988mA" "LOAD=443W" "WORK=0.046kWh"))
    
    



    (define (dispatch message)
      (case message 
        ((get-tab) get-tab)
        (else (error 'Powerplug-panel "unknown message ~a" message))))
    dispatch
    ))












