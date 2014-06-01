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

; Beschrijving: ADT GUI_analysticstab ; Argumenten: Majordomo-object, GUI-object
;               Het ADT is een voorgeschreven panel met dat grafieken weergeeft over de toestellen
;
; Output: gui_graph object
;
; Messages:
;
; - get-analysticstab: argumenten: geen
;          output: panel%
;
; Commentaar: geen

#lang racket
(require racket/gui/base)
(require plot)
(#%require (prefix util: "Utillities.rkt"))
(#%require "GUI_utillities.rkt")
(#%require "Constants.rkt")
(provide make_GUI_analysticstab)


(define (make_GUI_analysticstab GUI majordomo)
  (let ((tab-panel (util:send GUI 'get-tabpanel))
    (graphlist '()))

  (define (get-analysticstab) analysticstab)
  
  (define analysticstab (new vertical-panel% 
                        [parent tab-panel]
                        [alignment (list 'center 'center)]
                        ))
  
  (define (generate-list-graph givenlst)
    (define (loop lst counter result)
      (if (= counter 0)
          result
          (loop (cdr lst) (- counter 1) (cons (vector counter (car lst) ) result))))
    (loop givenlst 24 '()))
  
    
    
  
  (define (graph-maker name linked_tab) (new canvas%	 
                     [parent linked_tab]
                     [horiz-margin 350]
                         [vert-margin 100]
                      	[min-width 400]	 
   	 	[min-height 400]
                     [paint-callback
                      (lambda (canvas dc)
                        (plot/dc (discrete-histogram
                                  (let* ((input (open-input-file (format "Sources/~a.txt" name)))
                                         
                                         (input (read input)))
                                    
                                    
                                    (generate-list-graph input))
                                  #:x-min 0	 	 	 	 
                                  #:x-max 24
                                  #:y-min 0	 	 	 	 
                                  #:y-max 120
                                  #:color 6 #:label "Average Uptime of devices in the room")
                                  #:y-label "Average Uptime (%)"
                                  #:x-label "How long ago (Hours)"
                                 dc
                                 
                                 1
                                 1
                                 300
                                 300
                                
                                 ))]))
  
  
   (define (buttongenerator-stewards lst tab)
      (cond ((empty? lst)
             (display "done"))
            (else (let* ((name (car lst))
                         
                         (linked-tab (new vertical-panel%
                                          [parent tab-panel]
                                          [spacing standard_spacing]	 
                                          [alignment (list 'center 'center)]
                                          ))
                         (graph  (graph-maker name linked-tab)))
                    
 

                        (new button% [parent tab] [label name]
                         [vert-margin standaardmargin_button]	
                         [horiz-margin standaardmargin_button]	 
                         [min-width standaardmargin_button]	 
                         [min-height standaardmargin_button]
                         [callback (lambda (button click)
                                   
                                     (send tab-panel change-children (lambda (x) (list linked-tab)))
                                     (util:send majordomo 'set-current-steward name)
                                       (set! graphlist (cons graph graphlist))
                                     (if (util:send majordomo 'offlinelimit? name)
                                         (new message% [parent linked-tab]
                                              [label (format "Following devices need to be checked: ~a" (util:send majordomo 'get_checkup_devices name))] )
                                         'done))])
                    
                    (buttongenerator-stewards (cdr lst) tab)))))
    
    ;aanmaken vna panel
 (buttongenerator-stewards (util:send majordomo 'get-listrooms) analysticstab)
  

  
(define (refresh_every_graph lst)
    (if (empty? lst)
        'done
        (begin (send (car lst) refresh)
               (refresh_every_graph (cdr lst)))))
  
  
  (thread (lambda ()  
            (do ([i 1 (+ i 1)])
              ((= i 0) (print "done"))
              
              (sleep 7)
              (print "Graph refresh")
              (newline)
              (refresh_every_graph graphlist))))
  
  
  (define (dispatch message)
    (case message 
      ((get-analysticstab) get-analysticstab)
      (else (error 'analysticstab "unknown message ~a" message))))
  
    (send tab-panel change-children (lambda (x) (list analysticstab)))
  dispatch
  ))




