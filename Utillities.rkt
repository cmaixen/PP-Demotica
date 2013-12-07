#lang racket


(#%provide send
           list_size
           list_neutralizer
           reverse-list)

;Hulpprocedures
;--------------
; Zend een boodschap (met eventuele parameters) naar een object georiëenteerd gebaseerde implementatie van een ADT.

(define (send object message . parameters)
  (let ((procedure (object message))) 
    ;Er van uit gaande dat het object zijn dispatcher altijd een procedure teruggeeft.
    (apply procedure parameters)))



;Berekent de lengte van een lijst

(define (list_size lst)
  
  (define (loop lst counter)
    
    (cond  ((empty? lst) 0)
           ((empty? (cdr lst)) counter )
       
           (else  (loop (cdr lst) (+ counter 1)))))
  (loop lst 1))


;zorgt er voor dat de resultaat lijst van de query met daarin vectors  ongevormt wordt  naar een lijst met daarin het gevraagde elementen
(define (list_neutralizer query el)
  
      (define (loop lst neut_lst)
        (if (empty? lst) 
            neut_lst
            (let* ((vector (car lst))
                   (name (vector-ref vector el)))
              (loop (cdr lst) (cons name neut_lst)))))
        (loop query '()))

(define (reverse-list lst)
  (define (loop lstt res)
     (if (empty? lstt)
         res
         (loop (cdr lstt) (cons (car lstt) res))))
  (loop lst '()))


