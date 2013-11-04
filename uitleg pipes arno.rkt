#lang racket

;Pipes
;Pipes zijn koppels input output ports, je leest er de expressies uit die je er naar schrijft
;Vermits read enkel s expressie leest is het belangrijk dat alles wat je wegschrijft dit ook is
;Let er ook op dat je evenveel (of meer) wegschrijft dan je leest, anders gaat de read oneindig
;lang op een antwoord wachten
(let-values ([(input output) (make-pipe)])
  (write '(dit moet een s expressie (aka datum) zijn) output)
  (read input)) ;geeft dezelfde s expressie terug
 
;Voor het project kan je pipes gebruiken aan de hand van costum input ports
;Hieronder een voorbeeld
(define my-costum-input-port
  (make-input-port
    'naam
    ;hieronder moet een procedure zijn die eof, bytes of een input-port terug geef
    (lambda (ignore)
      (let-values (((in out) (make-pipe))) ; maak een lokale pipe waarin je de antwoorden schrijft
        ;voor het project voer je hier dus de procedure uit die een antwoord genereerd
        ;deze schrijf je dan weg als een s expressie 
        (write '(1 ste exp) out) 
        (write '(2 de exp) out)
        in));je geeft de input port van de lokale pipe terug
    #f ;voorkomt dat er gepeekt kan worden (niet relevant voor het project)
    (lambda () 'closed))) ; procedure die opgeroepen wordt wanneer je de poort sluit
    
;vb gebruik
(read my-costum-input-port) ;->1ste exp
(read my-costum-input-port) ;->2de exp
(read my-costum-input-port) ;->blijft runnen omdat er geen input meer is in de lokale pipe
    
;Vermits we simuleren is het niet nodig dat je een costum output port schrijft, het nadeel is dan wel
;dat het uitvoeren van commandos ( '(PUT (POW ON)) ) pas gebeurt bij het readen