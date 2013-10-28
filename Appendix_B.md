# Appendix B: User interface gebruikers scenario’s

Interactie voorbeelden
======================


Doel                                            | Mogelijke Sequentie                                                           | Uitgevoerde actie           |
------------------------------------------------| ------------------------------------------------------------------------------|-----------------|
Toevoegen sensor                                | Selecteer type sensor & Selecteer protocol sensor & Ingeven naam & Ingeven unieke identifier (bvb.: mac adres of sensor id)| Sensor wordt toegevoegd in de lijst van sensoren|
Visualiseren energieverbruik                                   | Selecteer sensor in een lijst (of grondplan)   | De geschiedenis van verbruik wordt opgevraagd uit de persistentie laag en gevisualiseerd in een plot |
Aanzetten van een lamp                     | Selecteer de lamp in een lijst (of grondplan), klik op een ‘aan’ knop om de lamp aan te schakelen.                                          |De callback vertaalt de actie naar de juiste commando’s naar het domotica raamwerk. Deze commando’s worden dan doorgestuurd naar de correcte lamp. |
Visualiseren actieve actuatoren                                            | Klik op het overzicht om gedetailleerde informatie per sensor in een tabel te kunnen bekijken. Je kan op de kolommen klikken om te sorteren.    | Een lijst met alle informatie van alle sensoren en actuatoren wordt getoond. Actuatoren (en/of sensoren) die actief zijn krijgen een ander kleur. |
Bekijken van de acties  |  Klik op het log venster. |  De laatste 300 acties van het domotica systeem moeten in text (of grafisch) formaat getoond kunnen worden. Bvb.: “Sensor-1 [Type: Lichtsensor] werd toegevoegd”, “Lamp-2 werd uitgeschakeld”. |






OPTIONELE Interactie voorbeelden
================================
Doel                                                              | Mogelijke Sequentie                                          | test                                                      |
------------------------------------------------------------------| -------------------------------------------------------------|-----------------
Intelligente lamp: automatische activatie bij lage lightintensiteit | Maak een nieuwe regel aan en vul de minimale waarde van de lichtsensor in. Een timeout waarde (bvb 5 minuten) en de koppeling met de lamp & lichtsensor moet ook gekozen worden. | De lamp schakelt automatisch aan en uit op basis van de lichtintensiteit (zonder acties van de gebruiker). | Laatste week van de eerste zittijd                           |
In/Uitschakelen van een lamp op basis van tijd                      | Maak een nieuwe geplande taak aan. Vul het start en eindstip in. Koppel de taak aan een (of meerdere) lamp(en).                                         | De lamp schakelt automatisch aan en uit op basis van tijd (zonder acties van de gebruiker).
