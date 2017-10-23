Buchstabenanalyse
=================

Aufgabe
-------
Ein String wird _gültig_ genannt, wenn *eines* der beiden Kriterien gilt:

 * Alle in dem String auftretenden Buchstaben kommen gleich oft vor.
 * Nach dem Entfernen eines einzelnen Buchstabens aus dem String kommen alle Buchstaben gleich oft vor.

Beispiele:
----------
aabbcc   => gültig
baacdd   => ungültig
aabbccc  => gültig
aabbcccc => ungültig

Als Eingabe bekommt das Programm eine einzelne Zeilen mit dem String.
Die Ausgabe soll "JA" sein, wenn der String gültig ist, ansonsten "NEIN".

Hinweise
--------
 * Die maximale Stringlänge ist 10^5 Zeichen.
 * Die maximale Ausführungsdauer sollte unter 3 Sekunden liegen.
