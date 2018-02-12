Summe in Pyramide
=================

Aufgabe
-------
Gegeben ist eine Pyramide von Zahlen. Es soll die höchste Summe gefunden werden welche durch das Durchlaufen der Pyramide erreicht werden kann.
Beim Durchlaufen darf nicht in eine beliebige Zahl der nächsten Zeile gewechselt werden, es muss entweder das Element "unten links", oder "unten rechts" gewählt werden.
Als Beispiel ist die folgende Pyramide gegeben:

	   3
	  7 1
	 2 4 6
	8 5 9 3

Mögliche Wege sind: 3 -> 7 -> 2 ->8, 3 -> 7 -> 4 -> 5, aber nicht 3 -> 7 -> 6 -> 8.
Der Weg mit der höchsten Summe ist: 3 -> 7 -> 4 -> 9 = 23.
Die Ausgabe der Summe erfolgt einfach als Zahl. Die Pyramide wird als Textdatei, in folgendem Format geliefert:

01
02 03
04 05 06
07 08 09 10
...

Hinweise
--------
* Alle Zahlen sind maximal zweistellig. Wenn sie nicht zweistellig sind werden sie mit einer vorrangestellten 0 aufgefüllt.