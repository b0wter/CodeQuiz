Monopoly
========

Aufgabe
-------
Gegeben ist ein übliches Monopolyspiel. Es wird, anstelle eines sechseitigen Würfels mit zwei vierseitigen Würfeln gespielt. Andere Spieler müssen nicht berücksichtig werden. Die einzige Instanz die Ereignisse auslösen kann ist der Spieler selbst. Alle Spieler starten auf dem GO Feld. 

Die Felder des Spielfelds sind folgendermaßen definiert:

	 GO  | A1  | CC1 | A2  | T1  | R1  | B1  | CH1 | B2  | B3  | JAIL
	 H2  |                                                     | C1
	 T2  |                                                     | U1
	 H1  |                                                     | C2
	 CH3 |                                                     | C3
	 R4  |                                                     | R2
	 G3  |                                                     | D1
	 CC3 |                                                     | CC2
	 G2  |                                                     | D2
	 G1  |                                                     | D3
	 G2J | F3  | U2  | F2  | F1  | R3  | E3  | E2  | CH2 | E1  | FP

Ziel ist es zu ermitteln welches die drei meistbesuchten Felder sind. Die Wahrscheinlichkeiten der Felder sind nicht gleichverteilt weil:
 * Das G2J-Feld (go to jail) sorgt dafür, dass die Spielfigur auf das JAIL-Feld verlegt wird.
 * Wenn der Spieler drei Pasche hintereinander würfelt bewegt er sich nicht die Gesamtaugenzahl nach vorn, sondern direkt ins Gefängnis.
 * Wenn das CC Feld betreten wird ist eine Gemeinschaftskarte zu ziehen. Der Stapel beinhaltet 16 Karten von denen zwei Relevanz haben:
   * Rücke vor zum Start.
   * Gehe ins Gefängnis.
 * Wenn das CH-Feld betreten wird ist eine Zufallskarte zu ziehen. Der Stapel beinhaltet 16 Karten von denen 10 Relevanz haben:
   * Rüke vor zum Start.
   * Gehe ins Gefängnis.
   * Gehe nach C1.
   * Gehe nach E3.
   * Gehe nach H2.
   * Gehe nach R1.
   * Gehe zum nächsten R (Bahnhof).
   * Gehe zum nächsten R.
   * Gehe zum nächsten U.
   * Gehe drei Felder zurück.

Die Felder können von 0 bis 39 nummeriert werden. Ausgegeben werden sollen die Indizes der drei meistbesuchten Felder, ohne Trennzeichen. Z.B.: Jail, E3, Go => 102400.

Hinweise
--------
 * Die Kartenstapel für die CC/CH-Felder wird zu Beginn des Spiels gemischt.
 * Zu Zählen ist immer nur das finale Feld einer Bewegung. D.h. beispielsweise, dass das G2J-Feld eine Wahrscheinlichkeit von 0% hat!
 * Wenn man mit einem sechseitigen Würfel spielt sind die drei wahrscheinlichsten Felder: JAIL, E3 & GO. Dies kann zum Testen des eigenen Modells verwendet werden.
 * Es gibt für das JAIL-Feld keine Unterscheidung zwischen Besuchern und Insassen.
 * Die Regel, dass ein Pasch nötig ist um das Gefängnis zu verlassen, wird ignoriert.
