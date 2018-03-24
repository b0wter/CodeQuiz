Aufgabe: Game of Life
=====================
Die Aufgabe für diese Woche ist eine Implementation des Game of Life. Das Spiel wird auf einem Gitter mit Zellen gespielt, welche entweder tot, oder lebendig sind.
Der Standard ist, dass jede Zelle erst einmal als tot angenommen wird. Von einem Zeitpunkt zum nächsten bewegt man sich, indem man für jede Zelle (gleichzeitig!) den nächsten Status nach folgenden Regeln bestimmt:

 * Jede lebendige Zelle mit _weniger als zwei_ lebenden Nachbarn stirbt an Einsamkeit.
 * Jede lebendige Zelle mit _zwei, oder drei_ lebenden Nachbarn lebt auch im nächsten Zeitschritt.
 * Jede lebendige Zelle mit _vier, oder mehr_ lebenden Nachbarn stirbt an Überbevölkerung.
 * Jede tote Zelle mit _genau drei_ lebendigen Nachbarn lebt im nächsten Schritt.

Jede Zelle hat genau acht Nachbarn (diagonal).

Beispiele
---------
Es gibt verschiedene Muster welche interessante Eigenschaften zeigen. So gibt es beispielsweise stabile Muster, welche sich nicht verändern, wie z.B.:

	 1234
	1░░░░
	2░██░
	3░██░
	4░░░░

oder welche mit einer gewissen Periode oszillieren:

	 12345    12345    12345
	1░░░░░    ░░░░░    ░░░░░
	2░░░░░    ░░█░░    ░░░░░
	3░███░ => ░░█░░ => ░███░
	4░░░░░    ░░█░░    ░░░░░
	5░░░░░    ░░░░░    ░░░░░

Hinweise
--------
 * Wenn man auf einem zweidimensionalen Array arbeitet sollte man die Ränder des Gitter mit Bedacht wählen. Üblicherweise werden sie als "permanent" tot angenommen.
 * Eine ausführliche, englischsprachige Anleitung findet sich auch hier (viele Beispiele und Videos): https://www.theguardian.com/science/alexs-adventures-in-numberland/2014/dec/15/the-game-of-life-a-beginners-guide

_Aufgaben vorgesehen für Start am 25.03.2018._
