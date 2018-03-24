Aufgabe: Sonnensystem
=====================
Ziel dieser Aufgabe ist es das Sonnensystem zu simulieren. Die Ausgangsdaten sind dieser Tabelle zu entnehmen:

| Name    | Masse   | Position X | Position Y | Geschwindigkeit X | Geschwindigkeit Y |
|---------|---------|------------|------------|-------------------|-------------------|
| Sonne   | 1.00    | 0.00       | 0.00       | 0.00              | 0.00              |
| Merkur  | 1.20e-7 | 0.39       | 0.00       | 0.00              | 9.96              |
| Venus   | 2.40e-6 | 0.72       | 0.00       | 0.00              | 7.36              |
| Erde    | 1.50e-6 | 1.00       | 0.00       | 0.00              | 6.26              |
| Mars    | 3.30e-7 | 1.52       | 0.00       | 0.00              | 5.06              |
| Jupiter | 9.50e-4 | 5.20       | 0.00       | 0.00              | 2.75              |
| Saturn  | 2.75e-4 | 9.54       | 0.00       | 0.00              | 2.04              |
| Uranus  | 4.40e-5 | 19.19      | 0.00       | 0.00              | 1.43              |
| Neptun  | 5.10e-5 | 30.06      | 0.00       | 0.00              | 1.14              |
| Pluto   | 5.60e-9 | 39.53      | 0.00       | 0.00              | 0.99              |

Dabei gilt für die Einheiten:
 * Distanzen sind abei in AU (Astronimische Einheiten, Abstand Erde-Sonne) angegeben. 
 * Massen sind normiert auf die Masse der Sonne (2e30kg).
 * Geschwindigkeiten sind in AU pro Jahr gegeben.

Als (Kommandozeilen)Parameter soll folgendes in die Simulation eingehen:

 * Zeitschrittgröße
 * Anzahl Schritte
 * zu verwendender Algorithmus (s.u.)

Jeder berechnete Zeitschritt soll von dem Programm ausgegeben werden (entweder als Datei oder ins Terminal). Die Ausgabe sollte in folgendem Format erfolgen:

    Sonne_x; Sonne_y; Sonne_vx; Sonne_vy; Merkur_x; Merkur_y; Merkur_vx; Merkur_vy; ...; Pluto_vy;

Die Reihenfolge der Planeten richtet sich nach der obigen Tabelle. Es wird ein Tool geben mit welchem man die  ausgegebenen Werte am Ende graphisch anzeigen kann. Für bestimmte Teile der Aufgabe mag es nötig sein weitere Informationen auszugeben.
Die Zeitschrittgröße ist frei zu wählen, es sollten auch Zeiträume größer als 50 Jahre betrachtet werden!

Algorithmen
-----------
Die einzelnen Algorithmen sind relativ einfach zu implementieren, daher ist es ein Ziel der Aufgabe die gängigsten Methoden zu vergleichen. In diesem Fall heisst das:

 * Euler-Verfahren [Wikipedia](https://de.wikipedia.org/wiki/Explizites_Euler-Verfahren)
 * Runge-Kutta-Verfahren (4. Ordnung), [Wikipedia](https://de.wikipedia.org/wiki/Klassisches_Runge-Kutta-Verfahren)
 * Velocity-Verlet-Verfahren. [Wikipedia](https://en.wikipedia.org/wiki/Verlet_integration#Velocity_Verlet)

Natürlich können auch andere Verfahren verwendet werden, je nach Zeit und Interesse :)

Aufgabe
-------
Nachdem man eine laufende Simulation hat sind folgende Fragen zu beantworten:

 1. Bild über alle Körper die Summe gesamten kinetischen Energie (`1/2 * $Masse * $Geschwindigkeit^2`). Was sagt diese Größe aus und warum ist sie bei den verschiedenen Verfahren unterschiedlich? Wie sollte sie ideal aussehen? _Hinweis: Größe über längere Zeitraum plotten!_
 2. Lass die Simulation in der Zeit vorwärts laufen und drehe danach die Zeit um und messe wie weit du bei "Zeitschritt null" von den ursprünglichen Werten entfernt bist. Für welche Methode ist dieser Wert am besten?
 3. Berechne neben der kinetischen Energie auch die Gesamtenergie des Systems und plotte sie.
 4. Wird die Simulation immer genauer, je kleiner die Zeitschritte werden? Warum?
