#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define FILENAME "./../Pyramide.txt"				// Pfad zur einzulesenden Datei
#define MAX(a, b) (((a) > (b)) ? (a) : (b))		// Definition der MAX()-Funktion


int main(void) {
	// Datei einlesen
	FILE *fh;
	fh = fopen(FILENAME, "r");
	if (fh == NULL) {
		printf("Fehler: Die Datei konnte nicht geöffnet werden.\n");
		return -1;
	}

	// Anzahl der Elemente feststellen, um dynamisch Speicher allokieren zu können
	unsigned int items = 0;
	unsigned int *val;
	do {
		fscanf(fh, "%u", val);
		items++;
	} while (!feof(fh));

	if (items == 0) {
		printf("Fehler: Die Datei scheint keine Zahlenwerte zu beinhalten.\n");
		return -1;
	}

	// entsprechend großen Speicher reservieren
	unsigned int *pyramid = (unsigned int *) calloc(items, sizeof(*val));
	if (pyramid == NULL) {
		printf("Fehler: Es ist nicht ausreichend Speicherplatz allokierbar.\n");
		return -1;
	}

	// Datei nochmals auslesen und dabei alle Werte in einem Array (pyramid) festhalten
	rewind(fh);
	int index = 0;
	do {
		fscanf(fh, "%u", &pyramid[index++]);
	} while (!feof(fh));
	fclose(fh);

	unsigned int rows = (sqrt(8*items + 1) - 1) / 2;	// Anzahl der Zeilen anhand der Anzahl der Elemente berechnen
	printf("Die Pyramide enthält %u Elemente in %u Zeilen.\n", items, rows);





	// größtmögliche Summe eines zusammenhängenden Pfades berechnen
	int offset = rows - 1;	// Offset, der bei Addition zum Index genau die Zahl darunter (=links) liefert
	int col = offset - 1;	// Spalten-Index
	for (int i = items - rows - 1; i >= 0; i--) {	// Start: letzte Zahl in der vorletzten Zeile; Ende: erste/oberste Zahl
	 	pyramid[i] += MAX(pyramid[i + offset], pyramid[i + offset + 1]); // Summe links oder rechts größer?
	 	if (col-- <= 0) {
	 		col = (--offset) - 1;
	 	}
	}

	// Ergebnis ausgeben und Speicher wieder freigeben
	printf("Die größtmögliche Summe ist %u.\n", pyramid[0]);
	free(pyramid);

	return 0;
}
