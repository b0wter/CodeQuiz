# CodeQuiz

Wochenquiz mit verschiedenen Rätsel und Implementationsaufagen. 
Jede Woche wird ein neuer Ordner mit der Wochenaufgabe erstellt. 
Diese Ordner enthalten jeweils eine Beschreibungsdatei und eine Datei mit einem, oder mehreren Testfällen und deren Ergebnissen.

Lösungen werden per Pull-Request eingereicht und sollten in einem Ordner mit dem eigenen Namen in dem jeweiligen Wochenordner erstellt werden.

Die Binaries werden als Submodule gehandhabt und sollen an folgendes Repository gepusht werden: https://github.com/b0wter/CodeQuizBinaries

Download des Testrunner unter: https://github.com/b0wter/CodeQuiz/releases . Der Testrunner läuft mit GUI unter Windows, und im Terminal unter Linux/Mac/Win (.net core benötigt).

Der Konsolenclient kann auf zwei Arten verwendet werden:

1. Die Verwendung einer Konfigurationsdatei welche mit dem Argument '-c' übergeben wird. Die Datei muss dann folgendermaßen aufgebaut sein:

		{
		    "command": "$Command_To_Run"
		    "argument": "$Argument_To_Add_To_Command" (optional)
		    "test": "$Filename_of_jsonTest-File"
		}

2. Die Argumente können auch direkt übergeben werden:
	* -c Befehl der ausgeführt werden soll inkl. Pfad, muss gequoted sein
	* -a optionales Argument welches dem zu startenden Prozess mitgegeben wird
	* -t Pfad zur Json-Test-Datei, muss gequoted sein
