Aufgabe: API Client
===================
Diese Woche gilt es eine Web-API abzufragen. Leider war der Entwickler der API ein Schülerpraktikant und hatte kaum Erfahrung. Daher ist es durchaus möglich, dass die API an einigen Stellen nicht konsequent und fehlerfrei ist. Ziel ist es trotzdem alle Daten abzurufen.

Ziel der Aufgabe ist es einen funktionsfähigen Client zu programmieren, welcher folgende Kriterien erfüllt:

 * Unter keinen Umständen sollte der Client abstürzen.
 * Der Client sollte die Möglichkeit bieten von der API alle Datentypen abzurufen.
 * Eine Ausgabe der Daten kann unformatiert über die Konsole erfolgen.
 * Der Client sollte in der Lage sein auch große Mengen an Daten abzurufen, d.h. 500+ Anfragen.
 * Am Ende sollen für jeden Typ alle Einträge mit allen Details ausgegeben werden. Dabei müssen die Children natürlich an die passenden Parents gehängt werden.

Es können folgende Daten von der Api abgerufen werden:

	class Foo
	{
		uint Id;		
		string Name;
		bool IsClosed;
		List<Id> Children;		// Liste mit den Ids der Kindelemnte (entweder A- oder BEntity)
		CEntity Details;
	}

	class AEntity
	{
		uint Id;
		string Name;
		int Max;
		int Min;
		uint CEntityId;
	}

	class BEntity
	{
		uint Id;
		bool IsAwesome;
		bool IsTehSuck;
		uint CEntityId;
	}

	class CEntity
	{
		uint Id;
		string Description;
		string Hint;
	}
		
Die Daten können folgendermaßen von der API abgerufen werden:

**Foos:**
	http://cqbsapiquiz.azurewebsites.net/api/values/Foo/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/api/values/Foo/		// alle Elemente (aber gekürzt)

**AEntity:**
	http://cqbsapiquiz.azurewebsites.net/api/values/AEntity/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/api/values/AEntity/	// alle Elemente (aber gekürzt)

**BEntity:**
	http://cqbsapiquiz.azurewebsites.net/api/values/BEntity/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/api/values/BEntity/ 	// alle Elemente (aber gekürzt)

**CEntity:**
	http://cqbsapiquiz.azurewebsites.net/api/values/$ID/CEntity	// einzelne Elemente

Wenn etwas nicht abgerufen werden konnte, oder ein interner Fehler auftritt liefert der Server Fehler in der folgenden Form zurück:

	class Error
	{
		int ErrorCode;
		string Description;
	}

Die Antworten werden immer als JSON geliefert.

Wenn man das Programm startet soll es dem Nutzer die Möglichkeit bieten entweder alle AEntities (inklusive aller Details!), alle BEntities oder alle Foos in jeweils eigene Dateien zu speichern (als serialisiertes JSON, so wie es ankommt).

Hinweis
=======
Wie eingangs erwähnt arbeitet die API (gewollt) nicht in allen Fällen fehlerfrei. Ziel der Übung ist den Umgang mit einer unvollständigen/schlecht implementierten API.

Im die Korrektheit des Client zu prüfen hier folgender Hinweis:
Wenn man die Gesamtmenge aller Entities als Json serialisiert (in exakt der Form wie sie von der Api kommen, d.h. ohne Leerzeichen und mit eventuell ungültigen/leeren Feldern) erhält man folgende Anzahl Zeichen:

AEntities:	 4096 Zeichen
BEntities:	 3591 Zeichen
CEntities:	10019 Zeichen
Foos:		 5953 Zeichen

Die Zeichenzahlen sind beziehen sich auf serialisiertes JSON (ohne Leerzeichen und/oder Carriage Return (o.ä.). Für A- und BEntities wird auch nur die ID der CEntity gezählt, nicht deren komplette Form.

