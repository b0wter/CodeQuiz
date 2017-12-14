Aufgabe: API Client
===================
Diese Woche gilt es eine Web-API abzufragen. Leider hat war der Entwickler der API ein Schülerpraktikant und hatte kaum vorherige Erfahrung. Daher ist es durchaus möglich, dass die API an einigen Stellen nicht konsequent und fehlerfrei ist.

Ziel der Aufgabe ist es einen funktionsfähigen Client zu programmieren, welcher folgende Kriterien erfüllt:

	* Unter keinen Umständen sollte der Client abstürzen.
	* Der Client sollte die Möglichkeit bieten von der API alle Datentypen abzurufen.
	* Eine Ausgabe der Daten kann unformatiert über die Konsole erfolgen.
	* Der Client sollte in der Lage sein auch große Mengen an Daten abzurufen, d.h. 500+ Anfragen.

Es können folgende Daten von der Api abgerufen werden:

	class Foo
	{
		uint Id;		
		string Name;
		bool IsClosed;
		List Children;		//Children können AEntity und/order BEntity sein
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
	http://cqbsapiquiz.azurewebsites.net/Foo/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/Foo/All 	// alle elemente

**AEntity:**
	http://cqbsapiquiz.azurewebsites.net/AEntity/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/AEntity/All 	// alle elemente

**BEntity:**
	http://cqbsapiquiz.azurewebsites.net/BEntity/$ID 	// einzelne elemente
	http://cqbsapiquiz.azurewebsites.net/BEntity/All 	// alle elemente

**CEntity:**
	http://cqbsapiquiz.azurewebsites.net/$ID/CEntity	// einzelne Elemente

Wenn etwas nicht abgerufen werden konnte, oder ein interner Fehler auftritt liefert der Server Fehler in der folgenden Form zurück:

	class Error
	{
		int ErrorCode;
		string Description;
	}

Die Antworten werden immer als JSON geliefert.
