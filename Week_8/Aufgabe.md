Aufgabe
=======
Diese Woche gilt es selbst eine Web API zu implementieren. Unter folgender Url kann eine Datei heruntergeladen werden (oben, rechts ist ein Export-Button):

	https://data.iowa.gov/Communities-People/State-of-Iowa-Monthly-Voter-Registration-Totals-by/cp55-uurs

Die API soll die Daten der CSV Datei bereitstellen. Dazu soll ein Endpunkt bereitgestellt werden, welcher folgende optionalen Argumente entgegennimmt (als URL Parameter, z.B.: http://localhost/api?county=blabla&month=12):

	* county
	* month
	* party affiliation (soll die Spalten löschen welche die anderen Parteien referenzieren)
	* active_status (soll die Spalten löschen welche inactive referenzieren)
	* limit (als obere Grenze für die Zahl der zurückgelieferten Ergebnisse)

Das Ergebnis soll im JSON Format zurückgeliefert werden, und der Service soll stateless sein. Es müssen nur GET-Requests implementiert werden.

Beispiel für eine Anfrage welche keinerlei Parameter hat:

	{
		"Other - Active": 8,
		"Democrat - Active": 1041,
		"Republican - Active": 1906,
		"Libertarian - Inactive": 0,
		"No Party - Active": 2032,
		"Republican - Inactive": 61,
		"Libertarian - Active": 0,
		"County": "Adair",
		"Date": "2015-03-01 00:00:00",
		"No Party - Inactive": 157,
		"Other - Inactive": 0,
		"Democrat - Inactive": 66,
		"Grand Total": "5271"
	},
	...

Beispiel einer Anfrage mit Parametern:

	http://localhost:8080/get_voters_where?county=Clayton&party=no_party&status=active

Beispielergebnis:

	{
		"No Party - Active": 4784,
		"County": "Clayton",
		"Date": "2015-03-01 00:00:00",
		"Grand Total": "11858"
	},
	{
		"No Party - Active": 4777,
		"County": "Clayton",
		"Date": "2015-02-01 00:00:00",
		"Grand Total": "11882"
	},
	...

Als Bonus: neue Einträge sollten per POST hinzufügbar sein.
