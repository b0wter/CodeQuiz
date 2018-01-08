Aufgabe
=======
Diese Woche gilt es selbst eine Web API zu implementieren. Unter folgender Url kann eine Datei heruntergeladen werden (oben, rechts ist ein Export-Button):

	https://data.iowa.gov/Communities-People/State-of-Iowa-Monthly-Voter-Registration-Totals-by/cp55-uurs

Die API soll die Daten der CSV Datei bereitstellen. Dazu soll ein Endpunkt bereitgestellt werden, welcher folgende optionalen Argumente entgegennimmt (als URL Parameter, z.B.: http://localhost/api?county=blabla&month=12):

	* county
	* month
	* party affiliation
	* active_status
	* limit (als obere Grenze für die Zahl der zurückgelieferten Ergebnisse)

Das Ergebnis soll im JSON Format zurückgeliefert werden, und der Service soll stateless sein. Es müssen nur GET-Requests implementiert werden.

Als Bonus: neue Einträge sollten per POST hinzufügbar sein.
