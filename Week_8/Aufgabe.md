Aufgabe
=======
Diese Woche gilt es selbst eine Web API zu implementieren. Unter folgender Url kann eine Datei heruntergeladen werden (oben, rechts ist ein Export-Button):

	https://data.iowa.gov/Communities-People/State-of-Iowa-Monthly-Voter-Registration-Totals-by/cp55-uurs

Die API soll die Daten der CSV Datei bereitstellen. Dazu soll ein Endpunkt bereitgestellt werden, welcher folgende optionalen Argumente entgegennimmt:

	* county
	* month
	* party affiliation
	* active_status
	* limit (als obere Grenze für die Zahl der zurückgelieferten Ergebnisse)

Das Ergebnis soll im JSON Format zurückgeliefert werden, und der Service muss RESTful sein (Bedeutung: https://poe-php.de/tutorial/rest-einfuehrung-in-die-api-erstellung ).
