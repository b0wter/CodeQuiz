Aufgabe: Schreibe einen Base64 Decoder
======================================
Der Decoder soll spezifikationskonform sein und mir allen Inhalten klar kommen. D.h. er muss ungültige Inhalte erkennen und entsprechende Fehlermeldungen ausgeben.

Beispiele
---------
Zum Testen kann man beliebige Inhalte, wie z.B.:

Zmp0eXFrdDZoeGdkcHU2ZTl0dDRjZnlhaXdhZnRjCjc3bXljejkzNXpuOXFuc3EyazR0c2RuYXB2Mzc4NAppd3k0cmJja3l1N2djdDljaW10cGZzenphMms1NnoKNnYybnRrZ3o5bWFjenR2N3FncnFkaHFnYXE4eHMzCmR4Y2Q2bjJkeGpkdjJ4Nnh5dnVycmh3ZGhwY2x2OApmZmEyZWs1M2VpaG51eTlrbTVobXZtaWQya3plcGUKZGprbjloYXAzZTd6Zml2Z2c1cmJrNnd3c2JhZTU1CmJ4N3BrMnBrMzR3Yzlqcnp2YnNucHdqZ2FhZ3BzMwpkbmo1OW01dmc2NXA0ZzVka2ttNzdieXI2YmI2NmYKbjNzeXhhYWViM2J4MjR6a2gyZjJpeGUyamo0MXNqCg==

verwenden. Der obige String ist korrekt. Man kann ihn leicht "zerstören", indem man das Padding (== am Ende) entfernt, oder ungültige Zeichen einbaut.

