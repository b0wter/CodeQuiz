#!/usr/local/bin/python
# -*- coding: utf-8 -*-

import random

# --- Simulationsparameter ---
iterationen = 100000					# Anzahl der Iterationen/Spielzüge der Simulation
ausgabe = False							# ausführliche Debugging-Ausgabe nach jedem Zug

# --- Spielparameter ---
wuerfel1 = [1, 2, 3, 4]					# Werte des ersten Würfels
wuerfel2 = [1, 2, 3, 4]					# Werte des zweiten Würfels

gemeinschaftskarten = [-1] * 16;		# 16 Gemeinschaftskarten (-1 = keine Positionsänderung)
gemeinschaftskarten[0:2] = [0, 10];		# "Rücke vor auf Feld x"

ereigniskarten = [-1] * 16;				# 16 Ereigniskarten (-1 = keine Positionsänderung)
ereigniskarten[0:10] = [0, 10, 11, 24, 39, 5, -2, -2, -3, -4] # "Rücke vor auf Feld x" und positionsabhängige Ziele



felder = dict.fromkeys(range(40), 0)	# Besuchszähler für jedes Feld
position = 0							# aktuelle Position der Spielfigur
zaehler = 0								# Anzahl der Züge

random.shuffle(gemeinschaftskarten)		# Gemeinschaftskarten mischen
random.shuffle(ereigniskarten)			# Ereigniskarten mischen

def wuerfeln():
	global anzahl_pasch, wuerfel1, wuerfel2
	augen = 0
	anzahl_pasch = 0
	pasch = True
	while pasch:						# bei Pasch sofort gesamte Augensumme (ohne Zwischenfelder) zurückgeben
		random.shuffle(wuerfel1)
		random.shuffle(wuerfel2)
		augen += wuerfel1[0]
		augen += wuerfel2[0]
		pasch = False
		if wuerfel1[0] == wuerfel2[0]:
			pasch = True
			anzahl_pasch += 1
			if anzahl_pasch >= 3:
				return -1				# Abbruch durch dreimal Pasch
	return augen						# im Normalfall Augensumme zurückgeben

def karte_gem():						# Gemeinschaftskarte ziehen und "unter" den Stapel legen
	ziel = gemeinschaftskarten[0]
	gemeinschaftskarten.append(gemeinschaftskarten.pop(0))
	if ziel < -1:						# positionsabhängige Ziele
		if ziel == -2:					# nächster Bahnhof
			if position < 5:
				ziel = 5
			elif position < 15:
				ziel = 15
			elif position < 25:
				ziel = 25
			elif position < 35:
				ziel = 35
			else:
				ziel = 5

		elif ziel == -3:				# nächstes E/W-Werk
			if position < 12:
				ziel = 12
			elif position < 28:
				ziel = 28
			else:
				ziel = 12

		elif ziel == -4:				# drei Felder zurück
			ziel = position - 3
	return ziel

def karte_erg():						# Ereigniskarte ziehen und "unter" den Stapel legen
	ziel = ereigniskarten[0]
	ereigniskarten.append(ereigniskarten.pop(0))
	return ziel

for i in range(iterationen):			# einzelne Züge der Simulation
	info_str = ''
	augen = wuerfeln()
	ziel = position + augen				# angepeiltes Ziel nach dem Würfeln
	ziel = ziel % len(felder)			# nicht übers letzte Feld hinaus...
	if augen == -1:						# dreimal Pasch
		ziel = 10						# Gefängnis
		info_str = 'dreimal Pasch'
	
	elif ziel == 30:					# "Gehe ins Gefängnis!"
		ziel = 10
		info_str = 'Gefängnis'

	elif ziel in [2, 17, 33]:			# Gemeinschaftskarte
		karte = karte_gem()
		if karte > -1:					# keine Relevanz für Position
			ziel = karte
			info_str = 'Gemeinschaftskarte'
	
	elif ziel in [7, 22, 36]:			# Ereigniskarte
		karte = karte_erg()
		if karte > -1:					# keine Relevanz für Position
			ziel = karte
			info_str = 'Ereigniskarte'

	position = ziel						# Ziel als neue Position übernehmen
	felder[position] += 1				# Feldzähler inkrementieren

	zaehler += 1
	if ausgabe:
		print("Zug %i: %s ->Feld %i (Zähler = %i)" % (zaehler, info_str, position, felder[position]))



ausgabe = []							# Maxima suchen
maxima = sorted(felder.iteritems(), key=lambda(k, v): (v, k))

for m in range(3):						# Indizes der drei am häufigsten besuchten Felder sammeln
	ausgabe.append(maxima[len(maxima)-m-1][0])
print "".join(map(str, ausgabe))		# Indizes ausgeben
