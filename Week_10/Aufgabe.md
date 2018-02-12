Aufgabe
=======
Otto bekam vom Weihnachtsmann letztes Jahr nicht die Geschenke, die er sich gewunscht hatte. Deswegen spielt er ihm dieses Jahr einen Streich. Traditionell betritt der Weihnachtsmann die Gebaude durch den Schornstein von oben, welcher hier 10 Meter lang ist. Am unteren Ende des Schornsteins hat Otto nun ein Trampolin aufgestellt. Die Position des Weihnachtsmanns

    y(t)

zur Zeit t im Schornstein ist gegeben als Losung der Differentialgleichung

    y''(t) = - g,   mit g := 9.81

wobei die Reibung etc. vernachlassigt wird. Hier bezeichnet _g_ die Erdbeschleunigung in _m/s^2_ und die erste Zeitableitung _y'(t)_ die Geschwindigkeit des Weihnachtsmanns zur Zeit _t_. Das bedeutet, dass die Anfangsbedingungen gegeben sind als

    y(0) := 10, y'(0) := 0.

Jedes Mal, wenn der Weihnachtsmann auf das Trampolin auftrifft, wird er mit dem 0,9-fachen seiner Aufprallgeschwindigkeit wieder nach oben befordert. Wenn er also zur Zeit _t1_ unten angekommen ist und gerade nach oben geschleudert wird, mussen für diese Aufwärtsbewegung die Anfangsbedingungen

    y(t1) := 0, y'(t1) := -0.9 lim [e->0] y'(t1-e)

gelten, da sich das Trampolin unten _(y(t1) = 0)_ im Schornstein befindet und sich der Weihnachtsmann mit dem 0.9-fachen seiner Aufprallgeschwindigkeit _(lim [e->0] y'(t1-e))_ in die entgegengesetzte Richtung _(-)_ bewegt. Die gesamte Bewegung des Weihnachtsmanns lasst sich somit als eine Folge von Anfangswertproblemen beschreiben.

 * Bestimme die Bewegung des Weihnachtsmanns in Abhangigkeit von der Zeit. Beschreibe das Vorgehen, d.h. welche Gleichungssysteme du ggf. löst bzw. welche Lösung sich zu welchem Zeitpunkt wie ergibt und warum.
 * Schreibe zur Losung dieses Problems ein Stück Code und plotte die Losung für die ersten 20 Sekunden. Plz gut beschriften!
 * Gib eine Tabelle an, in der die maximale Sprunghohe nach den ersten 13 Sprüngen auftragen sind. Nach wie vielen Sprungen hüpft der Weihnachtsmann nur noch weniger als einen Meter?