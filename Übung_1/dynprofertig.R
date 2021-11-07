# Dynamische Programmierung mit R

# Achtung: Dieses Programm braucht und soll später nicht verändert
# werden mit Ausnahme der Funktion dynpro.path, die noch fertig
# programmiert werden soll.

#' Dynamische Programmierung (2D)
#'
#' @param n Anzahl Zeilen der Matrix oder eine Liste mit
#' Zeilennamen, aus denen n ermittelt wird.
#' @param m Anzahl Spalten der Matrix oder eine Liste mit
#' Spaltennamen, aus denen m ermittelt wird.
#' @param value Funktion, die einen Wert ermittelt. Parameter
#' sind i, j, d.
#' @param prevstates Funktion, die den Vorgängerzustand ermittelt. 
#' Parameter sind i, j, d.
#' @param decicions Funktion, die alle erlaubten Zustände angibt. 
#' Parameter sind i, j. Jeder Zustand muss mindestens eine
#' Entscheidung haben, ggf. eine künstliche Start-Entscheidung.
#' @param optimal Funktion, die den optimalen Wert aus einer Liste 
#' von Zahlen ermitelt. Standard ist max().
#' @param acc Funktion, die den aktuellen Wert v und die bisherigen
#' akkumulierten Wert a verbindet. Standard ist f(v, a) = v + a.
#' @param a0 Initialwert für akkumulierte Werte. Standard ist 0.
#' @param decMatrix Default-Wert ist FALSE. Falls TRUE wird der
#' decision-Funktion ein dritter Parameter D übergeben.
#' D ist die Matrix mit den bisherigen optimalen
#' Entscheidungen. Dies wird z.B. benötigt, wenn das Weiterführen einer
#' Lücke beim paarweisen Alignment anders bewertet werden soll. Für
#' die meisten Fälle wird dieser Modus nicht benötigt.
#' @param stateorder Funktion, die für alle Zellen, nummeriert durch
#' k von 1 bis n * m, die aktuelle Zelle (i, j) berechnet. Dadurch
#' wird die Reihenfolge der Zustandsberechnung festgelegt.
#' Standard ist dynpro.stateorder.ulbr(), die Indexpositionen zeilenweise
#' von oben links nach unten rechts berechnet.
#'
#' @return Liste mit zwei Elementen: A ist die Matrix der 
#' akkumlierten Werte, D die die Matrix mit den getroffenen 
#' Entscheidungen.
#' @export
dynpro.matrix = function(rows, columns,
                         value, prevstates, decisions,
                         optimal = function(v) max(v),
                         acc = function(v, a) v + a,
                         a0 = 0,
                         decMatrix = FALSE,
                         stateorder = dynpro.stateorder.ulbr) {
  n = length(rows) # Dimension der Matrizen
  m = length(columns)

  A = matrix(0, nrow = n, ncol = m) # Akk. Werte
  rownames(A) = rows; colnames(A) = columns
  D = matrix(0, nrow = n, ncol = m) # Entscheidungen
  rownames(D) = rows; colnames(D) = columns

  for (k in 1:(n * m)) { # Durchlaufe alle Zellen...
    idx = stateorder(k, n, m) # und ermittele Zelle
    i = idx[1]; j = idx[2]
    # Mögliche Entscheidungen:
    ds = if (decMatrix) decisions(i, j, D) else decisions(i, j)
    v = sapply(ds, function(d) { # Durchlaufe alle Entscheidungen.
      pidx = prevstates(i, j, d) # Vorgängerzustand
      if (length(pidx) == 2) { # Gibt es einen Vorgängerzustand?
        ip = pidx[1]; jp = pidx[2]
        acc(value(i, j, d), A[ip, jp]) # Verbinde Werte (z.B. v + a).
      } else {
        acc(value(i, j, d), a0) # mit Initialwert (z.B. v + 0)
      }
    })
    vo = optimal(v) # Ermittele optimalen Wert.
    A[i, j] = vo
    D[i, j] = ds[which(v == vo)[1]] # Merke (erste) Entscheidung
  }
  list(A = A, D = D)
}

#' Pfad mit optimalen Entscheidungen.
#'
#' @param D Matrix mit getroffenen Entscheidungen
#' @param prevstates Funktion, die den Vorgängerzustand ermittelt. 
#' Parameter sind i, j, d.
#' @param i Ausgangszustand der optimalen Lösung (Zeile)
#' @param j Ausgangszustand der optimalen Lösung (Spalte)
#'
#' @return Liste der optimalen Entscheidungen.
#' @export
dynpro.path = function(D, prevstates, i, j) {
  # Ihre Lösung
  # Tipps:
  # Funktion rev() dreht einen Vektor um.
  # Funktion length() gibt die Anzahl der Elemente eines Vektors an.
  ix = i; jx = j # Veränderliche Variablen.
  p = c() # Leere Liste von Zuständen.

  repeat { # Solange es Vorgängerzustände gibt...
    d = D[ix, jx] # Entscheidung
    p = c(p, d)# Füge diese zum Pfad hinzu.
    print(p)
    pidx = prevstates(ix, jx, d) # Vorgängerzustand
    if (length(pidx) == 0) break # Gibt es keinen Vorgängerzustand?
    ix = pidx[1]; jx = pidx[2]
    print(pidx)
  }

  rev(p) # Ergebnis liegt zunächst rückwärts vor.
}


#' Ermittelt die Zelle (i, j) in der Reihenfolge oben links
#' nach rechts unten.
#'
#' @param k Wert zwischen 1 und n * m.
#' @param n Anzahl Zeilen der Matrix.
#' @param m Anzahl Spalten der Matrix.
#'
#' @return
#' @export
dynpro.stateorder.ulbr = function(k, n, m) {
  row = (k - 1) %/% m + 1
  col = (k - 1) %% m + 1
  c(row, col)
}

