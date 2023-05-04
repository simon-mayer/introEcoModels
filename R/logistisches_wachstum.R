# Thema 5 Logistisches Wachstum



# Modell ------------------------------------------------------------------


#' Modellfunktion für diskretes logistisches Wachstum
#'
#' @param r Zahl zwischen 0 und 1, gibt Wachstumsrate an
#' @param K Zahl, gibt Carrying Capacity der Umwelt an ~ max Population
#' @param N Array von Zahlen, hält am Index 1 die Populationsgröße zur Zeit 1
#' @param t Zahl Endzeitpunkt der Simulation
#'
#' @returns Vektor von Zahlen, hält am Index t Populationsgröße zur Zeit t
#' @export
Nt <- function(r, K, N, t) { # Parameter

  for (i in 1:(t - 1)) {     # Schleife von 1 bis t-1

    # Population zum nächsten Zeitpunkt ist die Population zum aktuellen Zeitpunkt + Wachstum
    N[i + 1] <- N[i] + r * (1 - N[i]/K) * N[i]
  }
  N
}
