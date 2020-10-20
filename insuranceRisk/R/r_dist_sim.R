## Fonction de simulation de densité de la distance R par méthode de rejet ####
# Input
#   f : Fonction de densité
#   n : Nombre d'observations
#   a : Borne supérieure de f
# Output
#   r : Densité estimée
r_dist_sim <- function(f, n, a) {
  du <- runif(n, min = 0, max = a)
  dt <- f
  ds <- dt[du < dt]
  return(ds)
}

