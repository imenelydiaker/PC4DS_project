z_profit <- function(t) {
  premiums <- 5500000; z_array <- c(1000000); go.bust <- FALSE; profits <- 0
  for (i in 1:t) { # i : annee precedente car les tableaux en R commencent a 1 et z_array est deja initialise a z_0
    claims <- simu_X(n_samples = 100, alpha = 3, beta = 1e5)
    if (z_array[i] > 0)
      z_t <- max(z_array[i] + premiums - sum(claims), 0)
    else{
      go.bust <- TRUE
      z_array <- append(z_array, rep(0, 6 - i))
      break # when the company goes bust it stays there so we abort the loop
    }
    if (z_t > 1e6){
      z_array <- append(z_array, z_t - 1e6)
      profits <- profits + 1e6
    }
    else
      z_array <- append(z_array, z_t)
  }
  list(go.bust = go.bust, profits = profits, assets = z_array)
}
