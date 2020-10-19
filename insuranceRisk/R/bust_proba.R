bust_proba <- function(assets, profits = FALSE){
  count = 0; l = length(assets)
  if(profits){
    for(i in 1:l){
      if((i %% 2 == 1) && (i %% 3 != 0) && (assets[[i]] == TRUE))
        count = count + 1
    }
  }
  else {
    for(i in 1:l){
      if((i %% 2 == 1) && (assets[[i]] == TRUE))
        count = count + 1
    }
  }
  count / l
}
