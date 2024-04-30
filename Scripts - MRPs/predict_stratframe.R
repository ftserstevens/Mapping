
predict_stratframe = function(census, alpha, age, state, party) {
  print("Make sure census is formatted as AGE, SEX,PARTY,STATE")
  n_census <- nrow(census)
  n_alpha <- nrow(alpha)
  preds = array(NA, c(nrow(census), nrow(alpha)))
  
  
  pb <- txtProgressBar(min = 0, max = n_census, style = 3)
  for(i in 1:n_census) { #for evey row in census
    census_sex.i = as.numeric(census[i,"SEX"])
    census_age.i = as.numeric(census[i,"AGE"])
    census_state.i = as.numeric(census[i,"STATE"])
    census_party.i = as.numeric(census[i,"PARTY"])
    
    for (j in 1:n_alpha) { # for every iteration of estimated coef
      
      # add the coef of female if sex_fe is Trueequals 0 otherwise  
      preds[i,j] =  as.numeric(
        alpha[j,] + 
          age[j, census_age.i] + 
          state[j, census_state.i] +
          party[j,census_party.i])
      
    }
    setTxtProgressBar(pb, i)
  }
  
  return(preds)
}