getIAT <- function(){
  getRandom() * 10 + 5
}

getServiceTime1 <- function(){
  getRandom() * 6 + 14
}

getServiceTime2 <- function(){
  rand <- getRandom()
  if(rand <= .12) { st <- 5 }
  else if(rand <= .47)  { st <- 15 }
  else if(rand <= .90)  { st <- 25 }
  else if(rand <= .96)  { st <- 35 }
  else if(rand <= 1)  { st <- 45 }
  
  st
}

getNextEvent <- function(){
  minT <- pmin(next_arrival, next_dep1, next_dep2, na.rm = T)
  if(!is.na(next_arrival) && minT == next_arrival){
    event <- "arrival"
  } else if (!is.na(next_dep1) && minT == next_dep1){
    event <- "dep1"
  } else if (!is.na(next_dep2) && minT == next_dep2){
    event <- "dep2"
  }
  return(list(event, minT))
}
