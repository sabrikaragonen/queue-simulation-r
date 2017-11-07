rm(list = ls(envir = globalenv()), envir = globalenv());gc();

getRandom <- function(){
  randomVal <- randoms[Used == 0, Random[1]]
  randoms[Random == randomVal, Used := 1]
  randomVal
}
source("functions.R")

randoms <- data.table(Random = c(497, 380, 862, 20, 391, 975, 480, 905, 759, 560, 593, 744, 69, 370, 790, 176, 20, 714, 539, 928,
                        860, 717)/1000, Used = 0)


queue <- c()
next_arrival <- getIAT()
next_dep1 <- getServiceTime1()
next_dep2 <- NA
t <- 0
endTime <- 30
eventList <- data.table(event = "Start", t, next_arrival, next_dep1, next_dep2, queue = length(queue))

while(t < endTime){
  nextEvent <- getNextEvent()
  t <- nextEvent[[2]]
  nextEvent <- nextEvent[[1]]
  
  if(t > endTime){
    nextEvent = "Finish"
    t = endTime
  } else if (nextEvent == "arrival"){
    next_arrival <- t + getIAT()
    if(is.na(next_dep1)){
      next_dep1 <- t + getServiceTime1()
    } else if(is.na(next_dep2)){
      next_dep2 <- t + getServiceTime2()
    } else {
      queue <- c(queue, t)
    }
  } else if (nextEvent == "dep1"){
    if(length(queue) > 0){
      queue <- queue[-1]
      next_dep1 <- t + getServiceTime1()
    } else {
      next_dep1 <- NA
    }
  } else if (nextEvent == "dep2"){
    if(length(queue) > 0){
      queue <- queue[-1]
      next_dep2 <- t + getServiceTime2()
    } else {
      next_dep2 <- NA
    }
  }
  
  eventList <- rbind(eventList, data.table(event = nextEvent, t, next_arrival, next_dep1, next_dep2, queue = length(queue)))
  eventList
}

eventList[, timeInPeriod := shift(t, 1, endTime, type = "lead") - t]
server1Util <- eventList[, sum(timeInPeriod[!is.na(next_dep1)]) / sum(timeInPeriod)]
server2Util <- eventList[, sum(timeInPeriod[!is.na(next_dep2)]) / sum(timeInPeriod)]
