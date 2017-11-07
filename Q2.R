rm(list = ls(envir = globalenv()), envir = globalenv());gc();

getRandom <- function(){
  runif(1)
}
source("functions.R")

next_arrival <- getIAT()
next_dep1 <- getServiceTime1()
next_dep2 <- NA
t <- 0
totalDeparture <- 0

endTotalDeparture <- 1000
queue <- data.table(Arrival = 0, Server = 1, ServiceBegin = 0, ServiceTime = next_dep1, Departure = NA_real_)
eventList <- data.table(event = "Start", t, next_arrival, next_dep1, next_dep2, queue = queue[is.na(Server), .N], totalDeparture)

while(totalDeparture < endTotalDeparture){
  
  nextEvent <- getNextEvent()
  t <- nextEvent[[2]]
  nextEvent <- nextEvent[[1]]
  
  if (nextEvent == "arrival"){
    next_arrival <- t + getIAT()
    if(is.na(next_dep1)){
      ServiceTime <- getServiceTime1()
      next_dep1 <- t + ServiceTime
      queue <- rbind(queue, data.table(Arrival = t, Server = 1, ServiceBegin = t, ServiceTime, Departure = NA))
    } else if(is.na(next_dep2)){
      ServiceTime <- getServiceTime2()
      next_dep2 <- t + ServiceTime
      queue <- rbind(queue, data.table(Arrival = t, Server = 2, ServiceBegin = t, ServiceTime, Departure = NA))
    } else {
      queue <- rbind(queue, data.table(Arrival = t), fill = T)
    }
  } else if (nextEvent == "dep1"){
    totalDeparture <- totalDeparture + 1
    queue[is.na(Departure) & Server == 1, Departure := ServiceBegin + ServiceTime]
    if(queue[is.na(Server), .N] > 0){
      ServiceTimeT <- getServiceTime1()
      nextCustomer <- queue[is.na(Server), min(Arrival)]
      queue[Arrival == nextCustomer, `:=`(Server = 1, ServiceBegin = t, ServiceTime = ServiceTimeT)]
      next_dep1 <- t + ServiceTimeT
    } else {
      next_dep1 <- NA
    }
  } else if (nextEvent == "dep2"){
    totalDeparture <- totalDeparture + 1
    queue[is.na(Departure) & Server == 2, Departure := ServiceBegin + ServiceTime]
    if(queue[is.na(Server), .N] > 0){
      ServiceTimeT <- getServiceTime2()
      nextCustomer <- queue[is.na(Server), min(Arrival)]
      queue[Arrival == nextCustomer, `:=`(Server = 2, ServiceBegin = t, ServiceTime = ServiceTimeT)]
      next_dep2 <- t + ServiceTimeT
    } else {
      next_dep2 <- NA
    }
  }
  
  eventList <- rbind(eventList, data.table(event = nextEvent, t, next_arrival, next_dep1, next_dep2, queue = queue[is.na(ServiceBegin), .N], totalDeparture))
  eventList
}
queue <- queue[!is.na(Departure)][1:1000]

eventList[, Customer := (queue + !is.na(next_dep1) )]
eventList[, Customer := (Customer + !is.na(next_dep2))]
eventList[, timeInPeriod := shift(t, 1, type = "lead") - t]
eventList[is.na(timeInPeriod), timeInPeriod := 0]

avgCustomerInSystem <- eventList[, sum(Customer * timeInPeriod) / sum(timeInPeriod)]
avgCustomerInQueue <- eventList[, sum(queue * timeInPeriod) / sum(timeInPeriod)]
server1Util <- eventList[, sum(timeInPeriod[!is.na(next_dep1)]) / sum(timeInPeriod)]
server2Util <- eventList[, sum(timeInPeriod[!is.na(next_dep2)]) / sum(timeInPeriod)]
avgQueueTime <- queue[!is.na(Departure), mean(ServiceBegin - Arrival)]
queuePossibility <- queue[!is.na(Departure), mean(ServiceBegin - Arrival > 0)]


print(c(avgCustomerInSystem, avgCustomerInQueue, server1Util, server2Util, 
        avgQueueTime, queuePossibility))
print(t)
