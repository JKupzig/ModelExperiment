

static_routing <- function(incoming_water, previous_storage){
  riverVelocity <- 0.3
  new_storage <- (previous_storage * exp(-1/riverVelocity) +
                    incoming_water * riverVelocity * (1 - exp(-1/riverVelocity)))
  outflow <- incoming_water + previous_storage - new_storage

  return(list("outflow"=outflow, "new_storage"=new_storage))
}

els_routing <- function(incoming_water, incoming_water_previous){
  riverVelocity <- 0.3
  outflow = (incoming_water_previous * exp(-1/riverVelocity) +
               incoming_water * (1-exp(-1/riverVelocity)))
  return(list("outflow"=outflow, "new_storage"=outflow))
}

input_steep <- c(1,1,1,1,1,10,1,1,1,1,1)
input_slightly <- c(1,1,1,1,2,3,3,2,2,1,1)



outflow_steep <- c()
outflow_slightly <- c()
days <- 1:length(input_steep)

for (cell in 1:5){
  previous_storage_steep <- 0
  previous_storage_slightly <- 0

  for (day in days){
    steep <- els_routing(input_steep[day], previous_storage_steep)
    outflow_steep <- c(outflow_steep, steep$outflow)
    previous_storage_steep <- steep$new_storage
    input_steep[day] <- steep$outflow

    slightly <- els_routing(input_slightly[day], previous_storage_slightly)
    outflow_slightly <- c(outflow_slightly, slightly$outflow)
    previous_storage_slightly <- slightly$new_storage
    input_slightly[day] <- slightly$outflow
  }
}


plot(days, tail(outflow_steep, n=length(days)), type="l")
lines(days, tail(outflow_slightly, n=length(days)), col="firebrick")

