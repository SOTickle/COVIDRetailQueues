SC_whichtwo <- function(ServerMatrix,Population_Matrix,CurrentServers,Queue,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime){
  #server <- sample(CurrentServers,1)
  #customer <- sample(Queue,1)
  critical <- min(length(Queue),length(CurrentServers))
  number <- ceiling(runif(1,0,critical))
  customer <- Queue[number]
  server <- CurrentServers[number]
  result_SC <- runif(1)
  result_CS <- runif(1)
  Mask <- Population_Matrix[4,customer]
  p_CS <- (1/Mask_Infection_Suppressor_Factor)*Mask*p_CS + (1-Mask)*p_CS
  if(result_SC<p_SC){
    if(ServerMatrix[2,server]>0.5){
      if(ServerMatrix[3,server]>IncubationTime){
        Population_Matrix[5,customer] = 1
        Population_Matrix[6,customer] = 0.01
      }
    }
  }
  else if(result_CS<p_CS){
    if(Population_Matrix[5,customer]>0.5){
      if(Population_Matrix[6,customer]>IncubationTime){
        ServerMatrix[2,server] = 1
        ServerMatrix[3,server] = 0.01
      }
    }
  }
  twothings <- list(ServerMatrix,Population_Matrix)
  return(twothings)
}