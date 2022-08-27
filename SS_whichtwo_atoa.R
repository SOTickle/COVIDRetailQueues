SS_whichtwo <- function(ServerMatrix,CurrentServers,p_SS,IncubationTime){
  if(length(CurrentServers)<1.5){
    return(ServerMatrix)
  }
  else{
    two <- sample(CurrentServers,2,replace=FALSE)
    result <- runif(1)
    if(result<p_SS){
      one_time <- ServerMatrix[3,two[1]]
      two_time <- ServerMatrix[3,two[2]]
      a <- ServerMatrix[2,two[1]]
      b <- ServerMatrix[2,two[2]]
      if(one_time + two_time > IncubationTime){
        c <- max(a,b)
        ServerMatrix[2,two[1]] <- c
        ServerMatrix[2,two[2]] <- c
        if(one_time*two_time < 0.00000001){
          if(one_time == min(one_time,two_time)){
            ServerMatrix[3,two[1]] = 0.01
          }
          else{
            ServerMatrix[3,two[2]] = 0.01
          }
        }
      }
    }
  }
  return(ServerMatrix)
}