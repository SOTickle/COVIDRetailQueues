CC_whichtwo_Multiple <- function(Population_Matrix,Queues,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour=TRUE){
  possible_interactions <- pmax(unlist(lapply(Queues,length))-1,0)
  all_possible_interactions <- unlist(lapply(Queues,length))*(unlist(lapply(Queues,length))-1)/2
  if(max(possible_interactions)<0.5){
    return(Population_Matrix)
  }
  else if(NearestNeighbour){
    total.sum <- sum(possible_interactions)
    number <- runif(1,min=0,max=total.sum)
    queue <- which.max(cumsum(possible_interactions)>number)
    value <- possible_interactions[queue]
    where <- ceiling(runif(1,0,value))
    forward <- Queues[[queue]][where]
    back <- Queues[[queue]][where+1]
  }
  else{
    total.sum <- sum(all_possible_interactions)
    number <- runif(1,min=0,max=total.sum)
    queue <- which.max(cumsum(all_possible_interactions)>number)
    if(length(Queues[[queue]])<2.5){
      forward <- Queues[[queue]][1]
      back <- Queues[[queue]][2]
    }
    else{
      two <- sample(Queues[[queue]],2,replace=FALSE)
      forward <- two[1]
      back <- two[2]
    }
  }
    result <- runif(1)
    a <- Population_Matrix[4,forward]
    b <- Population_Matrix[4,back]
    p_CC <- (1/((Mask_Infection_Suppressor_Factor-1)*a+1))*(1/((Mask_Infection_Suppressor_Factor-1)*b+1))*p_CC
    if(result<p_CC){
      forward_time <- Population_Matrix[6,forward]
      back_time <- Population_Matrix[6,back]
      c <- Population_Matrix[5,forward]
      d <- Population_Matrix[5,back]
      if(forward_time + back_time > IncubationTime){
        e <- max(c,d)
        Population_Matrix[5,forward] <- e
        Population_Matrix[5,back] <- e
        if(forward_time*back_time < 0.000000001){
          if(forward_time == min(forward_time,back_time)){
            Population_Matrix[6,forward] = 0.01
          }
          else{
            Population_Matrix[6,back] = 0.01
          }
        }
      }
    }
  return(Population_Matrix)
}