CC_whichtwo <- function(Population_Matrix,Queue,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour){
  if(length(Queue)<1.5){
    return(Population_Matrix)
  }
  else if(length(Queue)<2.5){
    back = Queue[2]
    forward = Queue[1]
  }
  else{
    if(NearestNeighbour){
      forward_position <- sample(1:(length(Queue)-1),1)
      backward_position <- forward_position+1
      forward <- Queue[forward_position]
      back <- Queue[backward_position]
    }
    else{
      two <- sample(Queue,2,replace=FALSE)
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