Get.Number <- function(Population_Matrix,Queue,lasthour,IntensityFunc){
  which.group <- IntensityFunc[2,lasthour+1]
  Reduced_Population_Matrix <- Population_Matrix[,Population_Matrix[2,]==which.group]
  Possible <- Reduced_Population_Matrix[1,][!(Reduced_Population_Matrix[1,]%in%Queue)]
  if(length(Possible)>0.5){
    newarrival <- sample(Possible,1)
    Queue <- c(Queue,newarrival)
    return(Queue)
  }
  else{
    return(Queue)
  }
}