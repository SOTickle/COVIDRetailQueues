Get.Number_Multiple <- function(Population_Matrix,Queues,lasthour,IntensityFunc){
  which.group <- IntensityFunc[2,lasthour+1]
  Reduced_Population_Matrix <- Population_Matrix[,Population_Matrix[2,]==which.group]
  Possible <- Reduced_Population_Matrix[1,][!(Reduced_Population_Matrix[1,]%in%unlist(Queues))]
  if(length(Possible)>0.5){
    newarrival <- sample(Possible,1)
    v <- unlist(lapply(object,length))
    vector <- 1-as.logical(v - min(v))
    where <- runif(1)
    number <- 1/sum(vector)
    index <- ceiling(where/number)
    numbers <- (1:length(vector))[vector==1]
    value <- numbers[index]
    Queues[[value]] <- c(Queues[[value]],newarrival)
    return(Queues)
  }
  else{
    return(Queues)
  }
}