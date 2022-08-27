Completion_Multiple <- function(Queues,NumStat){
  values <- as.logical(unlist(lapply(Queues,length)))
  possible <- 1:length(Queues)[values]
  number <- runif(1)
  which.queue <- possible[ceiling(number*length(possible))]
  #r <- min(length(Queue),NumStat)
  #finished <- sample(1:r,1)
  return(which.queue)
}