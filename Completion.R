Completion <- function(Queue,NumStat){
  r <- min(length(Queue),NumStat)
  finished <- sample(1:r,1)
  return(finished)
}