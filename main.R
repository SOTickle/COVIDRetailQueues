source('queue_master_plotter_atoa.R')
#library('doParallel')

MatrixResult <- function(MaskProb,UnsafeInteractions,ServerMasks=T,ExtraSafety=T,NumTotal=8,NumStat=4,IncubationTime=6,Multiple=FALSE,NearestNeighbour=FALSE,Queue_Capacity=12){
Pop_Groups <- 1 #Number of divisions of the denominator population using the service
Denominator_Population_Size <- 10000 #Total Size of the Denominator Population
Group_Sizes <- c(10000) #- optional: stratify the population into distinct groups. These groups should have VERY MINIMAL interaction with each other and will be served by different servers during shifts
Prop_Vulnerable <- rep(0.2,Pop_Groups) #Proportion (or vector of proportions if Pop_Groups > 1) of the denominator population deemed to be particularly vulnerable to COVID-19 who nevertheless may use this service (will vary depending on service) 
Prob_Safer <- c(0.01) #Proportion (or vector of proportions if Pop_Groups > 1) of the denominator population who will wear a mask/take additional precautions to be less transmissible when using the service
P_start <- c(0.02) #probability that an arbitrary person in the population is infected at the beginning of the cycle
#Queue_Capacity <- 12 #To be calculated by the practitioner in accordance with safety guidelines - e.g. number of people who can be placed in a queue 2m apart from one another
Total_Number_Servers <- NumTotal #Total pool of workers who could act as servers (in distinct shifts as distinct groups)
Number_Stations <- NumStat #Total number of stations/servers of customers at any one time; assume that servers on same shift can affect one another and that this is an M/M/2 queue.
Wage <- 10.00 #Cost of employing server per unit time (use hour as a guide)
Fixed_Cost <- 1000.00 #Fixed cost per cycle (e.g. of keeping machines running, using a vehicle, renting a building) 
number_of_hours_open_per_day <- 12 #assume this does not vary by day of the week (e.g. Sunday trading laws are waived for simplicity)
Service_Rate <- 60 #number of customers that can be served per hour on average, assuming there is always a non-empty queue
UnsafeInteractionRate <- 12 #Effective "bad contact rate" per hour

p_CC <- 0.10 #probability (intensity) of a customer infecting another customer in the queue 

NumReps <- 20

Servers_Wear_Masks <- ServerMasks
Extra_Safety <- ExtraSafety

Mask_Infection_Suppressor_Factor <- 6 #Factor by which wearing a mask decreases the probability of transmission in an unsafe contact between two people.

Customer_to_Server_Suppression_Factor <- 2 #Factor by which additional precautions taken by employer (e.g. plexiglass to separate servers from customers) decreases the probability of transmission in an unsafe contact between a server and a customer

V <- 5.00 #single sale value
  N <- length(MaskProb)
  D <- length(UnsafeInteractions)
  EmptyMatrix <- matrix(rep(0,N*D),nrow=N,ncol=D)
  AnotherEmptyMatrix <- matrix(rep(0,N*D),nrow=N,ncol=D)
  FinalEmptyMatrix <- matrix(rep(0,N*D),nrow=N,ncol=D)
  #cl <- makeCluster(21)
  #registerDoParallel(cl)
  
  for(i in 1:N){
    for(j in 1:D){
      Prob_Safer[1] <- MaskProb[i]
      UnsafeInteractionRate <- UnsafeInteractions[j]
      result <- Quantile_Giver(NumReps,p_CC,Pop_Groups,Denominator_Population_Size,Group_Sizes,Prop_Vulnerable,Prob_Safer,P_start,Queue_Capacity,Total_Number_Servers,Number_Stations,Wage,Fixed_Cost,number_of_hours_open_per_day,Service_Rate,UnsafeInteractionRate,Servers_Wear_Masks,Extra_Safety,Mask_Infection_Suppressor_Factor,Customer_to_Server_Suppression_Factor,V,IncubationTime,Multiple,NearestNeighbour)
      EmptyMatrix[i,j] <- result[[4]][[4]]
      AnotherEmptyMatrix[i,j] <- result[[4]][[3]]
      FinalEmptyMatrix[i,j] <- result[[4]][[2]]
    }
  }
  
  #stopCluster(cl)
  
  ThreeMatrices <- list(MedianMatrix=AnotherEmptyMatrix,HighQuartileMatrix=EmptyMatrix,LowQuartileMatrix=FinalEmptyMatrix)
  return(ThreeMatrices)
}

single_atoa_G1S4FT_4.8 <- MatrixResult(c(0,0.5,1),c(1,6,30,60),F,T,NumTotal=8,NumStat=4,IncubationTime=0.001,Multiple=FALSE,NearestNeighbour=FALSE,Queue_Capacity=12)


