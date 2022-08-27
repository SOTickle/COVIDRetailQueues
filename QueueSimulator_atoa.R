#Runs one cycle of the queue and checks how many customers are served, as well as how many (new) people have become infected as a result of using the service

source('GetNumber.R')
source('CC_whichtwo_atoa.R')
source('SS_whichtwo_atoa.R')
source('SC_whichtwo_atoa.R')
source('Completion.R')

run.Queue <- function(UnsafeInteractionRate,Service_Rate,Population_Matrix,IntensityFunc,NumHours,NumStat,NumServers,Capacity,p_CC,p_CS,p_SC,p_SS,random_assign=TRUE,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour){
  Started_Infected <- sum(Population_Matrix[5,])
  Servers <- 1:NumServers
  ServerMatrix <- matrix(c(Servers,rep(0,2*NumServers)),nrow=3,byrow=TRUE) #all of the servers start off free of COVID-19 (can be ascertained by e.g. testing)
  ActiveServerMatrix <- matrix(rep(0,NumHours*7*NumStat),nrow=NumStat)
  if(random_assign==TRUE){#get random collection of servers for each hour
    for(i in 1:(7*NumHours)){
      Shift <- sample(Servers,NumStat,replace=FALSE)
      ActiveServerMatrix[,i] = Shift
    }
  }
  else{#design better shift pattern in which there are no crossovers of staff; fault if this is not possible.
    G <- max(Population_Matrix[2,]) #number of population groups
    if(NumStat*G>NumServers){
      print('Not enough staff to allow for this population segmentation!')
      stop()
    }
    Truncated <- NumStat*G
    cycle <- sample(1:Truncated,size=Truncated,replace=FALSE)
    shift <- rep(cycle,(7*NumHours*NumStat)/length(cycle))
    if(length(shift)<((7*NumHours*NumStat)-0.5)){
      Excess <- 7*NumHours*NumStat - length(shift)
      needed <- cycle[1:Excess]
      Total <- c(shift,needed)
    }
    else{
      Total <- shift
    }
    ActiveServerMatrix <- matrix(Total,nrow=NumStat,byrow=FALSE)
  }
  clock <- 0
  Queue <- c()
  v=rep(0,NumStat)
  w=rep(0,2)
  ActiveServerMatrix <- cbind(ActiveServerMatrix,v)
  IntensityFunc <- cbind(IntensityFunc,w)
  CurrentServers <- ActiveServerMatrix[,1]
  lasthour <- 0
  NumberServed <- 0
  NumberArrived <- 0
  CurrentIntensity <- IntensityFunc[,lasthour+1]
  while(clock<(NumHours)*7){#run through one cycle
    if(length(Queue)<0.5){#no-one is currently being served 
      if(NearestNeighbour){
        SS_interaction <- Inf
      }
      else{
        SS_interaction <- rexp(1,UnsafeInteractionRate*(NumStat)*(NumStat-1)/2)
      }
      #SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat^2)#time until unsafe interaction between two servers on the same shift occurs
      Arrival <- rexp(1,CurrentIntensity[2])#time until customer arrives
      if(SS_interaction < Arrival){#unsafe interaction occurs first - randomly assign which interact and Server Matrix if infection spreads
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else{#customer arrives first - work out their characteristics and send to first station
        clock <- clock + Arrival
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
        NumberArrived <- NumberArrived + 1
        Queue <- Get.Number(Population_Matrix,Queue,lasthour,IntensityFunc)
        #Queue <- c(Queue,NewArrival) #update queue
      }
    }
    else if(length(Queue)<1.5){
      SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat*(NumStat-1)/2)
      SC_interaction <- rexp(1,UnsafeInteractionRate*1)
      Arrival <- rexp(1,60*CurrentIntensity[2])
      Service_Completed <- rexp(1,length(Queue)*Service_Rate)
      Times <- c(SS_interaction,SC_interaction,Arrival,Service_Completed)
      if(which.min(Times)<1.5){
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){#unsafe interaction between server and customer occurs first
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo(ServerMatrix,Population_Matrix,CurrentServers,Queue,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        ServerMatrix <- int_SC[[1]]
        Population_Matrix <- int_SC[[2]]
      }
      else if(which.min(Times)<3.5){#an arrival occurs first
        clock <- clock + Arrival
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
        NumberArrived <- NumberArrived + 1
        Queue <- Get.Number(Population_Matrix,Queue,lasthour,IntensityFunc)
        #Queue <- c(Queue,NewArrival) #update queue
      }
      else{#a service completion occurs first
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        random_finish <- Completion(Queue,NumStat) # work out which is finished (relative to position in queue)
        NumberServed <- NumberServed + 1
        Queue <- Queue[-random_finish] #take out someone according a random completion of service
      }
    }
    else if(length(Queue)<(NumStat+0.5)){#there are customers - but all of them are currently being served at separate stations
      occupied <- min(NumStat,length(Queue))
      SS_interaction <- rexp(1,UnsafeInteractionRate*(NumStat)*(NumStat-1)/2) #time until unsafe interaction between two servers on the same shift occurs
      SC_interaction <- rexp(1,UnsafeInteractionRate*occupied)#time until unsafe interaction between a server and their customer occurs
      if(NearestNeighbour){
        CC_interaction <- rexp(1,UnsafeInteractionRate*(length(Queue)-1))
      }
      else{
        CC_interaction <- rexp(1,UnsafeInteractionRate*(length(Queue)-1)*length(Queue)/2)#time until unsafe interaction between two customers in the queue
      }
      Arrival <- rexp(1,CurrentIntensity[2])#time until new customer arrives
      Service_Completed <- rexp(1,length(Queue)*Service_Rate)#time until a (random) occupied server finishes serving their customer
      Times <- c(SS_interaction,SC_interaction,CC_interaction,Arrival,Service_Completed)
      if(which.min(Times)<1.5){#unsafe interaction between two servers occurs first
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){#unsafe interaction between server and customer occurs first
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo(ServerMatrix,Population_Matrix,CurrentServers,Queue,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        ServerMatrix <- int_SC[[1]]
        Population_Matrix <- int_SC[[2]]
      }
      else if(which.min(Times)<3.5){#unsafe interaction between two customers occurs first
        clock <- clock + CC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+CC_interaction)
        Population_Matrix <- CC_whichtwo(Population_Matrix,Queue,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
      }
      else if(which.min(Times)<4.5){#an arrival occurs first
        clock <- clock + Arrival
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
        NumberArrived <- NumberArrived + 1
        Queue <- Get.Number(Population_Matrix,Queue,lasthour,IntensityFunc)
        #Queue <- c(Queue,NewArrival) #update queue
      }
      else{#a service completion occurs first
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        random_finish <- Completion(Queue,NumStat) # work out which is finished (relative to position in queue)
        NumberServed <- NumberServed + 1
        Queue <- Queue[-random_finish] #take out someone according a random completion of service
      }
    }
    else if(length(Queue)<(Capacity-0.5)){#there are customers not being served and queueing, potentially up to capacity
      SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat*(NumStat-1)/2)#time until unsafe interaction between two servers on the same shift occurs
      SC_interaction <- rexp(1,UnsafeInteractionRate*NumStat)#time until unsafe interaction between a server and their customer occurs
      CC_interaction <- rexp(1,UnsafeInteractionRate*(length(Queue)-1)*length(Queue)/2)#time until unsafe interaction between two customers in the queue
      Arrival <- rexp(1,CurrentIntensity[2])#time until new customer arrives
      Service_Completed <- rexp(1,NumStat*Service_Rate)#time until a (random) occupied server finishes serving their customer
      Times <- c(SS_interaction,SC_interaction,CC_interaction,Arrival,Service_Completed)
      if(which.min(Times)<1.5){#unsafe interaction between two servers occurs first
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){#unsafe interaction between server and customer occurs first
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo(ServerMatrix,Population_Matrix,CurrentServers,Queue,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        Population_Matrix <- int_SC[[2]]
        ServerMatrix <- int_SC[[1]]
      }
      else if(which.min(Times)<3.5){#unsafe interaction between two customers occurs first
        clock <- clock + CC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+CC_interaction)
        Population_Matrix <- CC_whichtwo(Population_Matrix,Queue,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
      }
      else if(which.min(Times)<4.5){#an arrival occurs first
        clock <- clock + Arrival
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
        NumberArrived <- NumberArrived + 1
        Queue <- Get.Number(Population_Matrix,Queue,lasthour,IntensityFunc)
      }
      else{#a service completion occurs first
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        random_finish <- Completion(Queue,NumStat)
        NumberServed <- NumberServed + 1
        Queue <- Queue[-random_finish]
      }
    }
    else{#queue is at capacity - no-one else can arrive (they see queue is full and leave at once)
      SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat*(NumStat-1)/2)
      SC_interaction <- rexp(1,UnsafeInteractionRate*NumStat)
      CC_interaction <- rexp(1,UnsafeInteractionRate*(length(Queue)-1)*length(Queue)/2)
      Service_Completed <- rexp(1,NumStat*Service_Rate)
      Times <- c(SS_interaction,SC_interaction,CC_interaction,Service_Completed)
      if(which.min(Times)<1.5){
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo(ServerMatrix,Population_Matrix,CurrentServers,Queue,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        ServerMatrix <- int_SC[[1]]
        Population_Matrix <- int_SC[[2]]
      }
      else if(which.min(Times)<3.5){
        clock <- clock + CC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+CC_interaction)
        Population_Matrix <- CC_whichtwo(Population_Matrix,Queue,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
      }
      else{
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        NumberServed <- NumberServed + 1
        random_finish <- Completion(Queue,NumStat)
        Queue <- Queue[-random_finish]
      }
    }
    if(clock>=(lasthour+1)){ #This shows that the shifts are not "super-strict". We wait until a "current event" finishes before shifting workers.
      lasthour <- lasthour + 1
      CurrentServers <- ActiveServerMatrix[,lasthour+1]
      CurrentIntensity <- IntensityFunc[,lasthour+1]
    }
  }
  Final_Infected <- sum(Population_Matrix[5,])
  resulting_list <- list(NumberServed=NumberServed,NumberArrived=NumberArrived,Population=Population_Matrix,ServerMatrix=ServerMatrix,AdditionalInfected=Final_Infected-Started_Infected)
  return(resulting_list)
}