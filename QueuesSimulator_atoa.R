#Runs one cycle of the queue and checks how many customers are served, as well as how many (new) people have become infected as a result of using the service

source('GetNumber_Multiple.R')
source('CC_whichtwo_atoa_Multiple.R')
source('SS_whichtwo_atoa.R')
source('SC_whichtwo_atoa_Multiple.R')
source('Completion_Multiple.R')

run.Queues <- function(UnsafeInteractionRate,Service_Rate,Population_Matrix,IntensityFunc,NumHours,NumStat,NumServers,Capacity,p_CC,p_CS,p_SC,p_SS,random_assign=TRUE,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour){
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
  Queues <- rep(list(c()),NumStat)
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
    if(sum(unlist(lapply(Queues,length)))<0.5){#no-one is currently being served 
      #SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat^2)#time until unsafe interaction between two servers on the same shift occurs
      Arrival <- rexp(1,CurrentIntensity[2])#time until customer arrives
      clock <- clock + Arrival
      Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
      NumberArrived <- NumberArrived + 1
      Queues <- Get.Number_Multiple(Population_Matrix,Queues,lasthour,IntensityFunc)
      #Queue <- c(Queue,NewArrival) #update queue
    }
    else if(sum(unlist(lapply(Queues,length)))<(NumStat*Capacity-0.5)){
      #SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat^2)
      possible_interactions <- pmax(unlist(lapply(Queues,length))-1,0)
      all_possible_interactions <- unlist(lapply(Queues,length))*(unlist(lapply(Queues,length))-1)/2
      number <- sum(possible_interactions)
      if(number>0.5){
        if(NearestNeighbour){
          CC_interaction <- rexp(1,UnsafeInteractionRate*number)          
        }
        else{
          CC_interaction <- rexp(1,UnsafeInteractionRate*sum(all_possible_interactions))
        }
      }
      else{
        CC_interaction <- Inf
      }
      occupied <- sum(as.logical(unlist(lapply(Queues,length))))
      SC_interaction <- rexp(1,UnsafeInteractionRate*occupied)
      Arrival <- rexp(1,CurrentIntensity[2])
      Service_Completed <- rexp(1,occupied*Service_Rate*2)
      Times <- c(Inf,SC_interaction,Arrival,Service_Completed,CC_interaction)
      if(which.min(Times)<1.5){
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){#unsafe interaction between server and customer occurs first
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo_Multiple(ServerMatrix,Population_Matrix,CurrentServers,Queues,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        ServerMatrix <- int_SC[[1]]
        Population_Matrix <- int_SC[[2]]
      }
      else if(which.min(Times)<3.5){#an arrival occurs first
        clock <- clock + Arrival
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Arrival)
        NumberArrived <- NumberArrived + 1
        Queues <- Get.Number_Multiple(Population_Matrix,Queues,lasthour,IntensityFunc)
        #Queue <- c(Queue,NewArrival) #update queue
      }
      else if(which.min(Times)<4.5){#a service completion occurs first
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        random_finish <- Completion_Multiple(Queues,NumStat) # work out which is finished (relative to position in queue)
        NumberServed <- NumberServed + 1
        Queues[[random_finish]] <- Queues[[random_finish]][-1] #take out someone according a random completion of service
      }
      else{
        clock <- clock + CC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+CC_interaction)
        Population_Matrix <- CC_whichtwo_Multiple(Population_Matrix,Queues,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
      }
    }
    else{#queue is at capacity - no-one else can arrive (they see queue is full and leave at once)
      #SS_interaction <- rexp(1,UnsafeInteractionRate*NumStat^2)
      SC_interaction <- rexp(1,UnsafeInteractionRate*NumStat)
      if(NearestNeighbour){
        CC_interaction <- rexp(1,UnsafeInteractionRate*length(Queues)*Capacity*(Capacity-1)/2)
      }
      else{
        CC_interaction <- rexp(1,UnsafeInteractionRate*length(Queues)*(Capacity-1))
      }
      #CC_interaction <- rexp(1,UnsafeInteractionRate*(length(Queue)-1)*length(Queue)/2)
      Service_Completed <- rexp(1,NumStat*Service_Rate)
      Times <- c(Inf,SC_interaction,CC_interaction,Service_Completed)
      if(which.min(Times)<1.5){
        clock <- clock + SS_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SS_interaction)
        ServerMatrix <- SS_whichtwo(ServerMatrix,CurrentServers,p_SS,IncubationTime)
      }
      else if(which.min(Times)<2.5){
        clock <- clock + SC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+SC_interaction)
        int_SC <- SC_whichtwo_Multiple(ServerMatrix,Population_Matrix,CurrentServers,Queues,p_SC,p_CS,Mask_Infection_Suppressor_Factor,IncubationTime)
        ServerMatrix <- int_SC[[1]]
        Population_Matrix <- int_SC[[2]]
      }
      else if(which.min(Times)<3.5){
        clock <- clock + CC_interaction
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+CC_interaction)
        Population_Matrix <- CC_whichtwo_Multiple(Population_Matrix,Queues,p_CC,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
      }
      else{
        clock <- clock + Service_Completed
        Population_Matrix[6,] <- (Population_Matrix[6,]>0)*(Population_Matrix[6,]+Service_Completed)
        NumberServed <- NumberServed + 1
        random_finish <- Completion_Multiple(Queue,NumStat)
        Queue[[random_finish]] <- Queue[[random_finish]][-1]
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