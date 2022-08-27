##########################################
#Revenue (per cycle) generator for system#
##########################################

source('QueueSimulator_atoa.R')
source('QueuesSimulator_atoa.R')
source('PopulationSimulator_atoa.R')
#source('PopulationsSimulator_atoa.R')
source('IntensityGiver_atoa.R')

#Global Parameters

#giver <- function(NumReps){

#Profit_Vector <- c()
#Additional_Infected_Vector <- c()

#for(j in 1:NumReps){
Pop_Groups <- 2 #Number of divisions of the denominator population using the service
Denominator_Population_Size <- 10000 #Total Size of the Denominator Population
Group_Sizes <- c(9500,500) #- optional: stratify the population into distinct groups. These groups should have VERY MINIMAL interaction with each other and will be served by different servers during shifts
Prop_Vulnerable <- rep(0.2,Pop_Groups) #Proportion (or vector of proportions if Pop_Groups > 1) of the denominator population deemed to be particularly vulnerable to COVID-19 who nevertheless may use this service (will vary depending on service) 
Prob_Safer <- c(0.00,0.99) #Proportion (or vector of proportions if Pop_Groups > 1) of the denominator population who will wear a mask/take additional precautions to be less transmissible when using the service
P_start <- c(0.05,0.20) #probability that an arbitrary person in the population is infected at the beginning of the cycle
Queue_Capacity <- 12 #To be calculated by the practitioner in accordance with safety guidelines - e.g. number of people who can be placed in a queue 2m apart from one another
Total_Number_Servers <- 10 #Total pool of workers who could act as servers (in distinct shifts as distinct groups)
Number_Stations <- 2 #Total number of stations/servers of customers at any one time; assume that servers on same shift can affect one another and that this is an M/M/2 queue.
Wage <- 10.00 #Cost of employing server per unit time (use hour as a guide)
Fixed_Cost <- 1000.00 #Fixed cost per cycle (e.g. of keeping machines running, using a vehicle, renting a building) 
number_of_hours_open_per_day <- 12 #assume this does not vary by day of the week (e.g. Sunday trading laws are waived for simplicity)
Service_Rate <- 60 #number of customers that can be served per hour on average, assuming there is always a non-empty queue
UnsafeInteractionRate <- 12 #Effective "bad contact rate" per hour
IncubationTime <- 3

p_CC <- 0.50 #probability (intensity) of a customer infecting another customer in the queue 

Servers_Wear_Masks <- TRUE
Extra_Safety <- TRUE

Mask_Infection_Suppressor_Factor <- 6 #Factor by which wearing a mask decreases the probability of transmission in an unsafe contact between two people.

Customer_to_Server_Suppression_Factor <- 2 #Factor by which additional precautions taken by employer (e.g. plexiglass to separate servers from customers) decreases the probability of transmission in an unsafe contact between a server and a customer

V <- 5.00 #single sale value

#Quantile_Giver(NumReps,p_CC,Pop_Groups,Denominator_Population_Size,Group_Sizes,Prop_Vulnerable,Prob_Safer,P_start,Queue_Capacity,Total_Number_Servers,Number_Stations,Wage,Fixed_Cost,number_of_hours_open_per_day,Service_Rate,UnsafeInteractionRate,Servers_Wear_Masks,Extra_Safety,Mask_Infection_Suppressor_Factor,Customer_to_Server_Suppression_Factor,V)

#Assume all servers start off non-vulnerable and free of COVID-19.

Quantile_Giver <- function(NumReps,p_CC,Pop_Groups,Denominator_Population_Size,Group_Sizes,Prop_Vulnerable,Prob_Safer,P_start,Queue_Capacity,Total_Number_Servers,Number_Stations,Wage,Fixed_Cost,number_of_hours_open_per_day,Service_Rate,UnsafeInteractionRate,Servers_Wear_Masks,Extra_Safety,Mask_Infection_Suppressor_Factor,Customer_to_Server_Suppression_Factor,V,IncubationTime,Multiple,NearestNeighbour){
 
  Profit_Vector <- c()
  Additional_Infected_Vector <- c()
 
  for(i in 1:NumReps){

    #Infection-Specific Parameters (More likely to be estimated)
    p_SC <- p_CC #probability (intensity) of a server infecting the customer they are serving
    p_CS <- p_CC #probability (intensity) of a customer infecting the server serving them
    p_SS <- p_CC #probability (intensity) of a server on the same shift as another affecting another server
  
    if(Servers_Wear_Masks){
      p_SC <- (1/(Mask_Infection_Suppressor_Factor))*p_CC
      p_SS <- (1/Mask_Infection_Suppressor_Factor)*(1/Mask_Infection_Suppressor_Factor)*p_CC
    }
    if(Extra_Safety){
      p_CS <- (1/Customer_to_Server_Suppression_Factor)*p_CC
      p_SC <- (1/Customer_to_Server_Suppression_Factor)*p_SC
    }

    #Generate vector of denominator population/sub-populations

    PopMatrix <- get.Pop.Matrix(Pop_Groups,Denominator_Population_Size,specify=FALSE,Group_Sizes,Prop_Vulnerable,Prob_Safer,P_start,IncubationTime)

    #vec <- PopMatrix[2,]
    #PopMatrix[2,] <- rep(1,length(vec))
    
    #Intensity over the cycle - for arrival of customers
    
    #Pop_Groups <- 1

    IntensityFunc <- get.Intensity.function(Pop_Groups,number_of_hours_open_per_day=12,NumberperHour=c(30,60,60,90,120,120,90,90,60,90,60,30)*50)

    #What type of customer is arriving? - Are they already infected? Are they wearing a mask? Are they more vulnerable? What is their order difficulty? (E.g. size of order.) Will they leave straight away (due to queue length)?

    #Run a cycle - check the revenue, calculate the profit and look at number of new people infected as a result of use of the service.

    if(Multiple){
      Queue_Simulator <- run.Queues(UnsafeInteractionRate,Service_Rate,PopMatrix,IntensityFunc,number_of_hours_open_per_day,Number_Stations,Total_Number_Servers,Queue_Capacity,p_CC,p_CS,p_SC,p_SS,random_assign=FALSE,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)
    }
    else{
      Queue_Simulator <- run.Queue(UnsafeInteractionRate,Service_Rate,PopMatrix,IntensityFunc,number_of_hours_open_per_day,Number_Stations,Total_Number_Servers,Queue_Capacity,p_CC,p_CS,p_SC,p_SS,random_assign=FALSE,Mask_Infection_Suppressor_Factor,IncubationTime,NearestNeighbour)#,Do_Servers_Wear_Masks)
    }

    Profit <- Queue_Simulator[[1]]*V - Fixed_Cost - number_of_hours_open_per_day*7*Number_Stations*Wage

    Profit_Vector <- c(Profit_Vector,Profit)

    Additional_Infected <- Queue_Simulator[[5]]

    Additional_Infected_Vector <- c(Additional_Infected_Vector,Additional_Infected)
  }
  Profit_Vector_quantiles <- quantile(Profit_Vector,c(0.025,0.05,0.5,0.95,0.975))
  Additional_Infected_Vector_quantiles <- quantile(Additional_Infected_Vector,c(0.025,0.05,0.5,0.95,0.975))
  
  FinalList <- list(Profit_Vector,Additional_Infected_Vector,Profit_Vector_quantiles,Additional_Infected_Vector_quantiles)
  return(FinalList)
}
