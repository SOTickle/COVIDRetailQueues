get.Pop.Matrix <- function(G,P,specify=FALSE,Gvec=rep(P/G,G),Vul=rep(0.2,G),Safer=rep(0.4,G),Starter=rep(0.01,G),IncubationTime=3){
  #function to generate the Denominator Population which may use a specific service
  #first row gives everyone in the population a number, second row records which group they are in
  #third row gives a 1 if this person is particularly vulnerable to COVID-19, 0 otherwise
  #fourth row records if this person takes additional precautions like wearing a mask in the queue
  #fifth row records if this person has COVID-19 at the moment the service opens
  #sixth row records the time since infection if someone has been infected with covid-19
  
  numbers <- 1:P #numbers assigned to each member of the Denominator Population
  possible_groups <- 1:G #group numbers 1 to G
  if(specify==FALSE){
    average_number <- floor(P/G)
    groupings <- c()
    for(i in 1:G){
      groupings <- c(groupings,rep(i,average_number))
    }
    Pdash <- length(groupings)
    addon <- P - Pdash
    groupings <- c(groupings,rep(G,addon))
  }
  else{
    groupings <- c()
    for(i in 1:length(Gvec)){
      groupings <- c(groupings,rep(i,Gvec[i]))
    }
  }
  vulnerability_probabilities <- rep(0,P)
  Safer_probabilities <- rep(0,P)
  Starter_probabilities <- rep(0,P)
  for(i in 1:P){
    vulnerability_probabilities[i] <- Vul[groupings[i]]
    Safer_probabilities[i] <- Safer[groupings[i]]
    Starter_probabilities[i] <- Starter[groupings[i]]
  }
  vulnerability_indicators <- rbinom(P,1,vulnerability_probabilities)
  Safer_indicators <- rbinom(P,1,Safer_probabilities)
  Starter_indicators <- rbinom(P,1,Starter_probabilities)
  
  TimeSince <- IncubationTime*Starter_indicators
  
  FinalResult <- matrix(c(numbers,groupings,vulnerability_indicators,Safer_indicators,Starter_indicators,TimeSince),nrow=6,byrow=TRUE)
  return(FinalResult)
}