get.Intensity.function <- function(NumGroups,number_of_hours_open_per_day = 12,NumberperHour=rep(60,HoursperDay)){
  HoursperDay <- number_of_hours_open_per_day
  HoursoftheWeek <- 1:(7*HoursperDay) 
  WhoThere <- HoursoftheWeek %% NumGroups + 1
  Intensity_Matrix <- matrix(c(rep(NumberperHour,7),WhoThere),nrow=2,byrow=TRUE)
  return(Intensity_Matrix)
}