get_mortality_data <- function(state) {
  oocm <- read.csv('../../data/outcome-of-care-measures.csv')
  state_data <-oocm[which(oocm$State == state),]
  
  hospital_num <- length(unique(state_data$Hospital.Name))
  data = list(hospital_num)
  
  column_names = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                   "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  for (col_name in column_names) {
    mortality <- state_data[,col_name]
    mortality <- lapply(mortality, function(x) as.numeric((gsub("[^0-9.-]", NA, x))))
    mortality = unlist(mortality)
    mortality_min <- min(mortality, na.rm=TRUE)
    mortality_max <- max(mortality, na.rm=TRUE)
    data[[length(data)+1]] <- mortality_min
    data[[length(data)+1]] <- mortality_max
  }
  
  names(data) <- c("Hospital Count", 
                   "Heart Attack Mortality Min", "Heart Attack Mortality Max", 
                   "Heart Failure Mortality Min", "Heart Failure Mortality Max", 
                   "Pneumonia Mortality Min", "Pneumonia Mortality Max")
  return (data)
}
