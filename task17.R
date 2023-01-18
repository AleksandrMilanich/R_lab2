StateInfo <- function(state) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state <-data[which(data$State == state),]
  
  
  HeartAttackMort<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
  HeartAttackMort <- lapply(HeartAttackMort, function(x) gsub("[^0-9.-]", NA, x))
  HeartAttackMort <- lapply(HeartAttackMort, function(x) as.numeric(x))
  HeartAttackMort <- HeartAttackMort[!is.na(HeartAttackMort)]
  
  HeartAttackMin <- min(unlist(HeartAttackMort), na.rm=TRUE)
  HeartAttacMax <- max(unlist(HeartAttackMort), na.rm=TRUE)
  
  
  HeartFailureMort<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
  HeartFailureMort <- lapply(HeartFailureMort, function(x) gsub("[^0-9.-]", NA, x))
  HeartFailureMort <- lapply(HeartFailureMort, function(x) as.numeric(x))
  HeartFailureMort <- HeartFailureMort[!is.na(HeartFailureMort)]
  
  HeartFailureMin <- min(unlist(HeartFailureMort), na.rm=TRUE)
  HeartFailureMax <- max(unlist(HeartFailureMort), na.rm=TRUE)
  
  
  PneumoniaMort<-state[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",]
  PneumoniaMort <- lapply(PneumoniaMort, function(x) gsub("[^0-8.-]", NA, x))
  PneumoniaMort <- lapply(PneumoniaMort, function(x) as.numeric(x))
  PneumoniaMort <- PneumoniaMort[!is.na(PneumoniaMort)]
  
  PneumoniaMin <- min(unlist(PneumoniaMort), na.rm=TRUE)
  PneumoniaMax <-max(unlist(PneumoniaMort), na.rm=TRUE)
  
  
  Hcount <- length(unique(state$Hospital.Name))
  
  
  values <- list(HeartAttackMin, HeartAttacMax, HeartFailureMin,HeartFailureMax,PneumoniaMin,PneumoniaMax, Hcount)
  names(values) <- c("Heart Attack Mortality Min", "Heart Attack Mortality Max", "Heart Failure Mortality Min", 
                     "Heart Failure Mortality Max", "Pneumonia Mortality Min", "Pneumonia Mortality Max", "Hospital Count")
  return(values)
}


state <- "LA"
StateInfo(state)

