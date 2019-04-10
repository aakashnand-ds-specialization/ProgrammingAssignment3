best <- function(state, outcome) {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospitals<-c()
  # check if state and outcome are valid
  states<-unique(data$State)
  possible_outcome<-c("heart attack","heart failure","pneumonia")
  
  if (!(outcome %in% possible_outcome) || !(state %in% states)){
    message("please check the parameters again.")
  }
  else{
    
    # renaming column names for convinience.
    colnames(data)[11]<-"heart attack"
    colnames(data)[17]<-"heart failure"
    colnames(data)[23]<-"pneumonia"
    
    #subsetting dataframe using required columns only
    data<-subset(data,select = c("State","Hospital.Name","heart attack","heart failure","pneumonia"))
    
    # further subsetting dataframe to given outcome and state
    min_mortality_df<-subset(data, data$State==state, select=c(outcome,"Hospital.Name","State") )
    
    # Changing "Not Available" string to NA values

    min_mortality_df[min_mortality_df=="Not Available"] <- NA
    
    # finding best hospital
    min_mortality <- min(as.numeric(min_mortality_df[,eval(outcome)]),na.rm = TRUE)
    
    #print(min_mortality)
    best_hospital<-subset(min_mortality_df,min_mortality_df[,eval(outcome)]==min_mortality,select=c("Hospital.Name"))
    
    #handling ties
    best_hospital<-best_hospital[with(best_hospital,order("Hospital.Names")),]
    
    # returning best hospital
    best_hospital
  }
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}