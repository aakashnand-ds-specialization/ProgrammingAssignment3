rankhospital <- function(state, outcome,num="best") {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
    mortality_rates_df<-subset(data, data$State==state, select=c(outcome,"Hospital.Name","State") )
    
    # Changing "Not Available" string to NA values
    
    mortality_rates_df[mortality_rates_df=="Not Available"] <- NA
    mortality_rates_df<-na.omit(mortality_rates_df)
    
    mortality_rates_df[,eval(outcome)]<-as.numeric(mortality_rates_df[,eval(outcome)])
    # ordering dataframe according rate
    mortality_rates_df<-mortality_rates_df[order(mortality_rates_df[[outcome]],mortality_rates_df["Hospital.Name"]),]
    
    
    #add ranks to df
    mortality_rates_df$rank<-seq.int(nrow(mortality_rates_df))
    
    # get index for best and worst case
    if(num=="best") {num<-1} else if(num=="worst") {num<-nrow(mortality_rates_df)}
    
    if(num>nrow(mortality_rates_df)) return(NA)
    # returning best hospital
    mortality_rates_df[which(mortality_rates_df$rank==num),"Hospital.Name"]
  }
  
}