rankall <- function(outcome,num="best") {
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # check if state and outcome are valid
  states<-unique(data$State)
  possible_outcome<-c("heart attack","heart failure","pneumonia")
  
  if (!(outcome %in% possible_outcome)){
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
    mortality_rates_df<-subset(data, select=c(outcome,"Hospital.Name","State") )
    
    # Changing "Not Available" string to NA values
    
    mortality_rates_df[mortality_rates_df=="Not Available"] <- NA
    mortality_rates_df<-na.omit(mortality_rates_df)
    
    mortality_rates_df[,eval(outcome)]<-as.numeric(mortality_rates_df[,eval(outcome)])
    
    # ordering dataframe according rate
    mortality_rates_df<-mortality_rates_df[order(mortality_rates_df["State"],mortality_rates_df[[outcome]],mortality_rates_df["Hospital.Name"]),]
    
    # Create result dataframe
    
    #add ranks to df
    final_states<-unique(mortality_rates_df$State)
    #final_states<-order(final_states)
    result<-data.frame(matrix(nrow =length(final_states),ncol = 2))
    colnames(result)<-c('hospital','state')
    i<-1
    
    for(state in final_states){
      
      mortality_rates_df$rank[mortality_rates_df$State == state] <- seq_len(sum(mortality_rates_df$State == state))
      
      if(num=="best") {num<-1}
      
        if(sum(mortality_rates_df$State==state)<num){
          result[i,1]<-NA
          result[i,2]<-state
        }
       if(num=="worst"){
         last<-sum(mortality_rates_df$State == state)
         result[i,1]<-mortality_rates_df[which(mortality_rates_df$rank==last & mortality_rates_df$State == state)[1],"Hospital.Name"]
         result[i,2]<-state
       } 
       else{
         result[i,1]<-mortality_rates_df[which(mortality_rates_df$rank==num & mortality_rates_df$State == state)[1],"Hospital.Name"]
         result[i,2]<-state
       }
       
       i<-i+1
    }
    
    result
  }
  
}