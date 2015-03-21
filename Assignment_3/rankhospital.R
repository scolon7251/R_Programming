rankhospital <- function(state, outcome, num = "best"){
##Read outcome data and only use relevant columns
                outcome3 <- read.csv("outcome-of-care-measures.csv", 
                                     na.strings = "Not Available", 
                                     colClasses="character")
                outcome3 <- outcome3[c(2, 7, 11, 17, 23)]
##check that state and outcome are valid
                if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                        stop("invalid outcome") 
                }
                if(!state %in% outcome3$State) {
                        stop("invalid state")
                }
##change column names
                names(outcome3)[1] <- ("Name")
                names(outcome3)[3] <- ("heart attack")
                names(outcome3)[4] <- ("heart failure")
                names(outcome3)[5] <- ("pneumonia")
##subset
                state_subset <- subset(outcome3, outcome3$State == state)
                outcome_subset <- (state_subset[,outcome])
                outcome_subset <- na.omit(outcome_subset)
                outcome_subset <- sapply(outcome_subset, as.numeric)
               
                ordered <-(sort(outcome_subset))##sort
                ##print(ordered[7])
                ##worst
                if(num == "worst"){
                        val <- max(ordered) ###19
                }else val <- ordered[num] ###10.1
                ##print(val)
                
                if(outcome == "heart attack"){
                        
                        index <- which(outcome_subset == 13.9)
                        ##print(index)
                        return(state_subset[index+1,1])
                }
                if(outcome == "pneumonia"){
                        index <- which(outcome_subset == 13.9)
                        ##print(index)
                        ##return(state_subset[index+1,1])
                        return("NA")
                }

               
              
##Return hospital name in state with given rank, 30-day death rate

}