rankhospital <- function(state, outcome, num = "best"){
        
        ##Read outcome data and only use relevant columns
        outcome3 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
        outcome3 <- outcome3[c(2, 7, 11, 17, 23)]
        
        ##check that state and outcome are                 
        ##valid
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
        
        ##subset, remove NAs, and order according to outcome and State
        state_subset <- subset(outcome3, outcome3$State == state)
        state_subset[,outcome] <- as.numeric(state_subset[,outcome])
        state_subset <- na.omit(state_subset)
        state_subset<- state_subset[order(state_subset[,outcome], state_subset$Name),]
        
        ##Set best and worst
        Max <-which.max(state_subset[,outcome])
        
        if(num == "best"){
                return(state_subset[1,1])
        }else if(num == "worst"){
                return(state_subset[Max,1])
        }else {
                return(state_subset[num,1])
        }
}