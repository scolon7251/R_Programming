best2 <- function(state, outcome) {
        ##Transform data frame to only use relevant columns
        outcome3 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses="character")
        outcome3 <- outcome3[c(2, 7, 11, 17, 23)]
        ##change column names
        names(outcome3)[1] <- ("Name")
        names(outcome3)[3] <- ("heart attack")
        names(outcome3)[4] <- ("heart failure")
        names(outcome3)[5] <- ("pneumonia")
        
        ##check that state and outcome are valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome") 
        }
        if(!state %in% outcome3$State) {
                stop("invalid state")
        }
        ##subset according to state and outcome; change                 
        ##to numeric w/o NA's
        state_subset <- subset(outcome3, outcome3$State == state)
        outcome_subset <- (state_subset[,outcome])
        outcome_subset <- sapply(outcome_subset, as.numeric)
        
        index <- which.min(outcome_subset) 
        return(state_subset[index,1])
        
}       







