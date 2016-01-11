rankall <- function(outcome, num = "best") {
        
        outcome3 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
        
        outcome3 <- outcome3[c(2, 7, 11, 17, 23)]
        ##check outco1me is valid
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome") 
        }
        
        ##Change column names
        colnames(outcome3) <-c("Name", "State", "heart attack", "heart failure", "pneumonia")
      
        
        #Convert outcome vector to numeric
        outcome3[,outcome] <- as.numeric(outcome3[,outcome])
        
        ##Order outcome3 according to State and outcome
        ordered <- outcome3[order(outcome3$State, outcome3[,outcome],outcome3$Name),]
        ##Remove NA Values
        ordered <- na.omit(ordered)
       
        
        #Split into separate lists for each state
        split <- split(ordered, ordered$State)
        
        
        ##Populate Empty Matrix
        DF <- matrix(nrow=54, ncol=2)

        for(i in 1:length(split)){
                 if(num=="best"){
                        row <-1
                }else if(num =="worst"){
                        row <- nrow(split[[i]])
                }else {
                        row <- num
                }
                
                DF[i,1] <- split[[i]][row, 1]
                DF[i,2] <- split[[i]][row, 2]
        
               
                colnames(DF) <- c("Hospital", "State")    
        }
        return(as.data.frame(DF))
        
        ##For given rank and outcome, fills empty data frame 
        ##with correct hospital name and respective state
       
               
}

