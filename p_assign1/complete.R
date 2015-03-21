complete <- function(directory, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for (i in id) {
                current_file <- read.csv(files[i])
                row <- sum(complete.cases(current_file))
                add <-c(i, row)
                dat <- rbind(dat, add)
                
                
        }
        colnames(dat) <-c("id", "nobs")
        dat
        
}
