corr <- function(directory, threshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        empty<- c()
        for (i in 1:332) {
                current_file <- read.csv(files[i])
                row <- sum(complete.cases(current_file)) 
           
                if (row > threshold) {
                        s <- current_file[2]
                        n <- current_file[3]
                        cr<-cor(s, n, use ="complete.obs")
                        empty <- append(empty, cr)
                            }
               
        }
    empty
}