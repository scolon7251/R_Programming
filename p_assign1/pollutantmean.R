pollutantmean <- function(directory, pollutant, id = 1:332) {
        files <- list.files(directory, full.names = TRUE)
        dat <- data.frame()
        for (i in 1:332){
                dat <- rbind(dat, read.csv(files[i]))                  
        }
        View(tail(dat))
        id_subset <- dat[which(dat[, "ID"] %in% id), ]
        mean(id_subset[, pollutant], na.rm =TRUE)
                     
}

       

      

