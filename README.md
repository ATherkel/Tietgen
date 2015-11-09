nisse <- function(name){

    all <- c("Jonas","Liv","Jens","Christoffer","Aniella",
             "William","Josephine","Anders","Emilie",
             "Morten Skjalm","Kristina")
    
    source("http://dl.dropbox.com/s/ar8wjgt0mdhl9cu/Pincodes.R",
           local = TRUE)
    
    set.seed(100)
    
    masterpassword <- "Forsikring"
    
    all.seq <- seq_along(all)
    
    ## Pair the elements from the 'all' list to another element
    paired.with <- sample(all.seq)
    
    ## If any element is paired with itself, redo the pairing
    while(any(paired.with == all.seq))
        paired.with <- sample(all.seq)
    
    # print(all[paired.with][all == name])
    
    cat("Type your 4 digit password to verify yourself")
    line <- readline()
    
    if(line == masterpassword | 
           suppressWarnings(as.integer(line) == pincode[which(name == all)])){
        
        if(line == masterpassword) name <- all 

        print(paste(name, ", you are the elf of ",
                    all[paired.with][all == name],"!",sep = ""))
    } else {
        cat("Pincode wrong.")
    }
}

