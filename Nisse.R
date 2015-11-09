nisse <- function(name,mypin){

    all <- c("Jonas","Liv","Jens","Christoffer","Aniella",
             "William","Josephine","Anders","Emilie",
             "Morten Skjalm","Kristina")
    
    url <- getURL("https://raw.githubusercontent.com/ATherkel/Tietgen/master/Pincodes.R",
              ssl.verifypeer=0L, followlocation=1L)
    eval(parse(text=url))
    
    # Set seed for replication
    set.seed(100)
    # How to handle if/else commands
    msrpw <- "numeric"
    
    # Vector with element entry numbers
    all.seq <- seq_along(all)
    
    ## Pair the elements from the 'all' list to another element
    paired.with <- sample(all.seq)
    
    ## If any element is paired with itself, redo the pairing
    while(any(paired.with == all.seq))
        paired.with <- sample(all.seq)
    
    # print(all[paired.with][all == name])
    
    #cat("Type your 4 digit password to verify yourself")
    #line <- readline()
    line <- mypin
    
    if(line == msrpw | 
           suppressWarnings(as.integer(line) == pincode[which(name == all)])){
        
        if(line == msrpw) name <- all 

        print(paste(name, ", you are the elf of ",
                    all[paired.with][all == name],"!",sep = ""))
    } else {
        cat("Pincode wrong.")
    }
}

