nisse <- function(name,mypin){

    all <- c("Jonas","Liv","Jens","Christoffer","Aniella",
             "William","Josephine","Anders","Emilie",
             "Morten Skjalm","Kristina")
    
    #url <- getURL("https://raw.githubusercontent.com/ATherkel/Tietgen/master/Pincodes.R",
    #          ssl.verifypeer=0L, followlocation=1L)
    #eval(parse(text=url))
    
    set.seed(1)
    pincode <- floor(runif(length(all),1e3,1e4-1))

    
    # Set seed for replication
    set.seed(100)
    # How to handle if/else commands
    msrpw <- "numeric"
    
    # Vector with element entry numbers
    all.seq <- seq_along(all)
    
    ## Pair the elements from the 'all' list to another element
    paired.with <- sample(all.seq)
    
    ## Make sure nobody is paired with themselves
    while(any(paired.with == all.seq))
        paired.with <- sample(all.seq)
    
    # print(all[paired.with][all == name])
    
    #cat("Type your 4 digit password to verify yourself")
    #line <- readline()
    line <- mypin
    
    if(line == msrpw | 
           suppressWarnings(as.integer(line) == pincode[which(name == all)])){
        if(line == msrpw) name <- all 
        cat(paste(name, ", you are the elf of ",
                    all[paired.with][all == name],"!",sep = ""))
    } else {
        cat("Pincode wrong.")
    }
}

