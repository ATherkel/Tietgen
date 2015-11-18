nisse <- function(name,mypin){
    
    all <- c("Jonas","Liv","Jens","Christoffer","Aniella",
             "William","Josephine","Anders","Jasmin","Emilie",
             "Morten Gade","Morten Skjalm","Kristina")
    ## Capitalize first letter and make all other letters lower case
    name.out <- {
        s <- strsplit(name, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    
    if(!any(grepl(toupper(name), toupper(all)))){
        cat(paste("Sorry, ", name.out,
                  ", you are not on the Secret Santa list. :(",
                  sep = ""))
        return(cat("\n"))
    }
    
    #url <- getURL("https://raw.githubusercontent.com/ATherkel/Tietgen/master/Pincodes.R",
    #          ssl.verifypeer=0L, followlocation=1L)
    #eval(parse(text=url))
    
    set.seed(1042)
    pincode <- floor(runif(length(all),1e3,1e4-1))
    ## Make sure pincodes are not duplicated
    while(any(duplicated(pincode))){
        pincode <- floor(runif(length(all),1e3,1e4-1))
    }
    
    # Set seed for replication
    set.seed(18)
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
    
    
    # If we have not yet started, tell the user!
    if(Sys.Date() < "2015-11-18"){
        cat(paste(name.out, ", we don't start until the 25th November 2015!",
                  sep = ""))
        return(cat("\n"))
    }
    
    all.listed <- as.list(all)
    all.listed.cap <- lapply(seq(length(all.listed)), function(x){
        sapply(seq(length(all.listed[[x]])),function(y) {
            toupper(all.listed[[x]][y])
        })
    })
    
    if(mypin == msrpw) {
        cat(paste(all," was the Secret Santa of ", all[paired.with],
                  "!\n",sep = ""))
    } else if(any(suppressWarnings(as.integer(mypin) == 
                                       pincode[grep(toupper(name),
                                                    all.listed.cap)]))){
        ## Take only the name that matches with the entered pincode
        paired.with.pin <- which(
            grep(toupper(name), all.listed.cap) == match(TRUE,pincode == mypin))
        
        cat(paste(all[grep(toupper(name),all.listed.cap)][paired.with.pin],
                  ", you are the Secret Santa of ",
                  all[paired.with][
                      grep(toupper(name),all.listed.cap)][paired.with.pin],
                  "!",sep = ""))
    } else {
        cat("Pincode wrong.")
    }
}
