nisse <- function(name,mypin){
    ## 2015
    # all <- c("Jonas","Liv","Jens","Christoffer","Aniella",
    #          "William","Josephine","Anders","Jasmin","Emilie",
    #          "Morten Gade","Morten Skjalm","Kristina")
    
    ## 2016
    all <- c("Jonas","Liv","Eva","Christoffer","Aniella",
             "Matty","Nicoline","Anders","Jasmin","Emilie",
             "Morten Østergaard Gade","Morten Skjalm Madsen","Belinda")
    
    # startdate <- "2015-11-18" ## 2015
    startdate <- as.Date("2016-11-17") ## 2016
    
    
    ALL <- toupper(all)
    NAME <- toupper(name)
    
    ## Capitalize first letter and make all other letters lower case
    name.out <- {
        s <- strsplit(name, " ")[[1]]
        paste0(toupper(substring(s, 1,1)), substring(s, 2),
              collapse=" ")
    }
    
    if(!any(grepl(NAME, ALL))){
        cat(paste0("Sorry, ", name.out,
                  ", you are not on the Secret Santa list. :( \n",
                  "Perhaps you spelled your name wrong?"))
        return(cat("\n"))
    }
    
    
    # startdate <- "2015-11-18" ## 2015
    startdate <- as.Date("2016-11-17") ## 2016
    
    set.seed(507)
    pincode <- floor(runif(length(all),1e3,1e4-1))
    ## Make sure pincodes are not duplicated
    while(any(duplicated(pincode))){
        pincode <- floor(runif(length(all),1e3,1e4-1))
    }
    
    # Set seed for replication
    set.seed(18)
    
    
    msrpw <- "numeric"
    msrnm <- "santa claus"
    
    # Vector with element entry numbers
    all.seq <- seq_along(all)
    
    ## Pair the elements from the 'all' list to another element
    paired.with <- sample(all.seq)
    
    ## Make sure nobody is paired with themselves
    while(any(paired.with == all.seq))
        paired.with <- sample(all.seq)
    
    # If we have not yet started, tell the user!
    if(Sys.Date() > startdate & mypin != msrpw){
        cat(paste0(name.out, ", we don't start until ",
                   format(startdate,"%A, %dth of %B %Y"), "!"))
        return(cat("\n"))
    }
    
    all.listed.cap <- as.list(ALL)
    
    if(name == msrnm & mypin == msrpw) {
        cat(paste0(all," was the Secret Santa of ", all[paired.with],
                  "!\n"))
    } else if(!is.numeric(mypin)) {
        return(cat("Password must be numeric.\n"))
    } else if(any(suppressWarnings(as.integer(mypin) == 
                                   pincode[grep(NAME,
                                                all.listed.cap)]))){
        ## Take only the name that matches with the entered pincode
        paired.with.pin <- which(
            grep(NAME, all.listed.cap) == match(TRUE,pincode == mypin))
        
        cat(paste(all[grep(NAME,all.listed.cap)][paired.with.pin],
                  ", you are the Secret Santa of ",
                  all[paired.with][
                      grep(NAME,all.listed.cap)][paired.with.pin],
                  "!",sep = ""))
    } else {
        cat("Pincode wrong.\n")
    }
}
