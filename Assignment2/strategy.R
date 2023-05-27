getOrders <- function(store, newRowList, currentPos, info, params) {

    ###########################################################################
    # You do not need to edit this next part of the code
    ###########################################################################
    allzero  <- rep(0,length(newRowList)) # used for initializing vectors
    pos <- allzero

    if (is.null(store))
        store <- initStore(newRowList)
    else
        store <- updateStore(store, newRowList)
    ###########################################################################

    ###########################################################################
    # This next code section is the only one you
    # need to edit for getOrders
    #
    # The if condition is already correct:
    # you should only start computing the moving
    # averages when you have enough (close) prices
    # for the long moving average
    ###########################################################################
    if (store$iter > params$lookbacks$long) {
        # ENTER STRATEGY LOGIC HERE

        # remember to only consider the series in params$series
        for(i in params$series){


          pos[i] = getPosSignFromTMA(getTMA(store$cl[[i]], list(short=params$lookbacks$short, medium=params$lookbacks$medium, long=params$lookbacks$long))) * getPosSize(newRowList[[i]]$Close)
        }
        # You will need to get the current_close
        # either from newRowList or from store$cl



        # You will also need to get prices
        # from store$cl

        # With these you can use getTMA, getPosSignFromTMA
        # and getPosSize to assign positions to the vector pos
    }
    ###########################################################################

    ###########################################################################
    # You do not need to edit the rest of this function
    ###########################################################################

    marketOrders <- -currentPos + pos

    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################
checkE01 <- function(prices, lookbacks) {
    # Return FALSE if lookbacks contains named elements short, medium, and long
    # otherwise return TRUE to indicate an error

    names <- c("short","medium","long")
    return(!(all(names %in% names(lookbacks))))
}
checkE02 <- function(prices, lookbacks) {
    # Return FALSE if all the elements of lookbacks are integers (as in the R
    # data type) otherwise return TRUE to indicate an error
    return(!(all(sapply(lookbacks, is.integer) == TRUE)))

}
checkE03 <- function(prices, lookbacks) {
    # Return FALSE if lookbacks$short < lookbacks$medium < lookbacks$long
    # otherwise return TRUE to indicate an error

    df <- (lookbacks$short < lookbacks$medium) && (lookbacks$medium < lookbacks$long)

    return(!df)

}
checkE04 <- function(prices, lookbacks) {
    # Return FALSE if prices is an xts object, otherwise return TRUE to
    # indicate an error
    return(!(is.xts(prices)))
}
checkE05 <- function(prices, lookbacks) {
    # Return FALSE if prices has enough rows to getTMA otherwise return TRUE
    # to indicate an error
    return(!(nrow(prices) >= lookbacks$long))
}
checkE06 <- function(prices, lookbacks) {
    # Return FALSE if prices contains a column called "Close" otherwise return
    # TRUE to indicate an error
    for(i in names(prices)){
      if(i == "Close"){
        return(FALSE)
      }
    }
    return(TRUE)
}
###############################################################################
# You should not edit allChecks

atLeastOneError <- function(prices, lookbacks) {
    # return TRUE if any of the error checks return TRUE
    ret <- FALSE
    ret <- ret | checkE01(prices,lookbacks)
    ret <- ret | checkE02(prices,lookbacks)
    ret <- ret | checkE03(prices,lookbacks)
    ret <- ret | checkE04(prices,lookbacks)
    ret <- ret | checkE05(prices,lookbacks)
    ret <- ret | checkE06(prices,lookbacks)
    return(ret)
}

###############################################################################

getTMA <- function(prices, lookbacks, with_checks=FALSE) {

    # prices and lookbacks should pass (return FALSE) when used with
    # the 6 checks, as tested in the following call to allChecks that
    # you should not edit
    if (with_checks)
        if (atLeastOneError(close_prices, lookbacks))
            stop('At least one of the errors E01...E06 occured')


    shortValue  <- sum(tail(prices$Close, lookbacks$short))/lookbacks$short
    mediumValue <- sum(tail(prices$Close, lookbacks$medium))/lookbacks$medium
    longValue   <- sum(tail(prices$Close, lookbacks$long))/lookbacks$long
    ret <- list(short=shortValue, medium=mediumValue, long=longValue)

    # You need to replace the assignment to ret so that the returned object:
    #    - is a list
    #    - has the right names (short, medium, long), and
    #    - contains numeric and not xts objects
    #    - and contains the correct moving average values, which should
    #      have windows of the correct sizes that all end in the
    #      same period, be the last row of prices

    return(ret)
}

getPosSignFromTMA <- function(tma_list) {
    # This function takes a list of numbers tma_list with three elements
    # called short, medium, and long, which correspond to the SMA values for
    # a short, medium and long lookback, respectively.

    # Note that if both this function and getTMA are correctly implemented
    # then the following should work with correct input arguments:
    # getPositionFromTMA(getTMA(prices,lookbacks))

    # This function should return a single number that is:
    #       -1 if the short SMA < medium SMA < long SMA
    #        1 if the short SMA > medium SMA > long SMA
    #        0 otherwise
    if((tma_list$short < tma_list$medium) && (tma_list$medium < tma_list$long)){
      return(-1)
    }
    else if((tma_list$short > tma_list$medium) && (tma_list$medium > tma_list$long)){
      return(1)
    }
    else {
      return(0)
    }

}

getPosSize <- function(current_close,constant=5000) {
    # This function should return (constant divided by current_close)
    # rounded down to the nearest integer
    return(floor(constant/current_close))
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x,y) rbind(x,y$Close),clStore,newRowList,SIMPLIFY=FALSE)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=1,cl=initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList)
  return(store)
}
