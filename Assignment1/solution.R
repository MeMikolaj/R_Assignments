book.total_volumes <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The total volume in the book.


    askk <- sum(book$ask["size"])
    bidd <- sum(book$bid["size"])
    bookCopy <- list(ask = (askk[1]), bid = (bidd[1]))
    return(bookCopy)
}

book.best_prices <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   A list with "ask" and "bid", the values of which are the best prices in
    #       the book.

    askk <- min(book$ask["price"])
    bidd <- max(book$bid["price"])
    bookCopy <- list(ask = (askk[1]), bid = (bidd[[1]]))
    return(bookCopy)
}

book.midprice <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:


    askk <- min(book$ask["price"])
    bidd <- max(book$bid["price"])
    return((askk[[1]] + bidd[[1]])/2)

}

book.spread <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The spread of the book.


    askk <- min(book$ask$`price`)
    bidd <- max(book$bid$`price`)
    return(askk[[1]] - bidd[[1]])
}

book.add <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid", "side", "price" and "size" entries.
    #
    # Returns:
    #   The updated book.


    oid   <- as.character(message$oid)
    side  <- as.character(message$side)
    price <- as.numeric(message$price)
    size  <- as.numeric(message$size)

    book = book.sort(book)

    if(side == "S"){
      newList <- list(as.character(oid), as.numeric(price), as.numeric(size))
      if(nrow(book$ask) == 0){
        book$ask[1,] <- newList
      } else{
        book$ask = rbind(book$ask, newList, stringsAsFactors=FALSE)
      }

      book = book.sort(book)
    }
    if(side == "B"){
      newList <- list(oid, price, size)
      if(nrow(book$bid) == 0){
        book$bid[1,] <- newList
      } else{
        book$bid = rbind(book$bid, newList, stringsAsFactors=FALSE)
      }
      book = book.sort(book)
    }

    while((is.na(book.spread(book)) == FALSE) && (book.spread(book) <= 0) && (is.infinite(min(book$ask$`price`)) == FALSE) && (is.infinite(max(book$bid$`price`)) == FALSE)){

      if(book$ask[1,3][[1]] == book$bid[1,3][[1]]){
        book$ask <- book$ask[-1,]
        book$bid <- book$bid[-1,]
      } else if(book$ask[1,3][[1]] > book$bid[1,3][[1]]){
        book$ask[1,3] <- book$ask[1,3][[1]] - book$bid[1,3][[1]]
        book$bid <- book$bid[-1,]
      } else if(book$ask[1,3][[1]] < book$bid[1,3][[1]]){
        book$bid[1,3] <- book$bid[1,3][[1]] - book$ask[1,3][[1]]
        book$ask <- book$ask[-1,]
      }
      book = book.sort(book)
    }

    return(book)
}

book.reduce <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid" and "amount".
    #
    # Returns:
    #   The updated book.

    rSizeA <- book$ask[book$ask$`oid` == message$oid,3]
    rSizeB <- book$bid[book$bid$`oid` == message$oid,3]
    amount <- as.numeric(message$amount)
    if(length(rSizeA) == 1){
      if(rSizeA <= amount){
        rowA <- which(book$ask["oid"][[1]] == message[[1]])
        book$ask <- book$ask[-rowA,]
      } else{
        book$ask[book$ask$`oid` == message[[1]],3] <- rSizeA - amount
      }

    } else if(length(rSizeB) == 1){
        if(rSizeB <= amount){
          rowB <- which(book$bid["oid"][[1]] == message[[1]])
          book$bid <- book$bid[-rowB,]
        } else{
          book$bid[book$bid$`oid` == message[[1]],3] <- rSizeB - amount
        }
    }

    return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
    # See handout for instructions

    if(sum(book$ask["size"]) == size){
      return(NA)
    }

    sizee <- size
    bookCopy2 = book

    expectedPrice = 0

    while(nrow(bookCopy2$ask) > 0){
      bookCopy3 = book
      message <- list(oid = "uniqueOid", side = "B", price = bookCopy2$ask[1,2][[1]], size = sizee)
      bookCopy3 = book.add(bookCopy3, message)
      expectedPrice = expectedPrice + book.midprice(bookCopy3)
      bookCopy2$ask = bookCopy2$ask[-1,]
    }
    expectedPrice = expectedPrice/(nrow(book$ask))


    return(expectedPrice)
}



book.extra2 <- function(book, size) {
    # See handout for instructions


    if(sum(book$ask["size"]) == size){
      return(NA)
    }

    expectedPrice = 0
    sizee <- size


    bookCopy3 = book
    message <- list(oid = "uniqueOid", side = "B", price = book$ask[1,2][[1]], size = sizee)
    bookCopy3 = book.add(bookCopy3, message)
    expectedPrice = expectedPrice + book.midprice(bookCopy3)
    bookCopy3 = book
    message <- list(oid = "uniqueOid", side = "B", price = book$ask[nrow(book$ask),2][[1]], size = sizee)
    bookCopy3 = book.add(bookCopy3, message)
    expectedPrice = expectedPrice + book.midprice(bookCopy3)


    expectedPrice = expectedPrice/2


    return(expectedPrice)

}



book.extra3 <- function(book) {
    # See handout for instructions

    bookCopy = book
    thankYou <- sum(book$ask["size"]) - 1
    expectedValue = 0
    x <- c(1:thankYou)
    for(val in x){
      dang = val
      while(dang > 0){
        if(dang > bookCopy$ask[1,3][[1]]){
          dang <- dang - bookCopy$ask[1,3][[1]]
          message <- list(oid = bookCopy$ask[1,1][[1]], amount = bookCopy$ask[1,3][[1]])
          bookCopy = book.reduce(bookCopy, message)
          bookCopy = book.sort(bookCopy)
        } else{
          dang = 0
        }
      }
      expectedValue = expectedValue + book.midprice(bookCopy)
      bookCopy = Book
    }
    expectedValue = expectedValue/thankYou
    return(expectedValue)
}

book.extra4 <- function(book, k) {
    # See handout for instructions

    if(nrow(book$ask) == 0){
      return(0)
    }

    bookCopy = book
    percentToNumber = k/100
    allowed = book.midprice(book)
    allowed = allowed + allowed*percentToNumber
    whatSize = 0

    holdSize = bookCopy$ask[1,3][[1]]

    if(nrow(bookCopy$ask) > 1){
      bookCopy$ask <- bookCopy$ask[-1,]
    }

    while((nrow(bookCopy$ask) > 1) && (book.midprice(bookCopy) <= allowed)){

      whatSize = whatSize + holdSize
      bookCopy = book.sort(bookCopy)
      holdSize = bookCopy$ask[1,3][[1]]
      bookCopy$ask <- bookCopy$ask[-1,]
    }


    whatSize = whatSize + holdSize - 1
    return(whatSize)


}
