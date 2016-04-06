## Function to calculate the payback period (year) for a vector of cash flows -----


payback <- function(CF, number.periods.in.year = 1){
  paid <- ( cumsum(CF) > 0  ) * 1
  first.period.paid <- seq_along(paid)[paid == 1][1]
  if  ( is.na(first.period.paid) ) { return(cat("There is no payback\n")) 
  } else {
      payback.int <- first.period.paid - 1
      
  }



}
