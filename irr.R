## IRR Function:  Takes a vector of payments and returns the internal rate of return ----

irr <- function(x, period = 1, starting.value = .1){
    ## this wont catch cases where the sign changes 4 or 5 ... times.  Can probably use a cumulative product to check for any positive values.  
    if(prod(x) > 0) {
        return(cat("The signs of your cash flows change more than once -- therefore you may have two IRRs. \nThis function will only return the first IRR it finds. \n"))

    } else {
        
        irr.func <- function(r){ ( sum(x / (1 + r)^{0:(length(x)-1)}) )^2 }
        result <- optim(par = starting.value, fn = irr.func, method = "Brent", lower = -1000000, upper = 1000000)

        return(period * result$par)

    }
}
