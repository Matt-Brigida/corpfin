### IRR Function:  Takes a vector of payments and returns a list which includes the internal rate of return ($IRR) and possible word of warning ($beware) ----

irr <- function(x, period = 1, starting.value = .1){

### This should detect the number of sign changes.  Should correctly not warn if there are many negative cash flows (so long as there is only 1 change in sign).
    
    irr.func <- function(r){ ( sum(x / (1 + r)^{0:(length(x)-1)}) )^2 }
    result <- optim(par = starting.value, fn = irr.func, method = "Brent", lower = -1000000, upper = 1000000)

    ## detecting number of sign changes
    x.ge.0 <- 1 * (x >= 0)
    changes <- diff(x.ge.0)
    changes <- changes * changes
    num.changes <- sum(changes)
    
    if( num.changes > 1) {

        statement <- "Your cash flows change more than once -- so you may have multiple IRRs. This function will only return the first IRR it finds. To find the others, you can try different starting values.  However, note the IRR does not make sense if the signs change more than once (try Modified IRR or NPV)."
        value <- period * result$par
        return(list(beware = statement, IRR = value))

    } else {

        return(list(IRR = period * result$par))

    }
}
