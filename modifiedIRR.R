### Modified IRR (MIRR) Function:  Takes a vector of payments and returns the MIRR by ----

mirr <- function(x, period = 1, starting.value = .1, discount.rate = 0.1, investment.rate = 0.05){

    ## move cash flows
    ## negative
    cf.neg <- (x < 0) * x
    ## discounted
    pv.cf.neg <- cf.neg / (1 + discount.rate)^{0:(length(x)-1)}
    pv <- sum(pv.cf.neg)

    ## positive
    cf.pos <- (x > 0) * x
    fv.cf.pos <- cf.pos * (1 + investment.rate)^{0:(length(x)-1)}
    fv <- sum(fv.cf.pos)

    mirr.per.period <- ( fv / abs(pv) )^{1 / (length(x))} - 1

    return( period * mirr.per.period )

    

    ## modified.cf <- rep(0, length(x))
    ## modified.cf[1] <- pv.cf.neg
    ## modified.cf[length(x)] <- fv.cf.pos
    
    ## mirr.func <- function(r){ ( sum( modified.cf / (1 + r)^{0:(length(x)-1)}) )^2 }
    ## result <- optim(par = starting.value, fn = mirr.func, method = "Brent", lower = -1000000, upper = 1000000)

    ## return(period * result$par)

}
