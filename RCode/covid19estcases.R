# This function estimates the actual number of COVID-19 infected
# people for a given number of deaths under the assumption of a
# constant death and infection rate

# dr: Death rate
# ir: Infection rate
# tl: Time lapse for recovery or death in days
# xx: Target number of deaths for prediction
# nd: Number of deaths
# ni: Number of infected at time i
# nt: Total number of infected
# ti: Time i (days)

foo <- function(dr, ir, tl, xx) {
  
  ni <- 1
  ti <- 1
  nd <- 0
  nt <- 1

  while (nd < xx) {
    ti <- ti + 1
    nt <- sum(ni[max(0, ti - tl):(ti - 1)])
    ni <- c(ni, nt * ir)
    
    if (ti > (tl - 1)) {
      nd <- sum(ni[1:(ti - tl + 1)] * dr)
    }
  }
  
  list(infected_total = sum(ni),
       infected_now  = sum(ni[max(0, ti - tl + 1):ti]),
       time = ti,
       number_deaths = nd)
}

# Peru estimation

foo(.01, .15, 18, 500)

