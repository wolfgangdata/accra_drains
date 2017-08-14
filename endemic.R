library(deSolve)
## Define as a function
SIRBD <- function(pars){

  ## Show parameters
  print(pars)

  ## Additional parameters
  times <- seq(from = 0, to = 5000, by = 1)              # we want to run the model for 3000 time steps
  yinit <- c(Susc = 0.9, Infected = 0.1, Recovered = 0) # this parameter sets the initial conditions

  ## below is the code for the actual model including the equations that you should recognize
  SIR_model <- function(times, yinit, pars){

    with(as.list(c(yinit,pars)), {

      dSusc      <- birth - beta*Infected*Susc                     - death*Susc
      dInfected  <-         beta*Infected*Susc - recovery*Infected - death*Infected
      dRecovered <-                              recovery*Infected - death*Recovered

      return(list(c(dSusc, dInfected, dRecovered)))})
  }

  ## run the ode solver for the function specified (function defined above is used)
  ## return the value of each compartment (Susc, Infected, Recovered) for each time step.
  results <- ode(func = SIR_model, times = times, y = yinit, parms = pars)
  results <- as.data.frame(results)

  ## Return result
  return(results)
}

##############################################################################

test.pars <- c(beta = 1/30, recovery = 1/30, death = 0.001, birth = 0.001)
results   <- SIRBD(test.pars)

##############################################################################
## Plotting
matplot(results[, 1], results[, 2:4], type="l", lty=1)
legend("topright", col=1:3, legend=c("S", "I", "R"), lwd=1)

##############################################################################
#simulation for time course infectious numbers;
simul.infect <- function(N.pop, beta, recovery, death, birth, days){
  R0 <- beta/(recovery + death)
  S_star <- 1/R0
  I_star <- death/beta*(R0-1)
  R_star <- 1-1/R0-death/beta*(R0-1)

  #prob new infectious cases;
  p1 <- beta * S_star * I_star;
  #prob recovery (fixed duration of infectious 30 days for right now);
  #p2 ???
  #prob infectious cases death;
  p3 <- death * I_star;

  dur_infect <- ceiling(1/recovery)
  res <- matrix(NA,nrow=days+30,ncol=dur_infect)
  res[1,1] <- rbinom(1,N.pop,p1)
  for (i in 2:(days+30)){
    res[i,1] <- rbinom(1,N.pop,p1)
    for (j in 2:dur_infect){
      if (!is.na(res[i-1,j-1])){
        res[i,j] <- max(res[i-1,j-1] - rbinom(1,res[i-1,j-1],p3),0)
      }
    }
  }
  return(res);
}

##############################################################################
#Pathogen shedding curves;
#amount of pathogen shed over the whole shedding period;
N.shed <- 10^7

shed <- function(N.shed, r, p, days){
  mat.shed <- matrix(NA, nrow = days, ncol=30)
  vec.p.shed <- dnbinom(1:30, r, p)/sum(dnbinom(1:30, r, p))
  for (i in 1:days){
    mat.shed[i, ] <- rbinom(30, N.shed, vec.p.shed)
  }
  return(mat.shed)
}

#calculate how many pathogen were produced;
patho.count <- function(N.pop, beta, recovery, death, birth, days, N.shed, r, p){
  infect.res <- simul.infect(N.pop, beta, recovery, death, birth,days)[-(1:30), ]
  mat.shed <- shed(N.shed, r, p, days)
  patho.cnt <- rowSums(infect.res * mat.shed)
  return(patho.cnt)
}

test <- patho.count(N.pop=10000,beta=0.04,recovery=1/30,death=0.001,birth=0.001,days=1000,N.shed=10^7,r=5,p=1/3)

n.days = 1000
infect.res <- simul.infect(10000, 0.04, 1/30, 0.001, 0.001, n.days)[-(1:30), ]
mat.shed <- shed(N.shed, r = 5, p = 1/3, days = n.days)
patho.cnt <- rowSums(infect.res * mat.shed)

#Exponential Decay Function;
#Input two parameters: number of initial microbes, time in hours.
#This function currently have decay rate as 3 log decrease in 24 hours.
ex_decay <- function(n0, time, decay_rate = (log(0.001)/-24)){
  return(rbinom(1, n0, exp(-decay_rate * time)))
}
#example
ex_decay(100000, 24)
