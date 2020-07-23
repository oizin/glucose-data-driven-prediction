library(deSolve)

minimalModel <- function(time,state,params) {
  with(as.list(c(state,params)), {
    # calculations
    
    # equtions
    dI <- -n*I + p4*u1(t)
    dX <- -p2*X + p3*(I - Ib)
    dG <- -p1*G - X*G + p1*Gb + u2(t)/VolG
    
    return(list(c(dI,dX,dG)))
  })
}

u1 <- function(t) 5
u2 <- function(t) 50
u1b <- 1

p1 <- 0.035
p2 <- 0.05
p3 <- 0.000028
p4 <- 0.098
n <- 0.142
VolG <- 117.0
Gb <- 80.0

params <- c(
  p1 = p1,
  p2 = p2,
  p3 = p3,
  p4 = p4,
  n = n,
  VolG = VolG,
  Gb = Gb,
  Ib = (p4*u1b)/n
)

state0 <- c(I = (p4*u1b)/n, X = 0, G = Gb)

times <- seq(0, 200, by = 1)

out  <- ode(state0, times, minimalModel, params)

summary(out)



matplot(out[ , 1], out[ , 3],
        type = "l", 
        xlab = "time",
        ylab = "Conc",
        main = "Bergman Minimal Model", lwd = 2)



