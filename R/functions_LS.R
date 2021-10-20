# Set initial R0 and Rt at future time points

#

init <- nimue:::init(squire::get_population("Peru")$n, seeding_cases = seeding_cases)

# our susceptibles
S_0 <- init$S_0

# let's say 90% already vaccinated in over 15s
prop <- c(0,0,0,.2,.2,.3,.3,.3,.4,.5,.6,.6,.6,.7,.8,.8,.8)


S_0[,4]<-round(S_0[,1] * prop)


S_0[,1] <- S_0[,1] - S_0[,4]

# and update the init
init$S_0 <- S_0

