# Functional response of four predator species to varied densities of stink bug eggs

#bring data in
Cricket<-read.csv(file="../Cricket.csv", header=T)
Orius<-read.csv(file="../Orius.csv", header=T)
Grasshopper<-read.csv(file="../Grasshopper.csv", header=T)
Katydid<-read.csv(file="../Katydid.csv", header=T)

#create subsets as necessary for each species- by sex? 
Cricket.F<-Cricket[which(Cricket$predator_sex=="Female"),]
Cricket.M<-Cricket[which(Cricket$predator_sex=="Male"),]


#we're going to use the approach of a two stage analysis. Basically, step one is determine
#what the response is, then step 2 is fit the data to the appropriate response and build a plot

#step 1- find out the fit

# fit to Juilano 2001 model to determine type of functional response for each species
# juliano created an nth order logistic equation to model the relationship between the number
# of prey consumed and the initial prey density. It should be sufficient to take this model 
# just to cubic form, and if the fit is poor from there, reduce the order.
# The way this fit is used, if the linear coefficient (P1) is negative
# then we consider the functional response to be of type II, but if it's positive
# then the functional response is type III. If P1 is not significant, reduce the order 
# (ie set P3 to zero) and refit.

#first chose some starting values for the coefficients we're trying to estimate

P0<-0
P1<-0
P2<-0
P3<-0

Cricket.F.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Cricket.F)

summary(Cricket.F.fit)

#this results in a non-significant value for P1. Let's refit without the 3rd order
#polynomial
Cricket.F.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2)), 
                   start=list(P0=P0, P1=P1, P2=P2),
                   data=Cricket.F)

summary(Cricket.F.fit)

#this analysis results in weak evidence for a type II functional response among 
# female crickets (P<0.1)- so it's probably a type II response, but variable data-
# we'll see more when we fit the type II equation and plot the data


###
# step 2- fit the functional response

#fit curve of relevant type to determine consumption rate as a function of egg density

# to define the functional response, we can either fit the Holling disc equation. We'll assume 
# type II responses for now, and refit if necessary, but type II responses are generally more 
#in lab experiments

#Because the experiment allowed prey to become depleted without replacement, the appropriate
#model to fit in the context here is Rogers (1972) random predator equation. This takes the
# form Ne=N0*(1-exp(a(Th*Ne-a))) for a type II. HOWEVER! This model in general has poorer
#empirical fit than the Holling's disc equation, so we'll fit both, and let the data tell us
#how to proceed.

#Whatever the model, we're trying to find the values for a, attack rate, and Th, handling time
#so we need starting values
a<-1
Th<-0

#fit the random predator equation
Cricket.F.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                      start=list(a=a,Th=Th),
                      data=Cricket.F)

summary(Cricket.F.random)

#create plots to illustrate