# Functional response of four predator species to varied densities of stink bug eggs

#bring data in
Cricket<-read.csv(file="../Cricket.csv", header=T)
Orius<-read.csv(file="../Orius.csv", header=T)
Grasshopper<-read.csv(file="../Grasshopper.csv", header=T)
Katydid<-read.csv(file="../Katydid.csv", header=T)

#create subsets as necessary for each species- by sex 
Cricket.F<-Cricket[which(Cricket$predator_sex=="Female"),]
Cricket.M<-Cricket[which(Cricket$predator_sex=="Male"),]


#we're going to use the approach of a two stage analysis. Basically, step one is determine
#what the response is, then step 2 is fit the data to the appropriate response and build a plot

#step 1- find out the fit

# fit to Juilano 2001 model to determine type of functional response for each species (by sex)
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

#let's look at cricket males


Cricket.M.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Cricket.M)

summary(Cricket.M.fit)

#once again, a non-significant value for P1, so we'll reduce the complexity of the polynomial

Cricket.M.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2)), 
                   start=list(P0=P0, P1=P1, P2=P2),
                   data=Cricket.M)

summary(Cricket.M.fit)

#significant negative P1! We have a type II

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
#how to proceed. Note that 1/Th is the asymptote of the curve

#Whatever the model, we're trying to find the values for a, attack rate, and Th, handling time
#so we need starting values


a<-1
Th<-0

#fit the random predator equation
Cricket.F.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                      start=list(a=a,Th=Th),
                      data=Cricket.F)

summary(Cricket.F.random)
AIC(Cricket.F.random)

#fit the Holling's disc equation

Cricket.F.holling<-nls(eggs_eaten~eggs_start*a /(1+a*Th*eggs_start), 
                      start=list(a=a,Th=Th),
                      data=Cricket.F)

summary(Cricket.F.holling)
AIC(Cricket.F.holling)

#and what we find here is the Random predator equation has a better fit (ie it has a smaller AIC)
#but it doesn't produce a significant coefficient

#but we'd like an estimate of the asymptote anyway, so let's use the Holling model

Cricket.F.asymptote<-1/(summary(Cricket.F.holling)$coefficients[2,1])
Cricket.F.asymptote
#propagate the error
Cricket.F.asymptote.se<-Cricket.F.asymptote^2*summary(Cricket.F.holling)$coefficients[2,2]
Cricket.F.asymptote.se

######
#repeat this analysis with cricket males

#fit the random predator equation
Cricket.M.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                      start=list(a=a,Th=Th),
                      data=Cricket.M)

summary(Cricket.M.random)
AIC(Cricket.M.random)

#fit the Holling's disc equation

Cricket.M.holling<-nls(eggs_eaten~eggs_start*a /(1+a*Th*eggs_start), 
                       start=list(a=a,Th=Th),
                       data=Cricket.M)

summary(Cricket.M.holling)
AIC(Cricket.M.holling)

#and what we find here is the Random predator equation has a better fit (ie it has a smaller AIC)
#but it doesn't produce a significant coefficient

#but we'd like an estimate of the asymptote anyway, so let's use the Holling model

Cricket.M.asymptote<-1/(summary(Cricket.M.holling)$coefficients[2,1])
Cricket.M.asymptote
#propagate the error
Cricket.M.asymptote.se<-Cricket.M.asymptote^2*summary(Cricket.M.holling)$coefficients[2,2]
Cricket.M.asymptote.se


#create plots to illustrate fit

#for each taxon, we'll want to have M and F data plotted in the same chart. This means we'll
#need to operate on the full dataset when creating the plot. We want to do this in ggplot2
# and we'll need to create summary data to achieve this

library(plyr)
library(ggplot2)

#choose colors for each sex
female<-"green"
male<-"orange"

#calculate mean and SEM for each treatment
Cricket.summary<-ddply(Cricket, c("predator_sex", "eggs_start"), summarise,
                       mean_eggs_eaten=mean(eggs_eaten), n=length(eggs_eaten),
                       sem=sd(eggs_eaten)/sqrt(n))

#create objects describing functions for plotting our fits
#females
a<-summary(Cricket.F.holling)$coefficients[1,1]
Th<-summary(Cricket.F.holling)$coefficients[2,1]
Cricket.F.func.holling<-function(x)  x*a /(1+a*Th*x)
#males
am<-summary(Cricket.M.holling)$coefficients[1,1]
Thm<-summary(Cricket.M.holling)$coefficients[2,1]
Cricket.M.func.holling<-function(x)  x*am /(1+am*Thm*x)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(3) # move them .05 to the left and right

Cricket.plot<-ggplot(Cricket.summary, aes(x=eggs_start, y=mean_eggs_eaten, 
                                          color=predator_sex, shape=predator_sex))+
  scale_color_manual(values=c(female, male), name="Predator sex")+
  scale_shape_manual(values=c(16,17), name="Predator sex")+
  stat_function(fun=Cricket.F.func.holling, colour=female, size=1, linetype="dashed")+
  stat_function(fun=Cricket.M.func.holling, colour=male, size=1, linetype="dotted")+ 
  geom_errorbar(aes(ymin=mean_eggs_eaten-sem, ymax=mean_eggs_eaten+sem, color=predator_sex), 
               position=pd, color="black", width=3, size=0.75, show.legend=FALSE) +
  xlim(0, 150)+ylim(0,110)+
  xlab("Starting egg density")+ylab("Eggs eaten")+
  geom_point(position=pd, size=5, show.legend=TRUE)+
  theme_bw()+ theme(legend.key=element_rect(colour=NA))
  

Cricket.plot
