# Functional response of four predator species to varied densities of stink bug eggs

#bring data in
Cricket<-read.csv(file="Cricket.csv", header=T)
Orius<-read.csv(file="Orius.csv", header=T)
Grasshopper<-read.csv(file="Grasshopper.csv", header=T)
Katydid<-read.csv(file="Katydid.csv", header=T)

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
# colors from a qualitative pallate from colorbrewer.org, chosen for colorblind friendliness
female<-"#d95f02"
male<-"#7570b3"

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
pd <- position_dodge(3) # move them 3 units to the left and right

Cricket.plot<-ggplot(Cricket.summary, aes(x=eggs_start, y=mean_eggs_eaten, 
                                          color=predator_sex, shape=predator_sex))+
  scale_color_manual(values=c(female, male), name="Predator sex")+
  scale_shape_manual(values=c(16,17), name="Predator sex")+
  stat_function(fun=Cricket.F.func.holling, colour=female, size=1, linetype="dashed")+
  stat_function(fun=Cricket.M.func.holling, colour=male, size=1, linetype="dotted")+ 
  geom_errorbar(aes(ymin=mean_eggs_eaten-sem, ymax=mean_eggs_eaten+sem, color=predator_sex), 
               position=pd, color="black", width=5, size=0.5, show.legend=FALSE) +
  xlim(0, 150)+ylim(0,110)+
  xlab("Starting egg density")+ylab("Eggs eaten")+
  geom_point(position=pd, size=4, show.legend=TRUE)+
  theme_bw()+ theme(legend.key=element_rect(colour=NA))
  

Cricket.plot


#save to pdf
pdf("Cricket_fig.pdf", height=4, width=5)
Cricket.plot
dev.off()


##next up ORIUS!!

Orius<-read.csv(file="Orius.csv", header=T)

#step 1- find out the fit
#first chose some starting values for the coefficients we're trying to estimate

P0<-0
P1<-0
P2<-0
P3<-0

Orius.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Orius)

summary(Orius.fit)

#once again, a non-significant value for P1, so we'll reduce the complexity of the polynomial

Orius.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2)), 
                   start=list(P0=P0, P1=P1, P2=P2),
                   data=Orius)

summary(Orius.fit)
#We have a negative, but non-significant value for P1- this suggests there may be a type II functional response
# but data are too variable or do not capture a sufficient range of the relationship to see this clearly.

#we can go on and attempt t fit Holling and Random predator but this probably won't yield a significant result


###
# step 2- fit the functional response

#Whatever the model, we're trying to find the values for a, attack rate, and Th, handling time
#so we need starting values


a<-1
Th<-0

#fit the random predator equation
Orius.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                      start=list(a=a,Th=Th),
                      data=Orius)

summary(Orius.random)
AIC(Orius.random)
#fit the Holling's disc equation

Orius.holling<-nls(eggs_eaten~eggs_start*a /(1+a*Th*eggs_start), 
                       start=list(a=a,Th=Th),
                       data=Orius)

summary(Orius.holling)
AIC(Orius.holling)

#and what we find here is the Random predator equation has a better fit (ie it has a smaller AIC) again
#but it doesn't produce a significant coefficient, and Holling's disc has no significant parameters

# in fact, the fit is rough enough that we're not going to get a viable Th or maximum voracity

#so for this, I think a linear response is the best we can do

#let's fit a linear model, for a type I response

orius.linear<-lm(eggs_eaten~eggs_start, data=Orius)

summary(orius.linear)
AIC(orius.linear)

#the AIC also indicates that this is not a great model, but at least there's a significant slope
#so how I'd interpret this is there is density dependance in the response, but it can't be defined
#because either the data are too variable, or an insufficient range of starting values are covered


#calculate mean and SEM for each treatment
Orius.summary<-ddply(Orius, c("eggs_start"), summarise,
                       mean_eggs_eaten=mean(eggs_eaten), n=length(eggs_eaten),
                       sem=sd(eggs_eaten)/sqrt(n))

#create objects describing functions for plotting our fit- we'll use the linear one because it's the simplest 
#model, and there's no real support for a curvilinear model, even though the random predator equation had the 
#lowest AIC

b<-summary(orius.linear)$coefficients[1,1]
m<-summary(orius.linear)$coefficients[2,1]
Orius.func.linear<-function(x)  x*m+b #y=mx+b, the linear equation general formula

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(3) # move them .05 to the left and right

Orius.plot<-ggplot(Orius.summary, aes(x=eggs_start, y=mean_eggs_eaten))+ 
  stat_function(fun=Orius.func.linear, size=1, linetype="dashed")+
  ##stat_function(fun=Orius.func.holling, colour=male, size=1, linetype="dotted")+ 
  geom_errorbar(aes(ymin=mean_eggs_eaten-sem, ymax=mean_eggs_eaten+sem), 
                position=pd, color="black", width=3, size=0.75, show.legend=FALSE) +
  xlim(0, 150)+ylim(0,6.0)+
  xlab("Starting egg density")+ylab("Eggs eaten")+
  geom_point(position=pd, size=5, show.legend=TRUE)+
  theme_bw()+ theme(legend.key=element_rect(colour=NA))

Orius.plot

#save to pdf
pdf("Orius_fig.pdf", height=4, width=5)
Orius.plot
dev.off()


##Next up Grasshopper!!
Grasshopper<-read.csv(file="Grasshopper.csv", header=T)

#create subsets as necessary for each species- by sex 
Grasshopper.F<-Grasshopper[which(Grasshopper$predator_sex=="Female"),]
Grasshopper.M<-Grasshopper[which(Grasshopper$predator_sex=="Male"),]

#step 1- find out the fit--Juliano

#first chose some starting values for the coefficients we're trying to estimate

P0<-0
P1<-0
P2<-0
P3<-0

Grasshopper.F.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Grasshopper.F)

summary(Grasshopper.F.fit)

#this results in a non-significant value for P1. Let's refit without the 3rd order
#polynomial
Grasshopper.F.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2)), 
                   start=list(P0=P0, P1=P1, P2=P2),
                   data=Grasshopper.F)

summary(Grasshopper.F.fit)

##We have a negative, but non-significant value for P1, as we had with orius- 
# this suggests there may be a type II functional response
# but data are too variable or do not capture a sufficient range of the relationship to see this clearly.

#we can attempt to fit Random Predator and Holling's Disc but I think we're just going to end up with a linear model
#like orius again


###
# step 2- fit the functional response

#Whatever the model, we're trying to find the values for a, attack rate, and Th, handling time
#so we need starting values


a<-1
Th<-0

#fit the random predator equation
Grasshopper.F.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                  start=list(a=a,Th=Th),
                  data=Grasshopper.F)

summary(Grasshopper.F.random)
AIC(Grasshopper.F.random)
#fit the Holling's disc equation

Grasshopper.F.holling<-nls(eggs_eaten~eggs_start*a /(1+a*Th*eggs_start), 
                   start=list(a=a,Th=Th),
                   data=Grasshopper.F)

summary(Grasshopper.F.holling)
AIC(Grasshopper.F.holling)


#again, we have better performance of the Random predator equation but no significant parameters in either. 
#I think we just need to use a straight line here

#let's fit a linear model, for a type I response

Grasshopper.F.linear<-lm(eggs_eaten~eggs_start, data=Grasshopper.F)

summary(Grasshopper.F.linear)
AIC(Grasshopper.F.linear)

#the AIC also indicates that this is not a great model, but again, at least there's a significant slope
#so how I'd interpret this is there is density dependance in the response, but it can't be defined
#because either the data are too variable, or an insufficient range of starting values are covered

#let's  do the same for Grasshopper males

Grasshopper.M.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Grasshopper.M)

summary(Grasshopper.M.fit)

#once again, a non-significant value for P1, so we'll reduce the complexity of the polynomial

Grasshopper.M.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2)), 
                   start=list(P0=P0, P1=P1, P2=P2),
                   data=Grasshopper.M)

summary(Grasshopper.M.fit)

##Removing the higher order changes P1 from negative to positive and neither are significant
## this means there's practically no evidence for Type II response, and I suspect we don't have sufficient range 
#of data even if we do have a type III response, so I think we should go straight to modelling the males as a linear relationship

#let's fit a linear model, for a type I response

Grasshopper.M.linear<-lm(eggs_eaten~eggs_start, data=Grasshopper.M)

summary(Grasshopper.M.linear)
AIC(Grasshopper.M.linear)

# we don't even have a significant slope in this case, but it may still be worthwhile to plot the line on our graph

#choose colors for each sex
# colors from a qualitative pallate from colorbrewer.org, chosen for colorblind friendliness
female<-"#d95f02"
male<-"#7570b3"

#calculate mean and SEM for each treatment
Grasshopper.summary<-ddply(Grasshopper, c("predator_sex", "eggs_start"), summarise,
                       mean_eggs_eaten=mean(eggs_eaten), n=length(eggs_eaten),
                       sem=sd(eggs_eaten)/sqrt(n))

#create objects describing functions for plotting our fits
#females

b<-summary(Grasshopper.F.linear)$coefficients[1,1]
m<-summary(Grasshopper.F.linear)$coefficients[2,1]
Grasshopper.F.func.linear<-function(x)  x*m+b #y=mx+b, the linear equation general formula

#males
bm<-summary(Grasshopper.M.linear)$coefficients[1,1]
mm<-summary(Grasshopper.M.linear)$coefficients[2,1]
Grasshopper.M.func.linear<-function(x)  x*mm+bm #y=mx+b, the linear equation general formula

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(3) # move them 3 units to the left and right

Grasshopper.plot<-ggplot(Grasshopper.summary, aes(x=eggs_start, y=mean_eggs_eaten, 
                                          color=predator_sex, shape=predator_sex))+
  scale_color_manual(values=c(female, male), name="Predator sex")+
  scale_shape_manual(values=c(16,17), name="Predator sex")+
  stat_function(fun=Grasshopper.F.func.linear, colour=female, size=1, linetype="dashed")+
  stat_function(fun=Grasshopper.M.func.linear, colour=male, size=1, linetype="dotted")+ 
  geom_errorbar(aes(ymin=mean_eggs_eaten-sem, ymax=mean_eggs_eaten+sem, color=predator_sex), 
                position=pd, color="black", width=5, size=0.5, show.legend=FALSE) +
  xlim(0, 150)+ylim(0,60)+
  xlab("Starting egg density")+ylab("Eggs eaten")+
  geom_point(position=pd, size=4, show.legend=TRUE)+
  theme_bw()+ theme(legend.key=element_rect(colour=NA))


Grasshopper.plot


#save to pdf
pdf("Grasshopper_fig.pdf", height=4, width=5)
Grasshopper.plot
dev.off()






##Instead of moving to step 2 for grasshopper, I will do step 1 for Katydid

###
#Now to Katydid!
Katydid<-read.csv(file="Katydid.csv", header=T)

#step 1- find out the fit
#first chose some starting values for the coefficients we're trying to estimate

P0<-0
P1<-0
P2<-0
P3<-0

Katydid.fit<-nls(Pconsumed~
                     exp(P0+
                           P1*eggs_start+
                           P2*eggs_start^2+
                           P3*eggs_start^3)/(
                             1+exp(P0+
                                     P1*eggs_start+
                                     P2*eggs_start^2+
                                     P3*eggs_start^3)), 
                   start=list(P0=P0, P1=P1, P2=P2, P3=P3),
                   data=Katydid)


## so this fit is failing entirely because the data is not covering the curve, so it's failing to converge
# (essentially that means that bcause there's no data at places where the model predicts the data to curve, the computer
#is whipping the line all around to find a solution but can't, because there's no data to fit the curve to, so many solutions 
# have equal fit)- I fiddled around with the defaults on the number of iterations and the tolerances for what it considers a
#solution, but it's really the data coverage that's the issue here.

#our only possibility here is to reduce the model- make it less curvy, to see if the computer can find a best fit them
#Let's refit without the 3rd order polynomial
Katydid.fit<-nls(Pconsumed~
                         exp(P0+
                               P1*eggs_start+
                               P2*eggs_start^2)/(
                                 1+exp(P0+
                                         P1*eggs_start+
                                         P2*eggs_start^2)), 
                       start=list(P0=P0, P1=P1, P2=P2),
                       data=Katydid)

summary(Katydid.fit)
# We're now getting a nonsignificant, but negative P1- so some possible weak evidence for a Type II response. 
# We can proceed as we did with Orius- test to see if Random Predator or Holling see anything, and if not, model it as a line


###
# step 2- fit the functional response

#Whatever the model, we're trying to find the values for a, attack rate, and Th, handling time
#so we need starting values


a<-1
Th<-0

#fit the random predator equation
Katydid.random<-nls(eggs_eaten~eggs_start*(1-exp(a*(Th*eggs_eaten-1))), 
                  start=list(a=a,Th=Th),
                  data=Katydid)
#convergence problems again, this time for the random predator equation. I suspect these data just don't fit a Type II 
#curve very well, and we're just getting more evidence of that.


#fit the Holling's disc equation

Katydid.holling<-nls(eggs_eaten~eggs_start*a /(1+a*Th*eggs_start), 
                   start=list(a=a,Th=Th),
                   data=Katydid)

summary(Katydid.holling)
AIC(Katydid.holling)

#Holling's disc has no significant parameters- and it's, in fact, saying negative handling 
# time (although non-significant) so that's a pretty strong indication to me of a poor fit!

#As before, I think a linear response is the best we can do

#let's fit a linear model, for a type I response

Katydid.linear<-lm(eggs_eaten~eggs_start, data=Katydid)

summary(Katydid.linear)
AIC(Katydid.linear)

# No significant slope
#so very little trend overall


#calculate mean and SEM for each treatment
Katydid.summary<-ddply(Katydid, c("eggs_start"), summarise,
                     mean_eggs_eaten=mean(eggs_eaten), n=length(eggs_eaten),
                     sem=sd(eggs_eaten)/sqrt(n))

#create objects describing functions for plotting our fit- we'll use the linear one because it's the simplest 
#model, and there's no real support for a curvilinear model, even though it's also not significant

b<-summary(Katydid.linear)$coefficients[1,1]
m<-summary(Katydid.linear)$coefficients[2,1]
Katydid.func.linear<-function(x)  x*m+b #y=mx+b, the linear equation general formula

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(3) # move them .05 to the left and right

Katydid.plot<-ggplot(Katydid.summary, aes(x=eggs_start, y=mean_eggs_eaten))+ 
  stat_function(fun=Katydid.func.linear, size=1, linetype="dashed")+
  geom_errorbar(aes(ymin=mean_eggs_eaten-sem, ymax=mean_eggs_eaten+sem), 
                position=pd, color="black", width=3, size=0.75, show.legend=FALSE) +
  xlim(0, 150)+ylim(0,13)+
  xlab("Starting egg density")+ylab("Eggs eaten")+
  geom_point(position=pd, size=5, show.legend=TRUE)+
  theme_bw()+ theme(legend.key=element_rect(colour=NA))

Katydid.plot

#save to pdf
pdf("Katydid_fig.pdf", height=4, width=5)
Katydid.plot
dev.off()
