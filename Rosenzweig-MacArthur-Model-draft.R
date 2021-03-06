rm(list=ls())
setwd("/Users/ta4ha/Documents/Biocomputing")


library(ggplot2)
library(deSolve)

# Rosenzweig-MacArthur Model

RMsim<-function(t,y,p){
  H=y[1] # prey initial value
  P=y[2] # predator initial value
  
  b=p[1] # prey birth rate
  w=p[2] # predator attack rate
  e=p[3] # conversion efficiency of prey to predators
  s=p[4] # predator death rate
  a=p[5] # prey self-limiting coefficient (1/K)
  d=p[6] # saturating functional response of predators to prey density
  
  dHdt=b*H*(1-(a*H))-w*(H/(d+H))*P # prey population
  dPdt=e*w*(H/(d+H))*P-(s*P)  #predator population
  
  return(list(c(dHdt,dPdt)))
}

# Initial Simulation
times= seq(0, 100, by=0.01)
y0=c(500,120)
params1=c(0.8,5,0.07,0.2,0.001,400)
RMsim1=ode(y=y0,times=times,func=RMsim,parms=params1)
RMout1=data.frame(time=RMsim1[,1],population=RMsim1[,2],animals=RMsim1[,3])
ggplot(RMout1,aes(x=time,y=population))+geom_line()+geom_line(data=RMout1,mapping=aes(x=time,y=animals),col='red')+theme_classic()+ggtitle("Predator and Prey Populations Over Time")

