install.packages("deSolve")
library("deSolve")
library(ggplot2)

#creating function to model the Rosenweig MacArthur model
#function will have six parameters and two state variables
ddSimRM=function(t,y,p){
  H=y[1]
  P=y[2]
  b=p[1]
  a=p[2]
  w=p[3]
  d=p[4]
  e=p[5]
  s=p[6]
  
  modelH=b*H*(1-a*H)-w*(H/(d+H))*P
  modelP=e*w*(H/(d+H))*P-s*P
  
  return(list(c(modelH,modelP)))
}

#case 1 with b=0.8, e=0.07, s=0.2, w=5, d=400, a=0.001
#initial conditions Ho=500 and Po=120
params1=c(0.8,0.001,5,400,0.07,0.2)
initial1=c(500,120)
times1=seq(1,100, by=0.1)

modelSim1=ode(y=initial1,times=times1,func=ddSimRM,parms=params1)
modelOutput1=data.frame(time=modelSim1[,1],H=modelSim1[,2],P=modelSim1[,3])
ggplot(modelOutput1,aes(x=time,y=H))+geom_line()+theme_classic()+geom_line(aes(x=time,y=P,color="red"))

#case 2 changing b
#all params stay same except b=2.4

params2=c(2.4,0.001,5,400,0.07,0.2)
initial2=c(500,120)
times2=seq(1,100, by=0.1)

modelSim2=ode(y=initial2,times=times2,func=ddSimRM,parms=params2)
modelOutput2=data.frame(time=modelSim2[,1],H=modelSim2[,2],P=modelSim2[,3])
ggplot(modelOutput2,aes(x=time,y=H))+geom_line()+theme_classic()+geom_line(aes(x=time,y=P,color="red"))
#shouldn't H increase if I multiplty it by 3?

#case 3 changing e by decreasing
#e is fifth parameter, new=0.035
params3=c(0.8,0.001,5,400,0.0035,0.2)
initial3=c(500,120)
times3=seq(1,100, by=0.1)

modelSim3=ode(y=initial3,times=times3,func=ddSimRM,parms=params3)
modelOutput3=data.frame(time=modelSim3[,1],H=modelSim3[,2],P=modelSim3[,3])
ggplot(modelOutput3,aes(x=time,y=H))+geom_line()+theme_classic()+geom_line(aes(x=time,y=P,color="red"))
#This makes sense, if e is halved then predators are driven to extinction
#because conversion efficiency so low


