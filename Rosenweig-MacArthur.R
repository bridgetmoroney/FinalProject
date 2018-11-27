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




