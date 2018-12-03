install.packages("deSolve")
library("deSolve")
library(ggplot2)
library(reshape2)

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
  modelP=e*w*(H/(d+H))*P-(s*P)
  
  return(list(c(modelH,modelP)))
}

#case 1 with b=0.8, e=0.07, s=0.2, w=5, d=400, a=0.001
#initial conditions Ho=500 and Po=120
params1=c(0.8,0.001,5,400,0.07,0.2)
initial1=c(500,120)
times1=seq(1,100, by=0.1)

modelSim1=ode(y=initial1,times=times1,func=ddSimRM,parms=params1)
modelOutput1=data.frame(time=modelSim1[,1],H=modelSim1[,2],P=modelSim1[,3])
modelOutput1=melt(modelOutput1,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput1,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Standard Parameters")+theme(plot.title = element_text(hjust = 0.5))

#case 2 changing b
#all params stay same except b=2.4
params2=c(0.26,0.001,5,400,0.07,0.2)
initial2=c(500,120)
times2=seq(1,100, by=0.1)

modelSim2=ode(y=initial2,times=times2,func=ddSimRM,parms=params2)
modelOutput2=data.frame(time=modelSim2[,1],H=modelSim2[,2],P=modelSim2[,3])
modelOutput2=melt(modelOutput2,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput2,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Decreased b Parameter")+theme(plot.title = element_text(hjust = 0.5))

#case 3 changing e by decreasing
#e is fifth parameter, new=0.035
params3=c(0.8,0.001,5,400,0.14,0.2)
initial3=c(500,120)
times3=seq(1,100, by=0.1)

modelSim3=ode(y=initial3,times=times3,func=ddSimRM,parms=params3)
modelOutput3=data.frame(time=modelSim3[,1],H=modelSim3[,2],P=modelSim3[,3])
modelOutput3=melt(modelOutput3,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput3,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Increased e Parameter")+theme(plot.title = element_text(hjust = 0.5))
#This makes sense, if e is halved then predators are driven to extinction
#because conversion efficiency so low

#case 4 changing s by increasing
#s is 6th parameter
params4=c(0.8,0.001,5,400,0.07,0.1)
initial4=c(500,120)
times4=seq(1,100, by=0.1)

modelSim4=ode(y=initial4,times=times4,func=ddSimRM,parms=params4)
modelOutput4=data.frame(time=modelSim4[,1],H=modelSim4[,2],P=modelSim4[,3])
modelOutput4=melt(modelOutput4,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput4,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Decreased S Parameter")+theme(plot.title = element_text(hjust = 0.5))

#case 5 changing w by decreasing
#w is 3rd parameter
params5=c(0.8,0.001,15,400,0.07,0.2)
initial5=c(500,120)
times5=seq(1,100, by=0.1)

modelSim5=ode(y=initial5,times=times5,func=ddSimRM,parms=params5)
modelOutput5=data.frame(time=modelSim5[,1],H=modelSim5[,2],P=modelSim5[,3])
modelOutput5=melt(modelOutput5,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput5,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Increased w Parameter")+theme(plot.title = element_text(hjust = 0.5))

#case 6 changing d by increasing
#d is 4th parameter
params6=c(0.8,0.001,5,200,0.07,0.2)
initial6=c(500,120)
times6=seq(1,100, by=0.1)

modelSim6=ode(y=initial6,times=times6,func=ddSimRM,parms=params6)
modelOutput6=data.frame(time=modelSim6[,1],H=modelSim6[,2],P=modelSim6[,3])
modelOutput6=melt(modelOutput6,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput6,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Decreased d Parameter")+theme(plot.title = element_text(hjust = 0.5))

#case 7 changing a by increasing
#a is 2nd para
params7=c(0.8,0.003,5,400,0.07,0.2)
initial7=c(500,120)
times7=seq(1,100, by=0.1)

modelSim7=ode(y=initial7,times=times7,func=ddSimRM,parms=params7)
modelOutput7=data.frame(time=modelSim7[,1],H=modelSim7[,2],P=modelSim7[,3])
modelOutput7=melt(modelOutput7,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutput7,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("Increased alpha Parameter")+theme(plot.title = element_text(hjust = 0.5))

#shifting carrying capacity between 800 and 2,000 in 200 increments
paramscc=c(0.8,0.0005,5,400,0.07,0.2)
initialcc=c(500,120)
timescc=seq(1,100, by=0.1)

modelSimcc=ode(y=initialcc,times=timescc,func=ddSimRM,parms=paramscc)

modelOutputcc=data.frame(time=modelSimcc[,1],H=modelSimcc[,2],P=modelSimcc[,3])
modelOutputcc=melt(modelOutputcc,id.vars = "time",variable.name = "Group",value.name = "Density")
ggplot(modelOutputcc,aes(x=time,y=Density))+geom_line(aes(color=Group))+theme_classic()+xlab("Time")+ggtitle("K=2,000")+theme(plot.title = element_text(hjust = 0.5))







