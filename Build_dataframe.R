## Load libraries

#Load libraries
library(rgdal)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gapminder)
library(ggplot2)
library(devtools)
#devtools::install_github("thomasp85/transformr")
library(transformr)
library(ggpmisc)
library(viridis)


#Read the input data and find the production fuction. Here I am using the "envelope" production function for corn and wheat.
crop='corn'
#crop='wheat'

if (crop=='corn'){   
  f_=expression(1870 - 13.8*w + 0.193*w^2 - 0.000246*w^3)} 


if (crop=='wheat'){
  
  f_<-expression(1680 -14.4*w + 0.116*w**2 -0.000139*w^3)} # mm and kg/ha 


if (crop=='corn'){   
    r=777.26} #$/ha It is the planting production cost that we reported in section 2.4.6

if (crop=='wheat'){   
    r=442.13} #$/ha It is the planting production cost that we reported in section 2.4.6



## Evaluate f(w) and f'(w)
w = seq(0,550,.5)
yield<-eval(f_) ## w for max yield 1.59


#MAX_w_corn = 484.5
ff_<-D(f_,'w')
f_profit=expression(pc*eval(f_) - pw*w)


## Create a dataframe of marginal product of water, w, yield
MPW_<-data.frame(eval(ff_), w, eval(f_))
colnames(MPW_)<-c("ff","w", "fw")


## find w corresponding to max MPW so we can focus on just the right side of curve
rownummaxff_<-which(MPW_$ff ==  max(MPW_$ff))
threshhold_w_crop<-MPW_[rownummaxff_,2]
MPW_<-MPW_[MPW_$w >=threshhold_w_crop,]

## Create a list of Pcs and Pws
pclist<-seq(0.1,0.35,.001)
pwlist<-seq(0,12,0.01)
i= 1
for (i in 1: length(pclist)) {
  tmp<-data.frame(pclist[i],pwlist)
  if(i == 1)
    allpricedata<-tmp
  else
    allpricedata<-rbind(allpricedata,tmp)
}
colnames(allpricedata)<-c("pc","pw")

head(allpricedata)


if (crop=='corn'){   
  MAX_w_crop=484.5} # We calculated this value using differential function and put it here

if (crop=='wheat'){   
  MAX_w_crop=485} # We calculated this value using differential function and adheres to what we presented. 

print (MAX_w_crop)


## Next evaluate the profit function for each pw and pc combination 
for (i in 1: length(allpricedata$pc)) {
  
  w=MPW_$w
  pc = allpricedata[i,1]
  pw = allpricedata[i,2]
  profit<-eval(f_profit)
  profitandw_crop<-data.frame(cbind(w,profit))
  head(profitandw_crop)
  w_maxprofit <- profitandw_crop[profitandw_crop$profit ==     max(profitandw_crop$profit),]$w
  ## select w and max profit corresponding Pw and Pc
  w = w_maxprofit
  TotalRevenue_Deficitcrop<-pc*eval(f_) + pw*(MAX_w_crop-w)
  RevenueAllFallow<-MAX_w_crop*pw +r
  w=MAX_w_crop
  RevenueAllCrop<-pc*eval(f_)
  ThisAllRevenueData<-data.frame(w_maxprofit,pc,pw,TotalRevenue_Deficitcrop,RevenueAllFallow, RevenueAllCrop)
  if(i == 1)  
    
    AllRevenueData<-ThisAllRevenueData
  
  else
    AllRevenueData<-rbind(AllRevenueData,ThisAllRevenueData)
  
}
colnames(AllRevenueData)<-c("Maxprof_w","pc","pw","TotRevDeficitAndLease", "TotRevAllFallow", "TotRevAllCrop")
head(AllRevenueData)




# Making other dataframe for comparison propose
AllRevenueOnlyMaxleasing<-AllRevenueData
# Deriving best alternative
AllRevenueOnlyMaxleasing <- transform(AllRevenueOnlyMaxleasing, Bestalternative = pmax(TotRevAllCrop, TotRevAllFallow))
AllRevenueOnlyMaxleasing$AddlRevFromDL<- AllRevenueOnlyMaxleasing$TotRevDeficitAndLease - AllRevenueOnlyMaxleasing$Bestalternative
AllRevenueOnlyMaxleasing <- AllRevenueOnlyMaxleasing[AllRevenueOnlyMaxleasing$AddlRevFromDL >= 0,] 
AllRevenueOnlyMaxleasing$optimalirrigation <-  AllRevenueOnlyMaxleasing$Maxprof_w
AllRevenueOnlyMaxleasing$optimallease <- max(AllRevenueOnlyMaxleasing$optimalirrigation) - AllRevenueOnlyMaxleasing$optimalirrigation

head(AllRevenueOnlyMaxleasing)



if (crop=='corn'){   
  AllRevenueOnlyMaxleasing$production<-(1870 - 13.8*AllRevenueOnlyMaxleasing$optimalirrigation + 0.193*AllRevenueOnlyMaxleasing$optimalirrigation^2 -  0.000246*AllRevenueOnlyMaxleasing$optimalirrigation^3)} 

if (crop=='wheat'){   
  AllRevenueOnlyMaxleasing$production<-(1680 -14.4*AllRevenueOnlyMaxleasing$optimalirrigation + 0.116*AllRevenueOnlyMaxleasing$optimalirrigation^2 -0.000139*AllRevenueOnlyMaxleasing$optimalirrigation^3)} 


AllRevenueOnlyMaxleasing$production_lost<-max(AllRevenueOnlyMaxleasing$production)-AllRevenueOnlyMaxleasing$production



head(AllRevenueOnlyMaxleasing)


write.csv(AllRevenueOnlyMaxleasing,  paste("Output/",crop, "_revenue.csv", sep=""),  row.names=FALSE)







