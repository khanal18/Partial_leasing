
# Load libraries
library(ggplot2)
library(rgdal)
library(tidyverse)
library(ggpmisc)
library (gganimate)
library (dplyr)
library(grid)
library(viridis)
library(ggpubr)



#******************************************************
# Instream flow augmentation potential (figure 12)
#****************************************************


# For basinwide potential considering upperbound case i.e. when crop price was $0.35/kg.
data=read.csv("Output/added_streamflow.csv", header = TRUE)
Data<-data
Data$bQuarter<- data$quarter+ data$Average.discharge
Data$cHalf<- data$half+ data$Average.discharge
Data$dtquarter<- data$thi_quarter+ data$Average.discharge
Data$eFull<-data$full+ data$Average.discharge

#view(Data)
head(Data)

p2<-ggplot(Data, aes(x=DOY)) +
  geom_line(aes(y=eFull, group=crop, color='F', linetype="F", size="F"))+
  geom_line(aes(y=dtquarter, group=crop, color='G', linetype="G", size="G"))+
  geom_line(aes(y=cHalf, group=crop, color='H', linetype="H", size="H"))+
  geom_line(aes(y=bQuarter, group=crop, color='Q', linetype="Q", size="Q"))+
  geom_line(aes(y=Average.discharge, group=crop, color='Z', linetype="Z", size="Z")) +
  
  #scale_color_manual(name='p',values=c('0 %'='red','25 %'='yellow', "50 %"='blue', "100 %"='black') , labels=c("z", 'q','h','f'))+
  
  scale_linetype_manual(values=c('Z'= 1,'Q'=1, "H"=1, "G"=1, "F"=1) , labels=c("100%", '75%','50%',  '25%','0%') )+
  scale_color_manual(values=c('Z'='black','Q'='red', "H"='blue',"G"='darkgreen', "F"='pink') , labels=c("100%", '75%','50%', '25%','0%') )+
  scale_size_manual(values=c('Z'=1,'Q'=1, "H"=1,"G"=1, "F"=1.2) , labels=c("100%", '75%','50%', '25%','0%') )+
  facet_wrap(vars(crop), scales = 'free')+
  #legend(labels=c('z'))
  theme(text = element_text(size=18), legend.position="right", legend.key.width=unit(1,"cm"))+
  labs(title = '', x = 'DOY', y = 'Average Discharge (m3/s)', 
       color="adoption % \n (of acreage)", linetype="adoption % \n (of acreage)",size="adoption % \n (of acreage)" )

p2

ggsave(plot =  p2, width = 8, height = 5, dpi = 300, filename =  paste("Figures/figure_12.png"))



#******************************************************
# Instream flow data (figure 2)
#****************************************************
#


DF=read.csv("Output/Detour_13_18.csv", header = TRUE)

all_columns = c('DOY','Instream_flow','Average_discharge')

colnames(DF)=all_columns

head (DF)


png("Figures/figure_2.png", width=650, height=400)

plot(DF$DOY,DF$Average_discharge,pch=10, type="b",col='black',axes=FALSE,ylab='',xlab = "",
     ylim=c(0,40))#, main =paste("Average Daily discharge in 2015 "))
axis(2, ylim=c(0,40), col="black", las=1)
mtext("Average discharge (m3/s)", side=2, line=2.5, font=2)
#abline(v=c(151,335), col=c("blue", "red"), lty=c(1,2), lwd=c(1, 3))
#abline(v=c(151,335), col=c("blue"), lty=c(2), lwd=c(3))
#abline(h=c(7.075), col=c("blue"), lty=c(2), lwd=c(3))

lines(x=c(151,335), y=c(7.075,7.075),col=c("blue"), lty=c(2), lwd=c(3))
box()

par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(DF$DOY,DF$Instream_flow, pch=10, type="b",xlab="", ylab="", ylim = c(0,40), axes=FALSE,  col='red' )

#par(new=TRUE)
rect(151,-2, 335, 45, density = 5, border = "red")

## Draw the time axis
#axis(1,pretty(range(DF$DOY),10))
axis(1, at=c(15,46,74,105,135, 166,196, 227, 258, 288, 319, 349), labels=c("Jan","Feb","Mar", "Apr","May","Jun",
                                                                           "Jul","Aug","Sep","Oct","Nov","Dec"))
#mtext("Time day of the year April 10=100",side=1,col="black",line=2.5)  
mtext("Time (Month)",side=1,col="black",line=2.5,font=2) 
#text(157,20, "closed", srt=90)
#text(323,20, "closed", srt=90)
text(220,20, "Closed to new \n water rights", cex=2,srt=0)
## Add Legend
legend("topright",legend=c("Average Discharge","Minimum Instream Flow Requirement"),
       text.col=c("black","red"),pch=c(14,15),col=c("black","red"))



dev.off()


