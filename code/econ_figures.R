


library(rgdal)
library(tidyverse)
library(ggpmisc)
library (gganimate)
library (dplyr)
library(grid)
library(viridis)
library(ggpubr)
library(tidyr)

# Readomg both dataframes

corn_data<-read.csv("Output/corn_revenue.csv", header = TRUE)
corn_data$crop<- 'corn'
corn_data$optimalleasepercentage<-corn_data$optimallease/max(corn_data$optimalirrigation)*100
corn_data$productionlostepercentage<-corn_data$production_lost/max(corn_data$production)*100


wheat_data<-read.csv("Output/wheat_revenue.csv", header = TRUE)
wheat_data$crop<- 'wheat'
wheat_data$optimalleasepercentage<-wheat_data$optimallease/max(wheat_data$optimalirrigation)*100
wheat_data$productionlostepercentage<-wheat_data$production_lost/max(wheat_data$production)*100


# combining two dataframes
data<-rbind(corn_data, wheat_data)



#******************************************************
# Additional Revenue (figure 11)
#  ****************************************************



# Creating a dataframe that takes value corresponding to 0.1 and 0.35 for demo
Dat_1<-corn_data[corn_data$pc==0.1,]
Dat_2<-corn_data[corn_data$pc==0.35,]
Dat_2
Dat<-rbind(Dat_1,Dat_2)

write.csv(Dat, paste( "Output/TWo_Crop_price_corn.csv"),  row.names=FALSE)



DF=read.csv( "Output/TWo_Crop_price_corn.csv",header=T)

r1<-which(DF$AddlRevFromDL == max(DF[DF$pc==0.1,]$AddlRevFromDL, na.rm = T))
pw1<-DF[r1,3]

r2<-which(DF$AddlRevFromDL == max(DF[DF$pc==0.35,]$AddlRevFromDL, na.rm = T))
pw2<-DF[r2,3] 


p2<-ggplot(DF) +
  geom_point(aes(pw, AddlRevFromDL, group=pc, color = pc)) +
  geom_vline(xintercept=pw1, linetype="dashed", color = "red")+
  geom_vline(xintercept=pw2, linetype="dashed", color = "red")+
  geom_text(x = pw1-0.3, y = 120, label = "Best alternative = full irrigation", color = "black", angle = 90, size=5)+
  geom_text(x = pw1+0.3, y = 120, label = "Best alternative = fallow", color = "black", angle = 90, size=5)+
  geom_text(x = pw2-0.3, y = 110, label = "Best alternative = full irrigation", color = "black", angle = 90, size=5)+
  geom_text(x = pw2 + 0.8, y = 110, label = "Best alternative = fallow", color = "black", angle = 90, size=5)+
  scale_color_viridis(" price of crop ($/kg) ")+
  theme(text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm") )+
  xlim(0,8.5)+
  labs(title = '', x = ' ', y = '')

p2

p1<-ggplot(data) +
  geom_point(aes(pw, AddlRevFromDL, group=crop, color = pc)) +
  scale_color_viridis(" price of crop ($/kg) " , limits=c(min(data$pc), max(data$pc)))+
  facet_grid(cols =  vars(crop))+
  theme(text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm"))+
  xlim(0,8.5)+
  labs(title = '', x = 'Leasing price of water ($/ha-mm)', y = 'Additional Revenue from Deficit Irrigation ($/ha)')

p1

p3<-ggarrange(p2, p1, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")


p3

ggsave(plot =  p3, width = 10, height = 10, dpi = 300, filename =  paste("Figures/figure_11.png"))






#******************************************************
# Optimal Irrigation (figure 8)
#  ***************************************************


anno <- data.frame(x1 = c(min(corn_data$pc),min(wheat_data$pc)), x2 = c(max(corn_data$pc),max(wheat_data$pc)), 
                   y1 = c(max(corn_data[which(corn_data$pc==min(corn_data$pc)),]$pw), max(wheat_data[which(wheat_data$pc==min(wheat_data$pc)),]$pw)),  
                   y2 = c(max(corn_data$pw), max(wheat_data$pw)),
                   lab = c("***", "**"),
                   region = c("corn", "wheat"))

anno

pla<-data.frame(x=c(0.1,0.35,0.1,0.35), y=c(2.89+ 0.1, 10.14+0.1, 1.4+0.1, 4.94+0.1), crop=c('corn','corn','wheat','wheat'),
                end_x=c(0.175, 0.175, 0.175, 0.175), end_y=c(7.5, 7.5, 5, 5))

seg<-data.frame(crop=c('corn','wheat'),
                mid_x=c(0.225, 0.225), mid_y=c(4.5,  2.2),
                end_x=c(0.18, 0.19), end_y=c(6.2, 4.4))
seg
tex<-data.frame(crop=c('corn','wheat'),
                x=c(0.2, 0.2), y=c(6.5,  4.8))
tex
poly<-data.frame(x=c(0.1,0.1,0.35,0.35, 0.1,0.1,0.35,0.35), y=c(1.01, 8.2, 8.2,8.1, 0.35, 8, 8,3.78), 
                 crop=c('corn','corn','corn','corn','wheat','wheat', 'wheat','wheat')    )


p4<-ggplot(data) +
  geom_point(aes(pc, pw,  color = optimalirrigation)) +
  geom_polygon (data=poly, aes(x=x, y=y), fill="grey"  )+
  geom_text(data=tex, aes(x=x, y=y, ),label='Fallow and lease all CU', color='black', size=7) +
  geom_segment(data=seg, aes(x=mid_x,xend=end_x,y=mid_y, yend=end_y),arrow=arrow(length=unit(0.3,"cm")), size=2 )+
  scale_color_viridis(" optimal irrigation (mm)" , limits=c(min(data$optimalirrigation), max(data$optimalirrigation)))+
  theme(text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm"))+
  facet_grid(cols =  vars(crop))+
  ylim(0, 8.2)+
  labs(title = '', x = 'price of crop ($/kg)', y = 'Leasing price of water ($/ha-mm)')

p4

ggsave(plot =  p4, width = 10, height = 6, dpi = 300, filename =  paste("Figures/figure_8.png"))



#******************************************************
# Optimal lease (figure 9)
#****************************************************



p5<-ggplot(data) +
  geom_point(aes(pw, optimallease, group=crop, color = pc)) +
  scale_y_continuous(" Optimal leasing (mm)", sec.axis = sec_axis(~./4.85, name = "% of full irrigation demand leased " ))+
  scale_color_viridis(" Price ($/kg)")+
  facet_wrap(vars(crop), scales = 'free')+
  theme(axis.title.y.right = element_text(angle =90),text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm"))+
  
  labs(title = '', x = 'Leasing price of water ($/ha-mm)', y = ' Optimal Leasing (mm)')

p5

ggsave(plot =  p5, width = 10, height = 6, dpi = 300, filename =  paste("Figures/figure_9.png"))



#******************************************************
# Production lost (figure 10)
#****************************************************


p8<-ggplot(data = corn_data) +
  geom_point(aes(pw, production_lost, group=crop, color = pc)) +
  scale_y_continuous(" Loss in production (kg/ha)", sec.axis = sec_axis(~./125, name = " " ))+
  scale_color_viridis(" Price ($/kg)")+
  facet_wrap(vars(crop), scales = 'free')+
  theme(axis.title.y.right = element_text(angle =90),text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm"))+
  labs(title = '', x = 'Leasing price of water ($/ha-mm)', y = 'Loss in production (kg/ha)')

p8


p9<-ggplot(data = wheat_data) +
  geom_point(aes(pw, production_lost, group=crop, color = pc)) +
  scale_y_continuous( sec.axis = sec_axis(~./62, name = "% of full irrigation production lost " ))+
  scale_color_viridis(" Price ($/kg)")+
  facet_wrap(vars(crop), scales = 'free')+
  theme(axis.title.y.right = element_text(angle =90),text = element_text(size=18), legend.position="bottom", legend.key.width=unit(3,"cm"))+
  labs(title = '', x = 'Leasing price of water ($/ha-mm)', y = ' ')

p9

p11<-ggarrange(p8, p9, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

p11

ggsave(plot = p11, width = 10, height = 6, dpi = 300, filename =  paste("Figures/figure_10.png"))

