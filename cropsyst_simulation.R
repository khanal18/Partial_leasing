#install.packages("gganimate")
# Loading required libraries
library(rgdal)
library(tidyverse)
library(ggpmisc)
library (gganimate)
library (dplyr)
library(grid)
library(viridis)
library(ggpubr)


data<-read.csv("Output/cropsyst_output_data.csv")


# Creating subset to remove the Envelope strategy for first plot
Data<-data[data$Simulation !='Envelope',]


# Creating 3rd degree polynomial
my.formula <- y ~ poly(x,3, raw= TRUE)

#colnames(data)


#**********************************************
#  Equation and plot with each strategy
#*********************************************
  
  

p1<-ggplot(data = data, mapping = aes(x = IrrigationAppliedMM, y = Yield, color=crop)) +
  geom_point( aes(group=crop, color = crop)) +
  
  stat_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE), size = 1, aes(group = crop, color = crop))+
  labs( title= " ", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+
  stat_poly_eq(formula = my.formula,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE) +
  theme(text = element_text(size=18), legend.position="bottom") +
  #scale_color_viridis(discrete = TRUE)+
  scale_color_manual(values = c("corn" = "black", "wheat" = "blue"))+
  scale_y_continuous(breaks = pretty(data$Yield, n = 5)) +
  #facet_grid(rows=cols = vars(Simulation))
  facet_wrap( ~ Simulation,ncol = 2, nrow=3,dir="v")

p1
ggsave(plot =  p1, width = 10, height = 10, dpi = 300, filename =  paste("Figures/rawpoly.png"))


#**************************************************
#  Plotting all strategies plot (Figure 6 of paper)
#**************************************************


p2<-ggplot(data = Data, mapping = aes(x = IrrigationAppliedMM, y = Yield, color=crop)) +
  geom_point( aes(group=crop, color = crop)) +
  
  stat_smooth(method = "lm", formula = y ~ poly(x,3, raw=TRUE), size = 1, aes(group = crop, color = crop))+
  labs( title= " ", y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+
  stat_poly_eq(formula = my.formula,
               aes(label = paste( ..rr.label.., sep = "~~~")),label.x = 'left',label.y='top',
               parse = TRUE) +
  theme(text = element_text(size=18), legend.position="bottom") +
  #scale_color_viridis(discrete = TRUE)+
  scale_color_manual(values = c("corn" = "black", "wheat" = "blue"))+
  #scale_color_manual(values = c("black",  "blue"))+
  scale_y_continuous(breaks = pretty(data$Yield,n=2)) +
  #facet_grid(rows=cols = vars(Simulation))
  facet_wrap( ~ Simulation,ncol = 2, nrow=2,dir="v")

p2

ggsave(plot =  p2, width = 10, height = 8, dpi = 300, filename =  paste("Figures/figure_6.png"))



#**************************************************
#  Plotting all strategies plot (Figure 7 of paper)
#**************************************************
#*
#Making simulation name short for figre 7

data$Simulation[data$Simulation == "Continuous deficit all growing season (CDAS)"] <- "CDAS"
data$Simulation[data$Simulation == "Continuous deficit except during flowering (CDEF)"] <- "CDEF"
data$Simulation[data$Simulation == "Continuous deficit after June 1 (CDAJ1)"]  <- "CDAJ1"
data$Simulation[data$Simulation == "Continuous deficit after flowering (CDAF)"] <- "CDAF"


# plotting it now
p3<-ggplot(data = data, mapping = aes(x = IrrigationAppliedMM, y = Yield)) +
  #geom_point( aes(color = Simulation)) +
  
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=F, aes( group = Simulation, color = Simulation, size=Simulation))+
  labs( title=" ",  y="Yield ( kg/ha)", x = "Irrigation water applied (mm/year)")+
  scale_size_manual(values = c("CDAS" = 1, "CDEF" = 1, "CDAJ1"= 1,"CDAF"=1,"Envelope"=2))+
  #scale_color_viridis(discrete = TRUE)+
  scale_color_manual(values = c("CDAS" = "red", "CDEF" = "purple",
                                "CDAJ1"= 'green',"CDAF"='blue',"Envelope"='black' ))+
  #scale_y_continuous(breaks = pretty(data$Yield, n = 5)) +
  #scale_x_continuous(breaks = pretty(data$IrrigationAppliedMM, n = 5)) +
  facet_grid(cols=vars(crop))+
  theme(text = element_text(size=20), legend.position="right")
p3

ggsave(plot = p3, width = 10, height = 6, dpi = 300, filename =  paste("Figures/figure_7.png"))




