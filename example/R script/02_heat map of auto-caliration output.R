#' plot model calibration outcomes trialed with matrix of parameter values

#---
# 1. read in model calibration outcome
#---

calibration<-read.csv("../R output/auto-calibration.csv")
colnames(calibration)[4]<-"nse.temp"

min(calibration$nse.temp)
max(calibration$nse.temp)

#---
# 2. heap map
#---
library(ggplot2)
library(dplyr)

calibration%>%
  ggplot(aes(x=wse,y=vmc,fill=nse.temp))+
  geom_tile()+
  scale_fill_distiller(palette = "PuBu",direction = 1)+
  facet_grid(~lec)+
  xlab("Wind stirring efficiency")+
  ylab("Vertical mixing coefficient")+
  labs(title="Light extinction coefficient",fill="NSE")+
  theme_bw()+
  theme(plot.title = element_text(size=11,hjust = 0.5))+
  ggsave(filename = "../R figures/DYCD calibration_temp.png")

