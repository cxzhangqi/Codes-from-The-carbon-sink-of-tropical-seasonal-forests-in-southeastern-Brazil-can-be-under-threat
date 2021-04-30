rm(list=ls())

library(dplyr);library(tidyr)
library(corrplot);library(PerformanceAnalytics);library(psych)
library(ggplot2)
library(MuMIn)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(ncf)
library(ggplot2)
library(dplyr)
library(caret)
library(DHARMa)
library(performance)
library(jtools)
library(interactions)
library(dplyr);library(tidyr)
library(corrplot);library(PerformanceAnalytics);library(psych)
library(ggplot2)
library(ggsci)
library(randomForest)
library(cowplot)
library(gridExtra)

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_p_t=subset(data_dissert1,select= -c(c_gains,c_losses, c_gains,c_stocks_f))#colocar sua variavel resposta ai, por exemplo, biomassa
head(data_p_t)

preproc.param <- data_p_t %>% 
  preProcess(method = c("center", "scale"))

data_p_t_transformed <- preproc.param %>% predict(data_p_t)

#
data_p_t_y= cbind.data.frame(subset(data_dissert1,select= c(c_gains,c_losses, c_gains,c_stocks_f)) ,data_p_t_transformed)
head(data_p_t_y)

###correlaçao entre peditores
data_predictors=subset(data_p_t_transformed,select= -c(plot_code_interval  ,  plot_code  , 
                                                       forest_type ))

corr=cor(data_predictors, method = c("pearson")) #matriz de correlaçao

data_p_t_y_f=subset(data_p_t_y,select=c(forest_type,site_area_ha,map,mat,p,cec,clay, som))

names(data_p_t_y_f)=c("Forest_type","Site_area","MAP","MAT","P","CEC","Clay", "SOM")

#############################
rf_sites=randomForest(as.factor(Forest_type)~Site_area+MAP+MAT+P+CEC+Clay+SOM,importance=TRUE, data=data_p_t_y_f)

rf_sites

importance(rf_sites)

tiff("rf_importance.tiff", units="cm", width=15, height=10, res=77)

varImpPlot(rf_sites,main="Predictor importance")

dev.off()
system("open rf_importance.tiff")

colfunc<-colorRampPalette(c("darkorange2", "mediumseagreen", "purple3"))

colours_palletes=colfunc(3)

#########plot MAT P

p5=ggplot(data_dissert1, aes(x = log(p), y =mat , col = forest_type)) +
  geom_point(size=2.7) +
  geom_jitter()+
  theme_classic()+
  labs(x=expression(P~(mg~cm^- 3)[(log~scale)]),
       y="MAT (ºC)",
       colour=" ", tag="A)")+ 
  theme(text = element_text(size = 14,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+  
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ 
  guides(colour = guide_legend(override.aes = list(size=3)))

p5

#plot MAT CEC

p6=ggplot(data_dissert1, aes(x = cec, y =mat , col = forest_type)) +
  geom_point(size=2.7) +
  geom_jitter()+
  theme_classic()+
  labs(x=expression(CEC~(cmol[c]~dm^- 3)),
       y="MAT (ºC)",
       colour=" ", tag="B)")+ 
  theme(text = element_text(size = 14,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+  
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ 
  guides(colour = guide_legend(override.aes = list(size=3)))

p6

allplotslist <- align_plots(p5,p6, align = "hv")
grid2=grid.arrange(allplotslist[[1]],allplotslist[[2]],nrow = 1)

ggsave("f_types_mat_soil.tiff",grid2, he = 10, wi = 27, un = "cm", dpi = 100)
system("open f_types_mat_soil.tiff")
