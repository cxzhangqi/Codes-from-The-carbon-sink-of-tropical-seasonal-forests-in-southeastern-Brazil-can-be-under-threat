library(ggplot2)
library(dplyr);library(tidyr)
library(gridExtra)
library(MuMIn)

load("modelling1_c_stocks_f.RData")
load("modelling1_net_c_sink.RData")
load("modelling1_c_gains.RData")
load("modelling1_c_losses.RData")

vars_vec=c("mat","site_area_ha","year","cec","p","som","map", "clay","mat:year","map:year")
f_col=  "darkorange2"
col_line= "forestgreen" #"darkgoldenrod2"  #"gray69"
shape_t=21 
size_p=0.86
  
###########################################
#####################################################
#plot c_stocks_f

resm_c_stocks_f

r.squaredGLMM(m_global_c_stocks_f)

res=resm_c_stocks_f

coef_data <- as.data.frame(coefTable(res))
coef_data <- as.data.frame(coefTable(res))
coef_data2 <- coef_data
coef_data <- coef_data[-1,]
Interc <- coef_data2[1,1]
colnames(coef_data) <- c("Estim", "SE")
coef_data$Var <- rownames(coef_data)
coef_data$SE <- coef_data$SE * 1.96
coef_data <- data.frame(coef_data)
coef_data <- transform(coef_data)
coef_data

miss_vars=cbind.data.frame(Estim =0,      SE=0, NA.=NA, Var=vars_vec[!vars_vec %in% coef_data$Var])
coef_data=rbind(coef_data,miss_vars)

### grafico coeficientes e IC

graf1<-ggplot(coef_data, aes(x=Var, y=Estim))+
  geom_hline(yintercept=0, linetype="solid", colour="gray73",size=0.57)+
  #geom_point(stat="identity", fill="grey50", colour="black")+
  geom_pointrange(aes(ymax=Estim+SE, ymin=Estim-SE),size=size_p,shape =shape_t,fill=f_col, position=position_dodge(.9),colour=col_line)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=14.37,colour="black"),axis.text.x = element_text(size=14.37,colour="black"),
       axis.text.y=element_text(size=14.37,colour="black"))+
  scale_y_continuous("Effect size")+
  ggtitle("")+
  scale_x_discrete(" ", labels=c("map"="MAP","mat"="MAT","site_area_ha"="Site area","year"="Year",
                               "map:year"="MAP:Year","mat:year"="MAT:Year",  "p"="P","cec"="CEC","clay"="Clay","som"="SOM")) +  labs(tag="A)")+ theme(legend.position = "none")

graf1

###########################################
#####################################################
#plot net_c_sink

resm_net_c_sink

r.squaredGLMM(m_global_net_c_sink)

res=resm_net_c_sink

coef_data <- as.data.frame(coefTable(res))
coef_data <- as.data.frame(coefTable(res))
coef_data2 <- coef_data
coef_data <- coef_data[-1,]
Interc <- coef_data2[1,1]
colnames(coef_data) <- c("Estim", "SE")
coef_data$Var <- rownames(coef_data)
coef_data$SE <- coef_data$SE * 1.96
coef_data <- data.frame(coef_data)
coef_data <- transform(coef_data)
coef_data

miss_vars=cbind.data.frame(Estim =0,      SE=0, NA.=NA, Var=vars_vec[!vars_vec %in% coef_data$Var])
coef_data=rbind(coef_data,miss_vars)

### grafico coeficientes e IC

graf2<-ggplot(coef_data, aes(x=Var, y=Estim))+
  geom_hline(yintercept=0, linetype="solid", colour="gray73",size=0.57)+
  #geom_point(stat="identity", fill="grey50", colour="black")+
  geom_pointrange(aes(ymax=Estim+SE, ymin=Estim-SE),size=size_p,shape =shape_t,fill=f_col, position=position_dodge(.9),colour=col_line)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=14.37,colour="black"),axis.text.x = element_text(size=14.37,colour="black"),
        axis.text.y=element_text(size=14.37,colour="black"))+
  scale_y_continuous("Effect size")+
  ggtitle("")+
  scale_x_discrete(" ", labels=c("map"="MAP","mat"="MAT","site_area_ha"="Site area","year"="Year",
                                 "map:year"="MAP:Year","mat:year"="MAT:Year",  "p"="P","cec"="CEC","clay"="Clay","som"="SOM")) +   labs(tag="B)")+ theme(legend.position = "none")

graf2

###########################################
#####################################################
#plot c_gains

resm_c_gains

r.squaredGLMM(m_global_c_gains)

res=resm_c_gains

coef_data <- as.data.frame(coefTable(res))
coef_data <- as.data.frame(coefTable(res))
coef_data2 <- coef_data
coef_data <- coef_data[-1,]
Interc <- coef_data2[1,1]
colnames(coef_data) <- c("Estim", "SE")
coef_data$Var <- rownames(coef_data)
coef_data$SE <- coef_data$SE * 1.96
coef_data <- data.frame(coef_data)
coef_data <- transform(coef_data)
coef_data

miss_vars=cbind.data.frame(Estim =0,      SE=0, NA.=NA, Var=vars_vec[!vars_vec %in% coef_data$Var])
coef_data=rbind(coef_data,miss_vars)

### grafico coeficientes e IC

graf3<-ggplot(coef_data, aes(x=Var, y=Estim))+
  geom_hline(yintercept=0, linetype="solid", colour="gray73",size=0.57)+
  #geom_point(stat="identity", fill="grey50", colour="black")+
  geom_pointrange(aes(ymax=Estim+SE, ymin=Estim-SE),size=size_p,shape =shape_t,fill=f_col, position=position_dodge(.9),colour=col_line)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=14.37,colour="black"),axis.text.x = element_text(size=14.37,colour="black"),
        axis.text.y=element_text(size=14.37,colour="black"))+
  scale_y_continuous("Effect size")+
  ggtitle("")+
  scale_x_discrete(" ",labels=c("map"="MAP","mat"="MAT","site_area_ha"="Site area","year"="Year",
                                "map:year"="MAP:Year","mat:year"="MAT:Year",  "p"="P","cec"="CEC","clay"="Clay","som"="SOM")) +  labs(tag="C)")+ theme(legend.position = "none")

graf3

###########################################
#####################################################
#plot c_losses

resm_c_losses

r.squaredGLMM(m_global_c_losses)

res=resm_c_losses

coef_data <- as.data.frame(coefTable(res))
coef_data <- as.data.frame(coefTable(res))
coef_data2 <- coef_data
coef_data <- coef_data[-1,]
Interc <- coef_data2[1,1]
colnames(coef_data) <- c("Estim", "SE")
coef_data$Var <- rownames(coef_data)
coef_data$SE <- coef_data$SE * 1.96
coef_data <- data.frame(coef_data)
coef_data <- transform(coef_data)
coef_data

miss_vars=cbind.data.frame(Estim =0,      SE=0, NA.=NA, Var=vars_vec[!vars_vec %in% coef_data$Var])
coef_data=rbind(coef_data,miss_vars)

### grafico coeficientes e IC

graf4<-ggplot(coef_data, aes(x=Var, y=Estim))+
  geom_hline(yintercept=0, linetype="solid", colour="gray73",size=0.57)+
  #geom_point(stat="identity", fill="grey50", colour="black")+
  geom_pointrange(aes(ymax=Estim+SE, ymin=Estim-SE),size=size_p,shape =shape_t,fill=f_col, position=position_dodge(.9),colour=col_line)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=14.37,colour="black"),axis.text.x = element_text(size=14.37,colour="black"),
        axis.text.y=element_text(size=14.37,colour="black"))+
  scale_y_continuous("Effect size")+
  ggtitle("")+
  scale_x_discrete(" ", labels=c("map"="MAP","mat"="MAT","site_area_ha"="Site area","year"="Year",
                                 "map:year"="MAP:Year","mat:year"="MAT:Year",  "p"="P","cec"="CEC","clay"="Clay","som"="SOM")) +  labs(tag="D)")+ theme(legend.position = "none")

graf4

################

grid=grid.arrange(graf1,graf2, graf3,graf4 ,nrow = 2)

ggsave("overall_modelling.tiff",grid, he = 25, wi = 27, un = "cm", dpi = 600)
system("open overall_modelling.tiff")     
