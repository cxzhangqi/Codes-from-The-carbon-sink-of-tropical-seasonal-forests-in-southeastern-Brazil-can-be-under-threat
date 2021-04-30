data_dyn=read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
data_dyn=dplyr::mutate_if(data_dyn, is.character, as.factor)

library(ggplot2)
library(viridis)

library(mgcv)
library(lme4)
library(MuMIn)

#######################################

m_globgam_mat=gam(mat ~s(year)+ s(plot_code,bs="re"), method="REML",data=data_dyn)

gam.check(m_globgam_mat)
hist(resid(m_globgam_mat))
plot(resid(m_globgam_mat)~fitted(m_globgam_mat))

summary(m_globgam_mat)
plot(m_globgam_mat,residuals=TRUE,select=1)

AICc(m_globgam_mat)

pred_gam_mat=predict(m_globgam_mat,exclude = 's(plot_code)')

m_globlmm_mat=lmer(mat ~year+ (1|plot_code), REML=TRUE,data=data_dyn)
hist(resid(m_globlmm_mat))
plot(m_globlmm_mat)
summary(m_globlmm_mat)

AICc(m_globlmm_mat)

pred_lmm_mat=predict(m_globlmm_mat,re.form=NA)

p1= ggplot(data_dyn, aes(x =year , y = mat, col=map)) +
  geom_point(size=2.7,alpha=0.71)+
  
  geom_line(aes(y=pred_lmm_mat), col="chartreuse3", lwd=1.07,lty=1)+
  geom_line(aes(y=pred_gam_mat), col="black", lwd=1.07,lty=2)+
  
  theme_classic() +
  labs(
    x = "Year",
    y = "MAT (ºC)",
    color = "MAP (mm)",
    tag="A)")+
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+scale_color_gradientn(colours = inferno(7)) +
  
  annotate("text", x=1997, y=23.75, label= expression(atop(edf~7.21~italic(P)~"<"~0.001,
                                                           paste(slope~0.036~italic(P)~"<"~0.001))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p1

#ggsave("mat_time.tiff", he = 15, wi = 17, un = "cm", dpi = 300)
#system("open mat_time.tiff")

##############################

m_globgam_map=gam(map ~s(year)+ s(plot_code,bs="re"), method="REML",data=data_dyn)

gam.check(m_globgam_map)
hist(resid(m_globgam_map))
plot(resid(m_globgam_map)~fitted(m_globgam_map))

summary(m_globgam_map)
plot(m_globgam_map,residuals=TRUE,select=1)

AICc(m_globgam_map)

pred_gam_map=predict(m_globgam_map,exclude = 's(plot_code)')

m_globlmm_map=lmer(map ~year+ (1|plot_code), REML=TRUE,data=data_dyn)
hist(resid(m_globlmm_map))
plot(m_globlmm_map)
summary(m_globlmm_map)

AICc(m_globlmm_map)

pred_lmm_map=predict(m_globlmm_map,re.form=NA)

p2= ggplot(data_dyn, aes(x =year , y = map, col=mat)) +
  geom_point(size=2.7,alpha=0.71)+
  
  geom_line(aes(y=pred_lmm_map), col="chartreuse3", lwd=1.07,lty=1)+
  geom_line(aes(y=pred_gam_map), col="black", lwd=1.07,lty=2)+
  
  theme_classic() +
  labs(
    x = "Year",
    y = "MAP (mm)",
    color = "MAT (ºC)",
    tag="B)")+
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+scale_color_gradientn(colours = inferno(7)) +
  
  annotate("text", x=1997, y=1900, label= expression(atop(edf~8.17~italic(P)~"<"~0.001,
                                                          paste(slope~-10.15~italic(P)~"<"~0.001))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p2

#ggsave("map_time.tiff", he = 15, wi = 17, un = "cm", dpi = 300)
#system("open map_time.tiff")

########################################################
m_globgam_co2_change=gam(co2_change ~s(year)+ s(plot_code,bs="re"), method="REML",data=data_dyn)

gam.check(m_globgam_co2_change)
hist(resid(m_globgam_co2_change))
plot(resid(m_globgam_co2_change)~fitted(m_globgam_co2_change))

summary(m_globgam_co2_change)
plot(m_globgam_co2_change,residuals=TRUE,select=1)

AICc(m_globgam_co2_change)

pred_gam_co2_change=predict(m_globgam_co2_change,exclude = 's(plot_code)')

m_globlmm_co2_change=lmer(co2_change ~year+ (1|plot_code), REML=TRUE,data=data_dyn)
hist(resid(m_globlmm_co2_change))
plot(m_globlmm_co2_change)
summary(m_globlmm_co2_change)

AICc(m_globlmm_co2_change)

pred_lmm_co2_change=predict(m_globlmm_co2_change,re.form=NA)

p3=ggplot(data_dyn, aes(x = year, y = co2_change, col = co2_atm)) +
  geom_point(size=2.7) +  
  geom_line(aes(y=pred_lmm_co2_change), col="chartreuse3", lwd=1.07,lty=1)+
  geom_line(aes(y=pred_gam_co2_change), col="black", lwd=1.07,lty=2)+
  geom_jitter()+
  theme_classic()+
  labs(x="Year",
       y=expression(CO[2]~-change~(ppm~yr^-1)),
       colour=expression(CO[2]~(ppm)),
       shape=" ", tag="C)")+ 
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+scale_color_gradientn(colours = inferno(7)) +
  
  annotate("text", x=1997, y=3, label= expression(atop(edf~5.5~italic(P)~"<"~0.001,
                                                          paste(slope~0.04~italic(P)~"<"~0.001))))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))

p3
#ggsave("co2_c_time.tiff", he = 15, wi = 17, un = "cm", dpi = 300)
#system("open co2_c_time.tiff")

########################

library(gridExtra) 

library(cowplot)

allplotslist <- align_plots(p1, p2,p3, align = "hv")

grid=grid.arrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],nrow = 1)

ggsave("Figure_3.tiff",grid, he = 10, wi = 44, un = "cm", dpi = 600)
system("open Figure_3.tiff") 

###########################################################
###########################################################
###########################################################
##################################

p4=ggplot(data_dyn, aes(x = year, y = co2_atm, col = co2_change)) +
  geom_point(size=3) +
  geom_jitter()+
  theme_classic()+
  labs(x="Year",
      y=expression(CO[2]~(ppm)),
      colour=expression(CO[2]~-change~(ppm~yr^-1)),
      shape=" ", tag=" ")+scale_color_gradientn(colours = inferno(7))+
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))

#p4

ggsave("co2_atm_time.tiff", he = 15, wi = 21, un = "cm", dpi = 300)
system("open co2_atm_time.tiff")
