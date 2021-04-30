rm(list=ls())

library(mgcv)
library(ggplot2)
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
library(viridis)
library(voxel)
library(gridExtra) 
library(itsadug)

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_dissert= mutate_if(data_dissert1, is.character, as.factor)
length(levels(data_dissert$plot_code))

weig= data_dissert1$interval_length^(1/3) + data_dissert1$sampled_area_ha^(1/4)-1 

#####################################
#####################################
#####################################

#c_stocks_f

data_dissert1_str= read.table("data_dyn_temp_structure.csv",header=T,dec=".",sep=";")
head(data_dissert1_str)

data_dissert_str= mutate_if(data_dissert1_str, is.character, as.factor)
length(levels(data_dissert_str$plot_code))

weig_str=data_dissert1_str$sampled_area_ha^(1/3) 

m_globgam_c_stocks_f=gam(log(c_stocks_ha) ~s(year)+ s(plot_code,bs="re"), method="REML",weights=weig_str, data=data_dissert_str)

gam.check(m_globgam_c_stocks_f )
hist(resid(m_globgam_c_stocks_f ))
plot(resid(m_globgam_c_stocks_f )~fitted(m_globgam_c_stocks_f ))

summary(m_globgam_c_stocks_f )

plot(m_globgam_c_stocks_f ,residuals=TRUE,select=1)

m_globlmm_c_stocks_f =lmer(log(c_stocks_ha)  ~year+ (1|plot_code), REML=TRUE,weights=weig_str,data=data_dissert_str)
hist(resid(m_globlmm_c_stocks_f ))
plot(m_globlmm_c_stocks_f )

summary(m_globlmm_c_stocks_f )

pred_lmm_c_stocks_f =predict(m_globlmm_c_stocks_f ,re.form=NA)

r.squaredGLMM(m_globlmm_c_stocks_f)

###############################################
#net_c_sink

m_globgam_net_c_sink=gam(net_c_sink ~s(year,k=23)+ s(plot_code,bs="re"), method="REML",weights=weig,data=data_dissert)

gam.check(m_globgam_net_c_sink )
hist(resid(m_globgam_net_c_sink ))
plot(resid(m_globgam_net_c_sink )~fitted(m_globgam_net_c_sink ))

summary(m_globgam_net_c_sink )

plot(m_globgam_net_c_sink ,residuals=TRUE,select=1)

m_globlmm_net_c_sink =lmer(net_c_sink  ~year+ (1|plot_code),weights=weig, REML=TRUE,data=data_dissert)
hist(resid(m_globlmm_net_c_sink ))
plot(m_globlmm_net_c_sink )

summary(m_globlmm_net_c_sink )

pred_lmm_net_c_sink =predict(m_globlmm_net_c_sink ,re.form=NA)

r.squaredGLMM(m_globlmm_net_c_sink)

##############################
#c_gains

m_globgam_c_gains=gam(log(c_gains) ~s(year,k=7)+ s(plot_code,bs="re"),weights=weig, method="REML", data=data_dissert)

gam.check(m_globgam_c_gains )
hist(resid(m_globgam_c_gains ))
plot(resid(m_globgam_c_gains )~fitted(m_globgam_c_gains ))

summary(m_globgam_c_gains )

plot(m_globgam_c_gains ,residuals=TRUE,select=1)

m_globgam_c_gains1=gam(log(c_gains) ~s(year,k=7)+ s(plot_code,bs="re"), method="REML", data=data_dissert)
m_globgam_c_gains2=gam(log(c_gains) ~s(year,k=20)+ s(plot_code,bs="re"), method="REML", data=data_dissert)
AICc(m_globgam_c_gains1)
AICc(m_globgam_c_gains2)

m_globlmm_c_gains =lmer(log(c_gains)  ~year+ (1|plot_code),weights=weig, REML=TRUE,data=data_dissert)
hist(resid(m_globlmm_c_gains ))
plot(m_globlmm_c_gains )

summary(m_globlmm_c_gains )

r.squaredGLMM(m_globlmm_c_gains)

###########################################
#c_losses

m_globgam_c_losses=gam(log(c_losses) ~s(year)+ s(plot_code,bs="re"),weights=weig, method="REML",data=data_dissert)

gam.check(m_globgam_c_losses )
hist(resid(m_globgam_c_losses ))
plot(resid(m_globgam_c_losses )~fitted(m_globgam_c_losses ))

summary(m_globgam_c_losses )

plot(m_globgam_c_losses ,residuals=TRUE,select=1)

m_globlmm_c_losses =lmer(log(c_losses)  ~year+ (1|plot_code),weights=weig, REML=TRUE,data=data_dissert)
hist(resid(m_globlmm_c_losses ))
plot(m_globlmm_c_losses )

summary(m_globlmm_c_losses )

r.squaredGLMM(m_globlmm_c_losses)

#########################################
#plots

tiff("dyn1_time.tiff", units="cm", width=20, height=20, res=600)

par(mfrow=c(2,2))

col1= "deepskyblue2"
  
#c_stocks_f
par(mar=c(5,6,4,2)+0.1)

plot(data_dissert1_str$c_stocks_ha~data_dissert1_str$year,
     xlab=expression(Year), ylab=expression(Carbon~stocks~(Mg~C~ha^-1)),pch=21,
     col = col1,cex.lab=1.33,cex=1.47)

curve(exp(fixef(m_globlmm_c_stocks_f)[1]+fixef(m_globlmm_c_stocks_f)[2]*x),add=TRUE,lty=1,lwd=2,col="darkgoldenrod2")

plot_smooth(m_globgam_c_stocks_f, view='year',add=TRUE, rm.ranef=TRUE,rug = FALSE,
            col="black",shade=FALSE,lty=2,lwd=2.73,se=0,transform = exp)#h0 = 0

text(y=160,x=1998,cex=1.1,  expression(atop(edf~4.9~italic(P)~"<"~0.001,
                                            paste(slope~0.007~italic(P)~"<"~0.001))))

mtext('A)', side=3, line=1, at=min(data_dissert1_str$year),cex=1.3) #at the first value of x

#net_c_sink

par(mar=c(5,6,4,2)+0.1)

plot(data_dissert1$net_c_sink~data_dissert1$year,
     xlab=expression(Year), ylab=expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1)),pch=21,
     col = col1,cex.lab=1.33,cex=1.47)

curve(fixef(m_globlmm_net_c_sink)[1]+fixef(m_globlmm_net_c_sink)[2]*x,add=TRUE,lty=1,lwd=2,col="darkgoldenrod2")

plot_smooth(m_globgam_net_c_sink, view='year',add=TRUE, rm.ranef=TRUE,rug = FALSE,
            col="black",shade=FALSE,lty=2,lwd=2.73,se=0)#
abline(h=0,col="red",lty=3)
text(y=-4,x=2000,cex=1.1,  expression(atop(edf~2.88~italic(P)~"<"~0.001,
                                           paste(slope~-0.13~italic(P)~"<"~0.001))))

mtext('B)', side=3, line=1, at=min(data_dissert1$year),cex=1.3) #at the first value of x

#c_gains
par(mar=c(5,6,4,2)+0.1)

plot(data_dissert1$c_gains~data_dissert1$year,
     xlab=expression(Year), ylab=expression(Carbon~gains~(Mg~C~ha^-1~yr^-1)),pch=21,
     col = col1,cex.lab=1.33,cex=1.47)
curve(exp(fixef(m_globlmm_c_gains)[1]+fixef(m_globlmm_c_gains)[2]*x),add=TRUE,lty=1,lwd=2,col="darkgoldenrod2")
plot_smooth(m_globgam_c_gains, view='year',add=TRUE, rm.ranef=TRUE,rug = FALSE,
            col="black",shade=FALSE,lty=2,lwd=2.73,se=0,transform = exp)#h0 = 0
text(y=6.3,x=1999,cex=1.1,  expression(atop(edf~2.46~italic(P)~"<"~0.001,
                                            paste(slope~-0.03~italic(P)~"<"~0.001))))
mtext('C)', side=3, line=1, at=min(data_dissert1$year),cex=1.3) #at the first value of x

#c_losses

par(mar=c(5,6,4,2)+0.1)

plot(data_dissert1$c_losses~data_dissert1$year,
     xlab=expression(Year), ylab=expression(Carbon~losses~(Mg~C~ha^-1~yr^-1)),pch=21,
     col = col1,cex.lab=1.33,cex=1.47)
curve(exp(fixef(m_globlmm_c_losses)[1]+fixef(m_globlmm_c_losses)[2]*x),add=TRUE,lty=1,lwd=2,col="darkgoldenrod2")
plot_smooth(m_globgam_c_losses, view='year',add=TRUE, rm.ranef=TRUE,rug = FALSE,
            col="black",shade=FALSE,lty=2,lwd=2.73,se=0,transform = exp)#
text(y=7,x=2000,cex=1.1, expression(atop(edf~2.8~italic(P)~"<"~0.001,
                                         paste(slope~0.034~italic(P)~"<"~0.001)))) 
mtext('D)', side=3, line=1, at=min(data_dissert1$year),cex=1.3) #at the first value of x

#######################################

dev.off()
system("open dyn1_time.tiff")

