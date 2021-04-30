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

weig= data_dissert$interval_length^(1/3) + data_dissert$sampled_area_ha^(1/4)-1 

###############################################
#c_stocks_f

data_dissert1_str= read.table("data_dyn_temp_structure.csv",header=T,dec=".",sep=";")
head(data_dissert1_str)

data_dissert_str= mutate_if(data_dissert1_str, is.character, as.factor)
length(levels(data_dissert_str$plot_code))

weig_str=data_dissert_str$sampled_area_ha^(1/3) 

m_globgam_c_stocks_f=gam(log(c_stocks_ha) ~s(year)+ s(plot_code,bs="re"), weights=weig_str, method="REML", data=data_dissert_str)

###############################################
#net_c_sink

m_globgam_net_c_sink=gam(net_c_sink ~s(year,k=23)+ s(plot_code,bs="re"), weights=weig, method="REML",data=data_dissert)

##############################
#c_gains

m_globgam_c_gains=gam(log(c_gains) ~s(year,k=7)+ s(plot_code,bs="re"),weights=weig,  method="REML", data=data_dissert)

###########################################
#c_losses

m_globgam_c_losses=gam(log(c_losses) ~s(year)+ s(plot_code,bs="re"),weights=weig,  method="REML",data=data_dissert)

#########################################
df_preds=cbind.data.frame(average=c(rep("obs",4),rep(2003,4),rep(2018,4)), 
                          carbon_dyn=rep(c("c_stocks","net_c_sink", "c_gains","c_losses"),3),min=NA, mean=NA,max=NA)

df_pred2003cs=cbind.data.frame(year=2003,plot_code=data_dissert1_str$plot_code)

df_pred2018cs=cbind.data.frame(year=2018,plot_code=data_dissert1_str$plot_code)

df_pred2003=cbind.data.frame(year=2003,plot_code=data_dissert$plot_code)

df_pred2018=cbind.data.frame(year=2018,plot_code=data_dissert$plot_code)

df_preds$min[1]= min(data_dissert_str$c_stocks_ha) #
df_preds$min[2]= min(data_dissert$net_c_sink)
df_preds$min[3]= min(data_dissert$c_gains)
df_preds$min[4]= min(data_dissert$c_losses)

df_preds$min[5]= min(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2003cs)))
df_preds$min[6]= min((predict(m_globgam_net_c_sink, newdata = df_pred2003)))
df_preds$min[7]= min(exp(predict(m_globgam_c_gains, newdata = df_pred2003)))
df_preds$min[8]= min(exp(predict(m_globgam_c_losses, newdata = df_pred2003)))

df_preds$min[9]= min(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2018cs)))
df_preds$min[10]= min((predict(m_globgam_net_c_sink, newdata = df_pred2018)))
df_preds$min[11]= min(exp(predict(m_globgam_c_gains, newdata = df_pred2018)))
df_preds$min[12]= min(exp(predict(m_globgam_c_losses, newdata = df_pred2018)))

df_preds$mean[1]= mean(data_dissert_str$c_stocks_ha)
df_preds$mean[2]= mean(data_dissert$net_c_sink)
df_preds$mean[3]= mean(data_dissert$c_gains)
df_preds$mean[4]= mean(data_dissert$c_losses)

df_preds$mean[5]= mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2003cs)))
df_preds$mean[6]= mean((predict(m_globgam_net_c_sink, newdata = df_pred2003)))
df_preds$mean[7]= mean(exp(predict(m_globgam_c_gains, newdata = df_pred2003)))
df_preds$mean[8]= mean(exp(predict(m_globgam_c_losses, newdata = df_pred2003)))

df_preds$mean[9]= mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2018cs)))
df_preds$mean[10]= mean((predict(m_globgam_net_c_sink, newdata = df_pred2018)))
df_preds$mean[11]= mean(exp(predict(m_globgam_c_gains, newdata = df_pred2018)))
df_preds$mean[12]= mean(exp(predict(m_globgam_c_losses, newdata = df_pred2018)))

df_preds$max[1]= max(data_dissert_str$c_stocks_ha)
df_preds$max[2]= max(data_dissert$net_c_sink)
df_preds$max[3]= max(data_dissert$c_gains)
df_preds$max[4]= max(data_dissert$c_losses)

df_preds$max[5]= max(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2003cs)))
df_preds$max[6]= max((predict(m_globgam_net_c_sink, newdata = df_pred2003)))
df_preds$max[7]= max(exp(predict(m_globgam_c_gains, newdata = df_pred2003)))
df_preds$max[8]= max(exp(predict(m_globgam_c_losses, newdata = df_pred2003)))

df_preds$max[9]= max(exp(predict(m_globgam_c_stocks_f, newdata = df_pred2018cs)))
df_preds$max[10]= max((predict(m_globgam_net_c_sink, newdata = df_pred2018)))
df_preds$max[11]= max(exp(predict(m_globgam_c_gains, newdata = df_pred2018)))
df_preds$max[12]= max(exp(predict(m_globgam_c_losses, newdata = df_pred2018)))

df_preds

#write.csv(df_preds,"predictions_gam.csv")

##########inflection

#carbon stocks

df_pred_inflec_cs=cbind.data.frame(year=2011,plot_code=data_dissert1_str$plot_code)

mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred_inflec_cs)))


df_pred_inflec_cs=cbind.data.frame(year=2012,plot_code=data_dissert1_str$plot_code)

mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred_inflec_cs)))


df_pred_inflec_cs=cbind.data.frame(year=2013,plot_code=data_dissert1_str$plot_code)

mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred_inflec_cs)))


df_pred_inflec_cs=cbind.data.frame(year=2014,plot_code=data_dissert1_str$plot_code)

mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred_inflec_cs)))


df_pred_inflec_cs=cbind.data.frame(year=2015,plot_code=data_dissert1_str$plot_code)

mean(exp(predict(m_globgam_c_stocks_f, newdata = df_pred_inflec_cs)))

############

df_pred_inflec_ncs=cbind.data.frame(year=2011,plot_code=data_dissert1_str$plot_code)

mean((predict(m_globgam_net_c_sink, newdata = df_pred_inflec_ncs)))


df_pred_inflec_ncs=cbind.data.frame(year=2012,plot_code=data_dissert1_str$plot_code)

mean((predict(m_globgam_net_c_sink, newdata = df_pred_inflec_ncs)))


df_pred_inflec_ncs=cbind.data.frame(year=2013,plot_code=data_dissert1_str$plot_code)

mean((predict(m_globgam_net_c_sink, newdata = df_pred_inflec_ncs)))


df_pred_inflec_ncs=cbind.data.frame(year=2014,plot_code=data_dissert1_str$plot_code)

mean((predict(m_globgam_net_c_sink, newdata = df_pred_inflec_ncs)))