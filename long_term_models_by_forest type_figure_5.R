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

options(na.action = na.fail)

over <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

vif<- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_dissert1$weig= data_dissert1$interval_length^(1/3) + data_dissert1$sampled_area_ha^(1/4)-1 

data_dissert1_str= read.table("data_dyn_temp_structure.csv",header=T,dec=".",sep=";")
head(data_dissert1_str)

data_dissert1_str$weig_str=data_dissert1_str$sampled_area_ha^(1/3) 
##
colfunc<-colorRampPalette(c("darkorange2", "mediumseagreen", "purple3"))

colours_palletes=colfunc(3)

#############################
m_global_c_stocks_dec=lmer(log(c_stocks_ha)  ~ year +(1|plot_code),weights=data_dissert1_str[data_dissert1_str$forest_type=="Deciduous",]$weig_str, REML=FALSE,data=data_dissert1_str[data_dissert1_str$forest_type=="Deciduous",])
summary(m_global_c_stocks_dec)

m_global_c_stocks_ever=lmer(log(c_stocks_ha)  ~ year+(1|plot_code),weights=data_dissert1_str[data_dissert1_str$forest_type=="Evergreen",]$weig_str, REML=FALSE,data=data_dissert1_str[data_dissert1_str$forest_type=="Evergreen",])
summary(m_global_c_stocks_ever)

m_global_c_stocks_semi=lmer(log(c_stocks_ha)  ~ year +(1|plot_code),weights=data_dissert1_str[data_dissert1_str$forest_type=="Semideciduous",]$weig_str, REML=FALSE,data=data_dissert1_str[data_dissert1_str$forest_type=="Semideciduous",])
summary(m_global_c_stocks_semi)

m_global_c_stocks=lmer(log(c_stocks_ha)  ~ year*(forest_type ) +(1|plot_code),weights=weig_str, REML=FALSE,data=data_dissert1_str)
summary(m_global_c_stocks)

func_dec_ncs  <- function(x) {exp(fixef(m_global_c_stocks)["(Intercept)"]+fixef(m_global_c_stocks)["year"]*x  )     }

func_everg_ncs  <- function(x){exp(fixef(m_global_c_stocks)["(Intercept)"]+fixef(m_global_c_stocks)["forest_typeEvergreen"]+
    fixef(m_global_c_stocks)["year"]*x+ fixef(m_global_c_stocks)["year:forest_typeEvergreen"]*x)}

func_semi_ncs  <- function(x){exp(fixef(m_global_c_stocks)["(Intercept)"]+fixef(m_global_c_stocks)["forest_typeSemideciduous"]+
    fixef(m_global_c_stocks)["year"]*x+ fixef(m_global_c_stocks)["year:forest_typeSemideciduous"]*x)}

p1_c_stocks= ggplot(data_dissert1_str, aes(x = year, y = c_stocks_ha, col=forest_type)) +
  
  geom_point(size=2.7,alpha=0.67)+
  
  stat_function(fun = func_dec_ncs ,colour=colours_palletes[1],linetype="dashed", size=1.07,xlim = c(2001, 2019))+ 
  stat_function(fun = func_everg_ncs ,colour=colours_palletes[2],linetype="dashed",size=1.07,xlim = c(1995, 2019))+ 
  stat_function(fun = func_semi_ncs ,colour=colours_palletes[3],size=1.07)+
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~stocks~(Mg~C~ha^-1) ),
    color = " ",
    tag="A)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ theme(legend.direction = "horizontal", legend.position = "top")
p1_c_stocks

#################################
m_global_net_c_sink_dec=lmer(net_c_sink  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Deciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Deciduous",])
summary(m_global_net_c_sink_dec)

m_global_net_c_sink_ever=lmer(net_c_sink  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Evergreen",]$weig,data=data_dissert1[data_dissert1$forest_type=="Evergreen",])
summary(m_global_net_c_sink_ever)

m_global_net_c_sink_semi=lmer(net_c_sink  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Semideciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Semideciduous",])
summary(m_global_net_c_sink_semi)

m_global_net_c_sink=lmer(net_c_sink  ~ year*(forest_type ) +(1|plot_code), REML=FALSE,weights=weig,data=data_dissert1)
summary(m_global_net_c_sink)

func_dec_ncs  <- function(x) {fixef(m_global_net_c_sink)["(Intercept)"]+fixef(m_global_net_c_sink)["year"]*x       }

func_everg_ncs  <- function(x){fixef(m_global_net_c_sink)["(Intercept)"]+fixef(m_global_net_c_sink)["forest_typeEvergreen"]+
fixef(m_global_net_c_sink)["year"]*x+ fixef(m_global_net_c_sink)["year:forest_typeEvergreen"]*x}

func_semi_ncs  <- function(x){fixef(m_global_net_c_sink)["(Intercept)"]+fixef(m_global_net_c_sink)["forest_typeSemideciduous"]+
    fixef(m_global_net_c_sink)["year"]*x+ fixef(m_global_net_c_sink)["year:forest_typeSemideciduous"]*x}

p1_net_c_sink= ggplot(data_dissert1, aes(x = year, y = net_c_sink, col=forest_type)) +
  
  geom_point(size=2.7,alpha=0.67)+
  
  stat_function(fun = func_dec_ncs ,colour=colours_palletes[1],size=1.07,xlim = c(2001, 2019))+ 
  stat_function(fun = func_everg_ncs ,colour=colours_palletes[2],size=1.07,xlim = c(1995, 2019))+ 
  stat_function(fun = func_semi_ncs ,colour=colours_palletes[3],size=1.07)+
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1) ),
    color = " ",
    tag="B)")+ geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ theme(legend.direction = "horizontal", legend.position = "top")
p1_net_c_sink

#############################
m_global_c_gains_dec=lmer(log(c_gains)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Deciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Deciduous",])
summary(m_global_c_gains_dec)

m_global_c_gains_ever=lmer(log(c_gains)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Evergreen",]$weig,data=data_dissert1[data_dissert1$forest_type=="Evergreen",])
summary(m_global_c_gains_ever)

m_global_c_gains_semi=lmer(log(c_gains)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Semideciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Semideciduous",])
summary(m_global_c_gains_semi)

m_global_c_gains=lmer(log(c_gains)  ~ year*(forest_type ) +(1|plot_code), REML=FALSE,weights=weig,data=data_dissert1)
summary(m_global_c_gains)

func_dec_ncs  <- function(x) {exp(fixef(m_global_c_gains)["(Intercept)"]+fixef(m_global_c_gains)["year"]*x  )     }

func_everg_ncs  <- function(x){exp(fixef(m_global_c_gains)["(Intercept)"]+fixef(m_global_c_gains)["forest_typeEvergreen"]+
                                     fixef(m_global_c_gains)["year"]*x+ fixef(m_global_c_gains)["year:forest_typeEvergreen"]*x)}

func_semi_ncs  <- function(x){exp(fixef(m_global_c_gains)["(Intercept)"]+fixef(m_global_c_gains)["forest_typeSemideciduous"]+
                                    fixef(m_global_c_gains)["year"]*x+ fixef(m_global_c_gains)["year:forest_typeSemideciduous"]*x)}

p1_c_gains= ggplot(data_dissert1, aes(x = year, y = c_gains, col=forest_type)) +
  
  geom_point(size=2.7,alpha=0.67)+
  
  stat_function(fun = func_dec_ncs ,colour=colours_palletes[1],size=1.07,xlim = c(2001, 2019))+ 
  stat_function(fun = func_everg_ncs ,colour=colours_palletes[2],size=1.07,xlim = c(1995, 2019))+ 
  stat_function(fun = func_semi_ncs ,colour=colours_palletes[3],size=1.07)+
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1) ),
    color = " ",
    tag="C)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ theme(legend.direction = "horizontal", legend.position = "top")
p1_c_gains

#############################

m_global_c_losses_dec=lmer(log(c_losses)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Deciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Deciduous",])
summary(m_global_c_losses_dec)

m_global_c_losses_ever=lmer(log(c_losses)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Evergreen",]$weig,data=data_dissert1[data_dissert1$forest_type=="Evergreen",])
summary(m_global_c_losses_ever)

m_global_c_losses_Semi=lmer(log(c_losses)  ~ year +(1|plot_code), REML=FALSE,weights=data_dissert1[data_dissert1$forest_type=="Semideciduous",]$weig,data=data_dissert1[data_dissert1$forest_type=="Semideciduous",])
summary(m_global_c_losses_Semi)

m_global_c_losses=lmer(log(c_losses)  ~ year*(forest_type ) +(1|plot_code),weights=weig, REML=FALSE,data=data_dissert1)
summary(m_global_c_losses)

func_dec_ncs  <- function(x) {exp(fixef(m_global_c_losses)["(Intercept)"]+fixef(m_global_c_losses)["year"]*x  )     }

func_everg_ncs  <- function(x){exp(fixef(m_global_c_losses)["(Intercept)"]+fixef(m_global_c_losses)["forest_typeEvergreen"]+
                                     fixef(m_global_c_losses)["year"]*x+ fixef(m_global_c_losses)["year:forest_typeEvergreen"]*x)}

func_semi_ncs  <- function(x){exp(fixef(m_global_c_losses)["(Intercept)"]+fixef(m_global_c_losses)["forest_typeSemideciduous"]+
                                    fixef(m_global_c_losses)["year"]*x+ fixef(m_global_c_losses)["year:forest_typeSemideciduous"]*x)}

p1_c_losses= ggplot(data_dissert1, aes(x = year, y = c_losses, col=forest_type)) +
  
  geom_point(size=2.7,alpha=0.67)+
  
  stat_function(fun = func_dec_ncs ,colour=colours_palletes[1],size=1.07,xlim = c(2001, 2019))+ 
  stat_function(fun = func_everg_ncs ,colour=colours_palletes[2],size=1.07,xlim = c(1995, 2019))+ 
  stat_function(fun = func_semi_ncs ,colour=colours_palletes[3],size=1.07)+
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1) ),
    color = " ",
    tag="D)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ theme(legend.direction = "horizontal", legend.position = "top")
p1_c_losses

###############
library(cowplot)

allplotslist <- align_plots(p1_c_stocks,p1_net_c_sink,p1_c_gains,p1_c_losses, align = "hv")

library(gridExtra)

grid=grid.arrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],allplotslist[[4]],nrow = 2)
ggsave("plot_interactions_forest_type.tiff",grid, he = 22, wi = 23.3, un = "cm", dpi = 600)
system("open plot_interactions_forest_type.tiff")
