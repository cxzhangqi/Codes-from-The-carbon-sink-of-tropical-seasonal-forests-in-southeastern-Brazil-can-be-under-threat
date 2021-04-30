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

data_dissert1= read.table("data_dyn_temp_structure.csv",header=T,dec=".",sep=";")
head(data_dissert1)

weig_str=data_dissert1$sampled_area_ha^(1/3) 

data_p_t=subset(data_dissert1,select= -c(c_stocks_ha))#colocar sua variavel resposta ai, por exemplo, biomassa
head(data_p_t)

preproc.param <- data_p_t %>% 
  preProcess(method = c("center", "scale"))

data_p_t_transformed <- preproc.param %>% predict(data_p_t)

#
data_p_t_y= cbind.data.frame(subset(data_dissert1,select= c(c_stocks_ha)) ,data_p_t_transformed)
head(data_p_t_y)

###correlaçao entre peditores
data_predictors=subset(data_p_t_transformed,select= -c(site_year_temp  ,  plot_code ,forest_type
))

corr=cor(data_predictors, method = c("pearson")) #matriz de correlaçao
n_max=8

###################

m_global_c_stocks_f=lmer(log(c_stocks_ha)  ~ site_area_ha  +year*(map+mat)+
                           p+cec+clay+som   +(1|plot_code),weights=weig_str,  REML=FALSE,data=data_p_t_y)
vif(m_global_c_stocks_f)

plot(m_global_c_stocks_f)
hist(resid(m_global_c_stocks_f))
shapiro.test(resid(m_global_c_stocks_f))

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_stocks_f, n = 500)
plot(simulationOutput)

#######################
morans<-correlog(x=data_dissert1$long,y=data_dissert1$lat,z=resid(m_global_c_stocks_f),latlon=TRUE,increment=100) ## testar increments
plot(morans)
str(morans)
morans$correlation
morans$p

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_stocks_f, n = 500)

simulationOutput1=recalculateResiduals(simulationOutput, group = data_dissert1$lat, aggregateBy = sum)

testSpatialAutocorrelation(simulationOutput = simulationOutput1, x = unique(data_dissert1$long), y= unique(data_dissert1$lat))


###############
coords<-cbind(data_dissert1$long,data_dissert1$lat);coords<-as.matrix(coords,longlat=T)
library(spdep)
#neighbours list
k1 <- knn2nb(knearneigh(coords,k=3, longlat = TRUE)) #do the same of poly2nb
k1
is.symmetric.nb(k1)

plot(k1, coords, col='green', lwd=2)

#max of list of vectors of distances corresponding to the neighbour object, 5 dists for each point
max.dist <- max(unlist(nbdists(k1, coords)))

#find neighbours base on max dist of knn
spat.weight<-dnearneigh(coords,0,max.dist, longlat = FALSE)
plot(spat.weight,coords,col='green', lwd=2)

spat.weight.W<-nb2listw(spat.weight, glist=NULL, style="W", zero.policy=TRUE)

moran.test(residuals(m_global_c_stocks_f), listw=spat.weight.W, zero.policy=TRUE,
           randomisation=FALSE)

moran.test(residuals(m_global_c_stocks_f), listw=spat.weight.W, zero.policy=TRUE,
           randomisation=TRUE)

moran.test(residuals(m_global_c_stocks_f), listw=spat.weight.W, zero.policy=TRUE,
           randomisation=FALSE,alternative="two.sided")

moran.mc(residuals(m_global_c_stocks_f), spat.weight.W, zero.policy=TRUE, 999)

###########################################
library(forecast)

Acf(residuals(m_global_c_stocks_f, type = "pearson"), type = c("correlation"),demean = TRUE, plot = TRUE, na.action = na.contiguous)
Pacf(residuals(m_global_c_stocks_f, type = "pearson"), lag.max = NULL, plot = TRUE, na.action = na.contiguous,demean = TRUE)

#################################
#model selection
dred1m_global_c_stocks_f=dredge(m_global_c_stocks_f,m.lim=c(0,n_max),subset=abs(corr) < 0.6,trace=2) 
dredm_c_stocks_f=(subset(dred1m_global_c_stocks_f, subset=delta <= 4))
dredm_c_stocks_f

resm_c_stocks_f=summary(model.avg(dredm_c_stocks_f))
resm_c_stocks_f

r.squaredGLMM(m_global_c_stocks_f)

m1_c_stocks_f=get.models(dredm_c_stocks_f, subset = 1)[[1]] #the best model
r.squaredGLMM(m1_c_stocks_f)

save(m_global_c_stocks_f,dred1m_global_c_stocks_f,dredm_c_stocks_f, resm_c_stocks_f,  file = "modelling1_c_stocks_f.RData")

########

for (i in 1:length(dredm_c_stocks_f)) {
  print(vif(get.models(dredm_c_stocks_f, subset = i)[[1]]))  
}

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

rm(list=ls())

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

weig= data_dissert1$interval_length^(1/3) + data_dissert1$sampled_area_ha^(1/4)-1 

data_p_t=subset(data_dissert1,select= -c(c_gains,c_losses, net_c_sink,c_stocks_f))#colocar sua variavel resposta ai, por exemplo, biomassa
head(data_p_t)

preproc.param <- data_p_t %>% 
  preProcess(method = c("center", "scale"))

data_p_t_transformed <- preproc.param %>% predict(data_p_t)

#
data_p_t_y= cbind.data.frame(subset(data_dissert1,select= c(c_gains,c_losses, net_c_sink,c_stocks_f)) ,data_p_t_transformed)
head(data_p_t_y)

###correlaçao entre peditores
data_predictors=subset(data_p_t_transformed,select= -c(plot_code_interval  ,  plot_code  , 
                                                       forest_type ))

corr=cor(data_predictors, method = c("pearson")) #matriz de correlaçao
n_max=5

###################

m_global_net_c_sink=lmer(net_c_sink  ~ site_area_ha  +year*(map+mat)+
                           p+cec+clay+som   +(1|plot_code), REML=FALSE,weights=weig,data=data_p_t_y)
vif(m_global_net_c_sink)

plot(m_global_net_c_sink)
hist(resid(m_global_net_c_sink))
shapiro.test(resid(m_global_net_c_sink))

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_net_c_sink, n = 500)
plot(simulationOutput)

#######################
morans<-correlog(x=data_dissert1$long,y=data_dissert1$lat,z=resid(m_global_net_c_sink),latlon=TRUE,increment=100) ## testar increments
plot(morans)
str(morans)
morans$correlation
morans$p

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_net_c_sink, n = 500)

simulationOutput1=recalculateResiduals(simulationOutput, group = data_dissert1$lat, aggregateBy = sum)

testSpatialAutocorrelation(simulationOutput = simulationOutput1, x = unique(data_dissert1$long), y= unique(data_dissert1$lat))

###############
coords<-cbind(data_dissert1$long,data_dissert1$lat);coords<-as.matrix(coords,longlat=T)
library(spdep)
#neighbours list
k1 <- knn2nb(knearneigh(coords,k=3, longlat = TRUE)) #do the same of poly2nb
k1
is.symmetric.nb(k1)

plot(k1, coords, col='green', lwd=2)

#max of list of vectors of distances corresponding to the neighbour object, 5 dists for each point
max.dist <- max(unlist(nbdists(k1, coords)))

#find neighbours base on max dist of knn
spat.weight<-dnearneigh(coords,0,max.dist, longlat = FALSE)
plot(spat.weight,coords,col='green', lwd=2)

spat.weight.W<-nb2listw(spat.weight, glist=NULL, style="W", zero.policy=FALSE)

moran.test(residuals(m_global_net_c_sink), listw=spat.weight.W,
           randomisation=FALSE)

moran.test(residuals(m_global_net_c_sink), listw=spat.weight.W,
           randomisation=TRUE)

moran.test(residuals(m_global_net_c_sink), listw=spat.weight.W,
           randomisation=FALSE,alternative="two.sided")

moran.mc(residuals(m_global_net_c_sink), spat.weight.W, 999)

###########################################
library(forecast)

Acf(residuals(m_global_net_c_sink, type = "pearson"), type = c("correlation"),demean = TRUE, plot = TRUE, na.action = na.contiguous)
Pacf(residuals(m_global_net_c_sink, type = "pearson"), lag.max = NULL, plot = TRUE, na.action = na.contiguous,demean = TRUE)

#################################
#model selection
dred1m_global_net_c_sink=dredge(m_global_net_c_sink,m.lim=c(0,n_max),subset=abs(corr) < 0.6,trace=2) 
dredm_net_c_sink=(subset(dred1m_global_net_c_sink, subset=delta <= 4))
dredm_net_c_sink

resm_net_c_sink=summary(model.avg(dredm_net_c_sink))
resm_net_c_sink

r.squaredGLMM(m_global_net_c_sink)

m1_net_c_sink=get.models(dredm_net_c_sink, subset = 1)[[1]] #the best model
r.squaredGLMM(m1_net_c_sink)

AICc(m1_net_c_sink)

#see mat:

dred1m_global_net_c_sink_mat=dredge(m_global_net_c_sink,m.lim=c(0,n_max),subset=abs(corr) < 0.6,fixed="mat",trace=2) 
dredm_net_c_sink_mat=(subset(dred1m_global_net_c_sink_mat, subset=delta <= 4))
dredm_net_c_sink_mat

m1_net_c_sink_mat=get.models(dredm_net_c_sink_mat, subset = 1)[[1]] #the best model
summary(m1_net_c_sink_mat)
AICc(m1_net_c_sink_mat)

save(m_global_net_c_sink,dred1m_global_net_c_sink,dredm_net_c_sink, resm_net_c_sink,m1_net_c_sink_mat,  file = "modelling1_net_c_sink.RData")

########

for (i in 1:length(dredm_net_c_sink)) {
  print(vif(get.models(dredm_net_c_sink, subset = i)[[1]]))  
}

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

rm(list=ls())

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

weig= data_dissert1$interval_length^(1/3) + data_dissert1$sampled_area_ha^(1/4)-1 

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
n_max=5

###################

m_global_c_gains=lmer(log(c_gains)  ~ site_area_ha  +year*(map+mat)+
                        p+cec+clay+som   +(1|plot_code),weights=weig, REML=FALSE,data=data_p_t_y)
vif(m_global_c_gains)

plot(m_global_c_gains)
hist(resid(m_global_c_gains))
shapiro.test(resid(m_global_c_gains))

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_gains, n = 500)
plot(simulationOutput)

########################
morans<-correlog(x=data_dissert1$long,y=data_dissert1$lat,z=resid(m_global_c_gains),latlon=TRUE,increment=100) ## testar increments
plot(morans)
str(morans)
morans$correlation
morans$p

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_gains, n = 500)

simulationOutput1=recalculateResiduals(simulationOutput, group = data_dissert1$lat, aggregateBy = sum)

testSpatialAutocorrelation(simulationOutput = simulationOutput1, x = unique(data_dissert1$long), y= unique(data_dissert1$lat))

###############
coords<-cbind(data_dissert1$long,data_dissert1$lat);coords<-as.matrix(coords,longlat=T)
library(spdep)
#neighbours list
k1 <- knn2nb(knearneigh(coords,k=3, longlat = TRUE)) #do the same of poly2nb
k1
is.symmetric.nb(k1)

plot(k1, coords, col='green', lwd=2)

#max of list of vectors of distances corresponding to the neighbour object, 5 dists for each point
max.dist <- max(unlist(nbdists(k1, coords)))

#find neighbours base on max dist of knn
spat.weight<-dnearneigh(coords,0,max.dist, longlat = FALSE)
plot(spat.weight,coords,col='green', lwd=2)

spat.weight.W<-nb2listw(spat.weight, glist=NULL, style="W", zero.policy=FALSE)

moran.test(residuals(m_global_c_gains), listw=spat.weight.W,
           randomisation=FALSE)

moran.test(residuals(m_global_c_gains), listw=spat.weight.W,
           randomisation=TRUE)

moran.test(residuals(m_global_c_gains), listw=spat.weight.W,
           randomisation=FALSE,alternative="two.sided")

moran.mc(residuals(m_global_c_gains), spat.weight.W, 999)

###########################################
library(forecast)

Acf(residuals(m_global_c_gains, type = "pearson"), type = c("correlation"),demean = TRUE, plot = TRUE, na.action = na.contiguous)
Pacf(residuals(m_global_c_gains, type = "pearson"), lag.max = NULL, plot = TRUE, na.action = na.contiguous,demean = TRUE)

#################################
#model selection
dred1m_global_c_gains=dredge(m_global_c_gains,m.lim=c(0,n_max),subset=abs(corr) < 0.6,trace=2) 
dredm_c_gains=(subset(dred1m_global_c_gains, subset=delta <= 4))
dredm_c_gains

resm_c_gains=summary(model.avg(dredm_c_gains))
resm_c_gains

r.squaredGLMM(m_global_c_gains)

m1_c_gains=get.models(dredm_c_gains, subset = 1)[[1]] #the best model
r.squaredGLMM(m1_c_gains)

AICc(m1_c_gains)

#see mat:

dred1m_global_c_gains_mat=dredge(m_global_c_gains,m.lim=c(0,n_max),subset=abs(corr) < 0.6,fixed="mat",trace=2) 
dredm_c_gains_mat=(subset(dred1m_global_c_gains_mat, subset=delta <= 4))
dredm_c_gains_mat

m1_c_gains_mat=get.models(dredm_c_gains_mat, subset = 1)[[1]] #the best model
summary(m1_c_gains_mat)
AICc(m1_c_gains_mat)

save(m_global_c_gains,dred1m_global_c_gains,dredm_c_gains, resm_c_gains,m1_c_gains_mat,  file = "modelling1_c_gains.RData")

########

for (i in 1:length(dredm_c_gains)) {
  print(vif(get.models(dredm_c_gains, subset = i)[[1]]))  
}

###################################################################
###################################################################
###################################################################
###################################################################
###################################################################

rm(list=ls())

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

weig= data_dissert1$interval_length^(1/3) + data_dissert1$sampled_area_ha^(1/4)-1 

data_p_t=subset(data_dissert1,select= -c(c_losses,c_losses, c_losses,c_stocks_f))#colocar sua variavel resposta ai, por exemplo, biomassa
head(data_p_t)

preproc.param <- data_p_t %>% 
  preProcess(method = c("center", "scale"))

data_p_t_transformed <- preproc.param %>% predict(data_p_t)

#
data_p_t_y= cbind.data.frame(subset(data_dissert1,select= c(c_losses,c_losses, c_losses,c_stocks_f)) ,data_p_t_transformed)
head(data_p_t_y)

###correlaçao entre peditores
data_predictors=subset(data_p_t_transformed,select= -c(plot_code_interval  ,  plot_code  , 
                                                       forest_type ))

corr=cor(data_predictors, method = c("pearson")) #matriz de correlaçao
n_max=5

###################

m_global_c_losses=lmer(log(c_losses)  ~ site_area_ha  +year*(map+mat)+
                         p+cec+clay+som   +(1|plot_code), REML=FALSE,weights=weig,data=data_p_t_y)
vif(m_global_c_losses)

plot(m_global_c_losses)
hist(resid(m_global_c_losses))
shapiro.test(resid(m_global_c_losses))

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_losses, n = 500)
plot(simulationOutput)

#######################
morans<-correlog(x=data_dissert1$long,y=data_dissert1$lat,z=resid(m_global_c_losses),latlon=TRUE,increment=100) ## testar increments
plot(morans)
str(morans)
morans$correlation
morans$p

#############
simulationOutput <- DHARMa::simulateResiduals(fittedModel = m_global_c_losses, n = 500)

simulationOutput1=recalculateResiduals(simulationOutput, group = data_dissert1$lat, aggregateBy = sum)

testSpatialAutocorrelation(simulationOutput = simulationOutput1, x = unique(data_dissert1$long), y= unique(data_dissert1$lat))

###############
coords<-cbind(data_dissert1$long,data_dissert1$lat);coords<-as.matrix(coords,longlat=T)
library(spdep)
#neighbours list
k1 <- knn2nb(knearneigh(coords,k=3, longlat = TRUE)) #do the same of poly2nb
k1
is.symmetric.nb(k1)

plot(k1, coords, col='green', lwd=2)

#max of list of vectors of distances corresponding to the neighbour object, 5 dists for each point
max.dist <- max(unlist(nbdists(k1, coords)))

#find neighbours base on max dist of knn
spat.weight<-dnearneigh(coords,0,max.dist, longlat = FALSE)
plot(spat.weight,coords,col='green', lwd=2)

spat.weight.W<-nb2listw(spat.weight, glist=NULL, style="W", zero.policy=FALSE)

moran.test(residuals(m_global_c_losses), listw=spat.weight.W,
           randomisation=FALSE)

moran.test(residuals(m_global_c_losses), listw=spat.weight.W,
           randomisation=TRUE)

moran.test(residuals(m_global_c_losses), listw=spat.weight.W,
           randomisation=FALSE,alternative="two.sided")

moran.mc(residuals(m_global_c_losses), spat.weight.W, 999)

###########################################
library(forecast)

Acf(residuals(m_global_c_losses, type = "pearson"), type = c("correlation"),demean = TRUE, plot = TRUE, na.action = na.contiguous)
Pacf(residuals(m_global_c_losses, type = "pearson"), lag.max = NULL, plot = TRUE, na.action = na.contiguous,demean = TRUE)

#################################
#model selection
dred1m_global_c_losses=dredge(m_global_c_losses,m.lim=c(0,n_max),subset=abs(corr) < 0.6,trace=2) 
dredm_c_losses=(subset(dred1m_global_c_losses, subset=delta <= 4))
dredm_c_losses

resm_c_losses=summary(model.avg(dredm_c_losses))
resm_c_losses

r.squaredGLMM(m_global_c_losses)

m1_c_losses=get.models(dredm_c_losses, subset = 1)[[1]] #the best model
r.squaredGLMM(m1_c_losses)

save(m_global_c_losses,dred1m_global_c_losses,dredm_c_losses, resm_c_losses,  file = "modelling1_c_losses.RData")

########

for (i in 1:length(dredm_c_losses)) {
  print(vif(get.models(dredm_c_losses, subset = i)[[1]]))  
}
