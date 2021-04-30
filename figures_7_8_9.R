rm(list=ls())

library(dplyr);library(tidyr)
library(corrplot);library(PerformanceAnalytics);library(psych)
library(ggplot2)
library(MuMIn)
library(lme4)
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
library(RColorBrewer)
library(cowplot)
library(gridExtra)

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp_structure.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_or_sc_str=data_dissert1

load("modelling1_c_stocks_f.RData")

resm_c_stocks_f

b0_cs= 4.481143       
b_map_cs=  0.103708 
b_year_cs= 0.057706 
b_year_cs_map=   0.029206 
b_mat_cs=  -0.256209 
b_year_cs_mat=  -0.032607 

#####################################################################

colfunc<-colorRampPalette(c("orange","orange2", "seagreen3", "forestgreen","purple2","slateblue3"))

colours_palletes=colfunc(7)

#year : map

func_yr_map_mean_x_year_cs <- function(x) {exp (b0_cs      + b_year_cs     *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year)  ) )}

func_yr_map_minus_sd_x_year_cs <- function(x){exp (b0_cs      + b_year_cs     *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year) ) +
                                                  b_year_cs_map  *( ((x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year))*-2) + b_map_cs     *(-2) )}

func_yr_map_mean_plus_sd_x_year_cs <- function(x) {exp (b0_cs      + b_year_cs      *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year) ) +
                                                       b_year_cs_map *( ((x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year))*+1 ) + b_map_cs  *(+1) )}

p1_c_stocks= ggplot(data_or_sc_str, aes(x = year, y = c_stocks_ha, col=map)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_x_year_cs,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_x_year_cs,colour=colours_palletes[1],size=1.07,xlim = c(2000, 2020))+ 
  stat_function(fun = func_yr_map_mean_plus_sd_x_year_cs,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~stocks~(Mg~C~ha^-1) ),
    color = expression(MAP~(mm)),
    tag="A)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1997, y=170, label= expression(atop(Slope~0.06~italic(P)~"<"~0.001,
                                                       paste(Interaction~0.03~italic(P)~"="~0.008))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p1_c_stocks

#map: year

func_yr_map_mean_cs <- function(x) {exp(b0_cs      + b_map_cs     *( (x- mean(data_or_sc_str$map)) /sd(data_or_sc_str$map)  ) )}

func_yr_map_minus_sd_cs <- function(x){exp(b0_cs      + b_map_cs     *( (x- mean(data_or_sc_str$map)) /sd(data_or_sc_str$map) ) +
                                          b_year_cs_map  *( ((x- mean(data_or_sc_str$map)) /sd(data_or_sc_str$map))*-1) + b_year_cs     *(-1) )}

func_yr_map_mean_plus_sd_cs <- function(x) {exp(b0_cs      + b_map_cs      *( (x- mean(data_or_sc_str$map)) /sd(data_or_sc_str$map) ) +
                                               b_year_cs_map *( ((x- mean(data_or_sc_str$map)) /sd(data_or_sc_str$map))*+1 ) + b_year_cs  *(+1) )}

p2_c_stocks= ggplot(data_or_sc_str, aes(x = map, y = c_stocks_ha, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_cs,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_cs,colour=colours_palletes[3.5],size=1.07)+ #
  stat_function(fun = func_yr_map_mean_plus_sd_cs,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAP~(mm)),
    y = expression(Carbon~stocks~(Mg~C~ha^-1) ),
    color = expression(Year),
    tag="A)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1100, y=160, label= expression(atop(Slope~0.1~italic(P)~"<"~0.001,
                                                         paste(Interaction~0.03~italic(P)~"="~0.008))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p2_c_stocks

#year: mat

func_yr_mat_mean_x_year_cs <- function(x) {exp(b0_cs      + b_year_cs    *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year)  ) )}

func_yr_mat_minus_sd_x_year_cs <- function(x){exp(b0_cs     + b_year_cs    *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year) ) +
                                                 b_year_cs_mat  *( ((x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year))*-1.7) + b_mat_cs     *(-1.7) )}

func_yr_mat_mean_plus_sd_x_year_cs <- function(x) {exp(b0_cs    + b_year_cs      *( (x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year) ) +
                                                      b_year_cs_mat *( ((x- mean(data_or_sc_str$year)) /sd(data_or_sc_str$year))*+1.7 ) + b_mat_cs  *(+1.7) )}

p3_c_stocks= ggplot(data_or_sc_str, aes(x = year, y = c_stocks_ha, col=mat)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_x_year_cs,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_x_year_cs,colour=colours_palletes[1],size=1.07,xlim = c(1997, 2020))+ 
  stat_function(fun = func_yr_mat_mean_plus_sd_x_year_cs,colour=colours_palletes[7],size=1.07,xlim = c(2000, 2020))+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~stocks~(Mg~C~ha^-1) ),
    color = "MAT (ºC) ",
    tag="B)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1998, y=170, label= expression(atop(Slope~0.06~italic(P)~"<"~0.001,
                                                         paste(Interaction~-0.03~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p3_c_stocks

#mat: year

func_yr_mat_mean_cs <- function(x) {exp(b0_cs      + b_mat_cs     *( (x- mean(data_or_sc_str$mat)) /sd(data_or_sc_str$mat)  ) )}

func_yr_mat_minus_sd_cs <- function(x){exp(b0_cs      + b_mat_cs     *( (x- mean(data_or_sc_str$mat)) /sd(data_or_sc_str$mat) ) +
                                          b_year_cs_mat  *( ((x- mean(data_or_sc_str$mat)) /sd(data_or_sc_str$mat))*-1) + b_year_cs     *(-1) )}

func_yr_mat_mean_plus_sd_cs <- function(x) {exp(b0_cs      + b_mat_cs      *( (x- mean(data_or_sc_str$mat)) /sd(data_or_sc_str$mat) ) +
                                               b_year_cs_mat *( ((x- mean(data_or_sc_str$mat)) /sd(data_or_sc_str$mat))*+1 ) + b_year_cs  *(+1) )}

p4_c_stocks= ggplot(data_or_sc_str, aes(x = mat, y = c_stocks_ha, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_cs,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_cs,colour=colours_palletes[3.5],size=1.07)+ #,xlim = c(1500, max(data_or_sc_str$mat))
  stat_function(fun = func_yr_mat_mean_plus_sd_cs,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAT~(ºC)),
    y = expression(Carbon~stocks~(Mg~C~ha^-1) ),
    color = expression(Year),
    tag="B)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=22, y=170, label= expression(atop(Slope~-0.26~italic(P)~"<"~0.001,
                                                     paste(Interaction~-0.03~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p4_c_stocks

######################################################################
######################################################################
######################################################################

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_or_sc=data_dissert1

load("modelling1_net_c_sink.RData")

resm_net_c_sink

b0_ncs= 0.64735    
b_map_ncs=  0.06614
b_year_ncs= -0.87143
b_year_ncs_map= 0.63744

m1_net_c_sink_mat

b0_ncs_mat= 0.53135            
b_year_ncs.m= -0.96098 
b_mat_ncs= -0.03915 
b_year_ncs_mat= -0.49920 

#####################################################################

colfunc<-colorRampPalette(c("orange","orange2", "seagreen3", "forestgreen","purple2","slateblue3"))

colours_palletes=colfunc(7)

#year : map

func_yr_map_mean_x_year_ncs <- function(x) {b0_ncs      + b_year_ncs     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) }

func_yr_map_minus_sd_x_year_ncs  <- function(x){b0_ncs      + b_year_ncs     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
    b_year_ncs_map  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1.7) + b_map_ncs     *(-1.7) }

func_yr_map_mean_plus_sd_x_year_ncs  <- function(x) {b0_ncs      + b_year_ncs      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
    b_year_ncs_map *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1 ) + b_map_ncs  *(+1) }

p1_net_c_sink= ggplot(data_or_sc, aes(x = year, y = net_c_sink, col=map)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_x_year_ncs ,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_x_year_ncs ,colour=colours_palletes[1],size=1.07,xlim = c(2003, 2019))+ 
  stat_function(fun = func_yr_map_mean_plus_sd_x_year_ncs ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1) ),
    color = expression(MAP~(mm)),
    tag="C)")+ geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=2000, y=-5, label= expression(atop(Slope~-0.87~italic(P)~"<"~0.001,
                                                        paste(Interaction~0.64~italic(P)~"="~0.004))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p1_net_c_sink

#map: year

func_yr_map_mean_ncs  <- function(x) {b0_ncs      + b_map_ncs     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map)  ) }

func_yr_map_minus_sd_ncs  <- function(x){b0_ncs      + b_map_ncs     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
    b_year_ncs_map  *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*-0.8) + b_year_ncs     *(-0.8) }

func_yr_map_mean_plus_sd_ncs  <- function(x) {b0_ncs      + b_map_ncs      *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
    b_year_ncs_map *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*+1.5 ) + b_year_ncs  *(+1.5) }

p2_net_c_sink= ggplot(data_or_sc, aes(x = map, y = net_c_sink, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_ncs ,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_ncs ,colour=colours_palletes[3],size=1.07)+ #
  stat_function(fun = func_yr_map_mean_plus_sd_ncs ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAP~(mm)),
    y = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="C)")+ geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1500, y=-5, label= expression(atop(Slope~0.07~italic(P)~"="~0.74,
                                                        paste(Interaction~0.64~italic(P)~"="~0.004))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p2_net_c_sink

#year: mat

func_yr_mat_mean_x_year_ncs  <- function(x) {b0_ncs_mat      + b_year_ncs.m     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) }

func_yr_mat_minus_sd_x_year_ncs  <- function(x){b0_ncs_mat      + b_year_ncs.m     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
    b_year_ncs_mat  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1.7) + b_mat_ncs     *(-1.7) }

func_yr_mat_mean_plus_sd_x_year_ncs  <- function(x) {b0_ncs_mat      + b_year_ncs.m      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
    b_year_ncs_mat *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1.7 ) + b_mat_ncs  *(+1.7) }

p3_net_c_sink= ggplot(data_or_sc, aes(x = year, y = net_c_sink, col=mat)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_x_year_ncs ,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_x_year_ncs ,colour=colours_palletes[1],size=1.07,xlim = c(1997, 2019))+ 
  stat_function(fun = func_yr_mat_mean_plus_sd_x_year_ncs ,colour=colours_palletes[7],size=1.07,xlim = c(2003, 2019))+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1) ),
    color = "MAT (ºC) ",
    tag="D)")+ geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=2000, y=-5, label= expression(atop(Slope~-0.96~italic(P)~"<"~0.001,
                                                        paste(Interaction~-0.5~italic(P)~"="~0.009))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p3_net_c_sink

#mat: year

func_yr_mat_mean_ncs  <- function(x) {b0_ncs_mat      + b_mat_ncs     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat)  ) }

func_yr_mat_minus_sd_ncs  <- function(x){b0_ncs_mat      + b_mat_ncs     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
    b_year_ncs_mat  *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*-0.8) + b_year_ncs.m     *(-0.8) }

func_yr_mat_mean_plus_sd_ncs  <- function(x) {b0_ncs_mat      + b_mat_ncs      *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
    b_year_ncs_mat *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*+1.5 ) + b_year_ncs.m  *(+1.5) }

p4_net_c_sink= ggplot(data_or_sc, aes(x = mat, y = net_c_sink, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_ncs ,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_ncs ,colour=colours_palletes[3],size=1.07)+ #,xlim = c(1500, max(data_or_sc$mat))
  stat_function(fun = func_yr_mat_mean_plus_sd_ncs ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAT~(ºC)),
    y = expression(Net~carbon~sink~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="D)")+ geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=20, y=-5, label= expression(atop(Slope~-0.04~italic(P)~"="~0.82,
                                                      paste(Interaction~-0.5~italic(P)~"="~0.009))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p4_net_c_sink

#####################################################################
#####################################################################
#####################################################################

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_or_sc=data_dissert1

load("modelling1_c_gains.RData")

resm_c_gains

b0_cg= 0.78936    
b_map_cg= 0.29529    
b_year_cg= -0.12601
b_year_cg_map=  0.17844

m1_c_gains_mat

b0_cg_mat= 0.76299             
b_year_cg.m=  -0.17404
b_mat_cg= -0.26150
b_year_cg_mat= -0.15586  

#####################################################################

colfunc<-colorRampPalette(c("orange","orange2", "seagreen3", "forestgreen","purple2","slateblue3"))

colours_palletes=colfunc(7)

#year : map

func_yr_map_mean_x_year_cg <- function(x) {exp (b0_cg      + b_year_cg     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) )}

func_yr_map_minus_sd_x_year_cg <- function(x){exp (b0_cg      + b_year_cg     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                     b_year_cg_map  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1.7) + b_map_cg     *(-1.7) )}

func_yr_map_mean_plus_sd_x_year_cg <- function(x) {exp (b0_cg      + b_year_cg      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                          b_year_cg_map *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1 ) + b_map_cg  *(+1) )}

p1_c_gains= ggplot(data_or_sc, aes(x = year, y = c_gains, col=map)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_x_year_cg,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_x_year_cg,colour=colours_palletes[1],size=1.07,xlim = c(2003, 2019))+ 
  stat_function(fun = func_yr_map_mean_plus_sd_x_year_cg,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1) ),
    color = expression(MAP~(mm)),
    tag="A)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=2000, y=6, label= expression(atop(Slope~-0.13~italic(P)~"="~0.007,
                                                       paste(Interaction~0.18~italic(P)~"="~0.005))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p1_c_gains

#map: year

func_yr_map_mean_cg <- function(x) {exp(b0_cg      + b_map_cg     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map)  ) )}

func_yr_map_minus_sd_cg <- function(x){exp(b0_cg      + b_map_cg     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
                                             b_year_cg_map  *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*-0.8) + b_year_cg     *(-0.8) )}

func_yr_map_mean_plus_sd_cg <- function(x) {exp(b0_cg      + b_map_cg      *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
                                                  b_year_cg_map *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*+1.5 ) + b_year_cg  *(+1.5) )}

p2_c_gains= ggplot(data_or_sc, aes(x = map, y = c_gains, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_cg,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_cg,colour=colours_palletes[3],size=1.07)+ #
  stat_function(fun = func_yr_map_mean_plus_sd_cg,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAP~(mm)),
    y = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="E)")+ # geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1250, y=6.5, label= expression(atop(Slope~0.3~italic(P)~"<"~0.001,
                                                         paste(Interaction~0.18~italic(P)~"="~0.005))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p2_c_gains

#year: mat

func_yr_mat_mean_x_year_cg <- function(x) {exp(b0_cg_mat      + b_year_cg.m     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) )}

func_yr_mat_minus_sd_x_year_cg <- function(x){exp(b0_cg_mat      + b_year_cg.m     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                    b_year_cg_mat  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1) + b_mat_cg     *(-1) )}

func_yr_mat_mean_plus_sd_x_year_cg <- function(x) {exp(b0_cg_mat      + b_year_cg.m      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                         b_year_cg_mat *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1.7 ) + b_mat_cg  *(+1.7) )}

p3_c_gains= ggplot(data_or_sc, aes(x = year, y = c_gains, col=mat)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_x_year_cg,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_x_year_cg,colour=colours_palletes[2],size=1.07,xlim = c(1997, 2019))+ 
  stat_function(fun = func_yr_mat_mean_plus_sd_x_year_cg,colour=colours_palletes[7],size=1.07,xlim = c(2003, 2019))+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1) ),
    color = "MAT (ºC) ",
    tag="B)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1998, y=6.5, label= expression(atop(Slope~-0.17~italic(P)~"<"~0.001,
                                                         paste(Interaction~-0.16~italic(P)~"="~0.005))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p3_c_gains

#mat: year

func_yr_mat_mean_cg <- function(x) {exp(b0_cg      + b_mat_cg     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat)  ) )}

func_yr_mat_minus_sd_cg <- function(x){exp(b0_cg      + b_mat_cg     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
                                             b_year_cg_mat  *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*-0.8) + b_year_cg     *(-0.8) )}

func_yr_mat_mean_plus_sd_cg <- function(x) {exp(b0_cg      + b_mat_cg      *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
                                                  b_year_cg_mat *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*+1.5 ) + b_year_cg  *(+1.5) )}

p4_c_gains= ggplot(data_or_sc, aes(x = mat, y = c_gains, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_cg,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_cg,colour=colours_palletes[3],size=1.07)+ #,xlim = c(1500, max(data_or_sc$mat))
  stat_function(fun = func_yr_mat_mean_plus_sd_cg,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAT~(ºC)),
    y = expression(Carbon~gains~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="F)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=22, y=6, label= expression(atop(Slope~-0.26~italic(P)~"<"~0.001,
                                                     paste(Interaction~-0.16~italic(P)~"="~0.005))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p4_c_gains

######################################################################
######################################################################
######################################################################

options(na.action = na.fail)

data_dissert1= read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
head(data_dissert1)

data_or_sc=data_dissert1

load("modelling1_c_losses.RData")

resm_c_losses

b0_cl= 0.44615        
b_map_cl=   0.32120
b_year_cl=0.33927
b_year_cl_map= -0.29477
b_mat_cl= -0.30337
b_year_cl_mat=  0.19450

#####################################################################

colfunc<-colorRampPalette(c("orange","orange2", "seagreen3", "forestgreen","purple2","slateblue3"))

colours_palletes=colfunc(7)

#year : map

func_yr_map_mean_x_year_cl <- function(x) {exp (b0_cl      + b_year_cl     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) )}

func_yr_map_minus_sd_x_year_cl  <- function(x){exp (b0_cl      + b_year_cl     *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                      b_year_cl_map  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1.7) + b_map_cl     *(-1.7) )}

func_yr_map_mean_plus_sd_x_year_cl  <- function(x) {exp (b0_cl      + b_year_cl      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                           b_year_cl_map *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1 ) + b_map_cl  *(+1) )}

p1_c_losses= ggplot(data_or_sc, aes(x = year, y = c_losses, col=map)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_x_year_cl ,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_x_year_cl ,colour=colours_palletes[1],size=1.07,xlim = c(2003, 2019))+ 
  stat_function(fun = func_yr_map_mean_plus_sd_x_year_cl ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1) ),
    color = expression(MAP~(mm)),
    tag="C)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=2000, y=6, label= expression(atop(Slope~0.34~italic(P)~"<"~0.001,
                                                       paste(Interaction~-0.29~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p1_c_losses

#map: year

func_yr_map_mean_cl  <- function(x) {exp(b0_cl      + b_map_cl     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map)  ) )}

func_yr_map_minus_sd_cl  <- function(x){exp(b0_cl      + b_map_cl     *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
                                              b_year_cl_map  *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*-0.8) + b_year_cl     *(-0.8) )}

func_yr_map_mean_plus_sd_cl  <- function(x) {exp(b0_cl      + b_map_cl      *( (x- mean(data_or_sc$map)) /sd(data_or_sc$map) ) +
                                                   b_year_cl_map *( ((x- mean(data_or_sc$map)) /sd(data_or_sc$map))*+1.5 ) + b_year_cl  *(+1.5) )}

p2_c_losses= ggplot(data_or_sc, aes(x = map, y = c_losses, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_map_mean_cl ,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_map_minus_sd_cl ,colour=colours_palletes[3],size=1.07)+ #,xlim = c(1500, max(data_or_sc$map))
  stat_function(fun = func_yr_map_mean_plus_sd_cl ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAP~(mm)),
    y = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="G)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1400, y=7.5, label= expression(atop(Slope~0.32~italic(P)~"<"~0.001,
                                                         paste(Interaction~-0.29~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p2_c_losses

#year: mat

func_yr_mat_mean_x_year_cl  <- function(x) {exp(b0_cl      + b_year_cl    *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year)  ) )}

func_yr_mat_minus_sd_x_year_cl  <- function(x){exp(b0_cl     + b_year_cl    *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                     b_year_cl_mat  *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*-1) + b_mat_cl     *(-1) )}

func_yr_mat_mean_plus_sd_x_year_cl  <- function(x) {exp(b0_cl    + b_year_cl      *( (x- mean(data_or_sc$year)) /sd(data_or_sc$year) ) +
                                                          b_year_cl_mat *( ((x- mean(data_or_sc$year)) /sd(data_or_sc$year))*+1.7 ) + b_mat_cl  *(+1.7) )}

p3_c_losses= ggplot(data_or_sc, aes(x = year, y = c_losses, col=mat)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_x_year_cl ,colour=colours_palletes[4],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_x_year_cl ,colour=colours_palletes[2],size=1.07,xlim = c(1997, 2019))+ 
  stat_function(fun = func_yr_mat_mean_plus_sd_x_year_cl ,colour=colours_palletes[7],size=1.07,xlim = c(2003, 2019))+
  
  
  theme_classic() +
  labs(
    x = expression(Year),
    y = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1) ),
    color = "MAT (ºC) ",
    tag="D)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=1998, y=6.5, label= expression(atop(Slope~0.34~italic(P)~"<"~0.001,
                                                         paste(Interaction~0.19~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p3_c_losses

#mat: year

func_yr_mat_mean_cl  <- function(x) {exp(b0_cl      + b_mat_cl     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat)  ) )}

func_yr_mat_minus_sd_cl  <- function(x){exp(b0_cl      + b_mat_cl     *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
                                              b_year_cl_mat  *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*-0.8) + b_year_cl     *(-0.8) )}

func_yr_mat_mean_plus_sd_cl  <- function(x) {exp(b0_cl      + b_mat_cl      *( (x- mean(data_or_sc$mat)) /sd(data_or_sc$mat) ) +
                                                   b_year_cl_mat *( ((x- mean(data_or_sc$mat)) /sd(data_or_sc$mat))*+1.5 ) + b_year_cl  *(+1.5) )}

p4_c_losses= ggplot(data_or_sc, aes(x = mat, y = c_losses, col=year)) +
  
  geom_point(size=2.7,alpha=0.59)+
  
  stat_function(fun = func_yr_mat_mean_cl ,colour=colours_palletes[5],size=1.07)+ 
  stat_function(fun = func_yr_mat_minus_sd_cl ,colour=colours_palletes[3],size=1.07)+ #,xlim = c(1500, max(data_or_sc$mat))
  stat_function(fun = func_yr_mat_mean_plus_sd_cl ,colour=colours_palletes[7],size=1.07)+
  
  
  theme_classic() +
  labs(
    x = expression(MAT~(ºC)),
    y = expression(Carbon~losses~(Mg~C~ha^-1~yr^-1) ),
    color = expression(Year),
    tag="H)")+ #geom_hline(yintercept=0,linetype="dashed")+
  
  theme(text = element_text(size=13.07,colour="black"))+
  theme(axis.text.x = element_text(colour="black",size=13.07))+
  theme(axis.text.y = element_text(colour="black",size=13.07))+scale_color_gradientn(colours = colours_palletes)+
  
  annotate("text", x=21, y=7.5, label= expression(atop(Slope~-0.30~italic(P)~"<"~0.001,
                                                       paste(Interaction~0.19~italic(P)~"<"~0.001))))+
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))
p4_c_losses

##########################################################################################

#figure 7
allplotslist <- align_plots(p1_c_stocks,p3_c_stocks,p1_net_c_sink,p3_net_c_sink, align = "hv")

grid=grid.arrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],allplotslist[[4]],nrow = 2)

ggsave("Figure_7.tiff",grid, he = 20, wi = 28, un = "cm", dpi = 600)
system("open Figure_7.tiff")

#figure 8
allplotslist <- align_plots(p1_c_gains,p3_c_gains,p1_c_losses,p3_c_losses, align = "hv")

grid=grid.arrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],allplotslist[[4]],nrow = 2)

ggsave("Figure_8.tiff",grid, he = 20, wi = 28, un = "cm", dpi = 600)
system("open Figure_8.tiff")

#figure 9

allplotslist <- align_plots(p2_c_stocks,p4_c_stocks,p2_net_c_sink,p4_net_c_sink,p2_c_gains,p4_c_gains,p2_c_losses,p4_c_losses, align = "hv")

grid=grid.arrange(allplotslist[[1]],allplotslist[[2]],allplotslist[[3]],allplotslist[[4]]
                  ,allplotslist[[5]],allplotslist[[6]],allplotslist[[7]],allplotslist[[8]],nrow = 4)

ggsave("Figure_9.tiff",grid, he = 37, wi = 26, un = "cm", dpi = 600)
system("open Figure_9.tiff")