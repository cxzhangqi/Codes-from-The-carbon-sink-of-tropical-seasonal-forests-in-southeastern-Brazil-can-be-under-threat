library(dplyr);library(tidyr)

data_dyn=read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
data_dyn=mutate_if(data_dyn, is.character, as.factor)

head(data_dyn)

data_sites= unique(data_dyn %>% group_by(plot_code) %>% summarise(lat=mean(lat), long=mean(long),
                                                                  forest_type=forest_type,
                                                           sampled_area_ha=mean(sampled_area_ha), 
                                                           n_census=mean(n_census),
                                                           year_i=min(year_i), year_f=max(year_f),
                                                           interval_length=mean(interval_length),
                                                           site_area_ha=mean(site_area_ha),
                                                           mat=mean(mat), 
                                                           map=mean(map)))
data_sites
#write.csv(data_sites,"summary_sites.csv")

data_sites$year_range= data_sites$year_f-data_sites$year_i

hist(data_sites$year_range)

data_sites %>% group_by(forest_type) %>% summarise(length(unique(plot_code)))

#########################

library(ggplot2)

plot_time_range=ggplot(data_sites, aes(x=year_range)) + 
  geom_histogram(binwidth=5)+  geom_vline(aes(xintercept=mean(year_range)),
                                          color="red", linetype="dashed", size=1)+
  theme_bw()+ labs(x = "Temporal sampling effort (years)", y ="Number of sites")+
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text( color="black"))+labs(tag= "B)")

plot_time_range

ggsave("time_range.tiff",he = 10, wi = 11, un = "cm", dpi = 177)
system("open time_range.tiff")

mean(data_sites$year_range);sd(data_sites$year_range)
min(data_sites$year_range);max(data_sites$year_range)

library(plyr)

ggplot(data_sites, aes(x=year_range)) + 
  geom_histogram(binwidth=4)+facet_wrap(~forest_type)+  geom_vline(data = ddply(data_sites, "forest_type",
                                                                                summarize, 
                                                                                wavg = mean(year_range)),
aes(xintercept=wavg),color="red", linetype="dashed", size=1) +
  theme_bw()+ labs(x = "Temporal sampling effort (years)", y ="Number of sites")+
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text( color="black"))

ggsave("time_range_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open time_range_forest_type.tiff")

data_sites1=data_sites %>% dplyr::group_by(forest_type ) %>% dplyr::summarise(mean=mean(year_range),sd=sd(year_range))
data_sites1

########################
mean(data_sites$sampled_area_ha);sd(data_sites$sampled_area_ha)
min(data_sites$sampled_area_ha);max(data_sites$sampled_area_ha)

library(plyr)

ggplot(data_sites, aes(x=sampled_area_ha)) + 
  geom_histogram(binwidth=1)+facet_wrap(~forest_type)+  geom_vline(data = ddply(data_sites, "forest_type",
                                                                                summarize, 
                                                                                wavg = mean(sampled_area_ha)),
                                                                   aes(xintercept=wavg),color="red", linetype="dashed", size=1) +
  theme_bw()+ labs(x = "Spatial sampling effort (ha)", y ="Number of sites")+
  theme(axis.text.x = element_text(color="black"),
        axis.text.y = element_text( color="black"))


ggsave("sampled_area_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open sampled_area_forest_type.tiff")

##############################################
##############################################
##############################################

rm(list=ls())

data_dissert1= read.table("data_final_raw.csv",header=T,dec=".",sep=";")

head(data_dissert1)
levels(as.factor(data_dissert1$plot_code))

length(levels(as.factor(data_dissert1$plot_code)))
dim(data_dissert1)

#drop mgt| lag| peb

data_dissert2=mutate_if(data_dissert1,is.character, as.factor)

exclude=c("LAG_07", "LAG_08" ,"LAG_09", "LAG_10",
          "LAG_11", "LAG_12" ,"LAG_13", "LAG_14",  "MGT_08", "MGT_09", "MGT_10", "MGT_11", "MGT_12",
          "MGT_13", "MGT_14", "MGT_15",  "PEB_1")  

data_dissert3=data_dissert2[!data_dissert2$plot_code %in% exclude,]
length(levels(as.factor(data_dissert3$plot_code)))
dim(data_dissert3)

head(data_dissert3)
#write.csv(data_dissert3,"data_final_raw.csv")

###############

data_dissert4=data_dissert3[data_dissert3$dbhq>0,]
dim(data_dissert4)

###########

data_dissert5=data_dissert4 %>% mutate(dbh_class=ifelse(dbhq >= 5 & dbhq < 10,"1) 5-10", 
                                                        ifelse(dbhq >= 10 & dbhq < 20,"2) 10-20",
                                                               ifelse(dbhq >= 20 & dbhq < 40,"3) 20-40",
                                                                      ifelse(dbhq >= 40, "4) > 40","error")))))

data_dissert5
summary(as.factor(data_dissert5$dbh_class))

##############################################
############## wd non weighted
head(data_dissert5)

wd_n_w= data_dissert5 %>% group_by(forest_type,plot_code,site_year_temp) %>% summarise(mean_wd=mean(wd))
wd_n_w

library(lme4)
library(emmeans)

m_wd_n_w=lmer(mean_wd~forest_type+(1|plot_code),data=wd_n_w)
hist(resid(m_wd_n_w))
cat=lsmeans(m_wd_n_w,list(pairwise~forest_type))
cat
cat1=as.data.frame(cat$`lsmeans of forest_type`)
cat1

#wd non weighted
plot_wd_n_w <-ggplot(data=cat1, aes(x=forest_type, y=lsmean)) +
  geom_bar(stat="identity") +
  theme_bw()+ labs(x = " ", y = expression(CWM~WD~(g~cm^-3))) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_text(aes(label=list("a","a","a")),position=position_dodge(1),vjust = -2, hjust= -5)

plot_wd_n_w

ggsave("plot_wd_n_w.tiff",he = 15, wi = 15, un = "cm", dpi = 177)
system("open plot_wd_n_w.tiff")

####################################
##############################################
############## wd weighted

wd_w= data_dissert5 %>% group_by(forest_type,plot_code,site_year_temp) %>% 
  summarise(mean_wd=weighted.mean(wd,w=agwb_combined))
wd_w

library(lme4)
library(emmeans)

m_wd_w=lmer(mean_wd~forest_type+(1|plot_code),data=wd_w)
cat=lsmeans(m_wd_w,list(pairwise~forest_type))
cat
cat1=as.data.frame(cat$`lsmeans of forest_type`)
cat1

#wd non weighted
plot_wd_n_w <-ggplot(data=cat1, aes(x=forest_type, y=lsmean)) +
  geom_bar(stat="identity") +
  theme_bw()+ labs(x = " ", y = expression(CWM~WD~(g~cm^-3))) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_text(aes(label=list("a","a","a")),position=position_dodge(1),vjust = -2, hjust= -5)

plot_wd_n_w

ggsave("plot_wd_w.tiff",he = 15, wi = 15, un = "cm", dpi = 177)
system("open plot_wd_w.tiff")

##############################################
##############################################
##############################################

rm(list=ls())

data_dissert1= read.table("data_final_raw.csv",header=T,dec=".",sep=";")

head(data_dissert1)
levels(as.factor(data_dissert1$plot_code))

length(levels(as.factor(data_dissert1$plot_code)))
dim(data_dissert1)

#drop mgt| lag| peb

data_dissert2=mutate_if(data_dissert1,is.character, as.factor)

exclude=c("LAG_07", "LAG_08" ,"LAG_09", "LAG_10",
          "LAG_11", "LAG_12" ,"LAG_13", "LAG_14",  "MGT_08", "MGT_09", "MGT_10", "MGT_11", "MGT_12",
          "MGT_13", "MGT_14", "MGT_15",  "PEB_1")  

data_dissert3=data_dissert2[!data_dissert2$plot_code %in% exclude,]
length(levels(as.factor(data_dissert3$plot_code)))
dim(data_dissert3)

head(data_dissert3)
#write.csv(data_dissert3,"data_final_raw.csv")

###############

data_dissert4=data_dissert3[data_dissert3$dbhq>0,]
dim(data_dissert4)

###########

data_dissert5=data_dissert4 %>% mutate(dbh_class=ifelse(dbhq >= 5 & dbhq < 10,"1) 5-10", 
                                                        ifelse(dbhq >= 10 & dbhq < 20,"2) 10-20",
                                                               ifelse(dbhq >= 20 & dbhq < 40,"3) 20-40",
                                                                      ifelse(dbhq >= 40, "4) > 40","error")))))

data_dissert5
summary(as.factor(data_dissert5$dbh_class))

#############################################################################

dbh_class=data_dissert5 %>% group_by(forest_type,plot_code) %>% count(dbh_class)

sum_per_site= dbh_class %>% group_by(plot_code) %>% summarise(n_total_site=sum(n))

dbh_class2=left_join(dbh_class,sum_per_site,by="plot_code" )

sum_per_forest_type= dbh_class %>% group_by(forest_type) %>% summarise(n_total_forest_type=sum(n))

dbh_class1=left_join(dbh_class2,sum_per_forest_type,by="forest_type" )
dbh_class1

##########################################################

#### site level raw
plot_dbh_class_site <-ggplot(data=dbh_class1, aes(x=dbh_class, y=n)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Number of trees") 

plot_dbh_class_site

ggsave("plot_raw_trees_site.tiff",he = 20, wi = 30, un = "cm", dpi = 177)
system("open plot_raw_trees_site.tiff")

#### site level percentage
plot_dbh_class_site_perc <-ggplot(data=dbh_class1, aes(x=dbh_class, y=(n/n_total_site)*100)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Percentage of trees") 

plot_dbh_class_site_perc

ggsave("plot_perc_tress_site.tiff",he = 23, wi = 37, un = "cm", dpi = 177)
system("open plot_perc_tress_site.tiff")

#### forest type level raw
plot_dbh_class_forest_type <-ggplot(data=dbh_class1, aes(x=dbh_class, y=n)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Number of trees") 

plot_dbh_class_forest_type

ggsave("plot_ind_raw.tiff",he = 10, wi = 23, un = "cm", dpi = 177)
system("open plot_ind_raw.tiff")

#### forest type level percentage
plot_dbh_class_forest_type_perc <-ggplot(data=dbh_class1, aes(x=dbh_class, y=(n/n_total_forest_type)*100)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Percentage of trees") 

plot_dbh_class_forest_type_perc

ggsave("plot_ind_perc.tiff",he = 10, wi = 23, un = "cm", dpi = 177)
system("open plot_ind_perc.tiff")

#################
#density 

data_dissert5

############

ggplot() + 
  geom_density(data=data_dissert5, aes(x=dbhq  , group=forest_type,color=forest_type), size=1,alpha=0.5, adjust=2) + 
  xlab("DBH (cm)") + theme_bw()+
  ylab("Density")+ labs(color=' ') 

ggsave("plot_density_forest_type_tog.tiff",he = 10, wi = 15, un = "cm", dpi = 177)
system("open plot_density_forest_type_tog.tiff")

ggplot() + 
  geom_density(data=data_dissert5, aes(x=dbhq  , group=forest_type,fill=forest_type),alpha=0.5, adjust=2) + 
  xlab("DBH (cm)") + theme_bw()+
  ylab("Density")+ labs(fill=' ') +facet_wrap(~forest_type) 

ggsave("plot_density_forest_type_cont.tiff",he = 10, wi = 20, un = "cm", dpi = 177)
system("open plot_density_forest_type_cont.tiff")

######

ggplot() + 
  geom_density(data=data_dissert5, aes(x=dbhq  , group=plot_code,color=plot_code), size=1,alpha=0.5, adjust=2) + 
  xlab("DBH (cm)") + theme_bw()+
  ylab("Density")+ labs(color=' ') 

ggsave("plot_density_tog.tiff",he = 10, wi = 15, un = "cm", dpi = 177)
system("open plot_density_tog.tiff")

ggplot() + 
  geom_density(data=data_dissert5, aes(x=dbhq  , group=forest_type,fill=forest_type),alpha=0.5, adjust=2) + 
  xlab("DBH (cm)") + theme_bw()+
  ylab("Density")+ labs(fill=' ') +facet_wrap(~plot_code) 

ggsave("plot_density_site.tiff",he = 17, wi = 31, un = "cm", dpi = 177)
system("open plot_density_site.tiff")

###########################
##############

#dominance

data_richness=data_dissert5 %>% group_by(forest_type,dbh_class) %>%  summarise(richness=length(unique(sp_final)))

###########
plot_dbh_class_richness<-ggplot(data=data_richness, aes(x=dbh_class, y=richness)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Number of species")

plot_dbh_class_richness

ggsave("plot_richness_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open plot_richness_forest_type.tiff")

##############

data_richness1=data_dissert5 %>% group_by(plot_code,dbh_class) %>%  summarise(richness=length(unique(sp_final)))

###########
plot_dbh_class_richness<-ggplot(data=data_richness1, aes(x=dbh_class, y=richness)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Number of species")

plot_dbh_class_richness

ggsave("plot_richness_site.tiff",he = 15, wi = 35, un = "cm", dpi = 177)
system("open plot_richness_site.tiff")

##################
library(vegan)

#shannon by site
matrix_rarefaction = as.data.frame(data_dissert5 %>% select(plot_code,sp_final,dbh_class) %>% group_by(plot_code,dbh_class, sp_final)%>%
                                     summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction[is.na(matrix_rarefaction)] <- 0

data_richness2=matrix_rarefaction %>% group_by(plot_code,dbh_class) %>%  
  group_map(~diversity(.x[,3:ncol(.x)],index = "shannon")  )

data_richness2

data_richness2_f=cbind.data.frame(matrix_rarefaction[,1:2],shannon=do.call(rbind, data_richness2))

plot_data_richness2_f<-ggplot(data=data_richness2_f, aes(x=dbh_class, y=shannon)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Shannon")

plot_data_richness2_f

ggsave("plot_shannon_site.tiff",he = 15, wi = 35, un = "cm", dpi = 177)
system("open plot_shannon_site.tiff")

#shannon by forest_type

matrix_rarefaction1 = as.data.frame(data_dissert5 %>% select(forest_type,sp_final,dbh_class) %>% group_by(forest_type,dbh_class, sp_final)%>%
                                      summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction1[is.na(matrix_rarefaction1)] <- 0

data_richness3=matrix_rarefaction1 %>% group_by(forest_type,dbh_class) %>%  
  group_map(~diversity(.x[,3:ncol(.x)],index = "shannon")  )

data_richness3

data_richness3_f=cbind.data.frame(matrix_rarefaction1[,1:2],shannon=do.call(rbind, data_richness3))

data_richness3_f

plot_data_richness3_f<-ggplot(data=data_richness3_f, aes(x=dbh_class, y=shannon)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Shannon")

plot_data_richness3_f

ggsave("plot_shannon_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open plot_shannon_forest_type.tiff")

#evenness by site

matrix_rarefaction = as.data.frame(data_dissert5 %>% select(plot_code,sp_final,dbh_class) %>% group_by(plot_code,dbh_class, sp_final)%>%
                                     summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction[is.na(matrix_rarefaction)] <- 0

data_evenness=matrix_rarefaction %>% group_by(plot_code,dbh_class) %>%  
  group_map(~(diversity(.x[,3:ncol(.x)],index = "shannon")/log(specnumber(.x[,3:ncol(.x)]))  ))

data_evenness

data_evenness_f=cbind.data.frame(matrix_rarefaction[,1:2],eveness=do.call(rbind, data_evenness))
data_evenness_f

plot_data_evenness_f<-ggplot(data=data_evenness_f, aes(x=dbh_class, y=eveness)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Evenness")

plot_data_evenness_f

ggsave("plot_evenness_site.tiff",he = 15, wi = 35, un = "cm", dpi = 177)
system("open plot_evenness_site.tiff")

#evennes by forest_type

matrix_rarefaction1 = as.data.frame(data_dissert5 %>% select(forest_type,sp_final,dbh_class) %>% group_by(forest_type,dbh_class, sp_final)%>%
                                      summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction1[is.na(matrix_rarefaction1)] <- 0

data_evennes3=matrix_rarefaction1 %>% group_by(forest_type,dbh_class) %>%  
  group_map(~(diversity(.x[,3:ncol(.x)],index = "shannon")/log(specnumber(.x[,3:ncol(.x)]))  ))

data_evennes3

data_evennes3_f=cbind.data.frame(matrix_rarefaction1[,1:2],eveness=do.call(rbind, data_evennes3))

data_evennes3_f

plot_data_evennes3_f<-ggplot(data=data_evennes3_f, aes(x=dbh_class, y=eveness)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Evenness")

plot_data_evennes3_f

ggsave("plot_evennes_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open plot_evennes_forest_type.tiff")

#simpson by site
matrix_rarefaction = as.data.frame(data_dissert5 %>% select(plot_code,sp_final,dbh_class) %>% group_by(plot_code,dbh_class, sp_final)%>%
                                     summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction[is.na(matrix_rarefaction)] <- 0

data_richness2=matrix_rarefaction %>% group_by(plot_code,dbh_class) %>%  
  group_map(~diversity(.x[,3:ncol(.x)],index = "simpson")  )

data_richness2

data_richness2_f=cbind.data.frame(matrix_rarefaction[,1:2],simpson=do.call(rbind, data_richness2))

plot_data_richness2_f<-ggplot(data=data_richness2_f, aes(x=dbh_class, y=simpson)) +
  geom_bar(stat="identity") +facet_wrap(~plot_code) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Simpson")

plot_data_richness2_f

ggsave("plot_simpson_site.tiff",he = 15, wi = 35, un = "cm", dpi = 177)
system("open plot_simpson_site.tiff")

#simpson by forest_type

matrix_rarefaction1 = as.data.frame(data_dissert5 %>% select(forest_type,sp_final,dbh_class) %>% group_by(forest_type,dbh_class, sp_final)%>%
                                      summarise(n=n())%>%spread(sp_final, n))

matrix_rarefaction1[is.na(matrix_rarefaction1)] <- 0

data_richness3=matrix_rarefaction1 %>% group_by(forest_type,dbh_class) %>%  
  group_map(~diversity(.x[,3:ncol(.x)],index = "simpson")  )

data_richness3

data_richness3_f=cbind.data.frame(matrix_rarefaction1[,1:2],simpson=do.call(rbind, data_richness3))

data_richness3_f

plot_data_richness3_f<-ggplot(data=data_richness3_f, aes(x=dbh_class, y=simpson)) +
  geom_bar(stat="identity") +facet_wrap(~forest_type) +
  theme_bw()+ labs(x = "DBH class (cm)", y ="Simpson")

plot_data_richness3_f

ggsave("plot_simpson_forest_type.tiff",he = 10, wi = 30, un = "cm", dpi = 177)
system("open plot_simpson_forest_type.tiff")
