library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(gridExtra)
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
mean(data_sites$sampled_area_ha);sd(data_sites$sampled_area_ha)

data_sites%>%group_by(forest_type) %>% summarise(mean=mean(sampled_area_ha),sd=sd(sampled_area_ha))

colfunc<-colorRampPalette(c("darkorange2", "mediumseagreen", "purple3"))

colours_palletes=colfunc(3)

p4=ggplot(data_dyn, aes(x = mat, y = map, col = forest_type,size=(sampled_area_ha*interval_length))) +
  geom_point() +
  geom_jitter()+
  theme_classic()+
  labs(x="MAT (ºC)",
       y="MAP (mm)",
       colour=" ",
       size=expression(Sampling~effort[(ha%*%yr)]), tag="A)")+ 
  theme(text = element_text(size = 14,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+  
  scale_colour_manual(values = c(colours_palletes[1], colours_palletes[2], colours_palletes[3]))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.7))+ 
  guides(colour = guide_legend(override.aes = list(size=5)))+
  scale_size_continuous(breaks=c(5,15.1, 25.2),labels=c("Min"," ", "Max"),limits=c(0,30))

p4

#####################

plot_sampled_area=ggplot(data_sites, aes(x=sampled_area_ha)) + 
  geom_histogram(binwidth=1)+  geom_vline(aes(xintercept=mean(sampled_area_ha)),
                                          color="red", linetype="dashed", size=1)+
  theme_bw()+ labs(x = "Spatial sampling effort (ha)", y ="Number of sites")+
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))+labs(tag= "B)")

plot_sampled_area

data_sites$year_range= data_sites$year_f-data_sites$year_i

plot_time_range=ggplot(data_sites, aes(x=year_range)) + 
  geom_histogram(binwidth=5)+  geom_vline(aes(xintercept=mean(year_range)),
                                          color="red", linetype="dashed", size=1)+
  theme_bw()+ labs(x = "Temporal sampling effort (years)", y ="Number of sites")+
  theme(text = element_text(size = 15,colour="black"))+
  theme(axis.text.x = element_text(colour="black"))+
  theme(axis.text.y = element_text(colour="black"))  +labs(tag= "C)")

plot_time_range

allplotslist <- align_plots(plot_sampled_area,plot_time_range, align = "hv")
grid2=grid.arrange(allplotslist[[1]],allplotslist[[2]],nrow = 2)

grid=grid.arrange(p4, grid2,
                  ncol = 2,widths = c(1,0.5))

ggsave("plot_figure_2.tiff",grid, he = 15, wi = 30, un = "cm", dpi = 600)
system("open plot_figure_2.tiff")
