library(caret)
library(corrplot)
library(dplyr)

data_dyn=read.table("data_dyn_temp.csv",header=T,dec=".",sep=";")
data_dyn$co2_atm= rowMeans(data_dyn %>% select(co2_i,co2_f))

head(data_dyn);names(data_dyn)

obj1<-subset(data_dyn,select = c("year", "co2_atm","co2_change", "site_area_ha","lat","long", "elevation", "mat", "max_temp", "map","cwd", "ph","ca","k","p",  "mg","al","sb", "cec", "som", "clay"))
names(obj1)=c("Year", "CO2","CO2-change","Site area","Lat","Long","Altitude", "MAT","Max temp.", "MAP","CWD", "pH","Ca","K", "P" , "Mg","Al","SB", "CEC",  "SOM",  "Clay")

#options(max.print=999999)

corr1=cor(obj1, method = c("pearson"))

corrplot(corr1, method = "number",number.cex=0.7,type="lower")

#save
tiff("correlations.tiff", units="cm", width=20, height=21, res=177)

corrplot(corr1, method = "number",number.cex=0.7,type="lower")

dev.off()
system("open correlations.tiff")
