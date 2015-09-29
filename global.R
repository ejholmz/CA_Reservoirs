library(foreign)
library(plyr)
library(dplyr)

##Load datasets
cares <- readRDS("Data/Calres_lookup.Rds")
resdata <- readRDS("Data/Calres_appdata.Rds")

##prepare to update cares DF with percent capacity data
recent <- ddply(resdata, .(code ),)
recent <-   resdata %>% group_by(code) %>% slice(which.max(stor_date))

cares <- merge(cares[,c(1:9)], recent, by = "code", all.y = T)
cares$max <- cares$max/1000
cares$perc <- (cares$storage/(cares$max)) * 100

##Add reservoir name and max storage capacity to storage time series data
resdata <- merge(resdata, cares[,c("code", "Reservoir", "max")], by = "code", all.x = T)

#save(cares, "/home/ejholmes/ShinyApps/CA_Reservoirs/Data/Testing_123.Rds")
