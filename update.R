library(foreign)
library(plyr)

##Load datasets
# load("C:/Users/ejholmes/Desktop/Test_apps/CA_Reservoirs/Data/CalRes_App_cache_2015-08-31.Rds")

load("/home/ejholmes/ShinyApps/CA_Reservoirs/Data/CalRes_App_cache_2015-08-31.Rds")
##Remove reservoirs that do not fit the criteria for mapping
res.centroids <- cares[!(cares$code %in% c(
  ##exclude due to wonky data
  "BLB", "DMV", "PRA", "RTD", "SCD", "SVO", "BHC", "CMI", 
  "CPL", "EDN", "JNC", "LEW", "SIV", "MRT", "OWN", "PRR",
  "PRS", "SJT", "BAR", "BIL", "FRM", "GDW", "MAR", "SPC",
  ##exclude due to small storage capacity < ~ 25,000 acre ft
  "VAR", "KES", "MMW", "ANT", "LVY", "GLL", "RBL", "NAT", 
  "RLF", "DNN", "INP", "LYS", "SWB", "SLB"
)),]

res.list <- unique(res.centroids$code)

##Download new data
start_date <- "2015-09-01"
end_date <- Sys.Date()

download <- data.frame()
for(i in res.list){
  print(paste("Downloading",i, "data"))
  temp<-read.table(paste("http://cdec.water.ca.gov/cgi-progs/queryCSV?station_id=", i, "&sensor_num=", 15, 
                         "&dur_code=D&start_date=", start_date,  "&end_date=",end_date,"&data_wish=View+CSV+Data", sep=""),
                   header=FALSE, sep=",", skip=2)
  temp<-temp[,c(1,3)]
  temp[ temp == "m" ] = NA
  temp$V3 <-as.numeric(as.character(temp$V3))
  temp$res <- i
  colnames(temp)<- c("stor_date", "storage","code")
  temp <- temp[is.na(temp$storage) == FALSE,]
  download <- rbind(download,temp)
}

##Combine data cache with newly downloaded data
data <- rbind(cache, download)

##Run outlier removal algorithm
results <- data.frame()
for(j in unique(download$code)){
  print(paste("removing outliers from",j))
  temp <- data[data$code == j,]
  temp$yr <- substr(temp$stor_date, 1,4)
  temp$dydx <- c(NA,100*diff(temp$storage)/temp[-nrow(temp),]$storage)
  tempmax <- aggregate(storage~yr, max, data=temp)
  tempmin <- aggregate(storage~yr, min, data=temp)
  temp <- subset(temp, temp$storage < quantile(tempmax$storage, probs=.75, na.rm = TRUE) + 1.5 * IQR(tempmax$storage, na.rm = TRUE))
  temp <- subset(temp, temp$storage > quantile(tempmin$storage, probs=.25, na.rm = TRUE) - 1.5 * IQR(tempmin$storage, na.rm = TRUE))
  temp <- subset(temp, temp$storage > 0)
  temp$Date <- as.Date(paste(substr(temp$stor_date, 1,4), substr(temp$stor_date, 5,6), substr(temp$stor_date, 7,8), sep = "-"))
  temp$Datenum <- as.numeric(temp$Date)
  
  ##Compute date range standardization for loess span smoothing parameterization
  #   temprange <- data.frame("X10121" = diff(range(temp$Datenum)), "code" = j)
  #   ranges <- rbind(ranges, temprange)
  #   ranges$stand <- 1/((ranges$X10121 - min(ranges$X10121))/(max(ranges$X10121)-min(ranges$X10121)) +.1)/300
  
  temp.loess <- loess(temp$storage ~ temp$Datenum, span = ranges[ranges$code == j,"stand"])
  temp$loess <- predict(temp.loess,newdata = temp$Datenum, se.fit = T)
  temp$exclude <- ifelse(abs(temp$storage - temp$loess) > sqrt(cares[cares$code == j,"max"])*65, "YES", "NO")
  
  ## Output graphs to visually inspect reservoir information and outlier removal algorithm performance
  #   png(paste("C:/Users/ejholmes/Dropbox/ShinyApps/CA_Reservoir_mapper/Output/",j,"_res.png",sep=""), 
  #          height=5,width=8,units="in",res=500, pointsize=12, family="serif")
  #   plot(temp$storage ~ temp$Date, type = "l", main = j)
  #   lines(temp$loess ~ temp$Date, col = "red")
  #   sub <- temp[temp$exclude == "YES",]
  #   points(sub$storage ~ sub$Date)
  #   grid()
  #   dev.off()
  
  results <- rbind(results,temp)
}

##Format data for use in the CA Reservoir Application
results <- results[results$exclude == "NO",]
results$stor_date <- as.Date(paste(substr(results$stor_date, 1,4), "-", substr(results$stor_date, 5,6), "-", substr(results$stor_date, 7, 8), sep=""))
results$storage <- as.character(results$storage)
results$storage <- as.numeric(results$storage)/1000
results <- results[is.na(results$storage) != TRUE,]
results$jday <- strptime(results$stor_date, "%Y-%m-%d")$yday+1
results <- ddply(results,.(code,jday), transform, avg.daily=mean(storage))
results$avg.daily <- ifelse(results$jday==366,NA,results$avg.daily)

##Save app data as R data file
# saveRDS(results[,c("stor_date", "storage", "code", "avg.daily")], 
#         file = "C:/Users/ejholmes/Desktop/Test_apps/CA_Reservoirs/Data/Calres_appdata.Rds")
saveRDS(results[,c("stor_date", "storage", "code", "avg.daily")], 
        file = "/home/ejholmes/ShinyApps/CA_Reservoirs/Data/Calres_appdata.Rds")
