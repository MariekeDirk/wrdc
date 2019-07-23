library(XML)
library(RCurl)
library(tidyr)
library(countrycode)
library(ggplot2)
library(viridis)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#Get all the country names -->paste links later
countries<-get_country_names()
saveRDS(countries,"./data/countrieswrdc.rds")

################################################
###############################################################
#loop trough all the countries and stations
#j= i=
for(j in 1:length(countries)){
nm<-countries[j]
print(nm)
stations<-get_station_names(nm)

for(i in 1:length(stations)){
stn<-stations[i]
print(stn)

yr<-get_station_start(nm,stn)
yr_seq<-seq(yr,2019,by=1)

df.wrdc.ecad<-lapply(yr_seq,function(x) try(get_station_data(nm,stn,yr=x),TRUE))

df.wrdc.ecad<-do.call("rbind",df.wrdc.ecad[sapply(df.wrdc.ecad, function(x) !inherits(x, "try-error"))])
try(write.table(df.wrdc.ecad,paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_may2019/country_",nm,"_stn_",stn,".txt"),
            row.names = FALSE,
            col.names = TRUE,
            sep=","))
try(p<-ggplot(df.wrdc.ecad,aes(ser_date,qq))+geom_point()+ggtitle(stn))
try(ggsave(p,filename = paste0("/usr/people/dirksen/Pictures/wrdc/fig/",nm,"_",stn,".png")))
rm(df.wrdc.ecad)

# try(df.meta<-get_station_meta(nm,stn))
# try(write.table(df.meta,paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/meta_may2019/",nm,"_meta.txt"),
#             row.names = FALSE,
#             col.names = !file.exists(paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/meta_may2019/",nm,"_meta.txt")),
#             sep=",",
#             append = TRUE))
# rm(df.meta)
}
}
###############################################################

###############################################################
###################QUALITY CONTROL#############################
###############################################################
meta_data<-list.files("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/",full.names = TRUE)
meta_data<-lapply(meta_data,fread)
meta_data<-rbindlist(meta_data)

meta_data$name <- gsub(",","",meta_data$name)
meta_data$name <- gsub("\\. / ","_",meta_data$name)
meta_data$name <- gsub("\\. ","_",meta_data$name)
meta_data$name <- gsub("\\.","_",meta_data$name)
meta_data$name <- gsub(" / ","_",meta_data$name)
meta_data$name <- gsub(" /","_",meta_data$name)
meta_data$name <- gsub("/ ","_",meta_data$name)
meta_data$name <- gsub("/","_",meta_data$name)
meta_data$name <- gsub("\\'","_",meta_data$name)
meta_data$name <- gsub(" ","_",meta_data$name)


stn_data<-list.files("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_may2019/",full.names=TRUE)
stn_names<-list.files("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_may2019/")
stn_country<-gsub("_stn_.*","",stn_names)
stn_country<-gsub("country_","",stn_country)
stn_names<-gsub(".*_stn_","",stn_names)
stn_names<-gsub(".txt","",stn_names)

for(i in 1:length(stn_data)){
stn<-stn_names[i]
print(stn)
nm<-stn_country[i]
stn_data_one<-fread(stn_data[i])
stn_name_one<-meta_data[which(stn_names[i]==meta_data$name),]
print(stn_name_one)

try(df.qc<-check_qq_time_series(stn_data_one,lat=stn_name_one$lat/3600,lon=stn_name_one$lon/3600),TRUE)
try(write.table(df.qc,paste0("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_qc_may2019/country_",nm,"_stn_",stn,"_qc.txt"),
                row.names = FALSE,
                col.names = TRUE,
                sep=","),TRUE)

try(df.qc$density<-get_density(df.qc$zenith_mean,df.qc$qq,  n = 100),TRUE)
try(qc<-ggplot(df.qc,aes(x=zenith_mean)) +
  geom_point(aes(y=qq,color=density)) +
  #geom_smooth(aes(y=Qmax_zen),color="red",linetype="dashed") +
  #geom_smooth(aes(y=Qrare_zen),color="orange",linetype="dashed") +
  geom_smooth(aes(y=Qmax),color="yellow") +
  geom_smooth(aes(y=Qmin),color="green") +
  xlab("Mean Daytime Solar Zenith") +
  ylab("Global Radiation") +
  scale_color_viridis() +
  ggtitle(paste0("Country = ", nm, ", Station = ",stn)) +
  theme_bw(),TRUE)
try(ggsave(qc,filename = paste0("/usr/people/dirksen/Pictures/wrdc/fig_qc/",nm,"_",stn,".png")),TRUE)
rm(df.qc)
rm(qc)
}

