library(XML)
library(RCurl)
library(tidyr)
library(countrycode)

#Get all the country names -->paste links later
countries<-get_country_names()
saveRDS(countries,"./data/countrieswrdc.rds")

################################################
###############################################################
#loop trough all the countries and stations
for(j in 1:length(countries)){
nm<-countries[j]
print(nm)
stations<-get_station_names(nm)

for(i in 1:length(stations)){
stn<-stations[i]
print(stn)

yr<-get_station_start(nm,stn)
yr_seq<-seq(yr,2018,by=1)

df.wrdc.ecad<-lapply(yr_seq,function(x) try(get_station_data(nm,stn,yr=x),TRUE))

df.wrdc.ecad<-do.call("rbind",df.wrdc.ecad[sapply(df.wrdc.ecad, function(x) !inherits(x, "try-error"))])
df.wrdc.ecad$qq<-try(df.wrdc.ecad$qq/3.6,TRUE)

try(write.table(df.wrdc.ecad,paste0("/nobackup/users/dirksen/data/radiation_europe/WRDC/data/country_",nm,"_stn_",stn,".txt"),
            row.names = FALSE,
            col.names = TRUE,
            sep=","),TRUE)

# write.table(df.meta,paste0("/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/",nm,"_meta.txt"),
#             row.names = FALSE,
#             col.names = !file.exists(paste0("/nobackup/users/dirksen/data/radiation_europe/WRDC/meta/",nm,"_meta.txt")),
#             sep=",",
#             append = TRUE)
}
}
###############################################################

#Get the table

