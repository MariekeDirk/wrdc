#'Country names
#'
#'
#'@description Get country names from the wrdc website ((\url{http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?wrdc/wrdc_new.htm}))
#'@param url_country link, standard to the european country list
#'
#'@export
get_country_names<-function(url_country =
                            "http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/t6.html"){
  requireNamespace("XML",quite=TRUE)
  requireNamespace("stats",quite=TRUE)

  # url_country<-"http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/t6.html"
  doc.html <- XML::htmlTreeParse(url_country , useInternal = TRUE)
  doc.text = unlist(XML::xpathApply(doc.html, '//p', XML::xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)
  countries <- data.frame(t(read.table(textConnection(doc.text[2]), stringsAsFactors = FALSE, sep = ",")))
  names(countries)<-"country"
  countries$country <- as.character(countries$country)
  countries <- countries[stats::complete.cases(countries$country),]
  countries <- gsub(" ", "_", countries)
  return(countries)
}

#'Stations names
#'
#'
#'@description Get station names from the countries on the wrdc website
#'@param nm name of the country according to the html link
#'
#'@export
get_station_names<-function(nm){
  requireNamespace("XML",quite=TRUE)

  url_station <- paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",nm,".html")
  doc.html <- XML::htmlTreeParse(url_station,useInternal=TRUE)
  doc.text = unlist(XML::xpathApply(doc.html, '//p', XML::xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)

  stations <- unlist(strsplit( doc.text[3], ',,' ))
  names(stations) <- "stn"
  # stations<-stations$stn[complete.cases(stations$stn)]
  stations <- gsub(",","",stations)
  stations <- gsub("\\. / ","_",stations)
  stations <- gsub("\\. ","_",stations)
  stations <- gsub("\\.","_",stations)
  stations <- gsub(" / ","_",stations)
  stations <- gsub(" /","_",stations)
  stations <- gsub("/ ","_",stations)
  stations <- gsub("/","_",stations)
  stations <- gsub("\\'","_",stations)
  stations <- gsub(" ","_",stations)
  return(stations)
}

#' Get the start year of the measurements
#'
#' @description downloads the start year of the measurement for a station. The format meta-data is returned in ecad format.
#' @param nm name of the country according to the html link
#' @param stn name of the station according to the html link
#'
#' @export
get_station_start<-function(nm,stn){
  requireNamespace("XML",quite=TRUE)

  url_years <- paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,".html")
  doc.html <- XML::htmlTreeParse(url_years, useInternal = TRUE)
  doc.text = unlist(XML::xpathApply(doc.html, '//p', XML::xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)

  start<-data.frame(t(read.table(textConnection(doc.text[6]),stringsAsFactors = FALSE,sep=",")))[2,]
}

#' Get the metadata from the station
#'
#' @description downloads the metadata of a station, the fist year of measurement is used to extract the metadata from
#' @param nm name of the country according to the html link
#' @param stn name of the station according to the html link
#'
#' @export
get_station_meta<-function(nm,stn){
  requireNamespace("XML",quite=TRUE)
  requireNamespace("countrycode",quite=TRUE)
  requireNamespace("stringr",quite=TRUE)

  url_years <- paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,".html")
  doc.html <- XML::htmlTreeParse(url_years,useInternal=TRUE)
  doc.text = unlist(XML::xpathApply(doc.html, '//p', XML::xmlValue))
  doc.text = gsub('\r\r\n', ',', doc.text)

  start<-data.frame(t(read.table(textConnection(doc.text[6]),stringsAsFactors = FALSE,sep=",")))[2,]
  # stop<-tail(data.frame(t(read.table(textConnection(doc.text[length(doc.text)]),stringsAsFactors = FALSE,sep=","))),n=2)[1,]
  #
  # years<-seq(as.numeric(start),as.numeric(stop),by=1)
  ################################################
  yr<-start[1]
  #get Metadata from the station
  link<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,"_",yr,"_t6.html")
  # link<-"http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/italy/s_valentino/s_valentino_2012_t6.html"

  #station id all unique numbers starting from 16324
  #altitude in 0.1m
  #lat and lon in seconds
  doc.html <- XML::htmlTreeParse(link,useInternal=TRUE)
  doc.text = unlist(XML::xpathApply(doc.html, '//p', XML::xmlValue))
  doc.text = gsub('\n', ' ', doc.text)

  country <- gsub("Country: ","",doc.text[2])
  coun_id <- countrycode::countrycode(country,'country.name','iso3c')

  name <- gsub("Station: ","",doc.text[3])

  lat <- gsub("Latitude =","",doc.text[5])
  ns <- substr(lat,nchar(lat),nchar(lat))

  lat<-(as.numeric(stringr::str_match(lat,"(.*?)°")[,2])+
          (as.numeric(stringr::str_match(lat,"°(.*?)'[A-Z]")[,2])/60))*3600


  if(ns=="N"){
    lat = lat
  } else {lat = -lat}

  lon<-gsub("Longitude =","",doc.text[6])
  ew<-substr(lon,nchar(lon),nchar(lon))

  lon<-(as.numeric(stringr::str_match(lon,"(.*?)°")[,2])+
          (as.numeric(stringr::str_match(lon,"°(.*?)'[A-Z]")[,2])/60))*3600

  if(ew=="E"){
    lon = lon
  } else {lon = -lon}

  elv<-gsub("Elevation = ","",doc.text[7])
  elev<-as.numeric(elv)*10

  wmo_id<-gsub("WMO Identifier: ","",doc.text[8])
  year<-gsub("Year ","",doc.text[9])
  df.meta<-data.frame(wmo_id,name,coun_id,lat,lon,elev)
  return(df.meta)
}

#' Get the station measurements
#'
#' @description Downloads the table with measurements and rewrites it to a long format. Note that not always all the years are avaiable for download.
#'
#' @param nm name of the country according to the html link
#' @param stn name of the station according to the html link
#' @param yr year of the measurements to be downloaded
#' @return returns a dataframe with the ser_date (yyyy-mm-dd), qq (radiation in J/cm2), qc (quality flag 0,1 or 2) and the columns qcm and qca with -9 values.
#' The last two quality control columns are to be filled during another check.
#' @export
get_station_data<-function(nm,stn,yr){
  requireNamespace("tidyr",quite=TRUE)
  requireNamespace("stats",quite=TRUE)
  requireNamespace("data.table")

  link<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,"_",yr,"_t6.csv.html")
  df<-fread(link,na.strings = "-32768")
  df.clean<-df[1:(nrow(df)-2),] #exclude the monthly means from the table


  drop<-seq(2,length(names(df.clean)),by=2) #exclude the quality flag
  df.clean<-subset(df.clean,select=-drop)


  #Get column names
  link.html<-paste0("http://wrdc.mgo.rssi.ru/wrdccgi/protect.exe?data_list_full_protected/t6/",nm,"/",stn,"/",stn,"_",yr,"_t6.html")
  header.tabel<-readHTMLTable(link.html)
  header.tabel<-list.clean(header.tabel,fun=is.null,recursive=FALSE)
  colnms<-names(data.frame(header.tabel))
  l<-length(colnms)
  drop<-seq(1,l,by=2)
  colnms<-colnms[-drop]
  colnms<-gsub("NULL.","",colnms)
  #
  df.clean<-df.clean[,1:length(colnms)] #select 12 months
  colnames(df.clean)<-colnms #name them accordingly
  df.clean$Day<-rownames(df.clean) #add the days, which are the row numbers

  #transpose to long data format
  df.clean<-tidyr::gather(df.clean,  Month, Radiation,-Day) #transpose to long format

  df.clean$Radiation<-as.numeric(df.clean$Radiation)/8.64 #in W/m2
  df.clean<-df.clean[stats::complete.cases(df.clean$Radiation),]
  df.clean$year<-yr
  # df.clean$id<-id
  df.clean$date<-as.Date(paste0(df.clean$year,"-",df.clean$Month,"-",df.clean$Day),format="%Y-%b-%d")

  df.wrdc.ecad<-df.clean[,c("date","Radiation")] #"id",
  names(df.wrdc.ecad)<-c("ser_date","qq") #"wmo_id",
  df.wrdc.ecad$qc<--9
  df.wrdc.ecad$qcm<--9
  df.wrdc.ecad$qca<--9
  return(df.wrdc.ecad)
}

