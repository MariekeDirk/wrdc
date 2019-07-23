library(data.table)
library(reshape2)
library(stringr)

main.dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_may2019/"
write.dir<-"/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/data_to_ECAD_juli2019/"
data.name<-list.files(main.dir,pattern=".txt")
data.name<-gsub(".*stn_","",data.name)
data.name<-gsub(".txt","",data.name)
data.path<-list.files(main.dir,pattern=".txt",full.names = TRUE)

#small problem: 1 station name changed over time
# df1<-fread(paste0(main.dir,"country_SPAIN_stn_LOGRONO.txt"))
# df2<-fread(paste0(main.dir,"country_SPAIN_stn_LOGRONO_AGONCILLO.txt")) #put this one somewhere else


change_names<-function(stations){
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

keyfile_WRDC<-fread("/net/pc150400/nobackup/users/dirksen/data/radiation_europe/WRDC/keyfile_WRDC_ECAD.txt",sep="\t")
names(keyfile_WRDC)<-"combi"
keyfile_WRDC$stn<-gsub("[0-9].*","",keyfile_WRDC$combi)
keyfile_WRDC$stn<-trimws(keyfile_WRDC$stn,"right")
keyfile_WRDC$ser_id<-gsub(".*[^0-9]","",keyfile_WRDC$combi) #extract numbers
keyfile_WRDC<-subset(keyfile_WRDC,select=c("stn","ser_id"))

keyfile_WRDC$stn_wrdc<-change_names(keyfile_WRDC$stn)


add_data_file_to_df<-function(pattern){
I<-which(!is.na(str_match(string=c(paste0(data.name,paste0(data.name,"A"))),pattern=pattern)))
# I<-which(!is.na(agrep(pattern,data.name,value=TRUE)))
print(data.name[I])
if(length(I)>1){

  I<-I[1]
  }
if(length(I)==0){I<-NA}

return(I)
}

I<-unlist(lapply(FUN=add_data_file_to_df,keyfile_WRDC$stn_wrdc))

keyfile_WRDC$data_file<-data.name[I]
keyfile_WRDC$data_path<-data.path[I]

######################################For the files with a complete match:
correct_key<-keyfile_WRDC[!is.na(keyfile_WRDC$data_file),]
correct_key<-correct_key[which(correct_key$stn_wrdc==correct_key$data_file)]

df.list<-lapply(correct_key$data_path,FUN=fread)
I<-sapply(df.list,function(x) dim(x)[1]>0) #check if there is data in the file (at least 1 has missing data)
correct_key<-correct_key[I,]
df.list<-df.list[sapply(df.list,function(x) dim(x)[1]>0)]

df.list<-mapply(cbind,df.list,ser_id=correct_key$ser_id,SIMPLIFY = FALSE)

mapply(x=df.list,y=correct_key$stn_wrdc, function(x,y) write.table(x,
                                      file=paste0(write.dir,
                                      y,".txt"),
                                      col.names = TRUE,
                                      row.names = FALSE,
                                      sep=","))
#######################################Manual entries
keyfile_WRDC[is.na(keyfile_WRDC$data_file),] #check file which need to be moved manually
keyfile_WRDC[which(keyfile_WRDC$stn_wrdc!=keyfile_WRDC$data_file),] #possible mismatches which need to be moved manuallly

#no match found
tavauxsa<-fread(paste0(main.dir,"country_FRANCE_stn_TAVAUX.txt"))
tavauxsa<-cbind(tavauxsa,ser_id=90815)
write.table(tavauxsa,file=paste0(write.dir,"TAVAUXSA.txt"),col.names = TRUE,row.names = FALSE,sep=",")

colombier<-fread(paste0(main.dir,"country_FRANCE_stn_COLOMBIER_-LE_-_JEUNE.txt"))
colombier<-cbind(colombier,ser_id=90684)
write.table(colombier,file=paste0(write.dir,"COLOMBIER-LE-JEUNE.txt"),col.names = TRUE,row.names = FALSE,sep=",")

##########not detected matches/mismatches:
wrong_match<-c(1,5,17,18,20)

#mismatch ok
df.ok<-keyfile_WRDC[which(keyfile_WRDC$stn_wrdc!=keyfile_WRDC$data_file),] #possible mismatches which need to be moved manuallly
df.ok<-df.ok[-wrong_match]

df.list<-lapply(df.ok$data_path,FUN=fread)
I<-sapply(df.list,function(x) dim(x)[1]>0) #check if there is data in the file (at least 1 has missing data)
df.list<-df.list[sapply(df.list,function(x) dim(x)[1]>0)]

df.list<-mapply(cbind,df.list,ser_id=df.ok$ser_id,SIMPLIFY = FALSE)

mapply(x=df.list,y=df.ok$stn_wrdc, function(x,y) write.table(x,
                                                             file=paste0(write.dir,
                                                                         y,".txt"),
                                                             col.names = TRUE,
                                                             row.names = FALSE,
                                                             sep=","))
#mismatch not ok
df.not.ok<-keyfile_WRDC[which(keyfile_WRDC$stn_wrdc!=keyfile_WRDC$data_file),]
df.not.ok<-df.not.ok[wrong_match]

agen<-fread(paste0(main.dir,"country_FRANCE_stn_AGEN.txt"))
agen<-cbind(agen,ser_id=90874)
write.table(agen,file=paste0(write.dir,"AGEN.txt"),col.names = TRUE,row.names = FALSE,sep=",")

gela<-fread(paste0(main.dir,"country_ITALY_stn_GELA.txt"))
gela<-cbind(gela,ser_id=90966)
write.table(gela,file=paste0(write.dir,"GELA.txt"),col.names = TRUE,row.names = FALSE,sep=",")

arta<-fread(paste0(main.dir,"country_GREECE_stn_ARTA.txt"))
arta<-cbind(arta,ser_id=90696)
write.table(arta,file=paste0(write.dir,"ARTA.txt"),col.names = TRUE,row.names = FALSE,sep=",")

anacona<-fread(paste0(main.dir,"country_ITALY_stn_ANCONA.txt"))
anacona<-cbind(anacona,ser_id=90714)
write.table(anacona,file=paste0(write.dir,"ANCONA.txt"),col.names = TRUE,row.names = FALSE,sep=",")

logrono<-fread(paste0(main.dir,"country_SPAIN_stn_LOGRONO.txt"))
logrono<-cbind(logrono,ser_id=90741)
write.table(logrono,file=paste0(write.dir,"LOGRONO.txt"),col.names = TRUE,row.names = FALSE,sep=",")

# write.table(keyfile_WRDC,file="/nobackup/users/dirksen/data/radiation_europe/WRDC/key_file_names_WRDC.txt",
#             col.names = TRUE,
#             row.names = FALSE,
#             sep=",")

# cp_file<-function(path){
# cmd<-sprintf("cp %s %s",path,"/nobackup/users/dirksen/data/radiation_europe/WRDC/data_to_ECAD/")
# system(cmd)
# }
# lapply(keyfile_WRDC$data_path,FUN=cp_file)


