#the following coding is maintained:
# 0=valid,1=suspect,9=missing,-9=not done

#offset test
# According to the CM11 Pyra-nometer manual, the output should not fall lower than−12Wm² (Kipp & Zonen, 2000)
# 1367 is the upper limit at the top of the atmosphere
# manual: https://brage.bibsys.no/xmlui/bitstream/handle/11250/292854/Grini2015.pdf?sequence=1
offset_test<-function(irr){
  if ( irr >= -120 | irr <= 13670) {
    qc=0
  } else if ( is.na(irr) ) {
    qc=9
  } else if ( irr < 0 ) {
    qc=1
  } else {
    qc=-9
  }
  return(qc)
}


data.files<-list.files("/nobackup/users/dirksen/data/radiation_europe/WRDC/data_to_ECAD/",full.names=TRUE,pattern=".txt")
df<-lapply(data.files,fread)

df.test<-lapply(df,function(x) {
  x$qc<-unlist(lapply(x$qq,offset_test))
  x
})




