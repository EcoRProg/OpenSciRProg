fixChlorophyllData<-function(chla) {
  actualYear<-chla$Year[1]
chla$Date <-ymd("1900-01-01")#aca le decimos que arme una nueva columna
for (i in 1:nrow(chla)){
  if (is.na(chla$Year[i])) {
    
    chla$Year[i]<-actualYear
  }else {
    actualYear<-chla$Year[i]
  }
}
chla$Date<-ymd(chla$Year,chla$Month,1)
               return(chla)
}
