fixChlorophyllData<-function(chla) {
  actualYear<-chla$Year[1]
chla$Date <-ymd("1900-01-01")#aca le decimos que arme una nueva columna
for (i in 1:nrow(chla)){
  if (is.na(chla$Year[i])) {
    
    chla$Year[i]<-actualYear
  }else {
    actualYear<-chla$Year[i]
  }
fechaTmp<-ymd(paste(chla$Year[i],chla$Month[i],1))
if(is.na(fechaTmp)){
  fechaTmp <-dmy(chla$Month[i])
} 
  chla$Date[i] <-fechaTmp
}


chla$IntegE1<-abs(chla$IntegE1)#para decirle que sea valor absoluto, y no negativo
chla$IntegE2<-abs(as.numeric(chla$IntegE2))#que me considere la columna como numerico y no caracter
return(chla)
}
