fixChlorophyllData <- function(chla)
{
  actualYear <- chla$Year[1]
  
  chla$Date <- ymd("1900-01-01")#cargo la columna con un estilo de fecha 
  
  for(i in 1:nrow(chla))
  {
    if (is.na(chla$Year[i]))# chla$Year[i]=chla[i,1]
    {chla$Year[i] <- actualYear}
    else
    {actualYear <- chla$Year[i]}
    
    
    fechaTmp1 <- ymd(paste(chla$Year[i],chla$Month[i],1))
    fechaTmp2 <- dmy(chla$Month[i])
    fechaTmp3 <- mdy(chla$Month[i])
    
    if(is.na(fechaTmp1)==FALSE)
    {
      chla$Date[i] <- fechaTmp1
    }else if(is.na(fechaTmp2)==FALSE) 
          {
            chla$Date[i] <- fechaTmp2
          }else if(is.na(fechaTmp3)==FALSE)
                {
                chla$Date[i] <- fechaTmp3
                }else if (chla$Month[i]=="Ago")
                      {
                      chla$Date[i]=ymd(paste(chla$Year[i],8,1))
                      }
    
    }  
    
#Sys.getlocale(category = "LC_ALL",locale="C")      
#chla$Date <- ymd(paste(chla$Year,chla$Month,1))

chla$IntegE2 <- abs(as.numeric(chla$IntegE2))#eliminamos lo que no son numeros
  
chla$IntegE1 <- abs(chla$IntegE1)

return(chla)  
}

