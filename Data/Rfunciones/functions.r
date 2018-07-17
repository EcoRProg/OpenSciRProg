
fixChlorophyllData <-  function (chla){
  
   actualYear <- chla$Year[1]
  
    chla$date <- ymd("1900-01-01")
    for(i in 1:nrow(chla)){  
    if(is.na(chla$Year[i])) {
      chla$Year[i] <- actualYear
      
    } else {
      actualYear <-  chla$Year[i]
    }
    }
    chla$Date <-  ymd( paste(chla$Year,chla$Month, 1 ) )
  return(chla)
  }  
    