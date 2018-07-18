fixChlorophylData <- function(chla){
  
  actualYear <- chla$Year[1]
  
  chla$Date <- ymd("1900-01-01")
  
  for(i in 1:nrow(chla)) {        #para el valor de la fila i de chla en el rango de fila 1:hasta nrow (el número total de filas)
    
    if(is.na(chla$Year[i])) {     #el valor de la fila i de chla de la columna Years es NA?
      
      chla$Year[i] <- actualYear  #si VERDADERO, el valor de la fila i de chla de la columna Years es actualYear (ver siguiente condición)
      
    } else {                      #si es FALSO
      
      actualYear <- chla$Year[i]  #el valor de la fila i de chla de la columna Years es el año que figura (a lo que se llama actualYear)
    }
    
  fechaTmp <- ymd(paste(chla$Year[i],chla$Month[i],1))   #
    
    if(is.na(fechaTmp)) {                 #es el dato de la celda de la columna fechaTmp NA?
      
     fechaTmp <- dmy(chla$Month[i])         #si VERDADERO, 
    
    }
  
    chla$Date[i] <- fechaTmp            #entonces el valor 
  
  }

  chla$IntegE1 <- round(abs(chla$IntegE1),2)       #valor absoluto y redondedo en 2 decimales
  
  chla$IntegE2 <- round(abs(as.numeric(chla$IntegE2)),2)   #cambiar tipo de dato numérico, valor absoluto y redondedo en 2 decimales
  
return(chla)
}

