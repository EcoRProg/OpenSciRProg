fixChlorophylData <- function(chla) {
  
  actualYear <- chla$Year[1]  #asigna un valor al año
  
  chla$Date <- ymd("1900-01-01") #agrega una nueva columna "Deta" que equivale a una variable 
  
  for(i in 1:nrow(chla)) {  #para la variable i desde la fila 1:hasta nrow (el número total de filas) = recorre cada registro del data frame
    
    if(is.na(chla$Year[i])) {  #el valor de la fila i de chla de la columna Years es NA?
      
      chla$Year[i] <- actualYear  #si VERDADERO, el valor de la fila i de chla de la columna Years es actualYear (ver siguiente condición)
      
        } else { #si es FALSO
      
          actualYear <- chla$Year[i]  #el valor de la fila i de chla de la columna Years es el año que figura (a lo que se llama actualYear)
        
            }
    
  fechaTmp <- ymd(paste(chla$Year[i],chla$Month[i],1))  #define el formato de una nueva variable "fechaTmp" e indica de dónde saca los valores
    
    if(is.na(fechaTmp)) {              
      
      if(chla$Month[i]=="Mar") { #es la variable i Marzo?
        
        fechaTmp <- ymd(paste(chla$Year[i], 3, 1)) #si VERDADERO, 
        
          } else if(chla$Month[i]=="Ago") { #si, FALSO, es entonces la variable i Agosto?
          
              fechaTmp <- ymd(paste(chla$Year[i], 8, 1)) #si VERDADERO,
          
                } else { #si FALSO
                  
                  fechaTmp <- dmy(chla$Month[i]) #la variable fechaTmp 
                
                    }
                      }
  
  chla$Date[i] <- fechaTmp #asocia la variable (columna) "Date a la variable fechaTmp           
  
  }

  chla$IntegE1 <- round(abs(chla$IntegE1),2)       #valor absoluto y redondedo en 2 decimales
  
  chla$IntegE2 <- round(abs(as.numeric(chla$IntegE2)),2)   #cambiar tipo de dato numérico, valor absoluto y redondedo en 2 decimales
  
return(chla)

}

