concotiza <- function(wd){library(dplyr)
  library("installr")
  url <-"http://www.bcra.gob.ar/PublicacionesEstadisticas/Planilla_cierre_de_cotizaciones.asp?dato=1&moneda=2"
  
  destfile <- paste0(wd, "/concotiza.html")
  
  download.file(url, destfile)
  
  library("rio")
  tc1 <- import("concotiza.html")
  
  convert("concotiza.html", "concotiza.csv")
  
  #tc1 <- import("concotiza.Rda")
  
  tc1 <- read.csv("concotiza.csv",header = T, sep = ",", dec = ",")
  
  tc1 <- as.data.frame(tc1)
  
  
  tc1 <-rename(tc1, Fecha = V1, Comprador = V2, Vendedor = V3)
  
  
  library(lubridate)
  
  tc1$Fecha <- as.character(tc1$Fecha)
  tc1$mes <- month(dmy(tc1$Fecha))
  tc1$mes <-  ifelse(nchar(tc1$mes)==1, paste0("0",substr(tc1$mes,1,1)), tc1$mes)
  tc1$anio <- year(dmy(tc1$Fecha))
  
  
  tc2 <- tc1 %>% group_by(anio, mes) %>% summarise(tc = mean(Vendedor))
  tc2<- as.data.frame(tc2)
  
  tc2$fecha <- (paste(tc2$mes,'/',tc2$anio,sep = ''))
  
  tc2$fecha <- as.character(tc2$fecha)
  # tc2$tc30 <- tc2$tc
  # tc2$tc30 <- ifelse(tc2$anio >2019, tc2$tc*1.3, tc2$tc)
  tc2$tc30 <- ifelse(tc2$anio >2019, ((tc2$tc*1.3)-tc2$tc) + ((tc2$tc*1.35)-tc2$tc) + tc2$tc, tc2$tc)
  
  
  tc <- tc2
  
  today <- format(Sys.Date(), "%d-%m-%Y")
  
  #save(tc, file= paste0('tc', today, '.Rda'))
  
  save(tc, file= paste0(wd, '/tc.Rda'))
  
  
  
  
  
} 




