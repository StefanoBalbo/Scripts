##################################################################

rm(list=ls())

library(dplyr)
library("installr")
library("rio")
library(tidyverse)
library(rvest)
library(openxlsx)
library(scales) 
library(data.table)

directorio <- "/Users/stefa/Documents/Code/BCRA/"
setwd(directorio); getwd()


########### Tipo de cambio: Dólar blue y dólar oficial ###########

tc <- read.csv("https://api.bluelytics.com.ar/v2/evolution.csv"); names(tc)

tc.blue <- subset(tc, tc$type == "Blue")
tc.of <- subset(tc, tc$type == "Oficial")

tc.blue <- rename(tc.blue, Fecha = day, Comprador = value_buy, Vendedor = value_sell); tc.blue 
tc.of <- rename(tc.of, Fecha = day, Comprador = value_buy, Vendedor = value_sell); tc.of 

summary(tc.blue)
summary(tc.of)

write.xlsx(tc.blue, "dolarblue.xlsx")
write.xlsx(tc.of, "dolaroficial.xlsx")

# # 

head(tc.blue); head(tc.of)

graph <- left_join(tc.blue, tc.of, by = "Fecha"); graph

graph <- graph %>%
  select(Fecha, Vendedor.x, Vendedor.y) %>%
  rename(Blue = Vendedor.x, Oficial = Vendedor.y) %>%
  mutate(Fecha = as.POSIXct(Fecha, format = "%Y-%m-%d"))

graph <- graph %>% 
  filter(year(Fecha) >= 2019)

graph$Brecha <- graph$Blue / graph$Oficial
head(graph)
graph$Brecha <- percent(graph$Brecha, accuracy = 0.1)
head(graph)

ggplot(data = graph, aes(x = Fecha)) + 
  geom_line(aes(y = Blue), color = "blue") + 
  geom_line(aes(y = Oficial), color = "red") +
  ggtitle("Dólar Blue vs Dólar Oficial") + 
  ylab("Valor dólar") +
  scale_x_datetime(labels = scales::date_format(format = "%Y-%m-%d", tz = "UTC", locale = NULL))

#ggplot(data = graph, aes(x = Fecha)) + 
#  geom_bar(aes(fill = Brecha), width = 30, color = "lightblue") + 
#  ggtitle("Brecha cambiaria (en %)") + 
#  ylab("Brecha") +
#  scale_x_datetime(labels = scales::date_format(format = "%Y-%m-%d", tz = "UTC", locale = NULL))

rm(list=ls())


# Tipo de Cambio Minorista de Referencia de la Ciudad de Buenos Aires
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

concotiza(directorio)
load("tc.Rda"); tc
head(tc)


############################## LELIQs ############################

url <- "https://www.bcra.gob.ar/PublicacionesEstadisticas/Historial-Leliq.asp"
file <- "/Users/IDECOR/Documents/Code/BCRA/leliqdata.html"
download.file(url, file)

leliq <- import("leliqdata.html")
convert("leliqdata.html", "leliqdata.csv")

leliq <- read.csv("/Users/IDECOR/Documents/Code/BCRA/leliqdata.csv", header = T, sep = ",", dec = ",")
leliq <- as.data.frame(leliq)
head(leliq)

leliq <- rename(leliq, Fecha_Subasta = V1, 
                Valor_nominal_pesos = V2, 
                Vto = V3, 
                Plazo_dias = V4, 
                Tasa_min = V5, 
                Tasa_max = V6, 
                Tasa_prom = V7); names(leliq)

options(scipen = 999) # Remueve notación científica
summary(leliq)

leliq$Valor_nominal_pesos <- as.numeric(gsub(",", ".", gsub("\\.", "", leliq$Valor_nominal_pesos))); class(leliq$Valor_nominal_pesos)

summary(leliq$Valor_nominal_pesos)
summary(leliq)
head(leliq)

write.xlsx(leliq, "leliqdata.xlsx")

graph = leliq  %>% 
  group_by(Vto)  %>% 
  summarise(LELIQs_pesos = sum(Valor_nominal_pesos))
graph

ggplot(data = graph, 
       aes(x = Vto, y = LELIQs_pesos)) + ggtitle("LELIQs (en $ trillones)") + scale_y_continuous(labels = scales::label_number_si()) 

rm(list=ls())





