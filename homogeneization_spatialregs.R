rm(list =ls())

# Used data and information not showed for privacy policies #
getwd()
directorio = "/Users/stefa/Documents/Code/vut/"; setwd(directorio)
save(directorio, file = "directorio.Rda")
zone_number = ""
save(zone_number, file = "zone_number.Rda")

library(sf)
require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
datos = st_read (con, query = "")
parcelas = st_read (con, query = "")
names(datos)
today = format(Sys.Date(), "%Y%d%m")
nombredatos = paste0(directorio, "datos", zone_number,"_", today,".Rda"); nombredatos; save(datos, file = nombredatos)
nombreparcelas = paste0(directorio, "parcelas", zone_number,"_", today,".Rda" ); nombreparcelas; save(parcelas, file = nombreparcelas)

#############################################

summary(datos$oferta_inm)
library(nngeo)
library(DescTools)
library(tibble)
library(sf)
library(spatialreg)
library(expss)
library(spdep)
library(dplyr)
library(rlang)

contains_any_na = sapply(datos, function(x) any(is.na(x)))
names(datos)[contains_any_na]
datos$fid = NULL


# Superficie
summary(datos$p_sup)
hist(datos$p_sup)
boxplot(datos$p_sup)
datos$regresion = ifelse(datos$p_sup < 6000, 1, 0)
table(datos$regresion)

# hist(ver$p_sup)
# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(ver) +
#   tm_dots("p_sup", style = "quantile") + tm_shape(datos) +
#   tm_dots("p_sup", style = "quantile"); mapa


# Forma
table(datos$p_forma)
#datos$p_forma = ifelse (as.character(datos$p_forma)=="2", "1", as.character(datos$p_forma))
class(datos$p_forma)
datos$m_forma = as.factor(as.character(datos$p_forma))
table(datos$m_forma)
#datos$p_forma = datos$forma


# Tipo de valor oferta y venta
class(datos$p_tipodevalor)
table(datos$p_tipodevalor)
datos$m_tipodevalor = ifelse((datos$p_tipodevalor == 1 | datos$p_tipodevalor == 2), 1, 0)
class(datos$m_tipodevalor)
datos$m_tipodevalor = as.factor(as.character(datos$m_tipodevalor))
table(datos$m_tipodevalor)


# Situacion juridica
class(datos$p_sj)
summary(as.factor(as.character(datos$p_sj)))
datos$m_sj = ifelse(datos$p_sj == 1, 0, 1)
datos$m_sj = as.factor(as.character(datos$m_sj ))
table(datos$m_sj)
class(datos$m_sj)


# Ubicacion en la cuadra
class(datos$p_ubicuadra)
table(datos$p_ubicuadra)
datos$m_ubicacion_cuadra = case_when( as.character(datos$p_ubicuadra)== "1" ~ "0",
                                      as.character(datos$p_ubicuadra) == "2" ~ "1",
                                      as.character(datos$p_ubicuadra) == "3" ~ "2",
                                      as.character(datos$p_ubicuadra) == "4" ~ "3",
                                      as.character(datos$p_ubicuadra) == "5" ~ "4")
table(datos$m_ubicacion_cuadra)
class(datos$m_ubicacion_cuadra)
datos$m_ubicacion_cuadra = as.factor(datos$m_ubicacion_cuadra)


# Actualización
source("/Users/stefa/Documents/Code/vut/Tipo_de_cambio/automat_fun.R")
concotiza(directorio)
load("tc.Rda")
table(datos$p_fechavalor)
class(datos$p_fechavalor)
class(tc$fecha) 
tc$fecha = as.character(tc$fecha)
datos$p_fechavalor = as.character(datos$p_fechavalor)
datos$fecha = datos$p_fechavalor

#datos1 = left_join(datos, tc[,c("ccl", "fecha")], by = "fecha")
datos1 = left_join(datos, tc, by = "fecha")
names(datos1)

table(datos1$fecha)
summary(datos1$tc30)
datos = datos1

rm(datos1)
table(is.na(datos$tc30))

names(datos)
table(datos$p_moneda)
summary(datos$p_valor)
datos$valor_pesos = ifelse(datos$p_moneda == "1", datos$p_valor*datos$tc30, datos$p_valor)
summary(datos$valor_pesos)

datos$vm2_original = datos$p_valor / datos$p_sup 

datos$valor_m2 = datos$valor_pesos / datos$p_sup
summary(datos$valor_m2)
hist(datos$valor_m2)
plot(as.factor(datos$fecha), datos$valor_m2)

# ver = subset(datos, valor_m2>500000 )#| valor_m2<100)
# ver2 = subset(datos, p_anio==2020 )
# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(ver) +
#   tm_dots("valor_m2") 
# mapa

save(datos, file = "datos.Rda")
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)

# #ver = subset(datos, valor_m2 > 10000)
# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(datos) +
#   tm_dots("valor_m2")
# mapa
save(parcelas, file = "parcelas.Rda")


# Cuantiles
load("datos.Rda")
load("parcelas.Rda")
cuantiles = 4
save(cuantiles, file = "cuantiles.Rda")
quant = as.data.frame(quantile(parcelas$vut_2023, probs = seq(0, 1, 1/cuantiles)))
datos$quant = case_when( datos$vut_2023 <= quant[2,] ~ "Q1",
                           datos$vut_2023 > quant[2,] & datos$vut_2023 <= quant[3,] ~ "Q2",
                           datos$vut_2023 > quant[3,] & datos$vut_2023 <= quant[4,] ~ "Q3",
                           datos$vut_2023 > quant[4,]~ "Q4")
table(datos$quant)
parcelas$quant = DescTools::CutQ(parcelas$vut_2023,
                                  breaks = quantile(parcelas$vut_2022,(seq(0, 1, by = 1/cuantiles))))

# quant = as.data.frame(quantile(parcelas$inc_edif, probs = seq(0, 1, 1/cuantiles)))
# datos$quant = case_when(  datos$inc_edif <= quant[2,] ~ "Q1",
#                           datos$inc_edif > quant[2,] & datos$inc_edif <= quant[3,] ~ "Q2",
#                           datos$inc_edif > quant[3,] & datos$inc_edif <= quant[4,] ~ "Q3",
#                           datos$inc_edif > quant[4,] & datos$inc_edif <= quant[5,] ~ "Q4",
#                           datos$inc_edif > quant[5,] & datos$inc_edif <= quant[6,] ~ "Q5",
#                           datos$inc_edif > quant[6,] & datos$inc_edif <= quant[7,] ~ "Q6",
#                           datos$inc_edif > quant[7,] & datos$inc_edif <= quant[8,] ~ "Q7",
#                           datos$inc_edif > quant[8,] & datos$inc_edif <= quant[9,] ~ "Q8",
#                           datos$inc_edif > quant[9,] & datos$inc_edif <= quant[10,] ~ "Q9",
#                           datos$inc_edif > quant[10,]~ "Q10")
# table(datos$quant)
# class(parcelas$inc_edif)
# parcelas$quant = DescTools::CutQ(parcelas$inc_edif,
#                                   breaks = quantile(parcelas$inc_edif,(seq(0, 1, by = 1/cuantiles))))
table(parcelas$quant)
class(parcelas$quant)
table(datos$quant)
class(datos$quant)
datos$quant = as.factor(datos$quant)

# col = rev(brewer.pal(5, "Spectral"))
# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(datos) +
#   tm_dots("quant", palette= col)
# mapa

plot(as.factor(datos$quant), datos$valor_m2)
quant_vut = st_drop_geometry(datos) %>% 
  group_by(quant) %>% 
  summarise(media = mean(valor_m2),
            sd = sd(valor_m2))
datos1 = left_join(datos, quant_vut, by = "quant")
summary(datos1$media)
datos = datos1
rm(datos1)

datos$outlier = ifelse(abs(datos$valor_m2)<(datos$media + 2 * datos$sd), 0, 1 )
table(datos$regresion, datos$outlier)
ver = subset(datos, outlier == 1)
plot(as.factor(ver$quant), ver$valor_m2)


library(RColorBrewer)
col = rev(brewer.pal(5, "Spectral"))
library(tmap)
tmap_mode("view")
mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
 tm_shape(ver) +
 tm_dots("quant", palette= col) +  tm_shape(datos) +
 tm_dots("quant")
mapa

table(datos$datos_coef)
raros = subset(datos, datos_coef==0)
datos = subset(datos, datos_coef==1)
# datos = subset(datos, regresion == 1 & outlier == 0)
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)
st_write(parcelas, "parcelas.gpkg", delete_dsn = T, delete_layer = T)
save(raros, file = "raros.Rda")
save(datos, file = "datos.Rda")
save(parcelas, file = "parcelas.Rda")


rm(list=ls())
######################################
getwd()
load("datos.Rda")
load("cuantiles.Rda")

summary(datos$x)
summary(datos$y)
datos$x = datos$x/100000
datos$y = datos$y/100000
names(datos)

otras_variables =   "d_viasprin  + d_alta + d_baja  + perc_edif + d_rio + ndvi + porc_eau + x + y + d_ruta + oferta_inm "
if(cuantiles == 1){
  
  form = "log(valor_m2) ~ log(tc30) + log(p_sup) + log(p_frente) + m_forma + m_ubicacion_cuadra + m_tipodevalor + m_sj "
  
} else {
  form = "log(valor_m2) ~ log(tc30) + log(tc30):quant + log(p_sup) + log(p_frente) +  m_forma  + m_ubicacion_cuadra + m_tipodevalor + m_sj"
}


if(exists("otras_variables")) {
  
  form = paste(form, otras_variables, sep = " + ")
  
}

form = as.formula(form)

summary(datos)
ols = lm(form, datos)
summary(ols)
cord = st_coordinates(datos)
d = dnearneigh(cord, 0, 500)
dlist = nbdists(d, coordinates(cord))
idlist = lapply(dlist, function(x) 1/x)
lw = nb2listw(d, glist=idlist ,style="W" , zero.policy = TRUE)
print(lw, zero.policy=TRUE)

# Analisis de la dependencia espacial en los residuos y calculo de multiplicadores de Lagrange
moran = lm.morantest(lm(form, datos),lw , zero.policy = TRUE) # H0: Independencia espacial

moran_lm = lm.LMtests(ols, lw ,test = "all", zero.policy = T)

# Eleccion de modelo
if (moran$p.value > 0.1){
  
  print("Se estiman las elasticidades mediante modelo lineal")
  
  b_sig_lm = data.frame(summary(ols)["coefficients"])
  b_sig_lm = rownames_to_column(b_sig_lm)
  names(b_sig_lm)[names(b_sig_lm) == "rowname"] = "vble"
  remove_rownames(b_sig_lm)
  
  b_total = cbind(b_sig_lm, b_sig_lm$coefficients.Estimate) #se la vuelvo a pegar para buscar una sola vez, en la col 6
  
} else {
  modelo = ifelse((moran_lm$SARMA$p.value < 0.1), "modelo SAC", ifelse((moran_lm$RLMerr$p.value < 0.1)
                                                                        & (moran_lm$RLMlag$p.value > 0.1), "modelo SEM",
                                                                        "modelo SAR"))
  
  print(paste("Se estiman las elasticidades mediante", modelo, sep=" "))
  
  
    if (modelo == "modelo SAC"){
    regresion = sacsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
  }
  if (modelo == "modelo SAR"){
    regresion = lagsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
  }
  if (modelo == "modelo SEM"){
    regresion = errorsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
  }
  
  print(summary(regresion, Nagelkerke = T))
  
  
}

#Significatividad de las coef
b_sig = data.frame(summary(regresion)["Coef"])
b_sig = rownames_to_column(b_sig)
names(b_sig)[names(b_sig) == "rowname"] = "vble"
remove_rownames(b_sig)
b_sig = subset(b_sig, vble != "(Intercept)")

# Impacto total - solo en modelos SAC y SAR
if(modelo == "modelo SAC" | modelo == "modelo SAR"){
  impactos = impacts(regresion, listw=lw)
  
  a = data.frame(impactos$total)
  b_total = cbind(b_sig,a)
  
}else{
  b_total = cbind(b_sig, b_sig$Coef.Estimate)
}

names(b_total)
b_coef = rename(b_total, p = Coef.Pr...z.. , b=impactos.total)
save(b_coef, file="b_coef.Rda")

################################ELASTICIDADES
elasticidad = data.frame(q=as.numeric(1:cuantiles))

if(cuantiles == 1){
  elasticidad$elasticidad[1] = vlookup("log(tc30)", b_total, 6)
}else{
  for(i in 2:cuantiles){
    elasticidad$elasticidad[1] = vlookup("log(tc30)", b_total, 6)
    elasticidad$elasticidad[i] = sum(vlookup(paste("log(tc30):quantQ", i, sep = ""), b_total, 6),
                                      vlookup("log(tc30)", b_total, 6))
  }
}

elasticidad$quant = paste("Q", elasticidad$q, sep="")
elasticidad$q = NULL
dir.create("Elasticidad")
save(elasticidad, file="Elasticidad/elasticidad.Rda")
resultado = list("Elasticidades por cuantil", elasticidad)
print(resultado)

#actualizamos
rm(list =ls())
load("datos.Rda")
load("raros.Rda")
datos = rbind(datos, raros)
rm(raros)
load("Elasticidad/elasticidad.Rda")
load("tc.Rda")
tc
tc_act = tc$tc30[[156]]

class(elasticidad$quant)

elasticidad$quant = as.factor(elasticidad$quant)
datos$elasticidad = NULL
datos = left_join(datos, elasticidad, by="quant")
datos$var_tc = (tc_act/(datos$tc30)) - 1
datos$valor_actualizado = (1 + datos$var_tc * datos$elasticidad) * datos$valor_m2

plot(as.factor(datos$anio), datos$valor_actualizado )

summary(datos$valor_actualizado)
summary(datos$valor_m2)
hist(datos$valor_actualizado)

summary(datos$valor_actualizado)

library(tmap)
tmap_mode("view")
mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
 tm_shape(datos) +
 tm_dots("zona")
mapa

save(datos, file ="datos_actualizados.Rda")

summary(datos$coef)
summary(datos$valor_actualizado)


rm(list=ls())
######################################HOMOGENEIZACIÓN

load("datos_actualizados.Rda")
coef = read.csv("param_coef_2020.csv",header = T, sep = ";", dec = ",")
getwd()
table(coef$Cluster)
coef = subset(coef, Cluster == "Localidades Serranas")

#variable superficie
b_sup = coef[,c("sup")]

# variable frente 
b_frente = coef[,c("frente")]

table(datos$m_sj)


b_sig = coef[,c("forma", "esquina", "interno", "sal_calles", "tv", "sj")]
load("b_coef.Rda")



if (subset(b_coef, vble=="log(p_sup)", select="p")<0.1){
  b_sup = as.numeric(subset(b_coef, vble=="log(p_sup)", select="b"))} else b_sup = coef[,c("sup")]

if (subset(b_coef, vble=="log(p_frente)", select="p")<0.1){
  b_frente = as.numeric(subset(b_coef, vble=="log(p_frente)", select="b"))} else b_frente = coef[,c("frente")]

if (subset(b_coef, vble=="m_forma1", select="p")<0.1){
  b_sig$forma = as.numeric(subset(b_coef, vble=="m_forma1", select="b"))}

if (subset(b_coef, vble=="m_ubicacion_cuadra1", select="p")<0.1){
  b_sig$esquina = as.numeric(subset(b_coef, vble=="m_ubicacion_cuadra1", select="b"))}

if (subset(b_coef, vble=="m_ubicacion_cuadra2", select="p")<0.1){
  b_sig$interno = as.numeric(subset(b_coef, vble=="m_ubicacion_cuadra2", select="b"))}

if (subset(b_coef, vble=="m_ubicacion_cuadra3", select="p")<0.1){
  b_sig$sal_calles = as.numeric(subset(b_coef, vble=="m_ubicacion_cuadra3", select="b"))}

if (subset(b_coef, vble=="m_tipodevalor1", select="p")<0.1){
b_sig$tv = as.numeric(subset(b_coef, vble=="m_tipodevalor1", select="b"))}

if (subset(b_coef, vble=="m_sj1", select="p")<0.1){
  b_sig$sj = as.numeric(subset(b_coef, vble=="m_sj1", select="b"))}

beta = t(as.matrix(b_sig))

getwd()
dir.create("Coeficientes")
save(b_sup, file = "Coeficientes/b_sup.Rda")
save(b_frente, file = "Coeficientes/b_frente.Rda")
save(b_sig, file = "Coeficientes/b_sig.Rda")

vbles = data.frame(id = as.numeric(1:(dim(datos)[1])))

vbles$forma = as.numeric(as.character(datos$m_forma))

vbles$ubicacion_cuadra1 = ifelse(as.numeric(as.character(datos$m_ubicacion_cuadra)) == 
                                    1, 1, 0)
vbles$ubicacion_cuadra2 = ifelse(as.numeric(as.character(datos$m_ubicacion_cuadra)) == 
                                    2, 1, 0)
vbles$ubicacion_cuadra3 = ifelse(as.numeric(as.character(datos$m_ubicacion_cuadra)) == 
                                    3, 1, 0)

vbles$m_tipodevalor = as.numeric(as.character(datos$m_tipodevalor))

vbles$m_sj = as.numeric(as.character(datos$m_sj))

largo = as.numeric(length(vbles$id))
vbles$id = NULL
names(vbles)

for (i in 1:largo) {
  a = t(as.matrix(as.numeric(vbles[i, ])))
  datos$expon[i] = a %*% beta
}

summary(datos$expon)
mediana_sup = coef[,c("sup_median")]
save(mediana_sup, file = "Coeficientes/mediana_sup.Rda")

mediana_frente = coef[,c("frente_median")]
save(mediana_frente, file = "Coeficientes/mediana_frente.Rda")

datos$coef_2023 = ((datos$p_sup/mediana_sup)^ b_sup) * ((datos$p_frente/mediana_frente)^b_frente) * (exp(datos$expon))
summary(datos$coef_2023)
datos$coef_2023 = ifelse(datos$coef_2023 < 0.2, 0.2, ifelse(datos$coef_2023 > 
                                                     1.5, 1.5, datos$coef_2023))

summary(datos$coef_2023)
hist(datos$coef_2023)
datos$m2_coef = datos$valor_actualizado/datos$coef_2023  

datos$vut = datos$m2_coef
summary(datos$vut)


########################
(median(datos$vut)/median(datos$vut_2022))-1
datos$aumento = (datos$vut/datos$vut_2022) - 1 
summary(datos$aumento)
# ver = subset(datos, aumento > 4)
# mapview::mapview(ver)
# datos = subset(datos, id==86034)
# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(ver) +
#   tm_dots("aumento")
# mapa


################################

names(datos)
datos=datos[,c("id","x","y","zona","p_id_origen","p_origen","p_tipodeinmueble","p_tipodevalor",
               "nomenclatura","p_sj","p_ubicuadra","p_sup","p_frente","p_forma","p_valor","p_moneda",
               "p_fechavalor","p_anio","par_idparcela","distancia","loc_catastro","d_ruta","d_viasprin","d_viassec",
               "d_alta","d_baja","d_lineadiv","d_depre","d_rio","d_cementerio","d_popu","prom_edif",
               "prom_lote","perc_edif","perc_baldm","perc_bald","perc_ph_cuenta","perc_val_urb","inc_edif","porc_uec",
               "porc_ued","porc_re","porc_eau","porc_bu","porc_ear","porc_agua","ind_con","bci",
               "rndsi","ui","ndbi","ndvi","dens_osm","heat_iibb","osm_iibb","fragment",
               "vut_2018","vut_2019","vut_2020","vut_2021","vut_2022","localidad","oferta_inm","fot",
               "coef","zonas_capital","barr_priv","datos_coef","regresion","m_forma","m_tipodevalor","m_sj",
               "m_ubicacion_cuadra","coef_2023","vut","aumento")]
save(datos, file ="datos_homog.Rda")
st_write(datos, "datos_homog.gpkg", delete_dsn = T, delete_layer = T)

library(tmap)
tmap_mode("view")
mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(datos) +
  tm_dots("vut", style = "quantile")
mapa
#getwd()

############################ENTREGA
rm(list=ls())
getwd()
dir.create("Entrega")
datos = st_read("datos_homog.gpkg")
names(datos)

datos = subset(datos, select = substr(colnames(datos), 1,2) != "m_")
names(datos)
datos$regresion = NULL
st_write(datos, "Entrega/datos_entrega.gpkg", delete_dsn = T, delete_layer = T)

rm(list=ls())
datos = st_read("Entrega/datos_entrega.gpkg")
datos$aumento = (datos$vut / datos$vut_2022) - 1
summary(datos$aumento)
datos = subset(datos, aumento < 3)
gmodels::ci(datos$aumento)

rm(list = ls())

######################################################################
require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
today = format(Sys.Date(), "%Y%m%d")
load("zone_number.Rda")
datos = st_read("Entrega/datos_entrega.gpkg")
nombre = paste0("z",zone_number, "_datos_homog_", as.character(today))
nombre; save(nombre, file="nombre.Rda")
summary(datos$vut)

st_write(obj = datos, con, Id(schema="",table = nombre), delete_dsn = T, delete_layer = T)

library(tibble)
a = as.data.frame(summary(datos$geom))
a = rownames_to_column(a)
geo = a[1,1]

dbGetQuery(con, paste0("",nombre,
                       ""))
tabla = dbGetQuery(con, "")

df = datos[duplicated(datos$id),]
dbGetQuery(con, paste0("",nombre,"ADD PRIMARY KEY (id);"))

dbGetQuery(con, paste0("",nombre,",
                             "))

datosver = st_read(con, query = paste0("",nombre))
class(datosver)
datosver$geom
summary(datosver$vut)

library(tmap)
tmap_mode("view")
mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(datosver) +
  tm_dots("vut", style = "quantile")
mapa

load("Coeficientes/b_sup.Rda")
load("Coeficientes/b_frente.Rda")
load("Coeficientes/b_sig.Rda")
load("Elasticidad/elasticidad.Rda")

cat(paste0("En la siguiente ubicación están los datos homogeneizados\nEsquema:  \n", 
           paste0("Tabla: ", nombre)),paste0("\nCantidad de observaciones: ", nrow(datos)),
    # paste0("\nNuevo coeficiente de homogeneización de superficie = ", round(b_sup, 4)),
    # paste0("\nNuevo coeficiente de homogeneización de frente = ", round(b_frente, 4)),
    # paste0("\nNuevo coeficiente de homogeneización de forma = ", round(b_sig[[1]], 4)),
    # paste0("\nNuevo coeficiente de homogeneización de esquina = ", round(b_sig[[2]], 4)),
    # paste0("\nNuevo coeficiente de homogeneización de interno = ", round(b_sig[[3]], 4)),
    # paste0("\nNuevo coeficiente de homogeneización de salida a 2+ calles = ", round(b_sig[[4]], 4)),
    paste0("\nNuevo coeficiente de homogeneización de tipo de valor = ", round(b_sig[[5]], 4)),
    paste0("\nNuevo coeficiente de homogeneización de situación jurídica = ", round(b_sig[[6]], 4)),
    "\nLas nuevas elasticidades para la variacion del tipo de cambio son las siguientes:"
)
elasticidad
################################

rm(list=ls())
