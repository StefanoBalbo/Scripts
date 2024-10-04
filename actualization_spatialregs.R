rm(list =ls())


############################## LIBRERIAS #############################

library(nngeo)
library(DescTools)
library(tibble)
library(sf)
library(spatialreg)
library(expss)
library(spdep)
library(dplyr)
library(rlang)
library(stringr)
require(RPostgres)
library(corrplot)
library(GWmodel)      
library(spgwr)       
library(spdep)       
library(gstat)    
library(RColorBrewer)
library(classInt)    
library(raster)    
library(gridExtra)  
library(ggplot2)    
library(latticeExtra)
library(RStoolbox)  
library(mapview)
#library(SpatialML)    # Geographical Weigted Random Forest

options(timeout = 9999999999999999999999999)
getwd()
directorio <- ""
setwd(directorio)
save(directorio, file = "directorio.Rda")


############################## DATA ALQUILERES #############################

con <- dbConnect(Postgres(), dbname = "", host = "172.25.1.248", port = 5432, 
                 user = "balbo.stefano", password = "")
datos <- st_read (con, query = "" )
today <- format(Sys.Date(), "%Y%d%m")

nombredatos <- paste0(directorio, "datos_alquileres_", today,".Rda" )
nombredatos
save(datos, file = nombredatos)

summary(datos)
mapview(datos)

table(datos$tipologia)

datos = subset(datos, is.na(tipologia)==F)
table(datos$p_origen)

table(datos$tipologia)

datos$tipologia_original <- datos$tipologia


table(datos$p_fechavalor) #FECHAS
datos$año = as.numeric(substr(datos$p_fechavalor, 4, 7))
table(datos$año)

datos = subset(datos, año>=2022)
datos = rename(datos, fecha = p_fechavalor)

datos$fecha

datos$mes = as.numeric(substr(datos$fecha, 1, 2))
datos$fecha = as.character(paste0(datos$mes, "/", datos$año))

datos$fecha


# # # # # # # # # # # # ACTUALIZACIÓN # # # # # # # # # # # # 

source("/automat_fun.R")
concotiza(directorio)
load("tc.Rda"); tc

tc$anio = as.numeric(tc$anio)
tc$mes = as.numeric(tc$mes)

tc$fecha = as.character(paste0(tc$mes, "/", tc$anio)) 
table(datos$fecha)
table(tc$fecha)

class(datos$fecha)
class(tc$fecha) # TIENEN QUE SER CHR

datos1 <- left_join(datos, tc[,c("fecha","tc30")], by="fecha")
names(datos1)

table(datos1$fecha)

summary(datos1$tc30)

datos <- datos1

rm(datos1)

############################## vm2 - VALORES EN PESOS #############################

names(datos)
table(datos$p_moneda)
datos$p_valor = as.numeric(datos$p_valor)
summary(datos$p_valor)

datos$valor_pesos <- ifelse(datos$p_moneda=="dolares", datos$p_valor*datos$tc30, datos$p_valor)
summary (datos$valor_pesos)

datos$p_valor = as.numeric(datos$p_valor)
summary(datos$p_sup)

datos$p_sup = as.numeric(datos$p_sup)

datos = subset(datos, p_sup > 18)
datos$vm2 = datos$valor_pesos / datos$p_sup
summary(datos$vm2)

boxplot(datos$vm2)
ver = subset(datos, vm2>50000) # OUTLIERS
mapview(ver)

mediana = median(datos$vm2)
sd = sd(datos$vm2)
datos = subset(datos, vm2 < mediana+(2*sd))
datos = subset(datos, vm2>100)
summary(datos$vm2)

mapview(datos, zcol = "vm2")

############################## IPC #############################

ipc = read.csv("index alquileres.csv", header = T, sep = ";")
ipc$fecha = as.character(paste0(ipc$mes, "/", ipc$año))
datos = left_join(datos, ipc)
datos$index

ver = subset(datos, is.na(index)==T)
table(ver$fecha)

datos$index = ifelse(is.na(datos$index)==T, 867.65 * 1.06, datos$index)
table(is.na(datos$index))
datos$index = datos$index / max(datos$index)

datos$vm2_act = datos$vm2 / datos$index

mapview(datos, zcol = "vm2_act")
names(datos)
datos = subset(datos, vm2_act < 4900) # OUTLIERS

############################## LOCALIDADES y TIPOLOGIA #############################

datos$fid <- NULL

class(datos$localidad)
table(datos$localidad)
datos$localidad = ifelse(datos$localidad=="LAS HIGUERAS","RIO CUARTO",
                         ifelse(datos$localidad=="VILLA NUEVA", "VILLA MARIA", datos$localidad))
table(datos$localidad)

class(datos$tipologia)
table(datos$tipologia)

datos = rename(datos, m_tipologia = tipologia)
table(datos$m_tipologia)
class(datos$m_tipologia)
datos$m_tipologia <- as.factor(datos$m_tipologia)

############################## CATEG CONSTRUCCION #############################

summary(datos$categ_construc)
class(datos$categ_construc)
table(datos$categ_construc)
datos$m_categ_construc = datos$categ_construc
# datos$m_categ_construc <- case_when(as.character(datos$categ_construc)== "baja" ~ "0", #Baja
#                                     as.character(datos$categ_construc) == "estandar" ~ "1", #Estandar
#                                     as.character(datos$categ_construc) == "media alta" ~ "2", #Media alta
#                                     as.character(datos$categ_construc) == "alta" ~ "3") #Alta
datos$m_categ_construc <- as.factor(datos$m_categ_construc)

table(datos$m_categ_construc)
class(datos$m_categ_construc)

############################## CATEG ESTADO CONSERV #############################

table(datos$estado_conserv)
class(datos$estado_conserv)
datos$m_estado_conserv = datos$estado_conserv
# datos$m_estado_conserv <- case_when(  as.character(datos$estado_conserv)== "en construccion" ~ "0", #En construccion
#                                       as.character(datos$estado_conserv) == "malo" ~ "1", #Malo
#                                       as.character(datos$estado_conserv) == "regular" ~ "2", #Regular
#                                       as.character(datos$estado_conserv) == "bueno" ~ "3") #Bueno

table(datos$m_estado_conserv)
class(datos$m_estado_conserv)
datos$m_estado_conserv <- as.factor(datos$m_estado_conserv)

############################## PATIO EN VIVIENDAS #############################

datos$m_perc_patio <- ifelse(str_detect(datos$tipologia, "vivienda") == T, datos$p_sup/datos$superficie_total_terreno, 1 )
summary(datos$m_perc_patio)
hist(datos$m_perc_patio)
datos$m_patio <- case_when(datos$m_perc_patio <= 0.5 ~ "patio_grande",
                           datos$m_perc_patio > 0.5 & datos$m_perc_patio < 0.95 ~ "patio_chico",
                           datos$m_perc_patio >= 0.95 ~ "no_patio")

table(datos$m_patio)
datos$m_patio = as.factor(datos$m_patio)

############################## ANTIGUEDAD #############################

summary(datos$antiguedad_pond)
table(is.na(datos$antiguedad_pond))
datos = rename(datos, m_antig = antiguedad_pond)

############################## UBICACION #############################

names(datos)
datos$m_ubicacion_cuadra = ifelse(datos$medial==1, "medial",
                                  ifelse(datos$esquina==1, "esquina",
                                         ifelse(datos$interno== 1, "interno", "2_calles")))
table(datos$m_ubicacion_cuadra)
datos$m_ubicacion_cuadra = as.factor(as.character(datos$m_ubicacion_cuadra))
class(datos$m_ubicacion_cuadra)


save(datos, file = "datos.Rda")
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)
summary(datos)

rm(list=ls())


# # # # # # # # # # # # HOMOGENEIZACIÓN # # # # # # # # # # # # 

getwd()
load("datos.Rda")
names(datos)

cuantiles = 1 

datos$m_antig2 <- datos$m_antig^2 # Evita valores (-)
names(datos)
table(is.na(datos$p_sup))
table(datos$m_tipologia)
table(datos$m_categ_construc)
table(datos$m_estado_conserv)
table(datos$m_patio)

save(datos, file = "datos.Rda")
table(datos$localidad)

datos$vm2_act

matrix = (datos[,c("vm2_act" ,  "p_sup" , "m_ubicacion_cuadra" , "m_tipologia"  , "m_categ_construc" , "m_antig" , "m_antig2" , "localidad" , "m_estado_conserv",
                  "m_patio" , "fragment" , "inc_edif" , "d_ruta" , "d_viasprin" ,  "d_viassec" , "d_alta" , "d_baja" , "d_lineadiv" , "d_depre" , "d_rio" , "prom_lote" , "perc_edif", 
                  "perc_baldm" , "perc_bald" , "ui" , "ndbi" , "ndvi" , "ind_con" , 
                  "osm_iibb" , "vut_2022" , "dens_osm")])
matrix = dplyr::select_if(matrix, is.numeric)
matrix = st_drop_geometry(matrix)
matrix

corr_mat=cor(matrix,method="s")
corrplot(corr_mat)
cor(datos$m_antig, datos$m_antig2)
hist(datos$vm2)

# # # # # # # # Dummy COMERCIO # # # # # # # # 

table(datos$m_tipologia)

# OLS
datos <- within(datos, m_tipologia <- relevel(m_tipologia, ref = "comercio"))
datos <- within(datos, m_estado_conserv <- relevel(m_estado_conserv, ref = "bueno"))
datos <- within(datos, m_categ_construc <- relevel(m_categ_construc, ref = "estandar"))
datos <- within(datos, m_ubicacion_cuadra <- relevel(m_ubicacion_cuadra, ref = "medial"))
datos_c <- subset(datos, m_tipologia == "comercio"); nrow(datos_c)

form = log(vm2_act) ~ log(p_sup) + m_categ_construc + m_estado_conserv + m_ubicacion_cuadra + ndvi + ndbi + osm_iibb + m_antig + m_antig2 + vut_2022

ols = lm(form, datos_c); summary(ols)
plot(fitted(ols), resid(ols), xlab = 'Valores', ylab = 'Residuos'); abline(0,0)

#H0: Homoscedasticity is present (the residuals are distributed with equal variance)
#H1: Heteroscedasticity is present (the residuals are not distributed with equal variance)
library(lmtest)
bptest(ols)

# WLS
wt <- 1/lm(abs(ols$residuals) ~ ols$fitted.values)$fitted.values^2
wls <- lm(form, data = datos_c, weights = wt)

wls$coefficients
  


# Kernel
train <- trainControl(method = "cv", number = 23)
kwG <- train(form, 
             data = datos_c, 
             method = "knn", 
             trControl = train, 
             tuneLength = 10)
kwG
k_optimo <- kwG$bestTune$k; k_optimo

#matrix <- knearneigh(xy, k=k_optimo, longlat = NULL, use_kd_tree=TRUE)
#matrix



# GWR
# H0: There is no predictor variable that affects Y
# H1: At least one predictor variable that affects Y
xy <- st_as_sf(datos_c[[90]])
xy <- st_centroid(st_geometry(xy), of_largest_polygon = TRUE); xy
xy = na.omit(xy); xy
class(xy); class(datos_c)

datos_c = st_drop_geometry(datos_c)
xy <- as_Spatial(xy); class(xy); class(datos_c)

SPDF <- SpatialPointsDataFrame(coords = xy, data = datos_c)

gwrG <- gwr(form, 
            data = SPDF, 
            bandwidth = k_optimo, 
            gweight = gwr.Gauss, 
            hatmatrix = TRUE)
gwrG
matriz_coef <- gwrG$coefficients; matriz_coef 

form = log(vm2_act) ~  log(p_sup) + m_ubicacion_cuadra + m_tipologia  + m_categ_construc + m_antig + m_antig2 + localidad + m_estado_conserv +
  m_patio + d_viasprin + d_alta + d_baja + perc_edif + vut_2022
# + fragment + d_ruta + d_viasprin +  d_viassec + d_alta + d_baja + d_lineadiv + d_depre + d_rio + prom_lote + perc_edif + 
# perc_baldm + perc_bald + ui + ndbi + ndvi + ind_con + 
# vut_2022 + dens_osm

save(form, file = "form.Rda")
datos = st_difference(datos)
form <- as.formula(form)
datos$localidad = as.factor(as.character(datos$localidad))
table(datos$m_categ_construc)


# OLS
# Inmueble típico
datos <- within(datos, m_tipologia <- relevel(m_tipologia, ref = "departamento"))
datos <- within(datos, m_estado_conserv <- relevel(m_estado_conserv, ref = "bueno"))
datos <- within(datos, m_categ_construc <- relevel(m_categ_construc, ref = "estandar"))
datos <- within(datos, m_ubicacion_cuadra <- relevel(m_ubicacion_cuadra, ref = "medial"))

ols = lm(form, datos)
summary(ols)
cord <- st_coordinates(datos)
d <- dnearneigh(cord, 0, 750)
dlist <- nbdists(d, sp::coordinates(cord))
idlist <- lapply(dlist, function(x) 1/x)
lw <- nb2listw(d, glist=idlist ,style="W" , zero.policy = TRUE)
summary(lw, zero.policy = TRUE)


#  dependencia espacial y calculo de multiplicadores de Lagrange
moran <- lm.morantest(lm(form, datos),lw , zero.policy = TRUE) 
moran_lm <- lm.LMtests(ols,lw,test="all", zero.policy = T)
moran
moran_lm
#H0: Independencia espacial - se rechaza


############################## ELECCIÓN DEL MODELO #############################

library(stats)
{
SAC <- sacsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
aicsac <- AIC(SAC)
SAR <- lagsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
aicsar <- AIC(SAR)
SEM <- errorsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)
aicsem <- AIC(SEM)
}
if(aicsac < aicsar & aicsac < aicsem){
  print("Se estima mediante modelo SAC") 
regresion <- sacsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit)

} else { ifelse((aicsar < aicsem & aicsar < aicsac), "Se estima mediante modelo SAR",
             regresion <- lagsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit))
       ifelse((aicsem < aicsar & aicsem < aicsac), "Se estima mediante modelo SEM",
             regresion <- errorsarlm(ols, data = datos, listw = lw, zero.policy = T, na.action = na.omit))
       }
print(summary(regresion, Nagelkerke = T))
  

############################## SIGNIFICATIVIDAD DE LOS COEFICIENTES #############################

b_sig <- data.frame(spdep::summary.sarlm(regresion)["Coef"])
b_sig <- rownames_to_column(b_sig)
names(b_sig)[names(b_sig) == "rowname"] <- "vble"
remove_rownames(b_sig)
b_sig <- subset(b_sig, vble != "(Intercept)")


############################## IMPACTO TOTAL (Sólo SAC y SAR) #############################

if(regresion == "SAC" | regresion == "SAR"){
  impactos <- impacts(regresion, listw=lw)
  
  a <- data.frame(impactos$total)
  b_total <- cbind(b_sig,a)
  
}else{
  b_total <- cbind(b_sig, b_sig$Coef.Estimate) 
}


# SUPERFICIE

b_sup <- subset(b_total, vble == "log(p_sup)")
b_sup <- b_sup[, c("vble", "impactos.total", "Coef.Pr...z..")] #elijo variables
names(b_sup)[names(b_sup) == "impactos.total"] <- "b"   #cambio nombre
names(b_sup)[names(b_sup) == "Coef.Pr...z.."] <- "p" #cambio nombre
p_valor <- 0.15
b_sup$b <- ifelse(b_sup$p <= p_valor, b_sup$b, 0)


# Variables sin log
library(stringr)
library(dplyr)
b_sig <- b_total %>%
  filter(str_detect(substr(vble, 1,3), "m_"))


##########

names(b_sig)

b_sig <- b_sig[, c("vble", "impactos.total", "Coef.Pr...z..")]
names(b_sig)[names(b_sig) == "impactos.total"] <- "b"
names(b_sig)[names(b_sig) == "Coef.Pr...z.."] <- "p"
b_sig$b <- ifelse(b_sig$p < p_valor, b_sig$b, 0)
#b_sig <- b_sig

# b_sig$b <- ifelse(b_sig$vble == "m_ubicacion_cuadra3", 0, b_sig$b )
# b_sig$b <- ifelse(b_sig$vble == "m_ubicacion_cuadra2", 0, b_sig$b )
#####
getwd()
dir.create("Coeficientes")
save(b_sup, file = "Coeficientes/b_sup.Rda")
#save(b_anio, file = "Coeficientes/b_anio.Rda")
save(b_sig, file = "Coeficientes/b_sig.Rda")



load("datos.Rda")


# datos$m_antig <- 2022 - datos$anio_construc
# datos$m_antig2 <- (2022 - datos$anio_construc)^2

vbles <- data.frame(id = as.numeric(1:(dim(datos)[1])))
names(datos)


library(fastDummies)
b_sig
class(datos)
names(datos)

dummies_corregida <- function (.data, select_columns = NULL, remove_first_dummy = FALSE, 
                               remove_most_frequent_dummy = FALSE, ignore_na = FALSE, split = NULL, 
                               remove_selected_columns = FALSE) 
{
  stopifnot(is.null(select_columns) || is.character(select_columns), 
            select_columns != "", is.logical(remove_first_dummy), 
            length(remove_first_dummy) == 1, is.logical(remove_selected_columns))
  if (remove_first_dummy == TRUE & remove_most_frequent_dummy == 
      TRUE) {
    stop("Select either 'remove_first_dummy' or 'remove_most_frequent_dummy'\n         to proceed.")
  }
  if (is.vector(.data)) {
    .data <- data.frame(.data = .data, stringsAsFactors = FALSE)
  }
  # data_type <- recipes::check_type(.data)
  if (!data.table::is.data.table(.data)) {
    .data <- data.table::as.data.table(.data)
  }
  if (!is.null(select_columns)) {
    char_cols <- select_columns
    cols_not_in_data <- char_cols[!char_cols %in% names(.data)]
    char_cols <- char_cols[!char_cols %in% cols_not_in_data]
    if (length(char_cols) == 0) {
      stop("select_columns is/are not in data. Please check data and spelling.")
    }
  }
  else if (ncol(.data) == 1) {
    char_cols <- names(.data)
  }
  else {
    char_cols <- sapply(.data, class)
    char_cols <- char_cols[char_cols %in% c("factor", "character")]
    char_cols <- names(char_cols)
  }
  if (length(char_cols) == 0 && is.null(select_columns)) {
    stop(paste0("No character or factor columns found. ", 
                "Please use select_columns to choose columns."))
  }
  if (!is.null(select_columns) && length(cols_not_in_data) > 
      0) {
    warning(paste0("NOTE: The following select_columns input(s) ", 
                   "is not a column in data.\n"), paste0(names(cols_not_in_data), 
                                                         "\t"))
  }
  for (col_name in char_cols) {
    if (is.factor(.data[[col_name]])) {
      unique_vals <- levels(.data[[col_name]])
      if (any(is.na(.data[[col_name]]))) {
        unique_vals <- c(unique_vals, NA)
      }
    }
    else {
      unique_vals <- unique(.data[[col_name]])
      unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE, 
                                       locale = "en_US", numeric = TRUE)
    }
    unique_vals <- as.character(unique_vals)
    if (!is.null(split)) {
      unique_vals <- unique(trimws(unlist(strsplit(unique_vals, 
                                                   split = split))))
    }
    if (ignore_na) {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }
    if (remove_most_frequent_dummy) {
      vals <- as.character(.data[[col_name]])
      vals <- data.frame(sort(table(vals), decreasing = TRUE), 
                         stringsAsFactors = FALSE)
      top_vals <- vals[vals$Freq %in% max(vals$Freq), 
                       ]
      other_vals <- vals$vals[!vals$Freq %in% max(vals$Freq)]
      other_vals <- as.character(other_vals)
      top_vals <- top_vals[stringr::str_order(top_vals$vals, 
                                              na_last = TRUE, locale = "en_US", numeric = TRUE), 
                           ]
      if (nrow(top_vals) == 1) {
        top_vals <- NULL
      }
      else {
        top_vals <- as.character(top_vals$vals[2:nrow(top_vals)])
      }
      unique_vals <- c(top_vals, other_vals)
      unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE, 
                                       locale = "en_US", numeric = TRUE)
    }
    if (remove_first_dummy) {
      unique_vals <- unique_vals[-1]
    }
    data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
    .data[, paste0(col_name, "", unique_vals)] <- 0L
    for (unique_value in unique_vals) {
      data.table::set(.data, i = which(data.table::chmatch(as.character(.data[[col_name]]), 
                                                           unique_value, nomatch = 0) == 1L), j = paste0(col_name, 
                                                                                                         "", unique_value), value = 1L)
      if (!is.na(unique_value)) {
        data.table::set(.data, i = which(is.na(.data[[col_name]])), 
                        j = paste0(col_name, "", unique_value), value = NA)
      }
      if (!is.null(split)) {
        max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]), 
                                                split = split), length))
        for (split_length in 1:max_split_length) {
          data.table::set(.data, i = which(data.table::chmatch(as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]), 
                                                                                                   split = split), `[`, split_length))), unique_value, 
                                                               nomatch = 0) == 1L), j = paste0(col_name, 
                                                                                               "", unique_value), value = 1L)
        }
        if (is.na(unique_value)) {
          .data[[paste0(col_name, "", unique_value)]][which(!is.na(.data[[col_name]]))] <- 0
        }
      }
    }
  }
  if (remove_selected_columns) {
    .data <- .data[-which(names(.data) %in% char_cols)]
  }
  # .data <- fix_data_type(.data, data_type)
  return(.data)
}



datos5 <- dummies_corregida(st_drop_geometry(datos)[,c("m_ubicacion_cuadra",
                                                "m_tipologia",
                                                "m_categ_construc",
                                                "m_antig",
                                                "m_antig2",
                                                "localidad",
                                                "m_estado_conserv", "m_patio")], 
                             select_columns = c("m_ubicacion_cuadra",
                                                "m_tipologia",
                                                "m_categ_construc",
                                                "m_localidad", "m_estado_conserv", "m_patio"))

names(datos5)
datos5[,c("m_ubicacion_cuadramedial","m_tipologiadepartamento", "m_categ_construcestandar",
          "m_estado_conservbueno", "localidad",
          "m_ubicacion_cuadra", "m_tipologia", "m_categ_construc",
          "m_estado_conserv", "m_patio", "m_pationo_patio")] <- list(NULL)


names(datos5)
b_sig

cat(paste(shQuote(names(datos5), type="cmd"), collapse=" , ")) 

vbles = datos5[,c("m_antig" , "m_antig2" , "m_ubicacion_cuadra2_calles" , "m_ubicacion_cuadraesquina" , "m_ubicacion_cuadrainterno" , "m_tipologiacomercio" , "m_tipologiavivienda" , "m_categ_construcalta" , "m_categ_construcbaja" , "m_categ_construcmedia alta" , "m_estado_conserven construccion" , "m_estado_conservmalo" , "m_estado_conservregular" , "m_patiopatio_chico" , "m_patiopatio_grande")]

aux = as.data.frame(names(vbles))
aux = rename(aux, vble = "names(vbles)")
aux = left_join(aux, b_sig[, c("vble","b")])
beta = as.matrix(aux[,c("b")])


# beta <- as.matrix(b_sig[, c("b")])
# matriz_beta <- beta
# matriz_beta


largo <- as.numeric(nrow(vbles))



antig_homog <- 1
vbles$m_antig <- vbles$m_antig - antig_homog
vbles$m_antig2 <- vbles$m_antig2 - antig_homog

summary(datos$m_antig)

#hacemos el exponente datos
i = 1
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  datos$expon[i] <- a %*% beta
}

summary(datos$expon)

mediana_sup <- round(median(datos$p_sup))
save(mediana_sup, file = "Coeficientes/mediana_sup.Rda")


#calculo coeficiente datoss

options(scipen = 999)
datos$coef <- (((datos$p_sup/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(datos$expon)))

summary(datos$coef)
hist(datos$coef)

save(datos, file = "datos.Rda")



#########################################################
#########################################################
#ACTUALIZACION


rm(list =ls())
load("datos.Rda")

datos$vua <- datos$vm2_act/datos$coef 

summary(datos$vua)
datos$alquiler_total_homog = datos$vua * 60
summary(datos$alquiler_total_homog)
hist(datos$alquiler_total_homog)
hist(datos$vua)
ver = subset(datos, alquiler_total_homog > 500000)
mapview(ver)

mapview::mapview(datos, zcol = "alquiler_total_homog", at=c(0,40000,60000,80000,100000,150000,250000,Inf))

save(datos, file="datos_homogeneizados.Rda")
st_write(datos, "datos_homogeneizados.gpkg", delete_layer = T)



rm(list = ls())
options(timeout = 9999999999999999999999999)
getwd()
directorio <- "/"
setwd(directorio)
save(directorio, file = "directorio.Rda")
con <- dbConnect(Postgres(), dbname = "", host = "172.25.1.248", port = 5432, 
                 user = "balbo.stefano", password = "")
datos <- st_read (con, query = "SELECT * FROM " )
today <- format(Sys.Date(), "%Y%d%m")
datos$año = as.numeric(substr(datos$fechavalor, 1, 4))
table(datos$año)
datos = subset(datos, año>=2022)
datos = rename(datos, fecha = fechavalor)
datos$fecha
datos$mes = as.numeric(substr(datos$fecha, 6, 7))
datos$fecha = as.character(paste0(datos$mes, "/", datos$año))
datos$fecha


############################## DATA TIPO DE CAMBIO #############################
load("tc.Rda")
tc
tc$anio = as.numeric(tc$anio)
tc$mes = as.numeric(tc$mes)
tc$fecha = as.character(paste0(tc$mes, "/", tc$anio)) 
table(datos$fecha)
table(tc$fecha)
class(datos$fecha)
class(tc$fecha) # CHR
datos1 <- left_join(datos, tc[,c("fecha","tc30")], by="fecha")
names(datos1)
table(datos1$fecha)
summary(datos1$tc30)
datos <- datos1
rm(datos1)

############################## vm2 - VALORES EN PESOS #############################
names(datos)
datos$valor
datos = rename(datos, p_valor = valor)
datos$p_valor = as.numeric(datos$p_valor)
summary(datos$p_valor)
datos$valor_pesos = datos$p_valor
summary (datos$valor_pesos)
datos$p_valor = as.numeric(datos$p_valor)

table(is.na(datos$superficiepropia))
table(is.na(datos$superficie_mejoras))
datos$p_sup = ifelse(is.na(datos$superficiepropia) == F, datos$superficiepropia, datos$superficie_mejoras)
table(is.na(datos$p_sup))
summary(datos$p_sup)
hist(datos$p_sup)

datos = subset(datos, p_sup > 18)
datos$vm2 = datos$valor_pesos / datos$p_sup
summary(datos$vm2)
boxplot(datos$vm2)
datos = subset(datos, vm2>100)
summary(datos$vm2)
datos = subset(datos, vm2 < 4000)
datos = st_difference(datos)
mapview::mapview(datos, zcol = "vm2")


############################## IPC #############################
ipc = read.csv("index alquileres.csv", header = T, sep = ";")
ipc$fecha = as.character(paste0(ipc$mes, "/", ipc$año))
datos = left_join(datos, ipc)
datos$index

ver = subset(datos, is.na(index)==T)
table(ver$fecha)

datos$index = ifelse(is.na(datos$index)==T, 867.65 * 1.06, datos$index)
table(is.na(datos$index))
datos$index = datos$index / max(datos$index)

datos$vm2_act = datos$vm2 / datos$index

mapview::mapview(datos, zcol = "vm2_act")
names(datos)
summary(datos$vm2_act)
datos = subset(datos, vm2_act < 5000) # OUTLIERS
mapview::mapview(datos, zcol = "vm2_act")


############################## LOCALIDADES y TIPOLOGIA #############################

datos$fid <- NULL
names(datos)
table(datos$porcentaje_copropiedad)

class(datos$localidad)
table(datos$localidad)
datos$categoriaconstructiva


table(datos$tipo)
datos$tipologia = ifelse(datos$tipo == "DEPARTAMENTO","departamento", "vivienda")
class(datos$tipologia)
table(datos$tipologia)
datos$m_tipologia = NULL
datos = rename(datos, m_tipologia = tipologia)
table(datos$m_tipologia)
class(datos$m_tipologia)
datos$m_tipologia <- as.factor(datos$m_tipologia)


############################## CATEG CONSTRUCCION #############################

table(datos$calidadediliciacuadra)
summary(datos$puntaje_pond)

datos$m_categ_construc <- case_when(datos$puntaje_pond < 48 ~ "baja", #Baja
                                    datos$puntaje_pond >= 48 & datos$puntaje_pond < 84 ~ "estandar", #Estandar
                                    datos$puntaje_pond >= 84 & datos$puntaje_pond < 96 ~ "media alta", #Media alta
                                   datos$puntaje_pond >= 96 ~ "alta") #Alta
datos$m_categ_construc <- as.factor(datos$m_categ_construc)

table(datos$m_categ_construc)
class(datos$m_categ_construc)


############################## CATEG ESTADO CONSERV #############################

table(datos$estado_conserv)
class(datos$estado_conserv)

datos$m_estado_conserv <- "bueno"

table(datos$m_estado_conserv)
class(datos$m_estado_conserv)
datos$m_estado_conserv <- as.factor(datos$m_estado_conserv)


############################## PATIO EN VIVIENDAS #############################

datos$m_perc_patio <- ifelse(str_detect(datos$m_tipologia, "vivienda") == T, datos$p_sup/datos$superficie_total_terreno, 1 )
summary(datos$m_perc_patio)
hist(datos$m_perc_patio)
datos$m_patio <- case_when(datos$m_perc_patio <= 0.5 ~ "patio_grande",
                           datos$m_perc_patio > 0.5 & datos$m_perc_patio < 0.95 ~ "patio_chico",
                           datos$m_perc_patio >= 0.95 ~ "no_patio")

table(datos$m_patio)
datos$m_patio = as.factor(datos$m_patio)
table(datos$m_patio, datos$m_tipologia)
summary(datos$d_viasprin)
# ver = subset(datos, datos$m_tipologia == "vivienda" & datos$m_patio == "no_patio" & datos$d_viasprin < 50)
# mapview::mapview(ver, zcol = "vm2")
datos$m_tipologia = as.character(datos$m_tipologia)
datos$m_tipologia = ifelse(datos$m_tipologia == "vivienda" & datos$m_patio == "no_patio" & datos$d_viasprin < 50, "comercio", datos$m_tipologia)
datos$m_tipologia = as.factor(datos$m_tipologia)
table(datos$m_tipologia)

############################## ANTIGUEDAD #############################

summary(datos$antiguedad_pond)
table(is.na(datos$antiguedad_pond))
datos = rename(datos, m_antig = antiguedad_pond)


############################## UBICACION #############################

names(datos)
datos$m_ubicacion_cuadra = ifelse(datos$medial==1, "medial",
                                  ifelse(datos$esquina==1, "esquina",
                                         ifelse(datos$interno==1, "interno", "2_calles")))
table(datos$m_ubicacion_cuadra)
datos$m_ubicacion_cuadra = as.factor(as.character(datos$m_ubicacion_cuadra))
class(datos$m_ubicacion_cuadra)


save(datos, file = "datos_sellos.Rda")
st_write(datos, "datos_sellos.gpkg", delete_dsn = T, delete_layer = T)


summary(datos)

vbles <- data.frame(id = as.numeric(1:(dim(datos)[1])))
names(datos)

dummies_corregida <- function (.data, select_columns = NULL, remove_first_dummy = FALSE, 
                               remove_most_frequent_dummy = FALSE, ignore_na = FALSE, split = NULL, 
                               remove_selected_columns = FALSE) 
{
  stopifnot(is.null(select_columns) || is.character(select_columns), 
            select_columns != "", is.logical(remove_first_dummy), 
            length(remove_first_dummy) == 1, is.logical(remove_selected_columns))
  if (remove_first_dummy == TRUE & remove_most_frequent_dummy == 
      TRUE) {
    stop("Select either 'remove_first_dummy' or 'remove_most_frequent_dummy'\n         to proceed.")
  }
  if (is.vector(.data)) {
    .data <- data.frame(.data = .data, stringsAsFactors = FALSE)
  }
  # data_type <- recipes::check_type(.data)
  if (!data.table::is.data.table(.data)) {
    .data <- data.table::as.data.table(.data)
  }
  if (!is.null(select_columns)) {
    char_cols <- select_columns
    cols_not_in_data <- char_cols[!char_cols %in% names(.data)]
    char_cols <- char_cols[!char_cols %in% cols_not_in_data]
    if (length(char_cols) == 0) {
      stop("select_columns is/are not in data. Please check data and spelling.")
    }
  }
  else if (ncol(.data) == 1) {
    char_cols <- names(.data)
  }
  else {
    char_cols <- sapply(.data, class)
    char_cols <- char_cols[char_cols %in% c("factor", "character")]
    char_cols <- names(char_cols)
  }
  if (length(char_cols) == 0 && is.null(select_columns)) {
    stop(paste0("No character or factor columns found. ", 
                "Please use select_columns to choose columns."))
  }
  if (!is.null(select_columns) && length(cols_not_in_data) > 
      0) {
    warning(paste0("NOTE: The following select_columns input(s) ", 
                   "is not a column in data.\n"), paste0(names(cols_not_in_data), 
                                                         "\t"))
  }
  for (col_name in char_cols) {
    if (is.factor(.data[[col_name]])) {
      unique_vals <- levels(.data[[col_name]])
      if (any(is.na(.data[[col_name]]))) {
        unique_vals <- c(unique_vals, NA)
      }
    }
    else {
      unique_vals <- unique(.data[[col_name]])
      unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE, 
                                       locale = "en_US", numeric = TRUE)
    }
    unique_vals <- as.character(unique_vals)
    if (!is.null(split)) {
      unique_vals <- unique(trimws(unlist(strsplit(unique_vals, 
                                                   split = split))))
    }
    if (ignore_na) {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }
    if (remove_most_frequent_dummy) {
      vals <- as.character(.data[[col_name]])
      vals <- data.frame(sort(table(vals), decreasing = TRUE), 
                         stringsAsFactors = FALSE)
      top_vals <- vals[vals$Freq %in% max(vals$Freq), 
                       ]
      other_vals <- vals$vals[!vals$Freq %in% max(vals$Freq)]
      other_vals <- as.character(other_vals)
      top_vals <- top_vals[stringr::str_order(top_vals$vals, 
                                              na_last = TRUE, locale = "en_US", numeric = TRUE), 
                           ]
      if (nrow(top_vals) == 1) {
        top_vals <- NULL
      }
      else {
        top_vals <- as.character(top_vals$vals[2:nrow(top_vals)])
      }
      unique_vals <- c(top_vals, other_vals)
      unique_vals <- stringr::str_sort(unique_vals, na_last = TRUE, 
                                       locale = "en_US", numeric = TRUE)
    }
    if (remove_first_dummy) {
      unique_vals <- unique_vals[-1]
    }
    data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
    .data[, paste0(col_name, "", unique_vals)] <- 0L
    for (unique_value in unique_vals) {
      data.table::set(.data, i = which(data.table::chmatch(as.character(.data[[col_name]]), 
                                                           unique_value, nomatch = 0) == 1L), j = paste0(col_name, 
                                                                                                         "", unique_value), value = 1L)
      if (!is.na(unique_value)) {
        data.table::set(.data, i = which(is.na(.data[[col_name]])), 
                        j = paste0(col_name, "", unique_value), value = NA)
      }
      if (!is.null(split)) {
        max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]), 
                                                split = split), length))
        for (split_length in 1:max_split_length) {
          data.table::set(.data, i = which(data.table::chmatch(as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]), 
                                                                                                   split = split), `[`, split_length))), unique_value, 
                                                               nomatch = 0) == 1L), j = paste0(col_name, 
                                                                                               "", unique_value), value = 1L)
        }
        if (is.na(unique_value)) {
          .data[[paste0(col_name, "", unique_value)]][which(!is.na(.data[[col_name]]))] <- 0
        }
      }
    }
  }
  if (remove_selected_columns) {
    .data <- .data[-which(names(.data) %in% char_cols)]
  }
  # .data <- fix_data_type(.data, data_type)
  return(.data)
}


load("Coeficientes/b_sig.Rda")
b_sig
# beta <- as.matrix(b_sig[, c("b")])
class(datos)
names(datos)
datos$m_antig2 = datos$m_antig * datos$m_antig

datos5 <- dummies_corregida(st_drop_geometry(datos)[,c("m_ubicacion_cuadra",
                                                       "m_tipologia",
                                                       "m_categ_construc",
                                                       "m_antig",
                                                       "m_antig2",
                                                       "localidad",
                                                       "m_estado_conserv", "m_patio")], 
                            select_columns = c("m_ubicacion_cuadra",
                                               "m_tipologia",
                                               "m_categ_construc",
                                               "m_localidad", "m_estado_conserv", "m_patio"))

names(datos5)
datos5[,c("m_ubicacion_cuadramedial","m_tipologiadepartamento", "m_categ_construcestandar",
          "m_estado_conservbueno", "localidad",
          "m_ubicacion_cuadra", "m_tipologia", "m_categ_construc",
          "m_estado_conserv", "m_patio", "m_pationo_patio")] <- list(NULL)


names(datos5)
b_sig

cat(paste(shQuote(names(datos5), type="cmd"), collapse=" , ")) 

vbles = datos5[,c("m_antig" , "m_antig2" , "m_ubicacion_cuadra2_calles" , "m_ubicacion_cuadraesquina" , "m_ubicacion_cuadrainterno" , "m_tipologiacomercio" , "m_tipologiavivienda" , "m_categ_construcalta" , "m_categ_construcbaja" , "m_categ_construcmedia alta" , "m_patiopatio_chico" , "m_patiopatio_grande")]

aux = as.data.frame(names(vbles))
aux = rename(aux, vble = "names(vbles)")
aux = left_join(aux, b_sig[, c("vble","b")])
beta = as.matrix(aux[,c("b")])

largo <- as.numeric(nrow(vbles))



antig_homog <- 1
vbles$m_antig <- vbles$m_antig - antig_homog
vbles$m_antig2 <- vbles$m_antig2 - antig_homog



#hacemos el exponente datos
i = 1
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  datos$expon[i] <- a %*% beta
}

summary(datos$expon)

# mediana_sup <- round(median(datos$p_sup))
load("Coeficientes/mediana_sup.Rda")
load("Coeficientes/b_sup.Rda")

#calculo coeficiente datoss

datos$coef <- ((datos$p_sup/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(datos$expon))


summary(datos$coef)
hist(datos$coef)

save(datos, file = "datos_sellos.Rda")

datos$vua <- datos$vm2_act/datos$coef  ##es el valor unitario de alquiler

summary(datos$vua)
datos$alquiler_total_homog = datos$vua * 60
summary(datos$alquiler_total_homog)
hist(datos$alquiler_total_homog)
hist(datos$vua)
# ver = subset(datos, alquiler_total_homog > 500000)
# mapview::mapview(ver)

mapview::mapview(datos, zcol = "alquiler_total_homog", at=c(0,40000,60000,80000,100000,150000,250000,Inf))
summary(datos$vua)
mapview::mapview(datos, zcol = "vua", at=c(0,500,1000,1500,2000,2500,5000,Inf))


save(datos, file="datos_sellos_homogeneizados.Rda")
st_write(datos, "datos_sellos_homogeneizados.gpkg", delete_layer = T)


