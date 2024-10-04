rm(list=ls())

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
datos = st_read (con, query = "" )
pred = st_read (con, query = "")
today = format(Sys.Date(), "%Y_%d_%m")


#####################################################
nombre1 = paste0(directorio, "/parcelario_",zone_number,"_", today,".gpkg" )
nombre1
save(nombre1, file="nombre1.Rda")
st_write(pred, nombre1,  delete_layer = T, delete_dsn = T) 
nombre2 = paste0( "datos_",zone_number,"_", today,".gpkg" )
nombre2
save(nombre2, file="nombre2.Rda")
datos$fid=NULL
st_write(datos, nombre2,  delete_layer = T, delete_dsn = T) 
#####################################################
#####################################################

library(dplyr)
class(datos$localidad)
datos$localidad = as.character(datos$localidad); class(datos$localidad)
ver = subset(datos,localidad == "OTROS")
table(datos$localidad)

library(tmap)
tmap_mode("view")
mapa =  tm_basemap(c(names = "CartoDB.PositronOnlyLabels", Politico = "OpenStreetMap.DE")) +
   tm_shape(datos) +
   tm_dots()
 mapa

#####################################################
datos$localidad_def = datos$localidad
summary(datos)
names(datos)
datos$vut
datos = st_difference(datos)

library(tidyverse)
coords = do.call(rbind, st_geometry(datos)) %>% 
  as_tibble() %>% setNames(c("x","y"))
datos$x = coords$x
datos$y = coords$y

# Dummies para factor FRAGMENT #
table(datos$fragment)
datos$fragment0 = ifelse(datos$fragment == 0, 1, 0)
datos$fragment1 = ifelse(datos$fragment == 1, 1, 0)
datos$fragment2 = ifelse(datos$fragment == 2, 1, 0)
datos$fragment3 = ifelse(datos$fragment == 3, 1, 0)
#datos$fragment4 = ifelse(datos$fragment ==4, 1,0)
#datos$fragment = NULL

names(datos)
getwd()

####################### Panel de Control #######################
limite_mape = 0.15
save(limite_mape, file="limite_mape.Rda")
min_obs_zonas = 8
save(min_obs_zonas, file = "min_obs_zonas.Rda")
obs_original = dim(datos)[1]
save(obs_original, file = "obs_original.Rda")
porc_eliminar = 0.3
save(porc_eliminar, file="porc_eliminar.Rda")

###################### Chequeo min zonas ######################
load("min_obs_zonas.Rda")
table(pred$localidad)
table(datos$localidad)
datos$localidad = as.factor(datos$localidad); class(datos$localidad)

zonas = datos %>% rowwise %>%
  group_by(localidad) %>%
  summarise(n = n())
chequeo = min(zonas$n)

if(chequeo < min_obs_zonas){
  print("Advertencia! La cantidad de datos en alguna/s zona/s de procesamiento será menor a la cantidad requerida. Se procederá a ajustar la zonificación")
  
  zonas = subset(zonas, zonas$n < min_obs_zonas)
  zonas$zona = droplevels(zonas$localidad)
  datos$localidad = as.character(datos$localidad)
  
  if (length(zonas$localidad) > 0)  {
    for ( i in 1:length(zonas$localidad)) {
      datos$localidad = ifelse(datos$localidad==as.character(zonas$localidad[i]), "OTROS", datos$localidad)
    }}
  
  library(expss)
  
  if (is.na(vlookup("OTROS", datos$localidad, 1)) == F) {
    
    if ((vlookup("OTROS", as.data.frame(table(datos$localidad)), 2)) < min_obs_zonas) {
      
      freq = as.data.frame(table(datos$localidad))
      freq = subset(freq, Var1!= "OTROS")
      
      min_loc = subset(freq, Freq==min(freq[,2]))
      
      datos$localidad = ifelse(datos$localidad==as.character(min_loc[1,1]), "OTROS", datos$localidad)
      
      
    }}
}

table(datos$localidad); table(datos$localidad_def)

datos$id_aux = as.numeric(1:dim(datos)[1])

list_round_0 = c("prom_lote")

for(i in list_round_0){
  datos[[paste0(i)]] = round(datos[[paste0(i)]], 0)
}

list_round_2 = c("perc_edif", "perc_baldm",
                  "perc_bald", "porc_uec", "porc_ued", "porc_re", 
                  "porc_eau", "porc_bu", "porc_ear", "porc_agua",
                  "bci", "rndsi", "ui", "ndbi", "ndvi", "ind_con",
                  "osm_iibb")

for(i in list_round_2){
  datos[[paste0(i)]] = round(datos[[paste0(i)]], 2)
}

summary(datos)
contains_any_na = sapply(datos, function(x) any(is.na(x)))
names(datos)[contains_any_na]


# Chequear el nombre de las variables #
datos$vut_original = datos$vut 
datos$vut = round(datos$vut_original, -2)
summary(datos$vut)

getwd()
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)
st_write(datos, "datos_sin_depurar.gpkg", delete_dsn = T, delete_layer = T)
save(datos, file = "datos.Rda")

names(datos)
summary(datos$vut/datos$vut_2022) - 1

rm(list=ls())
# load("datos.Rda")
datos = st_read("datos.gpkg")
datos$aumento = (datos$vut / datos$vut_2022) - 1
summary(datos$aumento)
save(datos, file ="datos.Rda")


###############################################################################################################################################
################### ENTRENAR MODELOS ##########################################################################################################
dir.create("Fold")
dir.create("Importancia de variables")

library(caret)
library(sf)

form = vut ~  localidad + d_ruta + d_viasprin + d_viassec + 
  d_alta + d_baja + d_lineadiv + d_depre + d_rio + prom_lote +
  perc_edif + perc_baldm + perc_bald + porc_uec + porc_ued +
  porc_re + porc_eau + porc_bu + porc_ear + porc_agua + bci + 
  rndsi + ui + ndbi + ndvi + ind_con + osm_iibb + vut_2019 +  vut_2018 + vut_2020  + vut_2021 + vut_2022 +
  fragment0 + fragment1 + fragment2 + fragment3 + dens_osm + oferta_inm 

save(form, file = "form.Rda")
rm(list=ls())

repeat {
  library(doParallel)
  cores=detectCores()
  cl = makeCluster(cores[1]) 
  registerDoParallel(cl)
  system.time( 
    prediccion =foreach(i = 1:10 , .combine = cbind , #c(4,10)
                         .packages = c("caret","rpart","MASS","spdep","gstat","sf","sp","dplyr")) %dopar% {
                           #===========    Preparación de la base    =====================================================
                           load("directorio.Rda")
                           setwd(directorio)
                           
                           load("datos.Rda")
                           cutoff=2500
                           seed = 11
                           set.seed(seed)
                           datos$id = sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
                           datos$localidad = as.factor(datos$localidad)
                           list = 1:10
                           # i= 2
                           set.seed(seed)
                           training = subset(datos, id %in% list[-i])
                           set.seed(seed)
                           testing = subset(datos, id %in% c(i))
                           # training[] = lapply(training, function(x) if(is.factor(x)) factor(x) else x)
                           # testing[] = lapply(testing, function(x) if(is.factor(x)) factor(x) else x)
                           fitControl = trainControl(method = "cv", number = 5)
                           
                           # form = vut ~  localidad + d_ruta + d_viasprin + d_viassec + 
                           #   d_alta + d_baja + d_lineadiv + d_depre + d_rio + prom_lote +
                           #   perc_edif + perc_baldm + perc_bald + porc_uec + porc_ued +
                           #   porc_re + porc_eau + porc_bu + porc_ear + porc_agua + bci + 
                           #   rndsi + ui + ndbi + ndvi + ind_con + osm_iibb + vut_2019 +  vut_2018 + vut_2020  +
                           #   fragment0 + fragment1 + fragment2 + fragment3 + dens_osm
                           # #grupo
                           # #    + prom_edif  + fragment4 +
                           # 
                           # 
                           # save(form, file = "form.Rda")
                           load("form.Rda")
                           
                           #===========      Gradient Boosting Machine    =================================================================================
                           set.seed(seed)
                           train = train(form, data = training,
                                          method = "gbm",
                                          trControl = fitControl)
                           testing$gbm = predict(train, newdata=testing)
                           
                           # KKNN en los residuos
                           training$error = training$vut - predict(train, newdata=training)
                           train = train(error ~ x + y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$gbm_kknn =testing$gbm + predict(train, newdata=testing)
                           #testing$gbm_kknn =testing$gbm
                           
                           # try({
                           #   inside_gbm = predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm = training$vut - inside_gbm
                           #   vario_gbm   = try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko = try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko = try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$gbm_kknn = NA} else {
                           #     testing$gbm_kknn = testing$gbm + ko$var1.pred}
                           # })
                           
                           
                           
                           #===========    Quantile Random Forest    =================================================================================
                           set.seed(seed)
                           train = train(form, data = training,
                                          method = "qrf",
                                          trControl = fitControl)
                           testing$qrf = predict(train, newdata=testing)
                           
                           
                           # KKNN en los residuos
                           training$error = training$vut - predict(train, newdata=training)
                           train = train(error ~ x + y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$qrf_kknn =testing$qrf + predict(train, newdata=testing)
                           #testing$qrf_kknn =testing$qrf 
                           
                           # try({
                           #   inside_gbm = predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm = training$vut - inside_gbm
                           #   vario_gbm   = try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko = try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko = try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$qrf_kknn = NA} else {
                           #     testing$qrf_kknn = testing$qrf + ko$var1.pred}
                           # })
                           
                           #===========    Suppert Vector Regression     =================================================================================
                           set.seed(seed)
                           # train = train(form, data = training,
                           #                      method = "svmRadial",
                           #                      trControl = fitControl)
                           # testing$svr = predict(train, newdata=testing)
                           library(rminer)
                           train = fit(form, data=training, model="svm")
                           testing$svr = predict(train, testing)
                           
                           ## KKNN en los residuos
                           training$error = training$vut - predict(train, newdata=training)
                           train = train(error ~ x + y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$svr_kknn =testing$svr + predict(train, newdata=testing)
                           #testing$svr_kknn =testing$svr 
                           
                           # try({
                           #   inside_gbm = predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm = training$vut - inside_gbm
                           #   vario_gbm   = try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko = try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko = try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$svr_kknn = NA} else {
                           #     testing$svr_kknn = testing$svr + ko$var1.pred}
                           # })
                           
                           #===========    Stacking    =================================================================================
                           library(caretEnsemble)
                           set.seed(seed)
                           my_control = trainControl(method="cv",number=5, savePredictions="final")#, 
                           #index=createResample(training$vut))
                           set.seed(seed)
                           model_list = caretList(form, data=training, trControl=my_control , 
                                                   methodList=c("gbm", "qrf","svmRadial") )
                           set.seed(seed)
                           glm_ensemble = caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
                           testing$stk = predict(glm_ensemble, newdata=testing)
                           
                           ## KKNN en los residuos
                           training$error = training$vut - predict(glm_ensemble, newdata=training)
                           train = train(error ~ x+y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$stk_kknn =testing$stk + predict(train, newdata=testing)
                           #testing$stk_kknn =testing$stk 
                           
                           
                           # try({
                           #   inside_gbm = predict(glm_ensemble, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm = training$vut - inside_gbm
                           #   vario_gbm   = try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko = try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko = try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$stk_kknn = NA} else {
                           #     testing$stk_kknn = testing$stk + ko$var1.pred}
                           # })
                           
                           #=========================================================================================================================
                           result = testing[,c("id_aux","vut","localidad","gbm","gbm_kknn",
                                                "qrf","qrf_kknn","svr","svr_kknn","stk","stk_kknn")]
                           save(result,  file = paste("Fold/fold" , i , ".Rda" , sep = "" ), replace=T)
                           print(paste0("Vamos bien! terminando la iteración ",i," de ",max(list),". Tranqui, prepara el mate."))
                         }
  )
  
  stopCluster(cl)
  
  rm(list=ls())
  load("directorio.Rda")
  setwd(directorio)
  
  load("Fold/fold1.Rda" )
  resultados=result
  
  
  for (i in 2:10) {
    load(file = paste("Fold/fold", 
                      i, ".Rda", sep = ""))
    resultados=rbind(assign(paste("result", i, sep = ""),result) , resultados)
    rm(list=(paste("result",i,sep = "")))
    
  }
  
  save(resultados, file="Fold/resultados_folds.Rda")
  #resultados = st_drop_geometry(resultados)
  summary(resultados)
  library(dplyr)
  tabla = resultados %>%
    group_by(localidad) %>% 
    summarise(gbm = mean(abs(gbm-vut)/vut),
              gbm_kknn = mean(abs(gbm_kknn-vut)/vut),
              qrf = mean(abs(qrf-vut)/vut),
              qrf_kknn = mean(abs(qrf_kknn-vut)/vut),
              stk = mean(abs(stk-vut)/vut),
              stk_kknn = mean(abs(stk_kknn-vut)/vut),
              svr = mean(abs(svr-vut)/vut),
              svr_kknn = mean(abs(svr_kknn-vut)/vut),
              n = n()) %>% 
    rowwise() %>% mutate(min = min(gbm, gbm_kknn, qrf, qrf_kknn, stk, stk_kknn, svr, svr_kknn, na.rm = TRUE)) %>% 
    st_drop_geometry()
  
  MAPE = sum((tabla$n/sum(tabla$n)) * tabla$min)
  
  modelos = as.data.frame(colnames(tabla[,2:10])[apply(tabla[,2:10],1,which.min)])
  colnames(modelos) = "modelo"
  modelos$localidad = tabla$localidad
  modelos = modelos[,c("localidad","modelo")]
  modelos$localidad = as.character(modelos$localidad)
  modelos$modelo = as.character(modelos$modelo)
  modelos$mape = as.character(round(tabla$min,3))
  modelos[nrow(modelos) + 1, ] = c("MAPE TOTAL", "varios",as.character(round(MAPE, 3)))
  
  save(modelos, file = "modelos.Rda")
  
  load("limite_mape.Rda")
  
  if(MAPE< limite_mape){ 
    message(paste("El proceso de modelado ha finalizado. El MAPE es igual a",
                  round(MAPE, digits = 3),"-()-",
                  sep=" "))
    message("Los modelos seleccionados son los siguientes:")
    print(knitr::kable(modelos, "rst"))
    break
  }
  
  eliminar = resultados %>%
    rowwise %>%  mutate(gbm = mean(abs(gbm-vut)/vut),
                        gbm_kknn = mean(abs(gbm_kknn-vut)/vut),
                        qrf = mean(abs(qrf-vut)/vut),
                        qrf_kknn = mean(abs(qrf_kknn-vut)/vut),
                        stk = mean(abs(stk-vut)/vut),
                        stk_kknn = mean(abs(stk_kknn-vut)/vut),
                        svr = mean(abs(svr-vut)/vut),
                        svr_kknn = mean(abs(svr_kknn-vut)/vut),
                        min = min (gbm, gbm_kknn, qrf, qrf_kknn, stk, stk_kknn, svr, svr_kknn, na.rm = TRUE))
  
  # eliminar$criterio = ifelse(eliminar$localidad=="VILLA CARLOS PAZ" &
  #                                eliminar$qrf > 0.7, "eliminar", "no eliminar")
  
  eliminar$criterio = ifelse(eliminar$min > 0.3 , "eliminar", "no eliminar")
  a = subset(eliminar, eliminar$criterio=="eliminar")
  b = subset(eliminar, eliminar$criterio=="no eliminar")
  
  load("obs_original.Rda")
  load("porc_eliminar.Rda")
  
  if(dim(b)[1] < (1-porc_eliminar) * obs_original){
    message(paste("Se detuvo el proceso porque se eliminarian mas del", 
                  porc_eliminar*100,
                  "% de los datos. El modelados se realizo con", dim(resultados)[1],
                  "datos. El MAPE queda igual a",
                  round(MAPE, digits = 3),"-()-",
                  sep=" "))
    message("Los modelos seleccionados son los siguientes:")
    print(knitr::kable(modelos, "rst"))
    break
  }
  
  
  print(paste("El MAPE por el momento es",format(round(MAPE, 3), nsmall=3),", y se procederá a eliminar", length(a$vut),
              "datos. El modelado continua con", length(b$vut), sep=" "))
  
  zonas = b %>% rowwise %>%
    group_by(localidad) %>%
    summarise(n = n())
  chequeo = min(zonas$n)
  
  load("min_obs_zonas.Rda")
  
  
  if(chequeo < min_obs_zonas){
    print("Advertencia! La cantidad de datos en alguna/s zona/s de procesamiento será menor a la cantidad requerida. Se procederá a ajustar la zonificación")
  }
  
  load(file="datos.Rda")
  
  eliminar = st_as_sf(eliminar)
  datos = st_join(datos, eliminar[,c("criterio")], join = st_intersects)
  datos = subset(datos, datos$criterio == "no eliminar")
  zonas = subset(zonas, zonas$n < min_obs_zonas)
  zonas$zona = droplevels(zonas$localidad)
  zonas$zona
  datos$localidad = as.character(datos$localidad)
  
  if (length(zonas$localidad) > 0)  {
    for ( i in 1:length(zonas$localidad)) {
      datos$localidad = ifelse(datos$localidad==as.character(zonas$localidad[i]), "OTROS", datos$localidad)
    }}
  
  library(expss)
  
  if (is.na(vlookup("OTROS", datos$localidad, 1)) == F) {
    
    if ((vlookup("OTROS", as.data.frame(table(datos$localidad)), 2)) < min_obs_zonas) {
      
      freq = as.data.frame(table(datos$localidad))
      freq = subset(freq, Var1!= "OTROS")
      
      min_loc = subset(freq, Freq==min(freq[,2]))
      
      datos$localidad = ifelse(datos$localidad==as.character(min_loc[1,1]), "OTROS", datos$localidad)
      
      
    }}
  
  
  datos$localidad = as.factor(datos$localidad)
  datos$criterio = NULL
  
  save(datos, file = "datos.Rda")
  
}

 
 ###############################################################################################################################################################
##======= PREDICCIÓN ======================================================================================================================================
 
rm(list=ls())

library(sf)
library(dplyr)
library(expss)
library(caret)
library(doParallel)
library(gstat)
library(tibble)
library(dplyr)


getwd()
load("directorio.Rda")
setwd(directorio)

load("datos.Rda")
summary(datos$aumento)
table(datos$localidad)
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer =T) 
load("modelos.Rda")
load("nombre1.Rda")

pred = st_read(nombre1)
table(pred$localidad)
table(datos$localidad)

names(pred)
names(datos)

pred$localidad_def = pred$localidad
pred$localidad = as.character(pred$localidad_def)
table(pred$localidad)

zonas = as.data.frame(datos) %>% 
  group_by(localidad) %>% 
  dplyr::summarise(n=n())

pred = left_join(pred, zonas, by="localidad")
pred$localidad = ifelse(is.na(pred$n)==TRUE, "OTROS", as.character(pred$localidad))
pred$n = NULL

pred$localidad = as.factor(pred$localidad)
datos$localidad = as.factor(datos$localidad)

table(pred$localidad)
table(datos$localidad)

table(pred$fragment)
class(pred$fragment)
pred$fragment = as.numeric(as.character(pred$fragment))
pred$fragment0 = ifelse(pred$fragment ==0, 1,0)
pred$fragment1 = ifelse(pred$fragment ==1, 1,0)
pred$fragment2 = ifelse(pred$fragment ==2, 1,0)
pred$fragment3 = ifelse(pred$fragment ==3, 1,0)
#pred$fragment4 = ifelse(pred$fragment ==4, 1,0)
#pred$fragment = NULL

library(tidyverse)
coords = do.call(rbind, st_geometry(st_centroid(pred))) %>% 
  as_tibble() %>% setNames(c("x","y"))
pred$x = coords$x
pred$y = coords$y

list_round_0 = c("prom_lote")

for(i in list_round_0){
  pred[[paste0(i)]] = round(pred[[paste0(i)]], 0)
}

list_round_2 = c("perc_edif", "perc_baldm",
                  "perc_bald", "porc_uec", "porc_ued", "porc_re", 
                  "porc_eau", "porc_bu", "porc_ear", "porc_agua",
                  "bci", "rndsi", "ui", "ndbi", "ndvi", "ind_con",
                  "osm_iibb")

for(i in list_round_2){
  pred[[paste0(i)]] = round(pred[[paste0(i)]], 2)
}
# pred = st_cast(pred, "POLYGON")  ### genera mas parcelas
# pred = st_centroid(pred)
names(pred)
names(datos)
getwd()


############################### Comienzo de la prediccion #############################################################################################

cores=detectCores()
cl = makeCluster(cores[1])
registerDoParallel(cl)
# parallel()

fitControl = trainControl(method = "cv", number = 5, allowParallel = T )
seed = 11
cutoff = 2500

# form = vut ~  localidad + d_ruta + d_viasprin + d_viassec + d_alta + d_baja + d_lineadiv +
#   d_depre + d_rio + prom_lote + perc_edif + perc_baldm + perc_bald + porc_uec +
#   porc_ued + porc_re + porc_eau + porc_bu + porc_ear + porc_agua + bci + rndsi + ui + ndbi +
#   ndvi + ind_con +   #fragment0 + fragment1 + fragment2 + fragment3 + fragment4 +
#   vut_2019 + vut_2018 + grupo

load("form.Rda")


# GBM ###########################################################################################################################################################

if((is.na(vlookup("gbm", modelos$modelo, 1))==F &
    is.na(vlookup("gbm_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("gbm", modelos$modelo, 1))==T &
    is.na(vlookup("gbm_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  library(gbm)
  set.seed(seed)
  train_gbm = train(form, data = datos,
                     method = "gbm",
                     trControl = fitControl,
                     verbose = F)
  #saveRDS(train_gbm,"train_gbm.rds")
  pred$gbm = predict(train_gbm, newdata=pred)
  
  #KKNN en los residuos   
  datos$error = datos$vut - predict(train_gbm, newdata=datos)
  train = train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$gbm_kknn =pred$gbm + predict(train, newdata=pred)
  # 
  #ko en los residuos   
  # inside = predict(train_gbm, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error = datos$vut - inside
  # vario = variogram(error~1, train_ko, cutoff = cutoff)
  # model = fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko = krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$gbm_kknn  = pred$gbm  + test_ko$var1.pred
  # 
  # 
  
  # Importancia de variables
  imp_gbm = as.data.frame(caret::varImp(train_gbm)[["importance"]])
  imp_gbm = rownames_to_column(imp_gbm)
  imp_gbm = rename(imp_gbm, variables = rowname)
  imp_gbm = rename(imp_gbm, importancia = Overall)
  imp_gbm = subset(imp_gbm, substr(imp_gbm$variables, 1,9)!="localidad")
  imp_gbm = subset(imp_gbm, substr(imp_gbm$variables, 1,3)!="vut")
  
  save(imp_gbm, file="Importancia de variables/imp_gbm.Rda")
  
  imp_gbm2 = imp_gbm[order(-imp_gbm$importancia),]
  imp_gbm2 = rownames_to_column(imp_gbm2)
  imp_gbm2 = imp_gbm2[1:15,]
  
  grafico_gbm =
    ggplot(data = imp_gbm2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo gbm",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  ggsave(grafico_gbm, file="Importancia de variables/grafico_gbm.png", width=4, height=4)
  
  
  if (is.na(vlookup("gbm", modelos$modelo, 1))==T){
    pred$gbm = NULL 
  }    
  
} else {
  if (is.na(vlookup("gbm", modelos$modelo, 1))==F){
    library(gbm)
    set.seed(seed)
    train_gbm = train(form, data = datos,
                       method = "gbm",
                       trControl = fitControl,
                       verbose=F)
    
    #saveRDS(train_gbm,"train_gbm.rds")
    pred$gbm = predict(train_gbm, newdata=pred)
    
    # Importancia de variables
    imp_gbm = as.data.frame(caret::varImp(train_gbm)[["importance"]])
    imp_gbm = rownames_to_column(imp_gbm)
    imp_gbm = rename(imp_gbm, variables = rowname)
    imp_gbm = rename(imp_gbm, importancia = Overall)
    imp_gbm = subset(imp_gbm, substr(imp_gbm$variables, 1,9)!="localidad")
    imp_gbm = subset(imp_gbm, substr(imp_gbm$variables, 1,3)!="vut")
    save(imp_gbm, file="Importancia de variables/imp_gbm.Rda")
    
    imp_gbm2 = imp_gbm[order(-imp_gbm$importancia),]
    imp_gbm2 = rownames_to_column(imp_gbm2)
    imp_gbm2 = imp_gbm2[1:15,]
    
    grafico_gbm =
      ggplot(data = imp_gbm2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo gbm",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_gbm, file="Importancia de variables/grafico_gbm.png", width=4, height=4)
    
    
  }}

# QRF ###########################################################################################################################################################

if((is.na(vlookup("qrf", modelos$modelo, 1))==F &
    is.na(vlookup("qrf_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("qrf", modelos$modelo, 1))==T &
    is.na(vlookup("qrf_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  set.seed(seed)
  train_qrf = train(form, data = datos,
                     method = "qrf",
                     trControl = fitControl,
                     importance = T)
  #saveRDS(train_qrf,"train_qrf.rds")
  pred$qrf = predict(train_qrf, newdata=pred)
  
  #KKNN en los residuos   
  datos$error = datos$vut - predict(train_qrf, newdata=datos)
  train = train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$qrf_kknn =pred$qrf + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside = predict(train_qrf, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error = datos$vut - inside
  # vario = variogram(error~1, train_ko, cutoff = cutoff)
  # model = fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko = krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$qrf_kknn = pred$qrf  + test_ko$var1.pred
  
  
  # Importancia de variables
  imp_qrf = as.data.frame(caret::varImp(train_qrf$finalModel))
  imp_qrf$Overall = ifelse(imp_qrf$Overall<0, 0, imp_qrf$Overall)
  imp_qrf$importancia = (imp_qrf$Overall / max(imp_qrf$Overall)) * 100
  imp_qrf = rownames_to_column(imp_qrf)
  imp_qrf = rename(imp_qrf, variables = rowname)
  imp_qrf = subset(imp_qrf, substr(imp_qrf$variables, 1,9)!="localidad")
  imp_qrf = subset(imp_qrf, substr(imp_qrf$variables, 1,3)!="vut")
  imp_qrf$Overall = NULL
  
  save(imp_qrf, file="Importancia de variables/imp_qrf.Rda")
  
  imp_qrf2 = imp_qrf[order(-imp_qrf$importancia),]
  imp_qrf2 = rownames_to_column(imp_qrf2)
  imp_qrf2 = imp_qrf2[1:15,]
  
  grafico_qrf =
    ggplot(data = imp_qrf2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo qrf",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  
  
  ggsave(grafico_qrf, file="Importancia de variables/grafico_qrf.png", width=4, height=4)
  
  
  if (is.na(vlookup("qrf", modelos$modelo, 1))==T){
    pred$qrf = NULL 
  }    
  
} else {
  if (is.na(vlookup("qrf", modelos$modelo, 1))==F){
    set.seed(seed)
    train_qrf = train(form, data = datos,
                       method = "qrf",
                       trControl = fitControl)
    #saveRDS(train_qrf,"train_gbm.rds")
    pred$qrf = predict(train_qrf, newdata=pred)
    
    # Importancia de variables
    imp_qrf = as.data.frame(caret::varImp(train_qrf$finalModel))
    imp_qrf$Overall = ifelse(imp_qrf$Overall<0, 0, imp_qrf$Overall)
    imp_qrf$importancia = (imp_qrf$Overall / max(imp_qrf$Overall)) * 100
    imp_qrf = rownames_to_column(imp_qrf)
    imp_qrf = rename(imp_qrf, variables = rowname)
    imp_qrf = subset(imp_qrf, substr(imp_qrf$variables, 1,9)!="localidad")
    imp_qrf = subset(imp_qrf, substr(imp_qrf$variables, 1,3)!="vut")
    
    imp_qrf$Overall = NULL
    
    save(imp_qrf, file="Importancia de variables/imp_qrf.Rda")
    
    imp_qrf2 = imp_qrf[order(-imp_qrf$importancia),]
    imp_qrf2 = rownames_to_column(imp_qrf2)
    imp_qrf2 = imp_qrf2[1:15,]
    
    grafico_qrf =
      ggplot(data = imp_qrf2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo qrf",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_qrf, file="Importancia de variables/grafico_qrf.png", width=4, height=4)
    
  }}

# SVR ###########################################################################################################################################################

if((is.na(vlookup("svr", modelos$modelo, 1))==F &
    is.na(vlookup("svr_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("svr", modelos$modelo, 1))==T &
    is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  library(rminer)
  library(stringr)
  form_character = as.character(form)
  list_vbles = paste0(form_character[[2]], " + ", form_character[[3]] ) 
  list_vbles = str_split(list_vbles, " [+] ")
  matriz = st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
  names(matriz)
  library(dplyr)
  matriz %>% 
    mutate_if(is.integer,as.numeric)
  set.seed(seed)
  train_svr = fit(form, data=matriz, model="svm")
  
  list_pred = str_split(form_character[[3]], " [+] ")
  matriz_pred = st_drop_geometry(pred [, c(paste(list_pred[[1]]))])
  names(matriz_pred)
  matriz_pred %>% 
    mutate_if(is.integer,as.numeric)
  
  pred$svr = predict(train_svr, matriz_pred)
  
  # #KKNN en los residuos   
  datos$error = datos$vut - predict(train_svr, newdata=matriz)
  train = train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$svr_kknn =pred$svr + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside = predict(train_svr, newdata=matriz)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error = datos$vut - inside
  # vario = variogram(error~1, train_ko, cutoff = cutoff)
  # model = fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko = krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$svr_kknn = pred$svr  + test_ko$var1.pred
  # 
  # 
  
  
  # Variables Importantes
  imp_svr1 = Importance(train_svr, matriz)
  imp_svr = as.data.frame(imp_svr1$imp)
  imp_svr$nombre = "nombre"
  
  a = ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
  
  for(i in a[[1]]){
    imp_svr[i,c("nombre")] = imp_svr1[["sresponses"]][[i]][[1]]
  }
  
  imp_svr = subset(imp_svr, nombre != "nombre")
  imp_svr$importancia = (imp_svr$`imp_svr1$imp` / max(imp_svr$`imp_svr1$imp`)) * 100
  imp_svr = rename(imp_svr, variables = nombre)
  imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
  imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
  imp_svr$`imp_svr1$imp` =NULL
  save(imp_svr, file="Importancia de variables/imp_svr.Rda")
  
  imp_svr2 = imp_svr[order(-imp_svr$importancia),]
  imp_svr2 = rownames_to_column(imp_svr2)
  imp_svr2 = imp_svr2[1:15,]
  
  grafico_svr =
    ggplot(data = imp_svr2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo svr",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  getwd()
  ggsave(grafico_svr, file="Importancia de variables/grafico_svr.png", width=4, height=4)
  
  
  
  if (is.na(vlookup("svr", modelos$modelo, 1))==T){
    pred$svr = NULL 
  }    
  
} else {
  if (is.na(vlookup("svr", modelos$modelo, 1))==F){
    
    # Entrenamiento
    library(rminer)
    library(stringr)
    form_character = as.character(form)
    list_vbles = paste0(form_character[[2]], " + ", form_character[[3]] ) 
    list_vbles = str_split(list_vbles, " [+] ")
    matriz = st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
    names(matriz)
    library(dplyr)
    matriz %>% 
      mutate_if(is.integer,as.numeric)
    set.seed(seed)
    train_svr = fit(form, data=matriz, model="svm")
    
    list_pred = str_split(form_character[[3]], " [+] ")
    matriz_pred = st_drop_geometry(pred [, c(paste(list_pred[[1]]))])
    names(matriz_pred)
    matriz_pred %>% 
      mutate_if(is.integer,as.numeric)
    
    pred$svr = predict(train_svr, matriz_pred)
    
    # Variables Importantes
    imp_svr1 = Importance(train_svr, matriz)
    imp_svr = as.data.frame(imp_svr1$imp)
    imp_svr$nombre = "nombre"
    
    a = ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
    
    for(i in a[[1]]){
      imp_svr[i,c("nombre")] = imp_svr1[["sresponses"]][[i]][[1]]
    }
    
    imp_svr = subset(imp_svr, nombre != "nombre")
    imp_svr$importancia = (imp_svr$`imp_svr1$imp` / max(imp_svr$`imp_svr1$imp`)) * 100
    imp_svr = rename(imp_svr, variables = nombre)
    imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
    imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
    imp_svr$`imp_svr1$imp` =NULL
    save(imp_svr, file="Importancia de variables/imp_svr.Rda")
    
    imp_svr2 = imp_svr[order(-imp_svr$importancia),]
    imp_svr2 = rownames_to_column(imp_svr2)
    imp_svr2 = imp_svr2[1:15,]
    
    grafico_svr =
      ggplot(data = imp_svr2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo svr",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    getwd()
    ggsave(grafico_svr, file="Importancia de variables/grafico_svr.png", width=4, height=4)
    
    
  }}


# STK ###########################################################################################################################################################

if((is.na(vlookup("stk", modelos$modelo, 1))==F &
    is.na(vlookup("stk_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("stk", modelos$modelo, 1))==T &
    is.na(vlookup("stk_kknn", modelos$modelo, 1))==F)
){
  
  library(caretEnsemble)
  set.seed(seed)
  my_control = trainControl(method="cv",number=5, savePredictions="final")
  set.seed(seed)
  model_list = caretList(form, data=datos, trControl=my_control,
                          methodList=c("gbm", "qrf","svmRadial"))
  set.seed(seed)
  glm_ensemble = caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
  
  #saveRDS(glm_ensemble,"train_ens.rds")
  pred$stk = predict(glm_ensemble, newdata=pred)
  
  # #KKNN en los residuos   
  datos$error = datos$vut - predict(glm_ensemble, newdata=datos)
  train = train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$stk_kknn =pred$stk + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside = predict(glm_ensemble, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error = datos$vut - inside
  # vario = variogram(error~1, train_ko, cutoff = cutoff)
  # model = fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko = krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$stk_kknn = pred$stk  + test_ko$var1.pred
  # 
  # 
  # Importancia variables
  ponderacion = caret::varImp(glm_ensemble$ens_model$finalModel)
  suma_modelos = sum(ponderacion$Overall)
  ponderacion$pond = (ponderacion$Overall/suma_modelos)
  ponderacion = rownames_to_column(ponderacion)
  ponderacion$Overall = NULL
  ponderacion = rename(ponderacion, modelos = rowname)
  
  imp_stk_qrf = caret::varImp(glm_ensemble$models$qrf$finalModel)
  imp_stk_qrf = rownames_to_column(imp_stk_qrf)
  imp_stk_qrf = rename(imp_stk_qrf, variables = rowname)
  imp_stk_qrf = subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,9)!="localidad")
  imp_stk_qrf = subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,3)!="vut")
  # imp_stk_qrf = subset(imp_stk_qrf, substr(imp_stk_gbm$variables, 1,5)!="grupo")
  suma = sum(imp_stk_qrf$Overall)
  imp_stk_qrf$pond_qrf = (imp_stk_qrf$Overall/suma)*100  
  imp_stk_qrf$Overall = NULL
  
  library(gbm)
  imp_stk_gbm = caret::varImp(glm_ensemble$models$gbm$finalModel)
  imp_stk_gbm = rownames_to_column(imp_stk_gbm)
  imp_stk_gbm = rename(imp_stk_gbm, variables = rowname)
  imp_stk_gbm = subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,9)!="localidad")
  imp_stk_gbm = subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,3)!="vut")
  # imp_stk_gbm = subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,5)!="grupo")
  suma = sum(imp_stk_gbm$Overall)
  imp_stk_gbm$pond_gbm = (imp_stk_gbm$Overall/suma)*100  
  imp_stk_gbm$Overall = NULL
  
  if((is.na(vlookup("svr", modelos$modelo, 1))==F |
      is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)){
    
    imp_stk_svr = imp_svr
    
  }else{
    # Entrenamiento
    library(rminer)
    library(stringr)
    form_character = as.character(form)
    list_vbles = paste0(form_character[[2]], " + ", form_character[[3]] ) 
    list_vbles = str_split(list_vbles, " [+] ")
    matriz = st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
    names(matriz)
    library(dplyr)
    matriz %>% 
      mutate_if(is.integer,as.numeric)
    set.seed(seed)
    train_svr = fit(form, data=matriz, model="svm")
    
    # Variables Importantes
    imp_svr1 = Importance(train_svr, matriz)
    imp_svr = as.data.frame(imp_svr1$imp)
    imp_svr$nombre = "nombre"
    
    a = ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
    
    for(i in a[[1]]){
      imp_svr[i,c("nombre")] = imp_svr1[["sresponses"]][[i]][[1]]
    }
    
    imp_svr = subset(imp_svr, nombre != "nombre")
    imp_svr = rename(imp_svr, variables = nombre)
    imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
    imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
    # imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,5)!="grupo")
    suma = sum(imp_svr$`imp_svr1$imp`)
    imp_svr$pond_svr = (imp_svr$`imp_svr1$imp`/suma)*100
    imp_svr$`imp_svr1$imp` =NULL
    
    imp_stk_svr = imp_svr
  }
  
  # Hacer promedio ponderado de los modelos
  library(dplyr)
  imp_stk = left_join(imp_stk_gbm, imp_stk_qrf, by="variables")
  imp_stk = left_join(imp_stk, imp_stk_svr, by="variables")
  pond = as.matrix(imp_stk[,c(-1)]) %*% as.matrix(ponderacion[,c(-1)])
  imp_stk = cbind(imp_stk, pond)
  imp_stk$importancia = (imp_stk$pond / max(imp_stk$pond)) * 100
  imp_stk[,c("pond_gbm", "pond_qrf", "pond_svr", "pond")] = list(NULL)
  
  save(imp_stk, file="Importancia de variables/imp_stk.Rda")
  
  
  imp_stk2 = imp_stk[order(-imp_stk$importancia),]
  imp_stk2 = rownames_to_column(imp_stk2)
  imp_stk2 = imp_stk2[1:15,]
  
  grafico_stk =
    ggplot(data = imp_stk2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo stk",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  ggsave(grafico_stk, file="Importancia de variables/grafico_stk.png", width=4, height=4)
  
  
  if (is.na(vlookup("stk", modelos$modelo, 1))==T){
    pred$stk = NULL 
  }    
  
} else {
  if (is.na(vlookup("stk", modelos$modelo, 1))==F){
    library(caretEnsemble)
    set.seed(seed)
    my_control = trainControl(method="cv",number=5, savePredictions="final")
    set.seed(seed)
    model_list = caretList(form, data=datos, trControl=my_control,
                            methodList=c("gbm", "qrf","svmRadial"))
    set.seed(seed)
    glm_ensemble = caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
    
    #saveRDS(glm_ensemble,"train_ens.rds")
    pred$stk = predict(glm_ensemble, newdata=pred)
    
    # Importancia variables
    ponderacion = caret::varImp(glm_ensemble$ens_model$finalModel)
    suma_modelos = sum(ponderacion$Overall)
    ponderacion$pond = (ponderacion$Overall/suma_modelos)
    ponderacion = rownames_to_column(ponderacion)
    ponderacion$Overall = NULL
    ponderacion = rename(ponderacion, modelos = rowname)
    
    imp_stk_qrf = caret::varImp(glm_ensemble$models$qrf$finalModel)
    imp_stk_qrf = rownames_to_column(imp_stk_qrf)
    imp_stk_qrf = rename(imp_stk_qrf, variables = rowname)
    imp_stk_qrf = subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,9)!="localidad")
    imp_stk_qrf = subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,3)!="vut")
    suma = sum(imp_stk_qrf$Overall)
    imp_stk_qrf$pond_qrf = (imp_stk_qrf$Overall/suma)*100  
    imp_stk_qrf$Overall = NULL
    
    library(gbm)
    imp_stk_gbm = caret::varImp(glm_ensemble$models$gbm$finalModel)
    imp_stk_gbm = rownames_to_column(imp_stk_gbm)
    imp_stk_gbm = rename(imp_stk_gbm, variables = rowname)
    imp_stk_gbm = subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,9)!="localidad")
    imp_stk_gbm = subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,3)!="vut")
    suma = sum(imp_stk_gbm$Overall)
    imp_stk_gbm$pond_gbm = (imp_stk_gbm$Overall/suma)*100  
    imp_stk_gbm$Overall = NULL
    
    if((is.na(vlookup("svr", modelos$modelo, 1))==F |
        is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)){
      
      imp_stk_svr = imp_svr
      
    }else{
      # Entrenamiento
      library(rminer)
      library(stringr)
      form_character = as.character(form)
      list_vbles = paste0(form_character[[2]], " + ", form_character[[3]] ) 
      list_vbles = str_split(list_vbles, " [+] ")
      matriz = st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
      names(matriz)
      library(dplyr)
      matriz %>% 
        mutate_if(is.integer,as.numeric)
      set.seed(seed)
      train_svr = fit(form, data=matriz, model="svm")
      
      # Variables Importantes
      imp_svr1 = Importance(train_svr, matriz)
      imp_svr = as.data.frame(imp_svr1$imp)
      imp_svr$nombre = "nombre"
      
      a = ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
      
      for(i in a[[1]]){
        imp_svr[i,c("nombre")] = imp_svr1[["sresponses"]][[i]][[1]]
      }
      
      imp_svr = subset(imp_svr, nombre != "nombre")
      imp_svr = rename(imp_svr, variables = nombre)
      imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
      imp_svr = subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
      suma = sum(imp_svr$`imp_svr1$imp`)
      imp_svr$pond_svr = (imp_svr$`imp_svr1$imp`/suma)*100
      imp_svr$`imp_svr1$imp` =NULL
      
      imp_stk_svr = imp_svr
    }
    
    # Hacer promedio ponderado de los modelos 
    library(dplyr)
    imp_stk = left_join(imp_stk_gbm, imp_stk_qrf, by="variables")
    imp_stk = left_join(imp_stk, imp_stk_svr, by="variables")
    pond = as.matrix(imp_stk[,c(-1)]) %*% as.matrix(ponderacion[,c(-1)])
    imp_stk = cbind(imp_stk, pond)
    imp_stk$importancia = (imp_stk$pond / max(imp_stk$pond)) * 100
    imp_stk[,c("pond_gbm", "pond_qrf", "pond_svr", "pond")] = list(NULL)
    
    
    save(imp_stk, file="Importancia de variables/imp_stk.Rda")
    
    imp_stk2 = imp_stk[order(-imp_stk$importancia),]
    imp_stk2 = rownames_to_column(imp_stk2)
    imp_stk2 = imp_stk2[1:15,]
    
    grafico_stk =
      ggplot(data = imp_stk2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo stk",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_stk, file="Importancia de variables/grafico_stk.png", width=4, height=4)
    
    
  }}



stopCluster(cl)

save(pred, file = "prediccion_modelos.Rda")
summary(pred)



############################### Eleccion de modelo por localidad ###############################

rm(list=ls())
getwd()
load("prediccion_modelos.Rda")
st_write(pred, "prediccion_modelos.gpkg", delete_dsn = T, delete_layer = T)
load("modelos.Rda")

pred$id = 1:nrow(pred)
names(pred)

lista_modelos = as.list(levels(as.factor(subset(modelos, modelo != "varios")[["modelo"]])))
pred2 = pred[,c("id", "localidad", paste(lista_modelos))]
pred2 = st_drop_geometry(pred2)

library(reshape2)
library(dplyr)
pred2 = melt(pred2, id.vars = c("id", "localidad"))
pred2 = rename(pred2, modelo_pred = variable)
pred2 = rename(pred2, vh = value)
names(pred2)

pred2 = left_join(pred2, modelos[, c("localidad", "modelo")], by="localidad")
pred2 = subset(pred2, as.character(modelo_pred) == as.character(modelo))
pred2$modelo_pred = NULL

pred[,c(paste(lista_modelos))] = list(NULL)

pred = left_join(pred, pred2[,c("id","modelo", "vh")], by="id")

rm(pred2)


# Cuantiles y redondeo VH ###################################################################
q = quantile(x = pred$vh, probs = seq(0, 1, 1/5))
q
summary(pred$vh)

names(pred)
pred$vh_round = ifelse(pred$vh <= q[2], floor(round(pred$vh/100, 2))*100, 
                        ifelse(pred$vh <= q[3], floor(round(pred$vh/200, 2))*200,
                               ifelse(pred$vh <= q[4], floor(round(pred$vh/250, 2))*250,
                                      ifelse(pred$vh <= q[5], floor(round(pred$vh/500, 2))*500,
                                             floor(round(pred$vh/1000, 2))*1000))))

pred$vh_round = ifelse(pred$vut_2022==1, 1, pred$vh_round)
summary(pred$vh_round)
pred$aumento = (pred$vh_round / pred$vut_2022) - 1
summary(pred$aumento)

# ver aumentos
pred$aux = (pred$vh_round/pred$vut_2022)-1
summary(pred$aux)
ver = subset(pred, aux < 0.1)

# mapview::mapview(ver)
pred$vh_round = ifelse(pred$aux < 0.1, pred$vut_2022*(1 + 0.1), pred$vh_round)
pred$aux = (pred$vh_round/pred$vut_2022)-1
summary(pred$aux)

# pred$aux = (pred$vh/pred$vut_2021)-1
ver = subset(pred, aux > 3)
# mapview::mapview(ver)

pred$vh_round = ifelse(pred$aux > 3, pred$vut_2022 * (1 + 3), pred$vh_round)
pred$aux = (pred$vh_round/pred$vut_2022)-1
summary(pred$aux)

ver = subset(pred, pred$vut_2022 == 1)
# mapview::mapview(ver)
nrow(subset(pred, vut_2022 == 1))

pred$vh_round = ifelse(pred$vut_2022 == 1, 1, pred$vh_round)
pred$aux = (pred$vh_round/pred$vut_2022)-1
summary(pred$aux)

ver = subset(pred, vh_round==1)
mapview::mapview(ver)

# pred$vh = ifelse(pred$aux>2, pred$vut_2021*(1+mean(pred$aux)), pred$vh)
# pred$aux = (pred$vh/pred$vut_2021)-1
summary(pred$aux)
summary(pred$vh_round)

save(pred, file = "prediccion_parcelas.Rda")
st_write(pred, "prediccion_parcelas.gpkg", delete_dsn = T, delete_layer = TRUE)


parcelas = st_read("prediccion_parcelas.gpkg")

library(BAMMtools)

mean((parcelas$vh_round/parcelas$vut_2022)-1)
summary((parcelas$vh_round/parcelas$vut_2022)-1)
load("datos.Rda")
summary(datos$vut)
summary(pred$vh_round)

save(parcelas, file = "prediccion_parcelas.Rda")
st_write(parcelas, "prediccion_parcelas.gpkg", delete_dsn = T, delete_layer = TRUE)


####################################################################################################################################
#### RATIOS IAAO ###################################################################################################################
####################################################################################################################################

rm(list=ls())
getwd()

load("datos.Rda")
load("Fold/resultados_folds.Rda")
load("modelos.Rda")
names(datos)

lista_modelos = as.list(levels(as.factor(subset(modelos, modelo != "varios")[["modelo"]])))
pred2 = resultados[,c("id_aux","vut", "localidad", paste(lista_modelos))]
pred2 = as.data.frame(pred2)
pred2$geom = NULL

library(reshape2)
library(dplyr)
pred2 = melt (pred2, id.vars = c("id_aux","vut", "localidad"))
pred2 = rename(pred2, modelo_pred = variable)
pred2 = rename(pred2, vh = value)
names(pred2)

pred2 = left_join(pred2, modelos[, c("localidad", "modelo")], by="localidad")
pred2 = subset(pred2, as.character(modelo_pred) == as.character(modelo))
pred2$modelo_pred = NULL

class(pred2)
datos = dplyr::left_join(datos, pred2[,c("id_aux","modelo", "vh")], by="id_aux")
summary(datos$vh)
table(datos$localidad, datos$modelo)

rm(pred2)


iaao_pre = st_drop_geometry(datos) %>%
  group_by(localidad) %>% 
  summarise(
    Mediana_ratio = median(vut_2022/vut),
    Media_ratio = mean(vut_2022/vut),
    Media_pond =  sum(vut_2022)/sum(vut),
    CV = mean(abs((vut_2022/vut)- Media_ratio))/Media_ratio,
    CD = mean(abs((vut_2022/vut)- Mediana_ratio))/Mediana_ratio,
    PRD = Media_ratio/Media_pond
  )

iaao_pre_total = st_drop_geometry(datos) %>%
  summarise(
    localidad = "TOTAL",
    Mediana_ratio = median(vut_2022/vut),
    Media_ratio = mean(vut_2022/vut),
    Media_pond =  sum(vut_2022)/sum(vut),
    CV = mean(abs((vut_2022/vut)- Media_ratio))/Media_ratio,
    CD = mean(abs((vut_2022/vut)- Mediana_ratio))/Mediana_ratio,
    PRD = Media_ratio/Media_pond
  )

iaao_pre = rbind(iaao_pre, iaao_pre_total)

iaao_post = st_drop_geometry(datos) %>%
  group_by(localidad) %>% 
  summarise(
    MAPE = mean(abs(vh-vut)/vut),
    Mediana_ratio = median(vh/vut),
    Media_ratio = mean(vh/vut),
    Media_pond =  sum(vh)/sum(vut),
    CV = mean(abs((vh/vut)- Media_ratio))/Media_ratio,
    CD = mean(abs((vh/vut)- Mediana_ratio))/Mediana_ratio,
    PRD = Media_ratio/Media_pond)

iaao_post_total = st_drop_geometry(datos) %>% 
  summarise(
    localidad = "TOTAL",
    MAPE = mean(abs(vh-vut)/vut),
    Mediana_ratio = median(vh/vut),
    Media_ratio = mean(vh/vut),
    Media_pond =  sum(vh)/sum(vut),
    CV = mean(abs((vh/vut)- Media_ratio))/Media_ratio,
    CD = mean(abs((vh/vut)- Mediana_ratio))/Mediana_ratio,
    PRD = Media_ratio/Media_pond)

iaao_post = rbind(iaao_post, iaao_post_total)

save(iaao_pre, file="iaao_pre.Rda")
save(iaao_post, file= "iaao_post.Rda")


######## ENTREGA ######################################################################################################################################
######################################################################################################################################################

rm(list=ls())
getwd()
dir.create("Entrega")
parcelas_entrega = st_read("prediccion_parcelas.gpkg")

summary(parcelas_entrega$vh_round/parcelas_entrega$vut_2022 - 1)
summary(parcelas_entrega$vh_round)

table(parcelas_entrega$localidad_def)
names(parcelas_entrega)

parcelas_entrega$superficie_geom

parcelas_entrega = parcelas_entrega[,c("par_idparcela", "vh_round","vut_2022","vut_2021","localidad_def")]  

parcelas_entrega = rename(parcelas_entrega, vut = vh_round, localidad = localidad_def)
parcelas_entrega$aumento = (parcelas_entrega$vut/parcelas_entrega$vut_2022) - 1 
summary(parcelas_entrega$aumento)
summary(parcelas_entrega$vut)
hist(parcelas_entrega$aumento)

#ver = subset(parcelas_entrega, aumento >10)

# library(tmap)
# tmap_mode("view")
# mapa =  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
#   tm_shape(ver) +
#   tm_dots("vut_2020")
# mapa

st_write(parcelas_entrega, "Entrega/parcelas_prediccion.gpkg", delete_dsn = T, delete_layer = T)

datos = st_read("datos.gpkg")

datos$localidad = NULL
names(datos)
datos = rename(datos,  localidad = localidad_def )#

datos[, c("fragment0", "fragment1", "fragment2", "fragment3" )] = list(NULL) 

st_write(datos, "Entrega/datos_utilizados.gpkg", delete_dsn = T, delete_layer = T)
names(datos)

table(parcelas_entrega$localidad)


###############################################################
################## Ahora subimos a la nube #################

rm(list=ls())
require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
today = format(Sys.Date() + 2, "%Y%m%d")
load("zone_number.Rda")

nombredatos = paste0("z", zone_number, "_datos_utilizados_",today)
nombredatos
nombreparcelas= paste0("z", zone_number, "_prediccion_",today)
nombreparcelas
save(nombredatos, file ="nombredatos.Rda")
save(nombreparcelas, file ="nombreparcelas.Rda")

parcelas_entrega = st_read("Entrega/parcelas_prediccion.gpkg")
datos = st_read("Entrega/datos_utilizados.gpkg")

st_write(obj = parcelas_entrega, con, Id(schema="",
                             table = nombreparcelas ), delete_dsn = T, delete_layer = T)

st_write(obj = datos, con, Id(schema="",
                                         table = nombredatos ),delete_dsn = T, delete_layer = T)

class(parcelas_entrega$geom)
dbGetQuery(con, paste0("ALTER TABLE ",nombredatos,""))
dbGetQuery(con, paste0("ALTER TABLE ",nombreparcelas,""))
tabla = dbGetQuery(con, "")

df = parcelas_entrega[duplicated(parcelas_entrega$par_idparcela),]
df = datos[duplicated(datos$id),]
names(datos)
dbGetQuery(con, paste("ALTER TABLE ",nombredatos," ADD PRIMARY KEY (id);"))
dbGetQuery(con, paste("ALTER TABLE ",nombreparcelas," ADD PRIMARY KEY (par_idparcela);"))

load("nombredatos.Rda")
load("nombreparcelas.Rda")

dbGetQuery(con, paste0("GRANT select ON TABLE ",nombredatos," TO ,
                             , ;"))

dbGetQuery(con, paste0("GRANT select ON TABLE ",nombreparcelas," TO ,
                             , ;"))

parcelasver = st_read(con, query = paste0("SELECT * FROM ", nombreparcelas ))
class(parcelasver)
parcelasver$geom

datosver = st_read(con, query = paste0("SELECT * FROM ", nombredatos ))
class(datosver)
datosver$geom

getwd()
tabla1 = read.csv("Entrega/Aumentos_muestra.csv")
tabla2 = read.csv("Entrega/Aumentos_prediccion.csv")
tabla3 = read.csv("Entrega/MAPE_localidades.csv")

tabla1 = rename(tabla1, id = X)
tabla2 = rename(tabla2, id = X)
tabla3 = rename(tabla3, id = X)

nombre_t1 = paste0("z", zone_number, "_aumentos_muestra_",today)
nombre_t1
save(nombre_t1, file ="nombre_t1.Rda")
st_write(obj = tabla1, con, Id(schema="",
                              table = nombre_t1 ),delete_dsn = T, delete_layer = T)
csv_ver = st_read(con, query = paste0("SELECT * FROM ",nombre_t1 ))

nombre_t2 = paste0("z", zone_number, "_aumentos_prediccion_",today)
nombre_t2
save(nombre_t2, file ="nombre_t2.Rda")
st_write(obj = tabla2, con, Id(schema="",
                               table = nombre_t2 ),delete_dsn = T, delete_layer = T)
csv_ver = st_read(con, query = paste0("SELECT * FROM ",nombre_t2 ))

nombre_t3 = paste0("z", zone_number, "_mape_localidades_",today)
nombre_t3
save(nombre_t3, file ="nombre_t3.Rda")
st_write(obj = tabla3, con, Id(schema="",
                               table = nombre_t3 ),delete_dsn = T, delete_layer = T)
csv_ver = st_read(con, query = paste0("SELECT * FROM ",nombre_t3 ))


tabla = dbGetQuery(con, "SELECT * FROM 
                   WHERE table_schema=''")

df = tabla1[duplicated(tabla1$id),]
df = tabla2[duplicated(tabla2$id),]
df = tabla3[duplicated(tabla3$id),]

dbGetQuery(con, paste("ALTER TABLE ",nombre_t1," ADD PRIMARY KEY (id);"))
dbGetQuery(con, paste("ALTER TABLE ",nombre_t2," ADD PRIMARY KEY (id);"))
dbGetQuery(con, paste("ALTER TABLE ",nombre_t3," ADD PRIMARY KEY (id);"))

dbGetQuery(con, paste0("GRANT select ON TABLE ",nombre_t1," TO ,
                             , ;"))
dbGetQuery(con, paste0("GRANT select ON TABLE ",nombre_t2,"  ,
                             , ;"))
dbGetQuery(con, paste0("GRANT select ON TABLE ",nombre_t3," TO ,
                             , ;"))

##########################
cat((paste0("Compartimos los resultados del cluster correspondiente a la zona ", zone_number,".\nLa predicción está en: ",
            nombreparcelas, 
            " (key = par_idparcela).\nLos datos utilizados están en: ",
            nombredatos, " (key = id).\nSe adjunta informe web con el detalle de los resultados.",
            "\nLa tabla de aumentos de la muestra está en ",nombre_t1, 
            " la key es id.\nLa tabla de aumentos de la predicción está en ",
            nombre_t2, " la key es id.\nLa tabla de mape por localidad está en ",
            nombre_t3, " la key es id.")))

            