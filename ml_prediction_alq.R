
rm(list=ls())
getwd()
directorio <- "/"
setwd(directorio)
save(directorio, file = "directorio.Rda")

library(sf)
require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
datos <- st_read(con, query = "SELECT * FROM " )
today <- format(Sys.Date(), "%Y_%d_%m")


nombre2 <- paste0( "datos_alquileres_", today,".gpkg" )
nombre2
save(nombre2, file="nombre2.Rda")
datos$fid<-NULL
st_write(datos, nombre2,  delete_layer = T, delete_dsn = T)

library(dplyr)


contains_any_na = sapply(datos, function(x) any(is.na(x)))
names(datos)[contains_any_na]

summary(datos)
class(datos$localidad)
datos$localidad <- as.character(datos$localidad)
table(datos$localidad)

datos$localidad = ifelse(datos$localidad == "LA CALERA", "SIERRAS CHICAS",
                         ifelse(datos$localidad == "LAS HIGUERAS", "RIO CUARTO",
                                ifelse(datos$localidad == "VILLA ALLENDE", "SIERRAS CHICAS",
                                       ifelse(datos$localidad == "VILLA NUEVA", "VILLA MARIA",
                                              ifelse(datos$localidad == "VILLA DEL LAGO (ESTANCIA VIEJA)", "VILLA CARLOS PAZ",
                                                     datos$localidad)))))

table(datos$localidad)
datos$localidad = as.factor(datos$localidad)
table(datos$localidad)

library(mapview)
mapview(datos, zcol = "vua")
mapview(datos, zcol = "localidad")


datos <- st_difference(datos)

# # Dummies para factor fragment
table(datos$fragment)
datos$fragment0 <- ifelse(datos$fragment ==0, 1,0)
datos$fragment1 <- ifelse(datos$fragment ==1, 1,0)
datos$fragment2 <- ifelse(datos$fragment ==2, 1,0)
datos$fragment3 <- ifelse(datos$fragment ==3, 1,0)
#datos$fragment4 <- ifelse(datos$fragment ==4, 1,0)
#datos$fragment <- NULL

# names(datos)
# getwd()
#### Panel de Control ####
limite_mape = 0.14
save(limite_mape, file="limite_mape.Rda")
min_obs_zonas = 10
save(min_obs_zonas, file = "min_obs_zonas.Rda")
obs_original <- dim(datos)[1]
save(obs_original, file = "obs_original.Rda")
porc_eliminar = 0.1
save(porc_eliminar, file="porc_eliminar.Rda")

datos$id_aux = 1:nrow(datos)
getwd()
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer = T)
st_write(datos, "datos_sin_depurar.gpkg", delete_dsn = T, delete_layer = T)
save(datos, file = "datos.Rda")


#### Entrenar modelos ####
dir.create("Fold")
dir.create("Importancia de variables")

cat(paste(shQuote(names(datos), type="cmd2"), collapse=" + "))
library(caret)
library(sf)
rm(list=ls())


form = vua ~  superficie_total_terreno + superficie_total_mejoras + inc_edif + d_ruta + d_viasprin + d_viassec + d_alta + d_baja +
  d_lineadiv + d_depre + d_rio + prom_lote + perc_edif + perc_baldm + perc_bald + porc_uec + porc_ued + porc_re + porc_eau + porc_bu + 
  porc_ear + porc_agua + bci + rndsi + ui + ndbi + ndvi + ind_con + osm_iibb + vut_2022 + dens_osm + localidad + 
  fragment1 + fragment2 + fragment3
save(form, file = "form.Rda")

repeat {
  rm(list=ls())
  library(doParallel)
  cores=detectCores()
  cl <- makeCluster(cores[1]) #not to overload your computer registerDoParallel(cl)
  registerDoParallel(cl)
  system.time( 
    prediccion <-foreach(i = 1:10 , .combine = cbind , #c(4,10)
                         .packages = c("caret","rpart","MASS","spdep","gstat","sf","sp","dplyr")) %dopar% {
                           #===========    Preparación de la base    =====================================================
                           load("directorio.Rda")
                           setwd(directorio)
                           
                           load("datos.Rda")
                           
                           cutoff=2500
                           seed = 11
                           set.seed(seed)
                           datos$id <- sample(rep(1:10, nrow(datos), length.out = nrow(datos)))
                           datos$localidad <- as.factor(datos$localidad)
                           list <- 1:10
                           # i = 1
                           set.seed(seed)
                           training <- subset(datos, id %in% list[-i])
                           set.seed(seed)
                           testing <- subset(datos, id %in% c(i))
                           # training[] <- lapply(training, function(x) if(is.factor(x)) factor(x) else x)
                           # testing[] <- lapply(testing, function(x) if(is.factor(x)) factor(x) else x)
                           fitControl <- trainControl(method = "cv", number = 5)
                           
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
                           train <- train(form, data = training,
                                          method = "gbm",
                                          trControl = fitControl)
                           testing$gbm <- predict(train, newdata=testing)
                           
                           # KKNN en los residuos
                           training$error <- training$vua - predict(train, newdata=training)
                           train <- train(error ~ x+y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$gbm_kknn <-testing$gbm + predict(train, newdata=testing)
                           #testing$gbm_kknn <-testing$gbm
                           
                           # try({
                           #   inside_gbm <- predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm <- training$vut - inside_gbm
                           #   vario_gbm   <- try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko <- try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko <- try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$gbm_kknn <- NA} else {
                           #     testing$gbm_kknn <- testing$gbm + ko$var1.pred}
                           # })
                           
                           
                           
                           #===========    Quantile Random Forest    =================================================================================
                           set.seed(seed)
                           train <- train(form, data = training,
                                          method = "qrf",
                                          trControl = fitControl)
                           testing$qrf <- predict(train, newdata=testing)
                           
                           
                           # KKNN en los residuos
                           training$error <- training$vua - predict(train, newdata=training)
                           train <- train(error ~ x+y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$qrf_kknn <-testing$qrf + predict(train, newdata=testing)
                           #testing$qrf_kknn <-testing$qrf 
                           
                           # try({
                           #   inside_gbm <- predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm <- training$vut - inside_gbm
                           #   vario_gbm   <- try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko <- try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko <- try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$qrf_kknn <- NA} else {
                           #     testing$qrf_kknn <- testing$qrf + ko$var1.pred}
                           # })
                           
                           #===========    Suppert Vector Regression     =================================================================================
                           set.seed(seed)
                           # train <- train(form, data = training,
                           #                      method = "svmRadial",
                           #                      trControl = fitControl)
                           # testing$svr <- predict(train, newdata=testing)
                           library(rminer)
                           train <- fit(form, data=training, model="svm")
                           testing$svr <- predict(train, testing)
                           
                           ## KKNN en los residuos
                           training$error <- training$vua - predict(train, newdata=training)
                           train <- train(error ~ x+y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$svr_kknn <-testing$svr + predict(train, newdata=testing)
                           #testing$svr_kknn <-testing$svr 
                           
                           # try({
                           #   inside_gbm <- predict(train, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm <- training$vut - inside_gbm
                           #   vario_gbm   <- try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko <- try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko <- try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$svr_kknn <- NA} else {
                           #     testing$svr_kknn <- testing$svr + ko$var1.pred}
                           # })
                           
                           #===========    Stacking    =================================================================================
                           library(caretEnsemble)
                           set.seed(seed)
                           my_control <- trainControl(method="cv",number=5, savePredictions="final")#, 
                           #index=createResample(training$vut))
                           set.seed(seed)
                           model_list <- caretList(form, data=training, trControl=my_control , 
                                                   methodList=c("gbm", "qrf","svmRadial") )
                           set.seed(seed)
                           glm_ensemble <- caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
                           testing$stk <- predict(glm_ensemble, newdata=testing)
                           
                           ## KKNN en los residuos
                           training$error <- training$vua - predict(glm_ensemble, newdata=training)
                           train <- train(error ~ x+y, data = training,
                                          method = "kknn",
                                          trControl = fitControl)
                           testing$stk_kknn <-testing$stk + predict(train, newdata=testing)
                           #testing$stk_kknn <-testing$stk 
                           
                           
                           # try({
                           #   inside_gbm <- predict(glm_ensemble, newdata=training)
                           #   train_ko = as(training, 'Spatial')
                           #   test_ko = as(testing, 'Spatial')
                           #   train_ko$error_gbm <- training$vut - inside_gbm
                           #   vario_gbm   <- try ({variogram(error_gbm~1,train_ko, cressie=T, cutoff = cutoff)})
                           #   model_gbm_ko <- try ({fit.variogram(vario_gbm, vgm(c("Sph", "Gau", "Exp")))})
                           #   ko <- try ({krige(error_gbm~ 1 , train_ko, test_ko, model_gbm_ko)})
                           #   if (class(ko)=="try-error") {testing$stk_kknn <- NA} else {
                           #     testing$stk_kknn <- testing$stk + ko$var1.pred}
                           # })
                           
                           # Sumarizamos resultados =========================================================================================================================
                           result <- testing[,c("id_aux","vua","localidad","gbm","gbm_kknn",
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
  tabla <- resultados %>%
    group_by(localidad) %>% 
    summarise(gbm = mean(abs(gbm-vua)/vua),
              gbm_kknn = mean(abs(gbm_kknn-vua)/vua),
              qrf = mean(abs(qrf-vua)/vua),
              qrf_kknn = mean(abs(qrf_kknn-vua)/vua),
              stk = mean(abs(stk-vua)/vua),
              stk_kknn = mean(abs(stk_kknn-vua)/vua),
              svr = mean(abs(svr-vua)/vua),
              svr_kknn = mean(abs(svr_kknn-vua)/vua),
              n = n()) %>% 
    rowwise() %>% mutate(min = min(gbm, gbm_kknn, qrf, qrf_kknn, stk, stk_kknn, svr, svr_kknn, na.rm = TRUE))
  
  MAPE = sum((tabla$n/sum(tabla$n)) * tabla$min)
  
  tabla = st_drop_geometry(tabla)
  
  modelos = as.data.frame(colnames(tabla[,2:9])[apply(tabla[,2:9],1,which.min)])
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
    message(paste("El MAPE es igual a",
                  round(MAPE, digits = 3),"-()-",
                  sep=" "))
    message("Los modelos seleccionados son los siguientes:")
    print(knitr::kable(modelos, "rst"))
    break
  }
  
  eliminar <- resultados %>%
    rowwise %>%  mutate(gbm = mean(abs(gbm-vua)/vua),
                        gbm_kknn = mean(abs(gbm_kknn-vua)/vua),
                        qrf = mean(abs(qrf-vua)/vua),
                        qrf_kknn = mean(abs(qrf_kknn-vua)/vua),
                        stk = mean(abs(stk-vua)/vua),
                        stk_kknn = mean(abs(stk_kknn-vua)/vua),
                        svr = mean(abs(svr-vua)/vua),
                        svr_kknn = mean(abs(svr_kknn-vua)/vua),
                        min = min (gbm, gbm_kknn, qrf, qrf_kknn, stk, stk_kknn, svr, svr_kknn, na.rm = TRUE))
  
  
  eliminar$criterio = ifelse(eliminar$min > 0.5 , "eliminar", "no eliminar")
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
  
  
  
  print(paste("El MAPE por el momento es",format(round(MAPE, 3), nsmall=3),", y se procederá a eliminar", length(a$vua),
              "datos. El modelado continua con", length(b$vua), sep=" "))
  
  zonas <- b %>% rowwise %>%
    group_by(localidad) %>%
    summarise(n = n())
  chequeo = min(zonas$n)
  
  load("min_obs_zonas.Rda")
  
  
  if(chequeo < min_obs_zonas){
    print("Advertencia!")
  }
  
  
  load(file="datos.Rda")
  datos$criterio <- NULL
  eliminar = st_as_sf(eliminar)
  
  datos <- left_join(datos, st_drop_geometry(eliminar[,c("criterio", "id_aux")]), by = "id_aux")
  #datos = st_join(datos, eliminar[,c("criterio")], join = st_intersects)
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
      
      freq <- as.data.frame(table(datos$localidad))
      freq <- subset(freq, Var1!= "OTROS")
      
      min_loc <- subset(freq, Freq==min(freq[,2]))
      
      datos$localidad = ifelse(datos$localidad==as.character(min_loc[1,1]), "OTROS", datos$localidad)
      
      
    }}
  
  
  datos$localidad = as.factor(datos$localidad)
  datos$criterio = NULL
  
  save(datos, file = "datos.Rda")
  
}


#======== Predicción ======
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
st_write(datos, "datos.gpkg", delete_dsn = T, delete_layer =T) 

load("modelos.Rda")

require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
pred <- st_read(con, query = "SELECT * FROM " )

names(pred)
class(pred$localidad)
table(pred$localidad)

pred$localidad2 = ifelse(as.character(pred$localidad) == "EL DIQUECITO" |
                           as.character(pred$localidad) == "LA CALERA" | 
                           as.character(pred$localidad) == "MENDIOLAZA" |
                           as.character(pred$localidad) == "SALDAN" |
                           as.character(pred$localidad) == "VILLA ALLENDE", "SIERRAS CHICAS", as.character(pred$localidad))
table(is.na(pred$localidad))
table(pred$localidad, pred$localidad2)
table(pred$localidad2)

ver = subset(pred, localidad == "SANTA CATALINA")
mapview(ver)



pred$localidad2 = ifelse(as.character(pred$localidad2) == "VILLA NUEVA" ,
                         "VILLA MARIA", as.character(pred$localidad2))

pred$localidad2 = ifelse(as.character(pred$localidad2) == "COLONIA TIROLESA" |
                           as.character(pred$localidad2) == "MALVINAS ARGENTINAS" |
                           as.character(pred$localidad2) == "GUIÑAZU",
                         "CORDOBA", as.character(pred$localidad2))


pred$localidad2 = ifelse(as.character(pred$localidad2) == "VILLA DEL LAGO (ESTANCIA VIEJA)" |
                           as.character(pred$localidad2) == "VILLA INDEPENDENCIA" |
                           as.character(pred$localidad2) == "SAN ANTONIO DE ARREDONDO",
                         "VILLA CARLOS PAZ", as.character(pred$localidad2))

pred$localidad2 = ifelse(as.character(pred$localidad2) == "LAS HIGUERAS" |
                           as.character(pred$localidad2) == "SANTA CATALINA" |
                           as.character(pred$localidad2) == "ESPINILLOS",
                         "RIO CUARTO", as.character(pred$localidad2))

pred$localidad2 = ifelse(as.character(pred$localidad2) == "PLAZA SAN FRANCISCO",
                         "SAN FRANCISCO", as.character(pred$localidad2))

pred = subset(pred, localidad2 != "SANABRIA" & localidad2 != "COLONIA LUIS A SAUZE" & localidad2 != "MALAGUEÑO")
table(pred$localidad2)
pred$localidad = pred$localidad2; pred$localidad2 = NULL; table(pred$localidad)
pred$localidad <- as.factor(as.character(pred$localidad))
table(pred$localidad)

table(pred$localidad); table(datos$localidad)


table(datos$fragment)
class(pred$fragment)
pred$fragment <- as.numeric(as.character(pred$fragment))
table(pred$fragment)
#pred$fragment0 <- ifelse(pred$fragment ==0, 1,0)
pred$fragment1 <- ifelse(pred$fragment ==1, 1,0)
pred$fragment2 <- ifelse(pred$fragment ==2, 1,0)
pred$fragment3 <- ifelse(pred$fragment ==3, 1,0)
#pred$fragment4 <- ifelse(pred$fragment ==4, 1,0)
#pred$fragment <- NULL

library(tidyverse)
coords <- do.call(rbind, st_geometry(st_centroid(pred))) %>% 
  as_tibble() %>% setNames(c("x","y"))
pred$x = coords$x
pred$y = coords$y

# list_round_0 <- c("prom_lote")
# 
# for(i in list_round_0){
#   pred[[paste0(i)]] <- round(pred[[paste0(i)]], 0)
# }
# 
# list_round_2 <- c("perc_edif", "perc_baldm",
#                   "perc_bald", "porc_uec", "porc_ued", "porc_re", 
#                   "porc_eau", "porc_bu", "porc_ear", "porc_agua",
#                   "bci", "rndsi", "ui", "ndbi", "ndvi", "ind_con",
#                   "osm_iibb")
# 
# for(i in list_round_2){
#   pred[[paste0(i)]] <- round(pred[[paste0(i)]], 2)
# }


# pred <- st_cast(pred, "POLYGON")  ###gnera mas parcelas
#pred <- st_centroid(pred)
names(pred)
names(datos)

getwd()
# Comienzo de la prediccion

cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer registerDoParallel(cl)
registerDoParallel(cl)

# parallel()

fitControl <- trainControl(method = "cv", number = 5, allowParallel = T )

seed = 11
cutoff <- 2500
# 
# form = vut ~  localidad + d_ruta + d_viasprin + d_viassec + d_alta + d_baja + d_lineadiv +
#   d_depre + d_rio + prom_lote + perc_edif + perc_baldm + perc_bald + porc_uec +
#   porc_ued + porc_re + porc_eau + porc_bu + porc_ear + porc_agua + bci + rndsi + ui + ndbi +
#   ndvi + ind_con +   #fragment0 + fragment1 + fragment2 + fragment3 + fragment4 +
#   vut_2019 + vut_2018 + grupo


load("form.Rda")


# GBM 

if((is.na(vlookup("gbm", modelos$modelo, 1))==F &
    is.na(vlookup("gbm_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("gbm", modelos$modelo, 1))==T &
    is.na(vlookup("gbm_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  library(gbm)
  set.seed(seed)
  train_gbm <- train(form, data = datos,
                     method = "gbm",
                     trControl = fitControl,
                     verbose = F)
  #saveRDS(train_gbm,"train_gbm.rds")
  pred$gbm <- predict(train_gbm, newdata=pred)
  
  #KKNN en los residuos   
  datos$error <- datos$vua - predict(train_gbm, newdata=datos)
  train <- train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$gbm_kknn <-pred$gbm + predict(train, newdata=pred)
  # 
  # inside <- predict(train_gbm, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error <- datos$vut - inside
  # vario <- variogram(error~1, train_ko, cutoff = cutoff)
  # model <- fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko <- krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$gbm_kknn  <- pred$gbm  + test_ko$var1.pred
  # 
  # 
  
  # Importancia de variables
  imp_gbm <- as.data.frame(caret::varImp(train_gbm)[["importance"]])
  imp_gbm <- rownames_to_column(imp_gbm)
  imp_gbm <- rename(imp_gbm, variables = rowname)
  imp_gbm <- rename(imp_gbm, importancia = Overall)
  imp_gbm <- subset(imp_gbm, substr(imp_gbm$variables, 1,9)!="localidad")
  #imp_gbm <- subset(imp_gbm, substr(imp_gbm$variables, 1,3)!="vut")
  
  save(imp_gbm, file="Importancia de variables/imp_gbm.Rda")
  
  imp_gbm2 <- imp_gbm[order(-imp_gbm$importancia),]
  imp_gbm2 <- rownames_to_column(imp_gbm2)
  imp_gbm2 <- imp_gbm2[1:15,]
  
  grafico_gbm <-
    ggplot(data = imp_gbm2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo gbm",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  ggsave(grafico_gbm, file="Importancia de variables/grafgbm.png", width=4, height=4)
  
  
  if (is.na(vlookup("gbm", modelos$modelo, 1))==T){
    pred$gbm <- NULL 
  }    
  
} else {
  if (is.na(vlookup("gbm", modelos$modelo, 1))==F){
    library(gbm)
    set.seed(seed)
    train_gbm <- train(form, data = datos,
                       method = "gbm",
                       trControl = fitControl,
                       verbose=F)
    
    #saveRDS(train_gbm,"train_gbm.rds")
    pred$gbm <- predict(train_gbm, newdata=pred)
    
    # Importancia de variables
    imp_gbm <- as.data.frame(caret::varImp(train_gbm)[["importance"]])
    imp_gbm <- rownames_to_column(imp_gbm)
    imp_gbm <- rename(imp_gbm, variables = rowname)
    imp_gbm <- rename(imp_gbm, importancia = Overall)
    imp_gbm <- subset(imp_gbm, substr(imp_gbm$variables, 1,9)!="localidad")
    #imp_gbm <- subset(imp_gbm, substr(imp_gbm$variables, 1,3)!="vut")
    save(imp_gbm, file="Importancia de variables/imp_gbm.Rda")
    
    imp_gbm2 <- imp_gbm[order(-imp_gbm$importancia),]
    imp_gbm2 <- rownames_to_column(imp_gbm2)
    imp_gbm2 <- imp_gbm2[1:15,]
    
    grafico_gbm <-
      ggplot(data = imp_gbm2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo gbm",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_gbm, file="Importancia de variables/grafgbm.png", width=4, height=4)
    
    
  }}

# QRF

if((is.na(vlookup("qrf", modelos$modelo, 1))==F &
    is.na(vlookup("qrf_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("qrf", modelos$modelo, 1))==T &
    is.na(vlookup("qrf_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  set.seed(seed)
  train_qrf <- train(form, data = datos,
                     method = "qrf",
                     trControl = fitControl,
                     importance = T)
  #saveRDS(train_qrf,"train_qrf.rds")
  pred$qrf <- predict(train_qrf, newdata=pred)
  
  #KKNN en los residuos   
  datos$error <- datos$vua - predict(train_qrf, newdata=datos)
  train <- train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$qrf_kknn <-pred$qrf + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside <- predict(train_qrf, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error <- datos$vut - inside
  # vario <- variogram(error~1, train_ko, cutoff = cutoff)
  # model <- fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko <- krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$qrf_kknn <- pred$qrf  + test_ko$var1.pred
  
  
  # Importancia de variables
  imp_qrf <- as.data.frame(caret::varImp(train_qrf$finalModel))
  imp_qrf$Overall <- ifelse(imp_qrf$Overall<0, 0, imp_qrf$Overall)
  imp_qrf$importancia <- (imp_qrf$Overall / max(imp_qrf$Overall)) * 100
  imp_qrf <- rownames_to_column(imp_qrf)
  imp_qrf <- rename(imp_qrf, variables = rowname)
  imp_qrf <- subset(imp_qrf, substr(imp_qrf$variables, 1,9)!="localidad")
  #imp_qrf <- subset(imp_qrf, substr(imp_qrf$variables, 1,3)!="vut")
  imp_qrf$Overall <- NULL
  
  save(imp_qrf, file="Importancia de variables/imp_qrf.Rda")
  
  imp_qrf2 <- imp_qrf[order(-imp_qrf$importancia),]
  imp_qrf2 <- rownames_to_column(imp_qrf2)
  imp_qrf2 <- imp_qrf2[1:15,]
  
  grafico_qrf <-
    ggplot(data = imp_qrf2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo qrf",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  
  
  ggsave(grafico_qrf, file="Importancia de variables/grafqrf.png", width=4, height=4)
  
  
  if (is.na(vlookup("qrf", modelos$modelo, 1))==T){
    pred$qrf <- NULL 
  }    
  
} else {
  if (is.na(vlookup("qrf", modelos$modelo, 1))==F){
    set.seed(seed)
    train_qrf <- train(form, data = datos,
                       method = "qrf",
                       trControl = fitControl)
    #saveRDS(train_qrf,"train_gbm.rds")
    pred$qrf <- predict(train_qrf, newdata=pred)
    
    # Importancia de variables
    imp_qrf <- as.data.frame(caret::varImp(train_qrf$finalModel))
    imp_qrf$Overall <- ifelse(imp_qrf$Overall<0, 0, imp_qrf$Overall)
    imp_qrf$importancia <- (imp_qrf$Overall / max(imp_qrf$Overall)) * 100
    imp_qrf <- rownames_to_column(imp_qrf)
    imp_qrf <- rename(imp_qrf, variables = rowname)
    imp_qrf <- subset(imp_qrf, substr(imp_qrf$variables, 1,9)!="localidad")
    #imp_qrf <- subset(imp_qrf, substr(imp_qrf$variables, 1,3)!="vut")
    
    imp_qrf$Overall <- NULL
    
    save(imp_qrf, file="Importancia de variables/imp_qrf.Rda")
    
    imp_qrf2 <- imp_qrf[order(-imp_qrf$importancia),]
    imp_qrf2 <- rownames_to_column(imp_qrf2)
    imp_qrf2 <- imp_qrf2[1:15,]
    
    grafico_qrf <-
      ggplot(data = imp_qrf2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo qrf",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_qrf, file="Importancia de variables/graficqrf.png", width=4, height=4)
    
  }}

# SVR

if((is.na(vlookup("svr", modelos$modelo, 1))==F &
    is.na(vlookup("svr_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("svr", modelos$modelo, 1))==T &
    is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)
){
  
  # Entrenamiento
  library(rminer)
  library(stringr)
  form_character <- as.character(form)
  list_vbles <- paste0(form_character[[2]], " + ", form_character[[3]] ) 
  list_vbles <- str_split(list_vbles, " [+] ")
  matriz <- st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
  names(matriz)
  library(dplyr)
  matriz %>% 
    mutate_if(is.integer,as.numeric)
  set.seed(seed)
  train_svr <- fit(form, data=matriz, model="svm")
  
  list_pred <- str_split(form_character[[3]], " [+] ")
  matriz_pred <- st_drop_geometry(pred [, c(paste(list_pred[[1]]))])
  names(matriz_pred)
  matriz_pred %>% 
    mutate_if(is.integer,as.numeric)
  
  pred$svr <- predict(train_svr, matriz_pred)
  
  # #KKNN en los residuos   
  datos$error <- datos$vua - predict(train_svr, newdata=matriz)
  train <- train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$svr_kknn <-pred$svr + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside <- predict(train_svr, newdata=matriz)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error <- datos$vut - inside
  # vario <- variogram(error~1, train_ko, cutoff = cutoff)
  # model <- fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko <- krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$svr_kknn <- pred$svr  + test_ko$var1.pred
  # 
  # 
  
  
  # Variables Importantes
  imp_svr1 <- Importance(train_svr, matriz)
  imp_svr <- as.data.frame(imp_svr1$imp)
  imp_svr$nombre <- "nombre"
  
  a <- ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
  
  for(i in a[[1]]){
    imp_svr[i,c("nombre")] <- imp_svr1[["sresponses"]][[i]][[1]]
  }
  
  imp_svr <- subset(imp_svr, nombre != "nombre")
  imp_svr$importancia <- (imp_svr$`imp_svr1$imp` / max(imp_svr$`imp_svr1$imp`)) * 100
  imp_svr <- rename(imp_svr, variables = nombre)
  imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
  #imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
  imp_svr$`imp_svr1$imp` <-NULL
  save(imp_svr, file="Importancia de variables/imp_svr.Rda")
  
  imp_svr2 <- imp_svr[order(-imp_svr$importancia),]
  imp_svr2 <- rownames_to_column(imp_svr2)
  imp_svr2 <- imp_svr2[1:15,]
  
  grafico_svr <-
    ggplot(data = imp_svr2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo svr",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  getwd()
  ggsave(grafico_svr, file="Importancia de variables/grafsvr.png", width=4, height=4)
  
  
  
  if (is.na(vlookup("svr", modelos$modelo, 1))==T){
    pred$svr <- NULL 
  }    
  
} else {
  if (is.na(vlookup("svr", modelos$modelo, 1))==F){
    
    # Entrenamiento
    library(rminer)
    library(stringr)
    form_character <- as.character(form)
    list_vbles <- paste0(form_character[[2]], " + ", form_character[[3]] ) 
    list_vbles <- str_split(list_vbles, " [+] ")
    matriz <- st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
    names(matriz)
    library(dplyr)
    matriz %>% 
      mutate_if(is.integer,as.numeric)
    set.seed(seed)
    train_svr <- fit(form, data=matriz, model="svm")
    
    list_pred <- str_split(form_character[[3]], " [+] ")
    matriz_pred <- st_drop_geometry(pred [, c(paste(list_pred[[1]]))])
    names(matriz_pred)
    matriz_pred %>% 
      mutate_if(is.integer,as.numeric)
    
    pred$svr <- predict(train_svr, matriz_pred)
    
    # Variables Importantes
    imp_svr1 <- Importance(train_svr, matriz)
    imp_svr <- as.data.frame(imp_svr1$imp)
    imp_svr$nombre <- "nombre"
    
    a <- ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
    
    for(i in a[[1]]){
      imp_svr[i,c("nombre")] <- imp_svr1[["sresponses"]][[i]][[1]]
    }
    
    imp_svr <- subset(imp_svr, nombre != "nombre")
    imp_svr$importancia <- (imp_svr$`imp_svr1$imp` / max(imp_svr$`imp_svr1$imp`)) * 100
    imp_svr <- rename(imp_svr, variables = nombre)
    imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
    #imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
    imp_svr$`imp_svr1$imp` <-NULL
    save(imp_svr, file="Importancia de variables/imp_svr.Rda")
    
    imp_svr2 <- imp_svr[order(-imp_svr$importancia),]
    imp_svr2 <- rownames_to_column(imp_svr2)
    imp_svr2 <- imp_svr2[1:15,]
    
    grafico_svr <-
      ggplot(data = imp_svr2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo svr",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    getwd()
    ggsave(grafico_svr, file="Importancia de variables/grafsvr.png", width=4, height=4)
    
    
  }}


# STK

if((is.na(vlookup("stk", modelos$modelo, 1))==F &
    is.na(vlookup("stk_kknn", modelos$modelo, 1))==F) |
   (is.na(vlookup("stk", modelos$modelo, 1))==T &
    is.na(vlookup("stk_kknn", modelos$modelo, 1))==F)
){
  
  library(caretEnsemble)
  set.seed(seed)
  my_control <- trainControl(method="cv",number=5, savePredictions="final")
  set.seed(seed)
  model_list <- caretList(form, data=datos, trControl=my_control,
                          methodList=c("gbm", "qrf","svmRadial"))
  set.seed(seed)
  glm_ensemble <- caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
  
  #saveRDS(glm_ensemble,"train_ens.rds")
  pred$stk <- predict(glm_ensemble, newdata=pred)
  
  # #KKNN en los residuos   
  datos$error <- datos$vua - predict(glm_ensemble, newdata=datos)
  train <- train(error ~ x+y, data = datos,
                 method = "kknn",
                 trControl = fitControl)
  pred$stk_kknn <-pred$stk + predict(train, newdata=pred)
  
  #ko en los residuos   
  # inside <- predict(glm_ensemble, newdata=datos)
  # train_ko = as(datos, 'Spatial') 
  # test_ko = as(st_centroid(pred), 'Spatial')
  # train_ko$error <- datos$vut - inside
  # vario <- variogram(error~1, train_ko, cutoff = cutoff)
  # model <- fit.variogram(vario, vgm(c("Sph","Exp")))
  # plot(vario,model)
  # test_ko <- krige(error~ 1, train_ko, test_ko, model, nmax = 30)
  # pred$stk_kknn <- pred$stk  + test_ko$var1.pred
  # 
  # 
  # Importancia variables
  ponderacion <- caret::varImp(glm_ensemble$ens_model$finalModel)
  suma_modelos <- sum(ponderacion$Overall)
  ponderacion$pond <- (ponderacion$Overall/suma_modelos)
  ponderacion <- rownames_to_column(ponderacion)
  ponderacion$Overall <- NULL
  ponderacion <- rename(ponderacion, modelos = rowname)
  
  imp_stk_qrf <- caret::varImp(glm_ensemble$models$qrf$finalModel)
  imp_stk_qrf <- rownames_to_column(imp_stk_qrf)
  imp_stk_qrf <- rename(imp_stk_qrf, variables = rowname)
  imp_stk_qrf <- subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,9)!="localidad")
  #imp_stk_qrf <- subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,3)!="vut")
  # imp_stk_qrf <- subset(imp_stk_qrf, substr(imp_stk_gbm$variables, 1,5)!="grupo")
  suma <- sum(imp_stk_qrf$Overall)
  imp_stk_qrf$pond_qrf <- (imp_stk_qrf$Overall/suma)*100  
  imp_stk_qrf$Overall <- NULL
  
  library(gbm)
  imp_stk_gbm <- caret::varImp(glm_ensemble$models$gbm$finalModel)
  imp_stk_gbm <- rownames_to_column(imp_stk_gbm)
  imp_stk_gbm <- rename(imp_stk_gbm, variables = rowname)
  imp_stk_gbm <- subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,9)!="localidad")
  #imp_stk_gbm <- subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,3)!="vut")
  # imp_stk_gbm <- subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,5)!="grupo")
  suma <- sum(imp_stk_gbm$Overall)
  imp_stk_gbm$pond_gbm <- (imp_stk_gbm$Overall/suma)*100  
  imp_stk_gbm$Overall <- NULL
  
  if((is.na(vlookup("svr", modelos$modelo, 1))==F |
      is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)){
    
    imp_stk_svr <- imp_svr
    
  }else{
    # Entrenamiento
    library(rminer)
    library(stringr)
    form_character <- as.character(form)
    list_vbles <- paste0(form_character[[2]], " + ", form_character[[3]] ) 
    list_vbles <- str_split(list_vbles, " [+] ")
    matriz <- st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
    names(matriz)
    library(dplyr)
    matriz %>% 
      mutate_if(is.integer,as.numeric)
    set.seed(seed)
    train_svr <- fit(form, data=matriz, model="svm")
    
    # Variables Importantes
    imp_svr1 <- Importance(train_svr, matriz)
    imp_svr <- as.data.frame(imp_svr1$imp)
    imp_svr$nombre <- "nombre"
    
    a <- ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
    
    for(i in a[[1]]){
      imp_svr[i,c("nombre")] <- imp_svr1[["sresponses"]][[i]][[1]]
    }
    
    imp_svr <- subset(imp_svr, nombre != "nombre")
    imp_svr <- rename(imp_svr, variables = nombre)
    imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
    #imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
    # imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,5)!="grupo")
    suma <- sum(imp_svr$`imp_svr1$imp`)
    imp_svr$pond_svr <- (imp_svr$`imp_svr1$imp`/suma)*100
    imp_svr$`imp_svr1$imp` <-NULL
    
    imp_stk_svr <- imp_svr
  }
  
  # Hacer promedio ponderado de los modelos
  library(dplyr)
  imp_stk <- left_join(imp_stk_gbm, imp_stk_qrf, by="variables")
  imp_stk <- left_join(imp_stk, imp_stk_svr, by="variables")
  pond <- as.matrix(imp_stk[,c(-1)]) %*% as.matrix(ponderacion[,c(-1)])
  imp_stk <- cbind(imp_stk, pond)
  imp_stk$importancia <- (imp_stk$pond / max(imp_stk$pond)) * 100
  imp_stk[,c("pond_gbm", "pond_qrf", "pond_svr", "pond")] <- list(NULL)
  
  save(imp_stk, file="Importancia de variables/imp_stk.Rda")
  
  
  imp_stk2 <- imp_stk[order(-imp_stk$importancia),]
  imp_stk2 <- rownames_to_column(imp_stk2)
  imp_stk2 <- imp_stk2[1:15,]
  
  grafico_stk <-
    ggplot(data = imp_stk2, aes(x = importancia, y = reorder(variables, importancia))) +
    geom_bar(stat = "identity",
             fill = "#00a1ab",
             colour = "#ffffff") + labs(title = "Importancia de variables",
                                        subtitle = "Modelo stk",
                                        caption = "") +
    xlab("Importancia Relativa (%)") + ylab("Variable")
  
  ggsave(grafico_stk, file="Importancia de variables/grafstk.png", width=4, height=4)
  
  
  if (is.na(vlookup("stk", modelos$modelo, 1))==T){
    pred$stk <- NULL 
  }    
  
} else {
  if (is.na(vlookup("stk", modelos$modelo, 1))==F){
    library(caretEnsemble)
    set.seed(seed)
    my_control <- trainControl(method="cv",number=5, savePredictions="final")
    set.seed(seed)
    model_list <- caretList(form, data=datos, trControl=my_control,
                            methodList=c("gbm", "qrf","svmRadial"))
    set.seed(seed)
    glm_ensemble <- caretStack(model_list, method="glm", metric="RMSE", trControl=my_control)
    
    #saveRDS(glm_ensemble,"train_ens.rds")
    pred$stk <- predict(glm_ensemble, newdata=pred)
    
    # Importancia variables
    ponderacion <- caret::varImp(glm_ensemble$ens_model$finalModel)
    suma_modelos <- sum(ponderacion$Overall)
    ponderacion$pond <- (ponderacion$Overall/suma_modelos)
    ponderacion <- rownames_to_column(ponderacion)
    ponderacion$Overall <- NULL
    ponderacion <- rename(ponderacion, modelos = rowname)
    
    imp_stk_qrf <- caret::varImp(glm_ensemble$models$qrf$finalModel)
    imp_stk_qrf <- rownames_to_column(imp_stk_qrf)
    imp_stk_qrf <- rename(imp_stk_qrf, variables = rowname)
    imp_stk_qrf <- subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,9)!="localidad")
    #imp_stk_qrf <- subset(imp_stk_qrf, substr(imp_stk_qrf$variables, 1,3)!="vut")
    suma <- sum(imp_stk_qrf$Overall)
    imp_stk_qrf$pond_qrf <- (imp_stk_qrf$Overall/suma)*100  
    imp_stk_qrf$Overall <- NULL
    
    library(gbm)
    imp_stk_gbm <- caret::varImp(glm_ensemble$models$gbm$finalModel)
    imp_stk_gbm <- rownames_to_column(imp_stk_gbm)
    imp_stk_gbm <- rename(imp_stk_gbm, variables = rowname)
    imp_stk_gbm <- subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,9)!="localidad")
    #imp_stk_gbm <- subset(imp_stk_gbm, substr(imp_stk_gbm$variables, 1,3)!="vut")
    suma <- sum(imp_stk_gbm$Overall)
    imp_stk_gbm$pond_gbm <- (imp_stk_gbm$Overall/suma)*100  
    imp_stk_gbm$Overall <- NULL
    
    if((is.na(vlookup("svr", modelos$modelo, 1))==F |
        is.na(vlookup("svr_kknn", modelos$modelo, 1))==F)){
      
      imp_stk_svr <- imp_svr
      
    }else{
      # Entrenamiento
      library(rminer)
      library(stringr)
      form_character <- as.character(form)
      list_vbles <- paste0(form_character[[2]], " + ", form_character[[3]] ) 
      list_vbles <- str_split(list_vbles, " [+] ")
      matriz <- st_drop_geometry(datos [, c(paste(list_vbles[[1]]))])
      names(matriz)
      library(dplyr)
      matriz %>% 
        mutate_if(is.integer,as.numeric)
      set.seed(seed)
      train_svr <- fit(form, data=matriz, model="svm")
      
      # Variables Importantes
      imp_svr1 <- Importance(train_svr, matriz)
      imp_svr <- as.data.frame(imp_svr1$imp)
      imp_svr$nombre <- "nombre"
      
      a <- ifelse(is.null(imp_svr1[["sresponses"]][[1]])==T, list(2:dim(imp_svr)[1]), list(1:(dim(imp_svr)[1]-1)))
      
      for(i in a[[1]]){
        imp_svr[i,c("nombre")] <- imp_svr1[["sresponses"]][[i]][[1]]
      }
      
      imp_svr <- subset(imp_svr, nombre != "nombre")
      imp_svr <- rename(imp_svr, variables = nombre)
      imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,9)!="localidad")
      #imp_svr <- subset(imp_svr, substr(imp_svr$variables, 1,3)!="vut")
      suma <- sum(imp_svr$`imp_svr1$imp`)
      imp_svr$pond_svr <- (imp_svr$`imp_svr1$imp`/suma)*100
      imp_svr$`imp_svr1$imp` <-NULL
      
      imp_stk_svr <- imp_svr
    }
    
    # Hacer promedio ponderado de los modelos
    library(dplyr)
    imp_stk <- left_join(imp_stk_gbm, imp_stk_qrf, by="variables")
    imp_stk <- left_join(imp_stk, imp_stk_svr, by="variables")
    pond <- as.matrix(imp_stk[,c(-1)]) %*% as.matrix(ponderacion[,c(-1)])
    imp_stk <- cbind(imp_stk, pond)
    imp_stk$importancia <- (imp_stk$pond / max(imp_stk$pond)) * 100
    imp_stk[,c("pond_gbm", "pond_qrf", "pond_svr", "pond")] <- list(NULL)
    
    
    save(imp_stk, file="Importancia de variables/imp_stk.Rda")
    
    imp_stk2 <- imp_stk[order(-imp_stk$importancia),]
    imp_stk2 <- rownames_to_column(imp_stk2)
    imp_stk2 <- imp_stk2[1:15,]
    
    grafico_stk <-
      ggplot(data = imp_stk2, aes(x = importancia, y = reorder(variables, importancia))) +
      geom_bar(stat = "identity",
               fill = "#00a1ab",
               colour = "#ffffff") + labs(title = "Importancia de variables",
                                          subtitle = "Modelo stk",
                                          caption = "") +
      xlab("Importancia Relativa (%)") + ylab("Variable")
    
    ggsave(grafico_stk, file="Importancia de variables/grafistk.png", width=4, height=4)
    
    
  }}


stopCluster(cl)

save(pred, file = "prediccion_modelos.Rda")



# Eleccion de modelo por localidad

rm(list=ls())

load("prediccion_modelos.Rda")
st_write(pred, "prediccion_modelos.gpkg", delete_dsn = T, delete_layer = T)
#parcelas <- st_read("parcelas_id.gpkg")
load("modelos.Rda")

pred$id <- 1:nrow(pred)
names(pred)

lista_modelos <- as.list(levels(as.factor(subset(modelos, modelo != "varios")[["modelo"]])))
pred2 <- pred[,c("id", "localidad", paste(lista_modelos))]
pred2 <- st_drop_geometry(pred2)

library(reshape2)
pred2 <- melt (pred2, id.vars = c("id", "localidad"))
pred2 <- rename(pred2, modelo_pred = variable)
pred2 <- rename(pred2, vh = value)
names(pred2)

library(dplyr)
pred2 <- left_join(pred2, modelos[, c("localidad", "modelo")], by="localidad")
pred2 <- subset(pred2, as.character(modelo_pred) == as.character(modelo))
pred2$modelo_pred <- NULL

pred[,c(paste(lista_modelos))] <- list(NULL)


pred <- left_join(pred, pred2[,c("id","modelo", "vh")], by="id")

rm(pred2)

# pred_map <- pred[, c("vh")]

# mapview(pred_map, zcol="vh", col.regions=col,
#         alpha.region=0.8, alpha=1, lwd=0.1 )

# save(pred, file="prediccion_centroid.Rda")
# st_write(pred, "prediccion_centroid.gpkg", delete_dsn = T, delete_layer = T)
# 
# load("prediccion_centroid.Rda")
# 
# pred1 <- st_drop_geometry(pred)
# 
# parcelas <- left_join(parcelas, pred1, by="id")
# 
# names(parcelas)
pred = rename(pred, vua = vh)


save(pred, file = "prediccion_parcelas.Rda")
st_write(pred, "prediccion_parcelas.gpkg", delete_dsn = T, delete_layer = TRUE)
summary(pred$vua)
load("datos.Rda")
summary(datos$vua)
head(pred$vua)



# Redondeo de VH
parcelas <- st_read("prediccion_parcelas.gpkg")
# library(BAMMtools)

parcelas$vua_round <- plyr::round_any(parcelas$vua , 10, f = floor)
summary(pred$vua)
summary(parcelas$vua_round)

# 
# 
# q <- quantile(x = parcelas$vh, probs = seq(0, 1, 1/5))
# # q <- getJenksBreaks(parcelas$vh, k=6)
# q
# summary(parcelas$vh)
# load("datos.Rda")
# summary(datos$vua)
# class(datos$vua)
# 
# 
# 
# names(parcelas)
# parcelas$vh_round <- ifelse(parcelas$vh <= q[2], floor(round(parcelas$vh/50, 2))*50, 
#                             ifelse(parcelas$vh <= q[3], floor(round(parcelas$vh/100, 2))*100,
#                                    ifelse(parcelas$vh <= q[4], floor(round(parcelas$vh/100, 2))*100,
#                                           ifelse(parcelas$vh <= q[5], floor(round(parcelas$vh/100, 2))*100,
#                                                  floor(round(parcelas$vh/250, 2))*250))))
# 
# 
# summary(parcelas$vh_round)

#parcelas$vh_round <- ifelse(parcelas$vut_2020 == 1, parcelas$vut_2020, parcelas$vh_round)


# hist(parcelas$var19)

# valor por metro cuadrado
#parcelas$valorm2 <- parcelas$vua_round * parcelas$coef


# parcelas$var19 <- (parcelas$vh_round / parcelas$vut_2019) -1
# parcelas$var18 <- (parcelas$vh_round / parcelas$vut_2018) -1
getwd()
save(parcelas, file ="prediccion_parcelas.Rda")
st_write(parcelas, "prediccion_parcelas.gpkg", delete_dsn = T, delete_layer = TRUE)


##############summary para mostrar
load("modelos.Rda")
summary(parcelas$vua_round)
hist(parcelas$vua_round)
load("datos.Rda")

summary(datos$vua)

############deshomogeneización

# library(sf)
# require(RPostgres)
# con = dbConnect(Postgres(), dbname = "", host = "", port = , 
#                  user = "stefano.balbo", password = "")
# 
# parcelas <- st_read (con, query = "SELECT * FROM")
# today <- format(Sys.Date(), "%Y%d%m")
# 
# nombreparcelas <- paste0(directorio, "parcelas_cuentas_", today,".Rda" )
# nombreparcelas
# save(parcelas, file = nombreparcelas)
# 
# names(parcelas)
# summary(parcelas)
# 
# table(is.na(parcelas$cuenta))
# 
# st_write(parcelas, "parcelas.gpkg")
# #load(nombreparcelas)
# 



#### Calculo de coeficiente

rm(list=ls())
library(sf)
library(tidyverse)
getwd()

load("prediccion_parcelas.Rda")
load("//b_sup.Rda")
load("//b_sig.Rda")

parcelas$m_tipologia = ifelse(parcelas$coef_comerc>1, "departamento","vivienda")
parcelas$m_tipologia = as.factor(parcelas$m_tipologia)
table(parcelas$m_tipologia)
############################## CATEG CONSTRUCCION #############################
class(parcelas$puntaje_pond)
parcelas$m_categ_construc <- case_when(parcelas$puntaje_pond < 48 ~ "baja", #Baja
                                       parcelas$puntaje_pond >= 48 & parcelas$puntaje_pond < 84 ~ "estandar", #Estandar
                                       parcelas$puntaje_pond >= 84 & parcelas$puntaje_pond < 96 ~ "media alta", #Media alta
                                       parcelas$puntaje_pond >= 96 ~ "alta")
table(parcelas$m_categ_construc)
class(parcelas$m_categ_construc)
parcelas$m_categ_construc = as.factor(parcelas$m_categ_construc)

############################## CATEG ESTADO CONSERV #############################
parcelas$m_estado_conserv = "bueno"
table(parcelas$m_estado_conserv)
class(parcelas$m_estado_conserv)
parcelas$m_estado_conserv = as.factor(parcelas$m_estado_conserv)
# datos$m_estado_conserv <- case_when(  as.character(datos$estado_conserv)== "en construccion" ~ "0", #En construccion
#                                       as.character(datos$estado_conserv) == "malo" ~ "1", #Malo
#                                       as.character(datos$estado_conserv) == "regular" ~ "2", #Regular
#                                       as.character(datos$estado_conserv) == "bueno" ~ "3") #Bueno



############################## PATIO EN VIVIENDAS #############################
parcelas$p_sup = parcelas$superficie_mejoras
parcelas$m_perc_patio <- ifelse(str_detect(parcelas$m_tipologia, "vivienda") == T, parcelas$p_sup/parcelas$superficie_total_terreno, 1 )
summary(parcelas$m_perc_patio)
hist(parcelas$m_perc_patio)
parcelas$m_patio <- case_when(parcelas$m_perc_patio <= 0.5 ~ "patio_grande",
                              parcelas$m_perc_patio > 0.5 & parcelas$m_perc_patio < 0.95 ~ "patio_chico",
                              parcelas$m_perc_patio >= 0.95 ~ "no_patio")

table(parcelas$m_patio)
class(parcelas$m_patio)
parcelas$m_patio = as.factor(parcelas$m_patio)

summary(parcelas)
############################## ANTIGUEDAD #############################

summary(parcelas$antiguedad_pond)
table(is.na(parcelas$antiguedad_pond))
parcelas = rename(parcelas, m_antig = antiguedad_pond)

ver = subset(parcelas, m_antig > 100)
parcelas$m_antig = ifelse(parcelas$m_antig > 100, 100, parcelas$m_antig)
hist(parcelas$m_antig)

############################## UBICACION #############################

names(parcelas)
parcelas$m_ubicacion_cuadra = ifelse(parcelas$medial==1, "medial",
                                     ifelse(parcelas$esquina==1, "esquina",
                                            ifelse(parcelas$interno== 1, "interno", "2_calles")))
table(parcelas$m_ubicacion_cuadra)
parcelas$m_ubicacion_cuadra = as.factor(as.character(parcelas$m_ubicacion_cuadra))
class(parcelas$m_ubicacion_cuadra)

vbles <- data.frame(id = as.numeric(1:(dim(parcelas)[1])))

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



b_sig
# beta <- as.matrix(b_sig[, c("b")])
parcelas$m_antig2 = parcelas$m_antig * parcelas$m_antig

datos5 <- dummies_corregida(st_drop_geometry(parcelas)[,c("m_ubicacion_cuadra",
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

vbles = datos5[,c("m_antig" , "m_antig2" , "m_ubicacion_cuadra2_calles" , "m_ubicacion_cuadraesquina" , "m_ubicacion_cuadrainterno" ,
                  "m_tipologiavivienda" , "m_categ_construcalta" , "m_categ_construcbaja" , 
                  "m_categ_construcmedia alta" , "m_patiopatio_chico" , "m_patiopatio_grande")]

aux = as.data.frame(names(vbles))
aux = rename(aux, vble = "names(vbles)")
aux = left_join(aux, b_sig[, c("vble","b")])
beta = as.matrix(aux[,c("b")])

largo <- as.numeric(nrow(vbles))



antig_homog <- 1
vbles$m_antig <- vbles$m_antig - antig_homog
vbles$m_antig2 <- vbles$m_antig2 - antig_homog



i = 1
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  parcelas$expon[i] <- a %*% beta
}

summary(parcelas$expon)


# mediana_sup <- round(median(datos$p_sup))
load("/mediana_sup.Rda")
load("/b_sup.Rda")


parcelas$coef <- ((parcelas$p_sup/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(parcelas$expon))


summary(parcelas$coef)
parcelas$coef = ifelse(parcelas$coef < 0.2, 0.2, parcelas$coef)
parcelas$coef = ifelse(parcelas$coef > 1.5, 1.5, parcelas$coef)
ver = subset(parcelas, p_sup == 0)
table(parcelas$estado)
parcelas$coef = ifelse(parcelas$estado == "BALDIO", 0, parcelas$coef)
hist(parcelas$coef)

parcelas$v_residencial = parcelas$vua * parcelas$coef
summary(parcelas$v_residencial)


table(parcelas$m_tipologia)
luz = subset(parcelas, par_idparcela == "117867")
luz$v_residencial[[13]] * luz$superficie_mejoras[[13]]
luz$vua[[13]]
save(parcelas, file = "parcelas_deshomog.Rda")

luz$superficie_mejoras[[13]] 

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

########################################################################################################

parcelas$m_antig <- ifelse(parcelas$antiguedad_pond > 90,  90, parcelas$antiguedad_pond )
parcelas$m_antig <- ifelse(parcelas$m_antig < 1,  1, parcelas$m_antig  )

parcelas$m_antig2 <- (parcelas$m_antig)^2

summary(parcelas$m_antig2)
summary(parcelas$antiguedad_pond)





vbles <- data.frame(id = as.numeric(1:(dim(parcelas)[1])))
# names(datos)

library(fastDummies)
b_sig
names(parcelas)
table(parcelas$medial)

#ubicacion cuadra
parcelas$m_ubicacion_cuadra <- case_when(  as.character(parcelas$medial)== "1" ~ "0",
                                           as.character(parcelas$esquina) == "1" ~ "1",
                                           as.character(parcelas$interno) == "1" ~ "2",
                                           as.character(parcelas$salida_calles) == "1" ~ "3",
                                           as.character(parcelas$pasillo) == "1" ~ "2") ##OJO
table(parcelas$m_ubicacion_cuadra)
table(is.na(parcelas$m_ubicacion_cuadra))
class(parcelas$m_ubicacion_cuadra)
parcelas$m_ubicacion_cuadra <- as.factor(parcelas$m_ubicacion_cuadra)

#m_tipologia

summary(as.numeric(parcelas$tipo))
table(is.na(parcelas$estado))
table(is.na(parcelas$tipo))

parcelas <- subset(parcelas, tipo != "BALDÍO")

parcelas$m_tipologia <- case_when(  as.character(parcelas$tipo)== "DEPARTAMENTO" ~ "D",
                                    as.character(parcelas$tipo) == "VIVIENDA" ~ "V") ##OJO
table((parcelas$m_tipologia))
table(is.na(parcelas$m_tipologia))
class(parcelas$m_tipologia)

parcelas$m_tipologia <- as.factor(parcelas$m_tipologia)
levels(parcelas$m_tipologia)

ver <- factor(parcelas$m_tipologia, labels = c("D", "V", "O"), ifany = T)

levels(parcelas$m_tipologia) <- c(levels(parcelas$m_tipologia), c("O", "C"))

#m_categ_cons
summary((parcelas$puntaje_pond))
table(is.na(parcelas$puntaje_pond))

inter <- max(parcelas$puntaje_pond)/4

parcelas$m_categ_construc <- case_when((parcelas$puntaje_pond) < inter ~ "0",
                                       inter <= (parcelas$puntaje_pond) & (parcelas$puntaje_pond) < inter*2 ~ "1",
                                       inter*2 <= (parcelas$puntaje_pond) & (parcelas$puntaje_pond) < inter*3 ~ "2",
                                       inter*3 <= (parcelas$puntaje_pond)& (parcelas$puntaje_pond)<= inter*4 ~ "3")


table((parcelas$m_categ_construc))
table(is.na(parcelas$m_categ_construc))



###localidad

table((parcelas$localidad))
table(is.na(parcelas$m_categ_construc))
library(osmdata)
bbox1 <- getbb("Municipio de Córdoba, Argentina", format_out = "sf_polygon")
bbox1$localidad <- "CORDOBA"

library(tmap)
tmap_mode("view")
mapa <-  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(ver) +
  tm_polygons()
mapa

bbox2 <- getbb("Municipio de Villa Carlos Paz, Argentina", format_out = "sf_polygon")
bbox2$localidad <- "VILLA CARLOS PAZ"
bbox3 <- getbb("Municipio de San Francisco, Argentina", format_out = "sf_polygon")
bbox3$localidad <- "SAN FRANCISCO"
bbox4 <- getbb("Municipio de Villa María, Argentina", format_out = "sf_polygon")
bbox4$localidad <- "VILLA MARIA"
bbox5 <- getbb("Municipio de Río Cuarto, Argentina", format_out = "sf_polygon")
bbox5$localidad <- "RIO CUARTO"

pol <- rbind(bbox1,bbox2, bbox3, bbox4, bbox5)
pol$m_localidad <- pol$localidad
pol<- st_transform(pol, 22174)

pol <- st_buffer(pol, dist = 5000)


parcelas<-st_join(parcelas, pol[,c("m_localidad")]) 

table(is.na(parcelas$m_localidad))

ver <- subset(parcelas, is.na(m_localidad) == T) 
table(ver$localidad)

parcelas <- subset(parcelas, is.na(m_localidad) == F)
table(parcelas$m_localidad)
table(is.na(parcelas$m_localidad))

#####estado de cons

parcelas$m_estado_conserv <- "2" #estado de cons regular
parcelas$m_estado_conserv <- as.factor(parcelas$m_estado_conserv)

levels(parcelas$m_estado_conserv) <- c(levels(parcelas$m_estado_conserv), c("0","1","3"))

table(parcelas$m_estado_conserv)
#####m_patio
class(parcelas$m_tipologia)
summary(parcelas)
ver <- subset(parcelas,m_tipologia == "V")
summary(ver)

parcelas <- subset(parcelas,superficie_tierra_urbana != 0)

parcelas$m_perc_patio <- ifelse( parcelas$m_tipologia == "V",
                                 parcelas$superficie_mejoras/parcelas$superficie_tierra_urbana, 1 )

summary(parcelas$m_perc_patio)
hist(parcelas$m_perc_patio)

####patio




parcelas$m_patio <- case_when(parcelas$m_perc_patio <= 0.5 ~ "patio_grande",
                              parcelas$m_perc_patio > 0.5 & parcelas$m_perc_patio < 0.95 ~ "patio_chico",
                              parcelas$m_perc_patio >= 0.95 ~ "no_patio")

table(parcelas$m_patio)
table(is.na(parcelas$m_patio))
parcelas$m_patio = as.factor(parcelas$m_patio)


#Superficie mejoras

parcelas <- subset(parcelas, parcelas$superficie_mejoras > 15 )

########

names(parcelas)


datos5 <- dummy_cols(st_drop_geometry(parcelas)[,c("m_ubicacion_cuadra",
                                                   "m_tipologia",
                                                   "m_categ_construc",
                                                   "m_antig",
                                                   "m_antig2",
                                                   "m_localidad",
                                                   "m_estado_conserv", "m_patio")], 
                     select_columns = c("m_ubicacion_cuadra",
                                        "m_tipologia",
                                        "m_categ_construc",
                                        "m_localidad", "m_estado_conserv", "m_patio"))


names(datos5)
datos5[,c("m_ubicacion_cuadra_0","m_tipologia_D", "m_categ_construc_1",
          "m_localidad_CORDOBA", "m_estado_conserv_2", 
          "m_ubicacion_cuadra", "m_tipologia", "m_categ_construc", 
          "m_localidad", "m_estado_conserv", "m_patio", "m_patio_no_patio")] <- list(NULL)


names(datos5)
b_sig

cat(paste(shQuote(names(datos5), type="cmd"), collapse=" , ")) 

vbles = datos5[,c( "m_ubicacion_cuadra_1" , "m_ubicacion_cuadra_2" , 
                   "m_ubicacion_cuadra_3" , "m_tipologia_C" , 
                   "m_tipologia_O" , "m_tipologia_V" , 
                   "m_categ_construc_0" , 
                   "m_categ_construc_2" , "m_categ_construc_3" ,
                   "m_antig" , "m_antig2" ,
                   "m_localidad_RIO CUARTO" , "m_localidad_SAN FRANCISCO" , 
                   "m_localidad_VILLA CARLOS PAZ" , "m_localidad_VILLA MARIA" ,
                   "m_estado_conserv_1" , "m_estado_conserv_3", "m_patio_patio_chico", "m_patio_patio_grande" )]


largo <- as.numeric(nrow(vbles))
# vbles$id <- NULL

# mes_homog <- 1 
# room_homog <- 1
# bathroom_homog <- 1
# 
# vbles$mes_acum <- vbles$mes_acum - mes_homog
# vbles$rooms <- vbles$rooms - room_homog
# vbles$bathrooms <- vbles$bathrooms - bathroom_homog

antig_homog <- 1
vbles$m_antig <- vbles$m_antig - antig_homog
vbles$m_antig2 <- vbles$m_antig2 - antig_homog

head(vbles)
summary(parcelas$m_antig)

#########

beta <- as.matrix(b_sig[, c("b")])
matriz_beta <- beta

parcelas$expon <- NULL
#hacemos el exponente datos
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  parcelas$expon[i] <- a %*% beta
}

summary(parcelas$expon)



load("//mediana_sup.Rda")

# # mediana_anio <- round(median(2022 - datos$anio_construc))
# mediana_anio <- 1
# save(mediana_anio, file = "Coeficientes/mediana_anio.Rda")

#calculo coeficiente datoss

summary(parcelas$superficie_mejoras)
ver <- subset(parcelas, parcelas$superficie_mejoras<1)

parcelas$coef <- ((parcelas$superficie_mejoras/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(parcelas$expon))

ver <- subset(parcelas, coef >10)

summary(parcelas$coef)
hist(parcelas$coef)

parcelas$coef <- ifelse(parcelas$coef < 0.2, 0.2, ifelse(parcelas$coef > 
                                                           1.5, 1.5, parcelas$coef))

st_write(parcelas, "parcelas_cuentas.gpkg", delete_dsn = T, delete_layer = T)


pred_vua <- st_read("prediccion_alquileres_rev_211216.gpkg")

parcelas1 <- left_join(parcelas,
                       st_drop_geometry(pred_vua[, c("vua", "par_idparcela")]), by = "par_idparcela")

parcelas <- parcelas1
rm(parcelas1)
table(parcelas$localidad)

summary(parcelas1$vua)
ver <- subset(parcelas1, is.na(parcelas1$vua) == T)

st_write(ver, "ver_par.gpkg")



library(tmap)
tmap_mode("view")
mapa <-  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(ver) +
  tm_polygons()

summary(parcelas$par_idparcela)


parcelas <- st_read("parcelas_cuentas.gpkg")
ver <- subset(parcelas, is.na(parcelas$vua) == T)


library(nngeo)
ver<-st_join(ver, pred_vua[, c("vua")], join = st_nn, k = 1) ## Hacemos el st_join.

save(ver, file = "ver.Rda")

# ver$vua.x <- NULL 
# ver <- rename( ver, vua = vua.y)
# 
# summary(ver)
###pegamos
parcelas2 <- parcelas

parcelas2 <- dplyr::bind_rows(parcelas2, ver)
parcelas2 <- subset(parcelas2, is.na(vua) == F)

summary(parcelas2)
parcelas <- parcelas2

#deshomog
parcelas$valorm2 <- parcelas$vua * parcelas$coef
summary(parcelas$valorm2)
######################################################

parcelas <- st_read("parcelas_cuentas.gpkg")

########

names(parcelas)
vbles <- data.frame(id = as.numeric(1:(dim(parcelas)[1])))
# names(datos)

library(fastDummies)
levels(parcelas$m_tipologia) <- c(levels(parcelas$m_tipologia), c("O", "C"))
summary(parcelas$m_tipologia)


levels(parcelas$m_estado_conserv) <- c(levels(parcelas$m_estado_conserv), c("0","1","3"))
summary(parcelas$m_estado_conserv)

datos5 <- dummy_cols(st_drop_geometry(parcelas)[,c("m_ubicacion_cuadra",
                                                   "m_tipologia",
                                                   "m_categ_construc",
                                                   "m_antig",
                                                   "m_antig2",
                                                   "m_localidad",
                                                   "m_estado_conserv", "m_patio")], 
                     select_columns = c("m_ubicacion_cuadra",
                                        "m_tipologia",
                                        "m_categ_construc",
                                        "m_localidad", "m_estado_conserv", "m_patio"))


names(datos5)
datos5[,c("m_ubicacion_cuadra_0","m_tipologia_D", "m_categ_construc_1",
          "m_localidad_CORDOBA", "m_estado_conserv_2", 
          "m_ubicacion_cuadra", "m_tipologia", "m_categ_construc", 
          "m_localidad", "m_estado_conserv", "m_patio", "m_patio_no_patio")] <- list(NULL)


names(datos5)
b_sig

cat(paste(shQuote(names(datos5), type="cmd"), collapse=" , ")) 

vbles = datos5[,c( "m_ubicacion_cuadra_1" , "m_ubicacion_cuadra_2" , 
                   "m_ubicacion_cuadra_3" , "m_tipologia_C" , 
                   "m_tipologia_O" , "m_tipologia_V" , 
                   "m_categ_construc_0" , 
                   "m_categ_construc_2" , "m_categ_construc_3" ,
                   "m_antig" , "m_antig2" ,
                   "m_localidad_RIO CUARTO" , "m_localidad_SAN FRANCISCO" , 
                   "m_localidad_VILLA CARLOS PAZ" , "m_localidad_VILLA MARIA" ,
                   "m_estado_conserv_1" , "m_estado_conserv_3", "m_patio_patio_chico", "m_patio_patio_grande" )]


largo <- as.numeric(nrow(vbles))

names(vbles)
vbles$m_tipologia_C <- 1
vbles$m_tipologia_O <- 0
vbles$m_tipologia_V <- 0

summary(vbles)

######

# vbles$id <- NULL

# mes_homog <- 1 
# room_homog <- 1
# bathroom_homog <- 1
# 
# vbles$mes_acum <- vbles$mes_acum - mes_homog
# vbles$rooms <- vbles$rooms - room_homog
# vbles$bathrooms <- vbles$bathrooms - bathroom_homog

antig_homog <- 1
vbles$m_antig <- vbles$m_antig - antig_homog
vbles$m_antig2 <- vbles$m_antig2 - antig_homog

head(vbles)
summary(parcelas$m_antig)

#########
load("//b_sup.Rda")

load("//b_sig.Rda")


beta <- as.matrix(b_sig[, c("b")])
matriz_beta <- beta

parcelas$expon <- NULL
#hacemos el exponente datos
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  parcelas$expon[i] <- a %*% beta
}


summary(parcelas$expon)


ver <- subset(parcelas, expon >5)
getwd()

load("/mediana_sup.Rda")

# # mediana_anio <- round(median(2022 - datos$anio_construc))
# mediana_anio <- 1
# save(mediana_anio, file = "Coeficientes/mediana_anio.Rda")



summary(parcelas$superficie_mejoras)
ver <- subset(parcelas, parcelas$superficie_mejoras<1)

parcelas$coef_c <- ((parcelas$superficie_mejoras/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(parcelas$expon))

ver <- subset(parcelas, coef >10)

summary(parcelas$coef_c)
hist(parcelas$coef_c)

parcelas$coef_c <- ifelse(parcelas$coef_c < 0.2, 0.2, ifelse(parcelas$coef_c > 
                                                               2.5, 2.5, parcelas$coef_c))

# coef_c <- parcelas[,"coef_c"]
# save(coef_c, file = "coef_c.Rda")
#
# parcelas2 <- parcelas
# parcelas2$coef_c <- coef_c$coef_c
# 
# parcelas <- parcelas2
# summary(parcelas2)

#deshomog
parcelas$vm2_comercios <- parcelas$vua * parcelas$coef_c
summary(parcelas$vm2_comercios)

##################################################################################3
###############################################################################
##oficinas

vbles$m_tipologia_C <- 0
vbles$m_tipologia_O <- 1
vbles$m_tipologia_V <- 0

summary(vbles)


parcelas$expon <- NULL
#hacemos el exponente datos
for (i in 1:largo) {
  a <- t(as.matrix(as.numeric(vbles[i, ])))
  parcelas$expon[i] <- a %*% beta
}

summary(parcelas$expon)


load("//mediana_sup.Rda")


#calculo coeficiente datoss

parcelas$coef_o <- ((parcelas$superficie_mejoras/mediana_sup)^ifelse(length(b_sup$b) != 0, b_sup$b, 0)) * (exp(parcelas$expon))



summary(parcelas$coef_o)
hist(parcelas$coef_o)

parcelas$coef_o <- ifelse(parcelas$coef_o < 0.2, 0.2, ifelse(parcelas$coef_o > 
                                                               2.5, 2.5, parcelas$coef_o))



#deshomog
parcelas$vm2_oficina <- parcelas$vua * parcelas$coef_o
summary(parcelas$vm2_oficina)



summary(parcelas)


parcelas$expon <- NULL

coords <- do.call(rbind, st_geometry(seal_sf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

ver <- st_drop_geometry(parcelas) %>% group_by(par_idparcela) %>% 
  summarise(sup_promedio = mean(superficie_mejoras), 
            alquiler_promedio = plyr::round_any(mean(valorm2) , 10, f = floor)) %>% 
  mutate(vt_prom = sup_promedio*alquiler_promedio )

promedios <- left_join(parcelas, ver, by = "par_idparcela")

st_write(promedios, "promedios.gpkg", delete_dsn = T, delete_layer = T)

table(parcelas$m_tipologia)
st_write(parcelas, "parcelas_cuentas.gpkg", delete_dsn = T, delete_layer = T)

parcelas$superficie_mejoras
