
rm(list=ls())

library(janitor) # Comparar dos databases

load("datos1.Rda"); names(datos1)
load("datos2.Rda"); names(datos2)

datos1 = rename(datos1, localidad = localidad_def)

datos1=datos1[,c("variables_columna")]
datos2=datos2[,c("variables_columna")]

mapview::mapview(datos1, zcol = "localidad")
mapview::mapview(datos2, zcol = "localidad")

compare_df_cols(datos1, datos2)

summary(datos1); summary(datos2)

library(arsenal)

summary(comparedf(st_drop_geometry(datos1), st_drop_geometry(datos2)))







