rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")
install.packages("ggplot2")
library('ggplot2')
library('tidyverse')
require("lightgbm")
directory.root  <-  "~/buckets/b1/"  #Google Cloud
setwd( directory.root )
library(dplyr)
library(stringr)
library(stringr)

datos_modelossanti  <- read.csv( "/home/leandroriverogonzalez/buckets/b1/datos_presentacion/modelos_bo.csv")
datos_modeloslean1  <- fread( "/home/leandroriverogonzalez/buckets/b1/datos_presentacion/work_E1019_E1019_1420vranknorm14__vmin_lgbm_BOlog.txt")
datos_modeloslean2  <- fread( "/home/leandroriverogonzalez/buckets/b1/datos_presentacion/work_E1021_E1021_1420vranknormrandom14__vmin_lgbm_BOlog.txt")

datos_modelossanti = datos_modelossanti[datos_modelossanti$modelo!='',]
datos_modelossanti = datos_modelossanti[datos_modelossanti$modelo!='Modelo 2',]
datos_modelossanti <- datos_modelossanti %>% filter(str_detect(modelo, '^Modelo '))

for(i in c(1,3,4,5,6,7,8,9)){
  datos_modelossanti[datos_modelossanti$modelo == paste('Modelo',as.character(i)), ]$modelo = as.character(i)
}


ggplot(datos_modelossanti, aes(x=as.factor(modelo), y=ganancia_acum_test, fill=modelo)) + 
  geom_boxplot() + theme(legend.position="none")  +
  labs(x = 'Modelo', y = 'Ganancia acumulada test')

