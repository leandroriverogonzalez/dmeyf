#limpio la memoria
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
dataset  <- fread( "./datasetsOri/paquete_premium.csv.gz")

datos_freq <- dataset %>%
  group_by(foto_mes, clase_ternaria) %>%
  summarize(Freq=n())


datos_freq$mes_char <- as.character(datos_freq$foto_mes)

ggplot(datos_freq[datos_freq$clase_ternaria!='CONTINUA',], aes(fill=clase_ternaria, y=Freq, x=mes_char)) + 
  geom_bar(position="fill", stat="identity") + scale_x_discrete(guide = guide_axis(angle = 90))
