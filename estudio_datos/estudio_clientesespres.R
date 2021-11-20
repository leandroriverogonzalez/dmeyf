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
library(dplyr)

dataset  <- fread( "/home/leandro/Documents/Maestria/DMEF/TP2/dmeyf/estudio_datos/datos_comp.csv.gz")


n_clientes <- unique(dataset$numero_de_cliente)
data_clienteespre <- data.frame(n_clientes, rep('corto',length(n_clientes)))
colnames(data_clienteespre) <- c('n_cliente', 'espre')
for(cliente in n_clientes){
  ultimo <- 202101 %in% dataset[dataset$numero_de_cliente==cliente]$foto_mes
  primero <- 201801 %in% dataset[dataset$numero_de_cliente==cliente]$foto_mes
  if(ultimo & primero){
    data_clienteespre[data_clienteespre$n_cliente==cliente,]$espre = 'largo'
  }
}




