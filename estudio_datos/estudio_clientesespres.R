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
library(reshape2)

#dataset  <- fread( "/home/leandro/Documents/Maestria/DMEF/TP2/dmeyf/estudio_datos/datos_comp.csv.gz")
dataset  <- fread( "/home/leandroriverogonzalez/dmeyf/estudio_datos/datos_comp.csv.gz")


n_clientes <- unique(dataset$numero_de_cliente)
data_clienteespre <- data.frame(n_clientes, rep('corto',length(n_clientes)))
colnames(data_clienteespre) <- c('n_cliente', 'espre')
j <- 0
for(cliente in n_clientes){
  ultimo <- 202101 %in% dataset[dataset$numero_de_cliente==cliente]$foto_mes
  primero <- 201801 %in% dataset[dataset$numero_de_cliente==cliente]$foto_mes
  if(ultimo & primero){
    data_clienteespre[data_clienteespre$n_cliente==cliente,]$espre = 'largo'
  }
  if(j%%10000==0){
    print(paste0('van: ', j, ' - de: ', dim(data_clienteespre)[1]))
  }
  j <- j+1
}

algo <- dcast(dataset, foto_mes ~ numero_de_cliente, value.var = "clase_ternaria")
colnames(algo)

rowSums(is.na(algo))

cliente_first <- dataset[dataset$foto_mes==201801,]$numero_de_cliente
cliente_last <- dataset[dataset$foto_mes==202101,]$numero_de_cliente
length(cliente_last)
length(cliente_first+cliente_last)

n_largos <- length(unique(c(cliente_first,cliente_last)))
n_todos <- length(unique(dataset$numero_de_cliente))
n_cortos <- n_todos - n_largos
dim(dataset[dataset$clase_ternaria=='BAJA+1'])[1]
