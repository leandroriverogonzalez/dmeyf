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
`%notin%` <- Negate('%in%')

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

##############################
#esto proximo tarda muchisimo, nunca lo termine de correr
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
##############################

# algo <- dcast(dataset, foto_mes ~ numero_de_cliente, value.var = "clase_ternaria")
# colnames(algo)
# 
# rowSums(is.na(algo))

cliente_first <- dataset[dataset$foto_mes==201801,]$numero_de_cliente
cliente_last <- dataset[dataset$foto_mes==202101,]$numero_de_cliente
length(cliente_last)
length(cliente_first+cliente_last)

n_largos <- length(unique(c(cliente_first,cliente_last)))
n_todos <- length(unique(dataset$numero_de_cliente))
n_cortos <- n_todos - n_largos
dim(dataset[dataset$clase_ternaria=='BAJA+1'])[1]

clientes_espres <- unique(dataset$numero_de_cliente)[unique(dataset$numero_de_cliente) %notin% unique(c(cliente_first,cliente_last))]

data_cespres <- dataset[dataset$numero_de_cliente %in% clientes_espres,]

data_cespres


##############################
#esto proximo tarda muchisimo, nunca lo termine de correr
n_clientes <- unique(data_cespres$numero_de_cliente)
data_clienteespre <- data.frame(n_clientes, rep(0,length(n_clientes)), rep(0,length(n_clientes)), rep(0,length(n_clientes)))
colnames(data_clienteespre) <- c('n_cliente', 'tiempo_meses', 'primer_mes', 'ultimo_mes')
j <- 0
for(cliente in n_clientes){
  ultimo <- max(data_cespres[data_cespres$numero_de_cliente==cliente]$foto_mes)
  primero <- min(data_cespres[data_cespres$numero_de_cliente==cliente]$foto_mes)
  anio_i <- as.numeric(substr(primero, 1, 4))
  anio_f <- as.numeric(substr(ultimo,1,4))
  mes_i <- as.numeric(substr(primero,5,6))
  mes_f <- as.numeric(substr(ultimo,5,6))
  f_i <- as.Date(paste0(anio_i,'-' ,mes_i,'-1'))
  f_f <- as.Date(paste0(anio_f,'-' ,mes_f,'-1'))
  data_clienteespre[data_clienteespre$n_cliente==cliente,]$tiempo_meses = elapsed_months(f_f, f_i)
  data_clienteespre[data_clienteespre$n_cliente==cliente,]$primer_mes = primero
  data_clienteespre[data_clienteespre$n_cliente==cliente,]$ultimo_mes = ultimo
  
  if(j%%1000==0){
    print(paste0('van: ', j, ' - de: ', dim(data_clienteespre)[1]))
  }
  j <- j+1
}
##############################

ggplot(data_clienteespre, aes(x=as.factor(primer_mes))) + 
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(data_clienteespre, aes(x=as.factor(ultimo_mes))) + 
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(dataset[dataset$clase_ternaria=='BAJA+2'], aes(x=as.factor(foto_mes))) + 
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 90))


ggplot(data_clienteespre, aes(x=tiempo_meses)) + 
  geom_histogram(binwidth=1)


