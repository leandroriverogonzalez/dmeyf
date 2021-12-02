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

#dataset_comp  <- fread( "/home/leandro/Documents/Maestria/DMEF/TP2/dmeyf/estudio_datos/datos_comp.csv.gz")
dataset_comp  <- fread( "/home/leandroriverogonzalez/dmeyf/estudio_datos/datos_comp.csv.gz")
dataset  <- fread( "./datasetsOri/paquete_premium.csv.gz")

`%notin%` <- Negate('%in%')

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

##############################
#esto proximo tarda muchisimo, nunca lo termine de correr
# n_clientes <- unique(dataset_comp$numero_de_cliente)
# data_clienteespre <- data.frame(n_clientes, rep('corto',length(n_clientes)))
# colnames(data_clienteespre) <- c('n_cliente', 'espre')
# j <- 0
# for(cliente in n_clientes){
#   ultimo <- 202101 %in% dataset_comp[dataset_comp$numero_de_cliente==cliente]$foto_mes
#   primero <- 201801 %in% dataset_comp[dataset_comp$numero_de_cliente==cliente]$foto_mes
#   if(ultimo & primero){
#     data_clienteespre[data_clienteespre$n_cliente==cliente,]$espre = 'largo'
#   }
#   if(j%%10000==0){
#     print(paste0('van: ', j, ' - de: ', dim(data_clienteespre)[1]))
#   }
#   j <- j+1
# }
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
dataset <- dataset[(dataset$foto_mes>=201801) & (dataset$foto_mes<=201901),]

cliente_first <- dataset[dataset$foto_mes==201801,]$numero_de_cliente
cliente_last <- dataset[dataset$foto_mes==201901,]$numero_de_cliente
length(cliente_last)
length(cliente_first+cliente_last)

n_largos <- length(unique(c(cliente_first,cliente_last)))
n_todos <- length(unique(dataset$numero_de_cliente))
n_cortos <- n_todos - n_largos
dim(dataset[dataset$clase_ternaria=='BAJA+1'])[1]

clientes_espres <- unique(dataset$numero_de_cliente)[unique(dataset$numero_de_cliente) %notin% unique(c(cliente_first,cliente_last))]

meses <- unique(dataset$foto_mes)

dataset$baja3 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$clase_ternaria=='BAJA+2'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja3 <- 'si'
}

dataset$baja4 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja3=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja4 <- 'si'
}

dataset$baja5 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja4=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja5 <- 'si'
}

dataset$baja6 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja5=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja6 <- 'si'
}

dataset$baja7 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja6=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja7 <- 'si'
}

dataset$baja8 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja7=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja8 <- 'si'
}

dataset$baja9 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja8=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja9 <- 'si'
}

dataset$baja10 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja9=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja10 <- 'si'
}


dataset$baja11 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja10=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja11 <- 'si'
}


dataset$baja12 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja11=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja12 <- 'si'
}

dataset$baja13 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja12=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja13 <- 'si'
}

dataset$baja14 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja13=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja14 <- 'si'
}

dataset$baja15 <- 'no'
for(i in seq(2,length(meses))){
  mes <- meses[i]
  mesa <- meses[i-1]
  clientes_ahora <- dataset[(dataset$foto_mes==mes) & (dataset$baja14=='si'),]$numero_de_cliente
  dataset[(dataset$foto_mes==mesa) & (dataset$numero_de_cliente %in% clientes_ahora),]$baja15 <- 'si'
}


ncsb1 <- sum((dataset$cpayroll_trx>0) & (dataset$clase_ternaria=='BAJA+1'))
nb1 <- sum((dataset$clase_ternaria=='BAJA+1'))
pcsb1 <- ncsb1/nb1

ncsb2 <- sum((dataset$cpayroll_trx>0) & (dataset$clase_ternaria=='BAJA+2'))
nb2 <- sum((dataset$clase_ternaria=='BAJA+2'))
pcsb2 <- ncsb2/nb2

ncsb3 <- sum((dataset$cpayroll_trx>0) & (dataset$baja3=='si'))
nb3 <- sum((dataset$baja3=='si'))
pcsb3 <- ncsb3/nb3

ncsb4 <- sum((dataset$cpayroll_trx>0) & (dataset$baja4=='si'))
nb4 <- sum((dataset$baja4=='si'))
pcsb4 <- ncsb4/nb4

ncsb5 <- sum((dataset$cpayroll_trx>0) & (dataset$baja5=='si'))
nb5 <- sum((dataset$baja5=='si'))
pcsb5 <- ncsb5/nb5

ncsb6 <- sum((dataset$cpayroll_trx>0) & (dataset$baja6=='si'))
nb6 <- sum((dataset$baja6=='si'))
pcsb6 <- ncsb6/nb6

ncsb7 <- sum((dataset$cpayroll_trx>0) & (dataset$baja7=='si'))
nb7 <- sum((dataset$baja7=='si'))
pcsb7 <- ncsb7/nb7

ncsb8 <- sum((dataset$cpayroll_trx>0) & (dataset$baja8=='si'))
nb8 <- sum((dataset$baja8=='si'))
pcsb8 <- ncsb8/nb8

ncsb9 <- sum((dataset$cpayroll_trx>0) & (dataset$baja9=='si'))
nb9 <- sum((dataset$baja9=='si'))
pcsb9 <- ncsb9/nb9

ncsb10 <- sum((dataset$cpayroll_trx>0) & (dataset$baja10=='si'))
nb10 <- sum((dataset$baja10=='si'))
pcsb10 <- ncsb10/nb10

ncsb11 <- sum((dataset$cpayroll_trx>0) & (dataset$baja11=='si'))
nb11 <- sum((dataset$baja11=='si'))
pcsb11 <- ncsb11/nb11

ncsb12 <- sum((dataset$cpayroll_trx>0) & (dataset$baja12=='si'))
nb12 <- sum((dataset$baja12=='si'))
pcsb12 <- ncsb12/nb12

ncsb13 <- sum((dataset$cpayroll_trx>0) & (dataset$baja13=='si'))
nb13 <- sum((dataset$baja13=='si'))
pcsb13 <- ncsb13/nb13

ncsb14 <- sum((dataset$cpayroll_trx>0) & (dataset$baja14=='si'))
nb14 <- sum((dataset$baja14=='si'))
pcsb14 <- ncsb14/nb14

ncsb15 <- sum((dataset$cpayroll_trx>0) & (dataset$baja15=='si'))
nb15 <- sum((dataset$baja15=='si'))
pcsb15 <- ncsb15/nb15


pcsb15a1 <- c(pcsb15, pcsb14, pcsb13, pcsb12, pcsb11, pcsb10, pcsb9, pcsb8, pcsb7, pcsb6, pcsb5, pcsb4, pcsb3, pcsb2, pcsb1)

dfpcsb <- data.frame(seq(15,1), pcsb15a1)

colnames(dfpcsb) <- c('Baja', 'Frecuencia')

ggplot(dfpcsb, aes(x=Baja, y=Frecuencia)) + geom_point() + scale_x_continuous(trans='reverse')

#sum((dataset$cpayroll_trx>0))
#dim(dataset)

##############################
colrent <- c('mrentabilidad')

cols_rank <- paste0( colrent, "_rango")
cols_nrank <- paste0( colrent, "_nrango")
dataset <- dataset[ , paste0( colrent, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="random" ), 
                      by= foto_mes,
                      .SDcols= colrent]
dataset[, paste0( colrent, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}),
         by= foto_mes,
         .SDcols = cols_rank]
for(colname in cols_rank){
  dataset[, eval(colname):=NULL]
}


mrrango1 <- mean(dataset[dataset$clase_ternaria=='BAJA+1',]$mrentabilidad_nrango)
mrrango2 <- mean(dataset[dataset$clase_ternaria=='BAJA+2',]$mrentabilidad_nrango)
mrrango3 <- mean(dataset[dataset$baja3=='si',]$mrentabilidad_nrango)
mrrango4 <- mean(dataset[dataset$baja4=='si',]$mrentabilidad_nrango)
mrrango5 <- mean(dataset[dataset$baja5=='si',]$mrentabilidad_nrango)
mrrango6 <- mean(dataset[dataset$baja6=='si',]$mrentabilidad_nrango)
mrrango7 <- mean(dataset[dataset$baja7=='si',]$mrentabilidad_nrango)
mrrango8 <- mean(dataset[dataset$baja8=='si',]$mrentabilidad_nrango)
mrrango9 <- mean(dataset[dataset$baja9=='si',]$mrentabilidad_nrango)
mrrango10 <- mean(dataset[dataset$baja10=='si',]$mrentabilidad_nrango)
mrrango11 <- mean(dataset[dataset$baja11=='si',]$mrentabilidad_nrango)
mrrango12 <- mean(dataset[dataset$baja12=='si',]$mrentabilidad_nrango)
mrrango13 <- mean(dataset[dataset$baja13=='si',]$mrentabilidad_nrango)
mrrango14 <- mean(dataset[dataset$baja14=='si',]$mrentabilidad_nrango)
mrrango15 <- mean(dataset[dataset$baja15=='si',]$mrentabilidad_nrango)

mrrango15a1 <- c(mrrango15, mrrango14, mrrango13, mrrango12, mrrango11, mrrango10, mrrango9, mrrango8, mrrango7, mrrango6, mrrango5, mrrango4, mrrango3, mrrango2, mrrango1)

dfmrrango1 <- data.frame(seq(15,1), mrrango15a1)

colnames(dfmrrango1) <- c('Baja', 'Monto_rentabilidad')

ggplot(dfmrrango1, aes(x=Baja, y=Monto_rentabilidad)) + geom_point() + scale_x_continuous(trans='reverse') +
labs(x = 'Baja', y = 'Monto rentabilidad [%]')

##############################

mprestamos_prendarios
colrent <- c('mprestamos_prendarios')

cols_rank <- paste0( colrent, "_rango")
cols_nrank <- paste0( colrent, "_nrango")
dataset <- dataset[ , paste0( colrent, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="random" ), 
                    by= foto_mes,
                    .SDcols= colrent]
dataset[, paste0( colrent, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}),
        by= foto_mes,
        .SDcols = cols_rank]
for(colname in cols_rank){
  dataset[, eval(colname):=NULL]
}


mpp1 <- mean(dataset[dataset$clase_ternaria=='BAJA+1',]$mprestamos_prendarios_nrango)
mpp2 <- mean(dataset[dataset$clase_ternaria=='BAJA+2',]$mprestamos_prendarios_nrango)
mpp3 <- mean(dataset[dataset$baja3=='si',]$mprestamos_prendarios_nrango)
mpp4 <- mean(dataset[dataset$baja4=='si',]$mprestamos_prendarios_nrango)
mpp5 <- mean(dataset[dataset$baja5=='si',]$mprestamos_prendarios_nrango)
mpp6 <- mean(dataset[dataset$baja6=='si',]$mprestamos_prendarios_nrango)
mpp7 <- mean(dataset[dataset$baja7=='si',]$mprestamos_prendarios_nrango)
mpp8 <- mean(dataset[dataset$baja8=='si',]$mprestamos_prendarios_nrango)
mpp9 <- mean(dataset[dataset$baja9=='si',]$mprestamos_prendarios_nrango)
mpp10 <- mean(dataset[dataset$baja10=='si',]$mprestamos_prendarios_nrango)
mpp11 <- mean(dataset[dataset$baja11=='si',]$mprestamos_prendarios_nrango)
mpp12 <- mean(dataset[dataset$baja12=='si',]$mprestamos_prendarios_nrango)
mpp13 <- mean(dataset[dataset$baja13=='si',]$mprestamos_prendarios_nrango)
mpp14 <- mean(dataset[dataset$baja14=='si',]$mprestamos_prendarios_nrango)
mpp15 <- mean(dataset[dataset$baja15=='si',]$mprestamos_prendarios_nrango)

mpp15a1 <- c(mpp15, mpp14, mpp13, mpp12, mpp11, mpp10, mpp9, mpp8, mpp7, mpp6, mpp5, mpp4, mpp3, mpp2, mpp1)

dfmpp <- data.frame(seq(15,1), mpp15a1)

colnames(dfmpp) <- c('Baja', 'Monto_prendario')

ggplot(dfmpp, aes(x=Baja, y=Monto_prendario)) + geom_point() + scale_x_continuous(trans='reverse') +
  labs(x = 'Baja', y = 'Monto prendario [%]')



##############################
mcomisiones
colrent <- c('mcomisiones')

cols_rank <- paste0( colrent, "_rango")
cols_nrank <- paste0( colrent, "_nrango")
dataset <- dataset[ , paste0( colrent, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="random" ), 
                    by= foto_mes,
                    .SDcols= colrent]
dataset[, paste0( colrent, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}),
        by= foto_mes,
        .SDcols = cols_rank]
for(colname in cols_rank){
  dataset[, eval(colname):=NULL]
}


mcom1 <- mean(dataset[dataset$clase_ternaria=='BAJA+1',]$mcomisiones_nrango)
mcom2 <- mean(dataset[dataset$clase_ternaria=='BAJA+2',]$mcomisiones_nrango)
mcom3 <- mean(dataset[dataset$baja3=='si',]$mcomisiones_nrango)
mcom4 <- mean(dataset[dataset$baja4=='si',]$mcomisiones_nrango)
mcom5 <- mean(dataset[dataset$baja5=='si',]$mcomisiones_nrango)
mcom6 <- mean(dataset[dataset$baja6=='si',]$mcomisiones_nrango)
mcom7 <- mean(dataset[dataset$baja7=='si',]$mcomisiones_nrango)
mcom8 <- mean(dataset[dataset$baja8=='si',]$mcomisiones_nrango)
mcom9 <- mean(dataset[dataset$baja9=='si',]$mcomisiones_nrango)
mcom10 <- mean(dataset[dataset$baja10=='si',]$mcomisiones_nrango)
mcom11 <- mean(dataset[dataset$baja11=='si',]$mcomisiones_nrango)
mcom12 <- mean(dataset[dataset$baja12=='si',]$mcomisiones_nrango)
mcom13 <- mean(dataset[dataset$baja13=='si',]$mcomisiones_nrango)
mcom14 <- mean(dataset[dataset$baja14=='si',]$mcomisiones_nrango)
mcom15 <- mean(dataset[dataset$baja15=='si',]$mcomisiones_nrango)

mcom15a1 <- c(mcom15, mcom14, mcom13, mcom12, mcom11, mcom10, mcom9, mcom8, mcom7, mcom6, mcom5, mcom4, mcom3, mcom2, mcom1)

dfmcom <- data.frame(seq(15,1), mcom15a1)

colnames(dfmcom) <- c('Baja', 'Monto_comisiones')

ggplot(dfmcom, aes(x=Baja, y=Monto_comisiones)) + geom_point() + scale_x_continuous(trans='reverse') +
  labs(x = 'Baja', y = 'Monto comisiones [%]')



##############################

datarent <- copy(dataset[, c('numero_de_cliente', 'foto_mes', 'mrentabilidad', 'clase_ternaria'), with = FALSE])
colrent <- c('mrentabilidad')

cols_rank <- paste0( colrent, "_rango")
cols_nrank <- paste0( colrent, "_nrango")
datarent <- datarent[ , paste0( colrent, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="random" ), 
                    by= foto_mes,
                    .SDcols= colrent]
datarent[, paste0( colrent, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}),
         by= foto_mes,
         .SDcols = cols_rank]
for(colname in colrent){
  datarent[, eval(colname):=NULL]
}
for(colname in cols_rank){
  datarent[, eval(colname):=NULL]
}

mean(datarent[datarent$clase_ternaria=='BAJA+1',]$mrentabilidad_nrango>.9)
mean(datarent[datarent$mrentabilidad_nrango>.9,]$clase_ternaria=='BAJA+1')
mean(datarent[datarent$mrentabilidad_nrango>.9,]$clase_ternaria=='BAJA+2')
mean(datarent$mrentabilidad_nrango>.9)

datarent[datarent$mrentabilidad_nrango>.9,]$clase_ter


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

################################################
#Lo copado esta acá
dataset_wfb2 <- dataset[dataset$clase_ternaria=='BAJA+2']
dataset_wfb2 <- dataset_wfb2[dataset_wfb2$foto_mes<202012]

dataset_wfcont <- dataset[dataset$clase_ternaria=='CONTINUA']
dataset_wfcont <- dataset_wfcont[dataset_wfcont$foto_mes<202012]

dataset_final <- dataset[dataset$foto_mes>202011]
dataset_final <- dataset_final[dataset_final$clase_ternaria!='BAJA+1']

dataset_cfb2 <- rbind(dataset_wfb2, dataset_final)

dataset_cfb2cont <- rbind(dataset_cfb2, dataset_wfcont)

dataset_cfb2cont[dataset_cfb2cont$clase_ternaria=='']$clase_ternaria = 'nosesabe'
q = c(.25, .5, .75)

variables_estudio <- colnames(dataset)[2:length(colnames(dataset))-1]

colnames(dataset_cfb2cont)

dataset_cfb2cont[(dataset_cfb2cont$numero_de_cliente %in% clientes_espres) & (dataset_cfb2cont$clase_ternaria=='BAJA+2'),]$clase_ternaria = 'espres'

variable_ahora <- 'mtarjeta_visa_consumo'


for(variable_ahora in variables_estudio){
  tryCatch({
    
    evaluo <- dataset_cfb2cont %>%
      group_by(foto_mes) %>%
      summarize(quant25 = quantile(eval(as.symbol(variable_ahora)), probs = q[1]), 
                quant50 = quantile(eval(as.symbol(variable_ahora)), probs = q[2]),
                quant75 = quantile(eval(as.symbol(variable_ahora)), probs = q[3]))
    
    
    ggplot(dataset_cfb2cont, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
      geom_boxplot(outlier.shape=NA) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Todossinoutliers_yespres/",variable_ahora,"_histogram_per_month.pdf"), width = 1366/72, height = 768/72, dpi = 72)
    
  }, error=function(e){print(variable_ahora)})
}


variables_co <- c("mprestamos_hipotecarios", "mtarjeta_visa_descuentos","mtarjeta_master_descuentos","tmobile_app","cmobile_app_trx",
"Master_delinquency","Master_status","Master_mfinanciacion_limite","Master_Fvencimiento","Master_Finiciomora",
"Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares",
"Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_fultimo_cierre","Master_mpagado",
"Master_mpagospesos","Master_mpagosdolares","Master_fechaalta","Master_mconsumototal","Master_cconsumos",
"Master_cadelantosefectivo","Master_mpagominimo","Visa_delinquency","Visa_status","Visa_mfinanciacion_limite",
"Visa_Fvencimiento","Visa_Finiciomora","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos",
"Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_fultimo_cierre",
"Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_fechaalta","Visa_mconsumototal","Visa_cconsumos",
"Visa_cadelantosefectivo","Visa_mpagominimo")

ktrain_subsampling <- 0.1
dataset_subs <- copy(dataset_cfb2cont)

vector_azar  <- runif( nrow(dataset_cfb2cont) )
dataset_subs <- dataset_subs[ !(( clase_ternaria=='CONTINUA') & (vector_azar > ktrain_subsampling )),  ]  

for(variable_ahora in variables_co){
  tryCatch({
    ggplot(dataset_subs, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + 
      geom_boxplot() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Todosconoutliers_yespres/",variable_ahora,"_histogram_per_month_conoutliers_10percent.pdf"), width = 1366/72, height = 768/72, dpi = 72)
    
  }, error=function(e){print(variable_ahora)})
}



########
# Estudio la tendencia de los que no son espres
library(BBmisc)

data_sespres <- copy(dataset_cfb2cont[dataset_cfb2cont$numero_de_cliente %notin% clientes_espres,])
foto_meses <- sort(unique(data_b2$foto_mes))




# data_mesbaja[data_mesbaja$numero_de_cliente==6255924,c('foto_mes','clase_ternaria')]

mes_min <- 5
for(i in mes_min:30){
  data_mesbaja <- copy(data_sespres[data_sespres$numero_de_cliente %in% (data_sespres[(data_sespres$foto_mes==foto_meses[i]) & (data_sespres$clase_ternaria=='BAJA+2'),]$numero_de_cliente),])
  print('primer paso')
  testeo <- dcast(data_mesbaja[,c('Master_msaldototal','numero_de_cliente','foto_mes')], foto_mes ~ numero_de_cliente, value.var = "Master_msaldototal")
  fotos_estos <- copy(testeo$foto_mes)
  testeo2 <- normalize(testeo, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
  testeo2$foto_mes <- fotos_estos
  if(i==1){
    plot(as.factor(fotos_estos)[1:i],rowMeans(testeo2[,-c(1)], na.rm = TRUE)[1:i],type='l')
  }else{
    lines(as.factor(fotos_estos)[1:i],rowMeans(testeo2[,-c(1)], na.rm = TRUE)[1:i])
  }
  print(i)
}
