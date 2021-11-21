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
