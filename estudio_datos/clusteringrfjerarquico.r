require("data.table")
require("randomForest")
#install.packages("ggplot2")
library('ggplot2')
library(dendextend)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#setwd( "~/buckets/b1/" )
directory.root  <-  "/home/leandroriverogonzalez/buckets/b1/"  #Google Cloud
setwd( directory.root )


#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset_ori  <- fread( "/home/leandroriverogonzalez/buckets/b1/datasetsOri/paquete_premium.csv.gz", stringsAsFactors= TRUE)
dataset  <- fread( "/home/leandroriverogonzalez/buckets/b1/datasetsOri/paquete_premium.csv.gz", stringsAsFactors= TRUE)
gc()

dataset_completo <- copy(dataset)

#achico el dataset
dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  clase_ternaria =="BAJA+1"  & foto_mes>=202001  & foto_mes<=202011, ]
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")


#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter


fwrite( dataset,
        paste0( "./datasets/clustering.csv.gz" ),
        logical01 = TRUE,
        sep= "," )

colnames(dataset)

dataset_f <- copy(dataset[dataset$foto_mes>202006,])


for(variable_ahora in campos_buenos){
  tryCatch({
    ggplot(dataset_f, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=as.factor(cluster2))) + #ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
      geom_boxplot() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
      ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/clustersrf/",variable_ahora,"_histogram_per_month.pdf"))
  }, error=function(e){print(variable_ahora)})
}


###########################################################

dataset_baja2 <- copy(dataset_completo[dataset_completo$clase_ternaria=='BAJA+2',])

clientes_b2 <- unique(dataset_baja2$numero_de_cliente)

dataset_ncb2 <- copy(dataset_completo[dataset_completo$numero_de_cliente %in% clientes_b2,])

algo <- dcast(dataset_ncb2, rowid(numero_de_cliente) ~ numero_de_cliente, value.var = "clase_ternaria")

algo <- dcast(dataset_ncb2, as.factor(foto_mes) ~ numero_de_cliente, value.var = "clase_ternaria")

dim(algo[2,]=='BAJA+2')
meses <- unique(dataset_ncb2$foto_mes)
algo2 <- algo[,algo[1,]=='BAJA+2']
for(j in seq(1,length(meses))){
  print(j)
  
  data_este <- copy(dataset_ncb2[(dataset_ncb2$foto_mes==meses[j]) & (dataset_ncb2$clase_ternaria=='BAJA+2'),])
  print(dim(data_este[data_este$foto_mes>meses[j],]))
}



#######################################################################
##################### Busco clientes suba

meses <- unique(dataset$foto_mes)
clientessuba <- c()
nclientespmes <- c()
for(mes in meses){
  clientes_ahora <- dataset_completo[(dataset_completo$foto_mes==mes) & (dataset_completo$clase_ternaria=='BAJA+1'),]$numero_de_cliente
  df_este <- copy(dataset_completo[dataset_completo$numero_de_cliente %in% clientes_ahora,])
  length(unique(df_este[df_este$foto_mes>mes,]$numero_de_cliente))
  print(paste0('mes: ', mes, ' - cantidad de bajas: ', length(unique(df_este$numero_de_cliente)), ' - cantidad de bajas que vuelven: ',length(unique(df_este[df_este$foto_mes>mes,]$numero_de_cliente))))
  nclientespmes <- c(nclientespmes, length(unique(df_este[df_este$foto_mes>mes,]$numero_de_cliente)))
  clientessuba <- c(clientessuba, unique(df_este[df_este$foto_mes>mes,]$numero_de_cliente))
}

plot(as.factor(meses), nclientespmes)

#######################################################################



#######################################################################
##################### Aca veo que onda los que son sube, es decir que se dieron de baja y luego volvieron a estado continua
dataset$sube2 <- 0
dataset[dataset$numero_de_cliente %in% clientessuba,]$sube2 <- 1

cluster_sube <- c()
for(i in seq(1,7)){
  valor_este <- table(dataset[dataset$cluster2==i,]$sube2)[2]/table(dataset[dataset$cluster2==i,]$sube2)[1]
  cluster_sube <- c(cluster_sube, valor_este)
}

#######################################################################

#######################################################################
##################### Otro tipo de clientes son los que se dan de baja rapido
`%notin%` <- Negate('%in%')
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
cliente_first <- dataset_ori[dataset_ori$foto_mes==201801,]$numero_de_cliente
cliente_last <- dataset_ori[dataset_ori$foto_mes==202101,]$numero_de_cliente
length(cliente_last)
length(cliente_first+cliente_last)

n_largos <- length(unique(c(cliente_first,cliente_last)))
n_todos <- length(unique(dataset_ori$numero_de_cliente))
n_cortos <- n_todos - n_largos
dim(dataset_ori[dataset_ori$clase_ternaria=='BAJA+1'])[1]

clientes_espres <- unique(dataset_ori$numero_de_cliente)[unique(dataset_ori$numero_de_cliente) %notin% unique(c(cliente_first,cliente_last))]

dataset$espres <- 0
dataset[dataset$numero_de_cliente %in% clientes_espres,]$espres <- 1

cluster_espres <- c()
for(i in seq(1,7)){
  valor_este <- table(dataset[dataset$cluster2==i,]$espres)[2]/table(dataset[dataset$cluster2==i,]$espres)[1]
  cluster_espres <- c(cluster_espres, valor_este)
}

cbf_7 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
           "#F0E442", "#0072B2", "#D55E00")

ggplot(dataset, aes(x=Visa_mconsumosdolares, y=Visa_mpagosdolares, color=factor(cluster2))) + 
  geom_point(size=6)  #+ scale_color_manual(values=cbf_7)


i <- 21
j <- 19
ggplot(dataset, aes(x=eval(as.symbol(campos_buenos[i])), y=eval(as.symbol(campos_buenos[j])), color=factor(cluster2))) + 
  geom_point(size=6)  + labs(x = paste(campos_buenos[i]), y = paste(campos_buenos[j]))

a <- 'Visa_cconsumos' 
b <- 'ccomisiones_otras'
  
ggplot(dataset, aes(x=eval(as.symbol(a)), y=eval(as.symbol(b)), color=factor(cluster2))) + 
  geom_point(size=1)  + labs(x = paste(a), y = paste(b))


a <- 'mcomisiones_otras' 
b <- 'ccomisiones_otras'

ggplot(dataset, aes(x=eval(as.symbol(a)), y=eval(as.symbol(b)), color=factor(cluster2))) + 
  geom_point(size=1)  + labs(x = paste(a), y = paste(b))


campos_scatter  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", 
                      "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Master_fechaalta", "chomebanking_transacciones", 
                     "mrentabilidad", "Master_Fvencimiento", "mcuenta_corriente",
                     "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "Master_mfinanciacion_limite",
                     "cproductos", "mcomisiones_otras",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "mtransferencias_emitidas", "cluster2")

data_scatter <- dataset[ , campos_scatter, with=FALSE ]

pairs(data_scatter, col=data_scatter$cluster2)

library(tidyr)

coupleg <- crossing(var1 = campos_scatter, var2 = campos_scatter)

ggplot(dataset, aes(x=eval(as.symbol(a)), y=eval(as.symbol(b)), color=factor(cluster2))) + 
geom_point(size=1)  + labs(x = paste(a), y = paste(b))
dim(coupleg)[1]
coupleg$var1[1]
for(i in seq(1,dim(coupleg)[1])){
  tryCatch({
    var1 <- coupleg$var1[i]
    var2 <- coupleg$var2[i]
    ggplot(dataset, aes(x=eval(as.symbol(var1)), y=eval(as.symbol(var2)), color=factor(cluster2))) + 
      geom_point(size=4)  + labs(x = paste(var1), y = paste(var2))+ guides(colour = guide_legend(override.aes = list(size=6)))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/clustersrf/scatter/",var1,'-',var2,"_scatter.pdf"))
  }, error=function(e){print(variable_ahora)})
}

ggplot(dataset, aes(x=eval(as.symbol(var1)), y=eval(as.symbol(var2)), color=factor(cluster2))) + 
  geom_point(size=4)  + labs(x = paste(var1), y = paste(var2))+ guides(colour = guide_legend(override.aes = list(size=6)))
