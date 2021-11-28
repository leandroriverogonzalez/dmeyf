require("data.table")
require("randomForest")
#install.packages("ggplot2")
library('ggplot2')


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#setwd( "~/buckets/b1/" )
directory.root  <-  "/home/leandroriverogonzalez/buckets/b1/"  #Google Cloud
setwd( directory.root )


#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
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
