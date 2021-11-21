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


################################################
variable_ahora <- 'ctrx_quarter'
q = c(.25, .5, .75)
evaluo <- dataset_cfb2cont %>%
  group_by(foto_mes) %>%
  summarize(quant25 = quantile(eval(as.symbol(variable_ahora)), probs = q[1]), 
            quant50 = quantile(eval(as.symbol(variable_ahora)), probs = q[2]),
            quant75 = quantile(eval(as.symbol(variable_ahora)), probs = q[3]))


ggplot(dataset_cfb2cont, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
  geom_boxplot(outlier.shape=NA) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Fecha", y = paste(variable_ahora))
variables_estudio <- colnames(dataset)[2:length(colnames(dataset))-1]

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
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Todossinoutliers/",variable_ahora,"_histogram_per_month.pdf"))
    ggplot(dataset_cfb2cont, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + #ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
      geom_boxplot(outlier.shape=NA) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Todosconoutliers/",variable_ahora,"_histogram_per_month.pdf"))
    
  }, error=function(e){print(variable_ahora)})
}
######################################


######################################


for(variable_ahora in variables_estudio){
  tryCatch({
    evaluo <- dataset_cfb2cont %>%
      group_by(foto_mes) %>%
      summarize(quant25 = quantile(eval(as.symbol(variable_ahora)), probs = q[1]), 
                quant50 = quantile(eval(as.symbol(variable_ahora)), probs = q[2]),
                quant75 = quantile(eval(as.symbol(variable_ahora)), probs = q[3]))
    
    
    ggplot(dataset_cfb2cont, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + #ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
      geom_boxplot() +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Todosconoutliers/",variable_ahora,"_histogram_per_month.pdf"))
    
  }, error=function(e){print(variable_ahora)})
}
######################################


ggplot(dataset_cfb2cont, aes(x=as.factor(foto_mes), y=eval(as.symbol(variable_ahora)), fill=clase_ternaria)) + #ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
  geom_boxplot() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Fecha", y = paste(variable_ahora))




variable_ahora <- variables_estudio[30]




evaluo <- dataset_cfb2cont %>%
  group_by(foto_mes, clase_ternaria) %>%
  summarise(na = sum(is.na(eval(as.symbol(variable_ahora)))))

dataset_cfb2cont %>% 
  mutate(x = factor(foto_mes)) %>% 
  ggplot(aes(x = as.factor(foto_mes), y = eval(as.symbol(variable_ahora)), fill = clase_ternaria)) + 
  geom_bar(stat = "identity", position = "dodge") + scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Fecha", y = paste(variable_ahora))
  ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Datosconnan/",variable_ahora,"_barplot_per_month.png"))


for(variable_ahora in variables_estudio){
  tryCatch({

    evaluo <- dataset_cfb2cont %>%
      group_by(foto_mes, clase_ternaria) %>%
      summarise(na = sum(is.na(eval(as.symbol(variable_ahora)))))
      
    dataset_cfb2cont %>% 
      mutate(x = factor(foto_mes)) %>% 
      ggplot(aes(x = as.factor(foto_mes), y = eval(as.symbol(variable_ahora)), fill = clase_ternaria)) + 
      geom_bar(stat = "identity", position = "dodge") + scale_x_discrete(guide = guide_axis(angle = 90)) +
      labs(x = "Fecha", y = paste(variable_ahora))
    ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/Datosconnan/",variable_ahora,"_barplot_per_month.png"))
    
  }, error=function(e){print(variable_ahora)})
}
########

dataset_copy <- copy(dataset)
cols <- colnames(dataset_copy)
cols <- cols[4:length(cols)-1]
cols_rank <- paste0( cols, "_rango")
cols_nrank <- paste0( cols, "_nrango")
dataset_copy <- dataset_copy[ , paste0( cols, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="dense" ), 
         by= foto_mes,
         .SDcols= cols]
dataset_copy[, paste0( cols, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}), .SDcols = cols_rank]
for(colname in cols){
  dataset_copy[, eval(colname):=NULL]
}
for(colname in cols_rank){
  dataset_copy[, eval(colname):=NULL]
}




dataset_copy[, sum(is.na(.SD)),  .SDcols = 10+159]

ggplot(dataset_copy[dataset_copy$foto_mes==201908,], aes(x = Visa_mpagominimo_nrango, fill = foto_mes)) + 
  geom_histogram()

ggplot(dataset_copy[dataset_copy$foto_mes==202006,], aes(x = active_quarter_rango, fill = foto_mes)) + 
  geom_histogram()



dataset_copy[, .(number_of_distinct_orders = length(unique(Visa_mpagominimo_nrango))), by = foto_mes]




################
#Pruebo 
cols <- colnames(dataset)
cols <- cols[4:length(cols)-1]

Rango  <- function( dataset, cols )
{
  cols_rank <- paste0( cols, "_rango")
  cols_nrank <- paste0( cols, "_nrango")
  dataset <- dataset[ , paste0( cols, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="dense" ), 
                                by= foto_mes,
                                .SDcols= cols]
  dataset[, paste0( cols, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}), .SDcols = cols_rank]
  for(colname in cols){
    dataset[, eval(colname):=NULL]
  }
  for(colname in cols_rank){
    dataset[, eval(colname):=NULL]
  }
  
  ReportarCampos( dataset )
}
palancas$range  <- TRUE  #(va 1ero)
if( palancas$range)  Rango( dataset, cols_analiticas)

######################################
install.packages("data.table")
library(data.table)
colnames(dataset)

data_comp <- copy(dataset[,c('numero_de_cliente','foto_mes','clase_ternaria')])

object.size(data_comp)

# Write CSV
fwrite(data_comp, "/home/leandroriverogonzalez/dmeyf/estudio_datos/datos_comp.csv")


write.csv(data_comp, file=gzfile("/home/leandroriverogonzalez/dmeyf/estudio_datos/datos_comp.csv.gz"))




###########################################

dataset_copy <- copy(dataset)
cols <- colnames(dataset_copy)
cols <- cols[4:length(cols)-1]
cols_rank <- paste0( cols, "_rango")
cols_nrank <- paste0( cols, "_nrango")
dataset_copy <- dataset_copy[ , paste0( cols, "_rango") := lapply( .SD, frankv, na.last="keep", ties.method="dense" ), 
                              by= foto_mes,
                              .SDcols= cols]
dataset_copy[, paste0( cols, "_nrango") := lapply(.SD, function(x){(x - min(x)) / (max(x) - min(x))}), .SDcols = cols_rank]
for(colname in cols){
  dataset_copy[, eval(colname):=NULL]
}
for(colname in cols_rank){
  dataset_copy[, eval(colname):=NULL]
}
