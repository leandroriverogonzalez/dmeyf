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
library(plyr)

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

for(variable_ahora in variables_estudio[94:length(variables_estudio)]){
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

variables_estudio <- colnames(dataset)[2:length(colnames(dataset))-1]

variable_ahora <- variables_estudio[10]

dim(dataset_cfb2cont[,c('foto_mes', variable_ahora, 'clase_ternaria')])

dataset_cfb2cont[,c('foto_mes', 'ctrx_quarter', 'clase_ternaria')] %>%
  filter(is.na('ctrx_quarter'))  %>%
  count(col_1)

dataset_cfb2cont[,c('foto_mes', 'ctrx_quarter', 'clase_ternaria')] %>% group_by('foto_mes','clase_ternaria') %>% summarise(non_na_count = sum(is.na('ctrx_quarter')))


