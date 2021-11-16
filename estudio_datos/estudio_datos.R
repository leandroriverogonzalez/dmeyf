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


ggplot(datos_freq[datos_freq$clase_ternaria=='BAJA+2',], aes(fill=clase_ternaria, y=Freq, x=mes_char)) + scale_x_discrete(guide = guide_axis(angle = 90))

table(dataset$clase_ternaria, dataset$foto_mes)

dataf_tabla <- as.data.frame(table(dataset[dataset$foto_mes<202012]$clase_ternaria, dataset[dataset$foto_mes<202012]$foto_mes))

ggplot(dataf_tabla[dataf_tabla$Var1!='CONTINUA',],                                      # Grouped barplot using ggplot2
       aes(x = Var2,
           y = Freq,
           fill = Var1)) +
  geom_bar(stat = "identity",
           position = "dodge")  + scale_x_discrete(guide = guide_axis(angle = 90)) +
            labs(title = "Fecuencias de Baja+1 y Baja+2 por mes-año", x = "Fecha", y = "Frecuencia") 


dataset_wf <- dataset[dataset$foto_mes<202012]

ggplot(dataset_wf[dataset_wf$clase_ternaria=='BAJA+2'], aes(x = ctrx_quarter, group = as.factor(foto_mes))) + 
  geom_histogram(bins = 10)

dataset_wfb2 <- dataset_wf[dataset_wf$clase_ternaria=='BAJA+2']

boxplot(ctrx_quarter~foto_mes, data=dataset_wf)

q = c(.25, .5, .75)

variables_estudio <- colnames(dataset_wfb2)[2:length(colnames(dataset_wfb2))-1]

for(variable_ahora in variables_estudio){
  tryCatch({
  evaluo <- dataset_wfb2 %>%
    group_by(foto_mes) %>%
    summarize(quant25 = quantile(eval(as.symbol(variable_ahora)), probs = q[1]), 
              quant50 = quantile(eval(as.symbol(variable_ahora)), probs = q[2]),
              quant75 = quantile(eval(as.symbol(variable_ahora)), probs = q[3]))
  
  ggplot(data = dataset_wfb2, aes(x=as.factor(foto_mes),y=eval(as.symbol(variable_ahora))))+geom_boxplot(outlier.shape=NA)+ ylim(min(evaluo$quant25) - (max(evaluo$quant75) - min(evaluo$quant25))/10, max(evaluo$quant75) + (max(evaluo$quant75) - min(evaluo$quant25))/10) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "Fecha", y = paste(variable_ahora))
  ggsave(paste0("/home/leandroriverogonzalez/dmeyf/estudio_datos/",variable_ahora,"_histogram_per_month.pdf"))
  }, error=function(e){})
}


ggplot(data = dataset_wfb2, aes(x=as.factor(foto_mes),y=mcaja_ahorro))+geom_boxplot(outlier.shape=NA)+ ylim(0, 10) +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(data = dataset_wfb2, aes(x=as.factor(foto_mes),y=ctarjeta_visa_transacciones))+geom_boxplot(outlier.shape=NA)+ ylim(0, 10) +
  scale_x_discrete(guide = guide_axis(angle = 90))

ggplot(data = dataset_wfb2, aes(x=as.factor(foto_mes),y=mcuentas_saldo))+geom_boxplot(outlier.shape=NA)+ ylim(0, 10) +
  scale_x_discrete(guide = guide_axis(angle = 90))

