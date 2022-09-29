#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("./") # Establezco el Working Directory
semillas <- c(864379, 300647, 125707, 962303, 983363)

# cargo el dataset
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia1_2022.csv")

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
    formula= "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
    data= dtrain,
    model= TRUE,
    xval= 0,
    cp= -1,
    minsplit= 2, # dejo que crezca y corte todo lo que quiera
    minbucket= 1,
    maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"BAJA+2"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 0.025 ) ) )

fwrite( entrega, paste0( "./kaggle/stopping_at_canaritos.csv"), sep="," )

pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "stopping_canarito.pdf"))
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

