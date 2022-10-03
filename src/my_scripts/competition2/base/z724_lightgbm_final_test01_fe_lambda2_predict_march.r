# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
source("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/labo/src/my_scripts/competition2/feature_engineering.r")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "experimiento_lambda"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202101 )
PARAM$input$future        <- c( 202103 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0142501265   #0.0142501265
PARAM$finalmodel$num_iterations    <-    615  #615
PARAM$finalmodel$num_leaves        <-   784  #784
PARAM$finalmodel$min_data_in_leaf  <-   5628  #5628
PARAM$finalmodel$feature_fraction  <-      0.8382482539  #0.8382482539
PARAM$finalmodel$semilla           <- 300647
#PARAM$finalmodel$lambda_l1         <- 1.42085154674419
#PARAM$finalmodel$lambda_l2         <- 2.87113448933938

#fecha	objective	metric	first_metric_only	boost_from_average	feature_pre_filter	verbosity	max_depth	min_gain_to_split	lambda_l1	max_bin	num_iterations	force_row_wise	seed	learning_rate	feature_fraction	min_data_in_leaf	num_leaves	envios	lambda_l2	ganancia	iteracion
#20220929 095508	binary	custom	TRUE	TRUE	FALSE	-100	-1	0	0	31	99	TRUE	864379	0.0653189880860748	0.397951270516677	1335	363	9228	2.87113448933938	27870000	195

#125707
#300647
#864379
#fecha	objective	metric	first_metric_only	boost_from_average	feature_pre_filter	verbosity	max_depth	min_gain_to_split	max_bin	num_iterations	force_row_wise	seed	learning_rate	feature_fraction	min_data_in_leaf	num_leaves	envios	lambda_l1	lambda_l2	ganancia	iteracion
# 20220929 120131	binary	custom	TRUE	TRUE	FALSE	-100	-1	0	31	315	TRUE	864379	0.0471928412518739	0.350976633415739	1936	581	8639	1.42085154674419	1.31192864891727	27850000	205

# hs <- makeParamSet( 
#          makeNumericParam("learning_rate",    lower=    0.005, upper=    0.3),
#          makeNumericParam("feature_fraction", lower=    0.2  , upper=    1.0),
#          makeIntegerParam("min_data_in_leaf", lower=    0L   , upper=  8000L),
#          makeIntegerParam("num_leaves",       lower=   16L   , upper=  1024L),
#          makeIntegerParam("envios",           lower= 5000L   , upper= 15000L)
#         )

# fecha	objective	metric	first_metric_only	boost_from_average	feature_pre_filter	verbosity	max_depth	min_gain_to_split	lambda_l2	max_bin	num_iterations	force_row_wise	seed	learning_rate	feature_fraction	min_data_in_leaf	num_leaves	envios	lambda_l1	ganancia	iteracion
# 20220927 071054	binary	custom	TRUE	TRUE	FALSE	-100	-1	0	0	31	118	TRUE	102191	0.0509728609400974	0.555250580140964	1637	287	8942	2.88922566228754	27640000	122

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
setwd( "./" )

#cargo el dataset donde voy a entrenar
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia2_2022.csv")


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

campos_buenos_custom <- function(ds) {
        return (setdiff( colnames(ds), c("clase_ternaria","clase01") ))
}
#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
date = format(Sys.time(), "%Y%m%d %H%M%S")
dir.create( "/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/PRED_LGB/",  showWarnings = FALSE ) 
dir.create( paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/PRED_LGB/", PARAM$experimento,"_",date, "/" ), showWarnings = FALSE )
setwd( paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/PRED_LGB/", PARAM$experimento,"_",date, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO

marzo <- dataset[ train==1L, campos_buenos, with=FALSE]
marzo <- do_feature_engineering(marzo)

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix( marzo ),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                   #lambda_l1=          PARAM$finalmodel$lambda_l1,
                                   #lambda_l2=          PARAM$finalmodel$lambda_l2
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------
mif = list(lgb.importance(modelo)$Feature[0:100])

mif


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]
dapply <- do_feature_engineering(dapply)
dapply$train = NULL

colnames(dapply[, campos_buenos, with=FALSE ])
colnames(dtrain)
names(dapply)
#tb_importancia$Feature[0:100]
dapply[mif]


best_cols = c(tb_importancia$Feature[0:100])
dapply[,..best_cols]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos_custom(dapply), with=FALSE ])                                 )


tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes,clase_ternaria ) ]
tb_entrega[  , prob := prediccion ]
setorder( tb_entrega, -prob )

tb_entrega

#genero la tabla de entrega


#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 18000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  x = sum((tb_entrega$Predicted == 1) * ifelse(tb_entrega$clase_ternaria == "BAJA+2", 78000, -2000))
  print(paste0("Corte:",envios," ganancia:",x)) 
#   fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
#           file= paste0(  PARAM$experimento, "_", format(Sys.time(), "%Y%m%d %H%M%S"), "_", envios, ".csv" ),
#           sep= "," )
}

#--------------------------------------

quit( save= "no" )
