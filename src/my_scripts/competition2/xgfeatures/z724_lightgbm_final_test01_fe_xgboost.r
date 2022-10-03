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
PARAM$experimento  <- "KA7240"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0509728609400974   #0.0142501265
PARAM$finalmodel$num_iterations    <-    118  #615
PARAM$finalmodel$num_leaves        <-   287  #784
PARAM$finalmodel$min_data_in_leaf  <-   1637  #5628
PARAM$finalmodel$feature_fraction  <-      0.555250580140964  #0.8382482539
PARAM$finalmodel$semilla           <- 864379
PARAM$finalmodel$lambda_l1         <- 2.88922566228754



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

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO

dataset[ train==1L]$clase01
marzo <- dataset[ train==1L, campos_buenos, with=FALSE]
marzo <- do_feature_engineering(marzo)
dataset$clase01
marzo$clase01

install.packages("xgboost")
require("xgboost")
xgmarzo <- marzo
xgmarzo$clase01

dtrain_nf <- xgb.DMatrix(
        data = data.matrix(xgmarzo),
        label = dataset[ train==1L]$clase01, missing = NA)

# Empecemos con algo muy básico
param_fe <- list(
            max_depth = 2,
            eta = 0.1,
            objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain_nf, nrounds = nrounds)

## ---------------------------
## Step 3: XGBoost, ... para generar nuevas variables
## ---------------------------

# https://research.facebook.com/publications/practical-lessons-from-predicting-clicks-on-ads-at-facebook/

new_features <- xgb.create.features(model = xgb_model, data.matrix(xgmarzo))
colnames(new_features)[150:173]
summary(new_features)


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix( new_features ),
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
                                   seed=               PARAM$finalmodel$semilla,
                                   lambda_l1=          PARAM$finalmodel$lambda_l1 
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


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 12000, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )