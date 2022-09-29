# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )

options(scipen=999)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection
install.packages("rlist")
require("data.table")
require("rlist")

require("rpart")
require("parallel")
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
require("DiceKriging")
require("mlrMBO")

setwd("./") 
source("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/labo/src/my_scripts/competition2/optimization.r")
source("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/labo/src/my_scripts/competition2/feature_engineering.r")

semillas <- c(864379, 300647, 125707, 962303, 983363)

#------------------------------------------------------------------------------

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia2_2022.csv")
marzo <- dataset[foto_mes == 202103]

marzo_better <- do_feature_engineering(marzo)
#marzo_better <- marzo
summary(marzo_better)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo_better$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo_better$clase_ternaria
marzo_better$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data   = data.matrix(marzo_better),
                       label  = clase_binaria,
                       # Truco jedi!
                       weight = ifelse(clase_real == "BAJA+2", 1.0000001, 1.0))


###################################################################################
#BUILD MODEL
###################################################################################
#fecha	max_bin	num_iterations	learning_rate	min_data_in_leaf	num_leaves	feature_fraction	xval_folds	ganancia	iteracion

#20220925 215832	14	150	0.0692310755816434	2000	80	0.301065991987519	5	26691434.6666667	59

#20220923 031436	14	97	0.0799970490210595	5	25870837.1134021	15
lgbm_parms <- c(
  # max_bin = 14,
  # num_iterations = 150, # Debe ser un número muy grande, recordar el double descent!!!.
  # max_depth = 12, # -1 = No limitar
  # min_data_in_leaf = 2000,
  # num_leaves = 80,
  # # Parámetros que fueron sacados de los rf porque lo que anda se mete:
  # feature_fraction = 0.301, # Porcentaje de columnas que van a usarse en un árbol
  # # feature_fraction_bynode si queremos que sea por nodo
  # bagging_fraction = 1.0, # % de registros a considerar en cada árbol
  # extra_tree = FALSE, # Los puntos de corte los elige al azar.
  # lambda_l1 = 0.0,
  # lambda_l2 = 0.0,
  # min_gain_to_split = 0.0,
  # learning_rate =  0.069231 # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
  # #early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.,
  max_bin = 14,
  num_iterations = 97, # Debe ser un número muy grande, recordar el double descent!!!.
  max_depth = 12, # -1 = No limitar
  min_data_in_leaf = 4000,
  num_leaves = 100,
  # Parámetros que fueron sacados de los rf porque lo que anda se mete:
  feature_fraction = 0.5, # Porcentaje de columnas que van a usarse en un árbol
  # feature_fraction_bynode si queremos que sea por nodo
  bagging_fraction = 1.0, # % de registros a considerar en cada árbol
  extra_tree = FALSE, # Los puntos de corte los elige al azar.
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  min_gain_to_split = 0.0,
  learning_rate =  0.079997, # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
  early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.,
)

modelo_last = LGBMBuildModel(semillas, dtrain, lgbm_parms)

TestEstimacionGanancia(lgbm_parms, dtrain)

###################################################################################
#DO PREDICTION
###################################################################################


mayo  <- dataset[ foto_mes==202105 ]  #defino donde voy a aplicar el modelo
dapply <- do_feature_engineering(mayo)
#dapply <- mayo
dapply$pred <- predict(modelo_last,data.matrix(dapply[, 1:249]))


base_prob = 0.035
for (i in c(1,2,3,4,5)){
  base_prob = base_prob + 0.01
  dapply[, Predicted := as.numeric(pred > base_prob)]
  print(base_prob)
  
  fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
    file = paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/competition_2/", "competencia-", Sys.time(),"_prob_",i,".csv"),
    sep = ","
  )
  Sys.sleep(5)
}

event_max_prob = 0.0541
dapply[, Predicted := as.numeric(pred > event_max_prob)]

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
  file = paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/competition_2/", "competencia-", Sys.time(),"_prob_",base_prob,".csv"),
  sep = ","
)


# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(pred > event_max_prob)]

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
  file = paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/competition_2/", "competencia-", Sys.time(),".csv"),
  sep = ","
)





#20220923 031436	14	97	0.0799970490210595	5	25870837.1134021	15

###################################################################################
#RUNNING BAYESIAN OPTIMIZATION
###################################################################################


GLOBAL_iteracion  <- 0
kBO_iter  <- 300   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
                    makeIntegerParam("max_bin", lower=10, upper=15),
                    makeIntegerParam("num_iterations", lower=100, upper=150),
                    makeNumericParam("learning_rate", lower=0.01, upper=0.1),
                    makeIntegerParam("min_data_in_leaf", lower=2000, upper=8000),
                    makeIntegerParam("num_leaves", lower=70, upper=200),
                    makeNumericParam("feature_fraction", lower=0.3, upper=0.8),
                    makeNumericParam("bagging_fraction", lower=0, upper=1),
                    makeNumericParam("lambda_l1", lower=0, upper=3),
                    makeNumericParam("lambda_l2", lower=0, upper=3),
                    makeNumericParam("min_gain_to_split", lower=0, upper=3)
                    )             
# minbuket NO PUEDE ser mayor que la mitad de minsplit

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/OB/",  showWarnings = TRUE ) 
dir.create( "/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/OB/test/", showWarnings = TRUE )
setwd("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/OB/test/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- paste0("HB-",format(Sys.time(), "%Y%m%d %H%M%S"),".txt")
archivo_BO   <- paste0("HB-",format(Sys.time(), "%Y%m%d %H%M%S"),".RDATA")

if( file.exists(archivo_log) )
{
    tabla_log  <- fread( archivo_log )
    GLOBAL_iteracion  <- nrow( tabla_log )
}

# Veremos en detalle esta función un poco más adelante
ganancia_lgbm  <- function(probs, datos) {
    ## Ingresar su estrategia! acá vamos a ir simplemente buscando la máxima gan de la curva.
    gan <- data.table("pred" = probs,
                                                                 # truco para separar las clases
                    "gan" = ifelse(getinfo(datos, "label") == 1 & getinfo(datos, "weight") > 1, 78000, -2000))
    setorder(gan, -pred)
    gan[, gan_acum :=  cumsum(gan)]
    return(list("name" = "ganancia",
                    "value" = gan[, max(gan_acum)] / 0.2,
                    "higher_better" = TRUE))
}

bayes_optimization(EstimarGanancia, hs, kBO_iter, archivo_BO)


LGBMBuildModel  <- function(semillas, data, lgbm_parms ) {

  # Armamos el dataset de train para LGBM
  modelo <- lightgbm(data = data,
              nrounds = 100,
              param = list(
                  # Seteamos la semilla. Se puede setear semillar para cada uso de parámetro aleatorio.
                  # - bagging_seed
                  # - feature_fraction_seed
                  seed= semillas[1],
                  # Definimos que tipo de problema vamos a resolver. De esta forma sabe que loss function utilizar.
                  objective = "binary",
                  # Que función va a usar para evaluar, en este caso es la que le pasamos en **eval**
                  # cuenta con una gran cantidad de funciones por out-of-the-box.
                  metric = "custom",

                  # Tipo de boosting: Usamos el valor por defecto, pero cuenta con más tipos: gbdt, rf, dart, goss
                  boosting = "gbdt",

                  # Valores del primer árbol usando el valor de la media de las clases. Ahorra quizás, 3 árboles (?)
                  boost_from_average = TRUE,

                  # Un parámetro más que importante! LightGBM bindea automaticamente las variables
                  # Hace que las variables pesen menos en memoria
                  # Hace más rápido en su ejecución
                  # Hace más robusto la predicción
                  max_bin = lgbm_parms["max_bin"],

                  # Por default puede trabajar con missing. Pero siempre hay un alumno talibán.
                  use_missing = TRUE,

                  # Variables de crecimiento del árbol.
                  max_depth = lgbm_parms["max_depth"], # -1 = No limitar
                  min_data_in_leaf = lgbm_parms["min_data_in_leaf"],
                  feature_pre_filter = FALSE, #feature_pre_filter: Evita que LightGBM deje de lado variables que considera malas.
                  num_leaves = lgbm_parms["num_leaves"],

                  # Parámetros que fueron sacados de los rf porque lo que anda se mete:
                  feature_fraction = lgbm_parms["feature_fraction"], # Porcentaje de columnas que van a usarse en un árbol
                  # feature_fraction_bynode si queremos que sea por nodo
                  bagging_fraction = lgbm_parms["bagging_fraction"], # % de registros a considerar en cada árbol
                  extra_tree = FALSE, # Los puntos de corte los elige al azar.

                  # Parámetros de las famosas regularizaciones!!
                  lambda_l1 = lgbm_parms["lambda_l1"],
                  lambda_l2 = lgbm_parms["lambda_l2"],
                  min_gain_to_split = lgbm_parms["min_gain_to_split"],
                  # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
                  learning_rate =  lgbm_parms["learning_rate"],
                  num_iterations = lgbm_parms["num_iterations"] # Debe ser un número muy grande, recordar el double descent!!!.
                  #early_stopping_rounds = lgbm_parms["early_stopping_rounds"] # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.,
              ),
              verbose = -1)
              return (modelo)
}


TestEstimacionGanancia  <- function( x, data ) {
   print(paste0("params: ", x))
   xval_folds  <- 5
   ganancias <- mcmapply( LGBMCrossValidation_object,
                           semillas,
                           MoreArgs= list ( data, param=x, qfolds= xval_folds),
                           SIMPLIFY= FALSE,
                           mc.cores = 5 )  #debe ir 1 si es Windows
  print(paste0("ganancia: ", ganancias))
   return( mean(unlist(ganancias)) )
}
