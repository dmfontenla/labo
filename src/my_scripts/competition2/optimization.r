
#limpio la memoria
#rm( list=ls() )  #remove all objects
#gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

require("dplyr")


library("rattle")				
require("treeClust")




#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}

#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

#------------------------------------------------------------------------------


LGBMCrossValidation  <- function( semilla, data, params, qfolds ) {
  model_lgbm_cv <- lgb.cv(

      # Configuración del CV:
      data = data,
      nfold = qfolds,
      stratified = TRUE,
      # Función que va a ser evaluada.
      eval = ganancia_lgbm,
      #param = params,
      # Veremos algunos, pero hay muchos más https://lightgbm.readthedocs.io/en/v3.3.2/Parameters.html
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
          max_bin = params$max_bin,
          #max_bin = param[[1]]$bin,
          #max_bin = params[[1]][1],

          # Por default puede trabajar con missing. Pero siempre hay un alumno talibán.
          use_missing = TRUE,

          # Variables de crecimiento del árbol.
          max_depth = -1, # -1 = No limitar
          #min_data_in_leaf = 4000,
          min_data_in_leaf = params$min_data_in_leaf,
          feature_pre_filter = FALSE, #feature_pre_filter: Evita que LightGBM deje de lado variables que considera malas.
          #num_leaves = 100,
          num_leaves = params$num_leaves,

          # Parámetros que fueron sacados de los rf porque lo que anda se mete:
          #feature_fraction = 0.50, # Porcentaje de columnas que van a usarse en un árbol
          feature_fraction = params$feature_fraction, # Porcentaje de columnas que van a usarse en un árbol
          # feature_fraction_bynode si queremos que sea por nodo
          bagging_fraction = params$bagging_fraction, # % de registros a considerar en cada árbol
          extra_tree = FALSE, # Los puntos de corte los elige al azar.

          # Parámetros de las famosas regularizaciones!!
          lambda_l1 = params$lambda_l1,
          lambda_l2 = params$lambda_l2,
          min_gain_to_split = params$min_gain_to_split,
          # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
          learning_rate =  params$learning_rate,
#          learning_rate =  param[[1]]$learning_rate,
#          learning_rate =  hs[[1]][1],
          #learning_rate = params[[1]][2],
          # Cuántos árboles vamos a generar
          #num_iterations = 100, # Debe ser un número muy grande, recordar el double descent!!!.
          #num_iterations = params[[1]][2], # Debe ser un número muy grande, recordar el double descent!!!.
          num_iterations = params$num_iterations, # Debe ser un número muy grande, recordar el double descent!!!.
          #unlist(hs[[1]]),
          early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.,
      ),
      verbose = -1
  )
  print(model_lgbm_cv)

  return( mean( unlist( model_lgbm_cv$record_evals$valid$ganancia$eval ) ) )
}

#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
   print(paste0("params: ", x))
   xval_folds  <- 5
   ganancias <- mcmapply( LGBMCrossValidation,
                           semillas,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds),
                           SIMPLIFY= FALSE,
                           mc.cores = 5 )  #debe ir 1 si es Windows


   #ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- mean(unlist(ganancias))
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------


#Aqui comienza la configuracion de la Bayesian Optimization
bayes_optimization <- function(f, params, iterations, bo_file){

    archivo_BO = bo_file
    
    funcion_optimizar  <- f

    configureMlr( show.learner.output= FALSE)

    obj.fun  <- makeSingleObjectiveFunction(
                fn=       funcion_optimizar,
                minimize= FALSE, 
                noisy=    TRUE,
                par.set=  params,
                has.simple.signature = FALSE   
                )

    ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
    ctrl  <- setMBOControlTermination(ctrl, iters= iterations )
    ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

    surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))
    des = generateDesign(n = 14L * 4, getParamSet(obj.fun), fun = lhs::maximinLHS)

    #inicio la optimizacion bayesiana
    if( !file.exists( archivo_BO ) ) {

    run  <- mbo( fun=    obj.fun, 
                learner= surr.km,
                control= ctrl,
                design = des)

    } else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

}



LGBMCrossValidation_object  <- function( semilla, data, params, qfolds ) {
  model_lgbm_cv <- lgb.cv(

      # Configuración del CV:
      data = data,
      nfold = qfolds,
      stratified = TRUE,
      # Función que va a ser evaluada.
      eval = ganancia_lgbm,
      #param = params,
      # Veremos algunos, pero hay muchos más https://lightgbm.readthedocs.io/en/v3.3.2/Parameters.html
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
          max_bin = params["max_bin"],
          #max_bin = param[[1]]$bin,
          #max_bin = params[[1]][1],

          # Por default puede trabajar con missing. Pero siempre hay un alumno talibán.
          use_missing = TRUE,

          # Variables de crecimiento del árbol.
          max_depth = -1, # -1 = No limitar
          #min_data_in_leaf = 4000,
          min_data_in_leaf = params["min_data_in_leaf"],
          feature_pre_filter = FALSE, #feature_pre_filter: Evita que LightGBM deje de lado variables que considera malas.
          #num_leaves = 100,
          num_leaves = params["num_leaves"],

          # Parámetros que fueron sacados de los rf porque lo que anda se mete:
          #feature_fraction = 0.50, # Porcentaje de columnas que van a usarse en un árbol
          feature_fraction = params["feature_fraction"], # Porcentaje de columnas que van a usarse en un árbol
          # feature_fraction_bynode si queremos que sea por nodo
          bagging_fraction = params["bagging_fraction"], # % de registros a considerar en cada árbol
          extra_tree = FALSE, # Los puntos de corte los elige al azar.

          # Parámetros de las famosas regularizaciones!!
          lambda_l1 = params["lambda_l1"],
          lambda_l2 = params["lambda_l2"],
          min_gain_to_split = params["min_gain_to_split"],
          # Otra variable más que importante! Cuanto aprende por cada nuevo árbol.
          learning_rate =  params["learning_rate"],
#          learning_rate =  param[[1]]$learning_rate,
#          learning_rate =  hs[[1]][1],
          #learning_rate = params[[1]][2],
          # Cuántos árboles vamos a generar
          #num_iterations = 100, # Debe ser un número muy grande, recordar el double descent!!!.
          #num_iterations = params[[1]][2], # Debe ser un número muy grande, recordar el double descent!!!.
          num_iterations = params["num_iterations"], # Debe ser un número muy grande, recordar el double descent!!!.
          #unlist(hs[[1]]),
          early_stopping_rounds = 100 # Corta cuando después de tantos árboles no vio una ganancia mejor a la máxima.,
      ),
      verbose = -1
  )
  print(model_lgbm_cv)

  return( mean( unlist( model_lgbm_cv$record_evals$valid$ganancia$eval ) ) )
}
