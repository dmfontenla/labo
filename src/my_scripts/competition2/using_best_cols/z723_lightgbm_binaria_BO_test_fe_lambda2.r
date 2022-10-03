# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
source("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/labo/src/my_scripts/competition2/feature_engineering.r")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.02, upper=    0.7),
         makeNumericParam("feature_fraction", lower=    0.3  , upper=    0.8),
         makeIntegerParam("min_data_in_leaf", lower=    1000L   , upper=  4000L),
         makeIntegerParam("num_leaves",       lower=   100   , upper=  700),
         makeIntegerParam("envios",           lower= 5000L   , upper= 15000L),
         makeNumericParam("lambda_l2", lower=    1  , upper=    3)
        )

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "HT7231"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 864379  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 250
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 864379  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

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
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")

  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )

  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]

  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global

  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          #lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )

  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
                         )

    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }


  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("./")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
#dataset  <- fread( PARAM$input$dataset )
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia2_2022.csv")

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
PARAM$experimento
#dir.create( "./exp/" ) 
#3dir.create( paste0( "./exp/", PARAM$experimento, "/") )
date = format(Sys.time(), "%Y%m%d %H%M%S")
dir.create( "/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/BO_LGB/",  showWarnings = FALSE ) 
dir.create( paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/BO_LGB/", PARAM$experimento,"_",date, "/" ), showWarnings = FALSE )
setwd( paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/BO_LGB/", PARAM$experimento,"_",date, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO

#setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento,format(Sys.time(), "%Y%m%d %H%M%S"), ".RDATA" )
klog        <- paste0( PARAM$experimento,format(Sys.time(), "%Y%m%d %H%M%S"), ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
          ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

marzo <- dataset[ training == 1L, campos_buenos, with=FALSE]
marzo <- do_feature_engineering(marzo)


best_cols = c("ctrx_quarter_normalizado","ctrx_quarter","mcaja_ahorro" ,"mpayroll_sobre_edad","mprestamos_personales","mv_msaldopesos","mpasivos_margen","visa_saldo_limite",
"mrentabilidad_annual","cpayroll_trx","master_saldo_limite","mvisa_consumo_limitef_ratio","visa_payroll_limite" ,"mv_status01" ,"mcuentas_saldo"  ,"mpayroll",
"mrentabilidad","loan_payroll_ratio","mpasivos_margen_saldo_ratio","mactivos_margen","mcomisiones_mantenimiento","mcuenta_corriente","Visa_fechaalta",
"commissions_qtx_ratio","ctarjeta_visa_transacciones","tx_saldo","cliente_edad" ,"commissions_saldo_ratio","mtarjeta_visa_consumo"   ,"visa_pagominimo_vsaldo",
"cdescubierto_preacordado","master_payroll_limite","mv_Fvencimiento","Master_fechaalta","mpasivos_margen_qtx_ratio","cproductos","tenure_over_age","mcomisiones_otras",
"mv_fechaalta","mv_mpagominimo" ,"chomebanking_transacciones","mvr_msaldopesos2","ccaja_seguridad","numero_de_cliente","Visa_Fvencimiento","Master_Fvencimiento",
"visa_pagominimo_limite","ccajas_consultas","mvisa_consumo_transacciones_ratio","ccomisiones_otras"  ,"mvisa_consumo_limite_ratio","Visa_mconsumospesos",
"Visa_mfinanciacion_limite","mvr_mpagominimo","mcomisiones","mtransferencias_recibidas","mcuenta_debitos_automaticos","mv_msaldototal" ,"Visa_cconsumos"  ,
"cliente_antiguedad","mautoservicio","Master_mfinanciacion_limite","Visa_mpagospesos","ccomisiones_mantenimiento","mcaja_ahorro_dolares","ctarjeta_debito_transacciones",
"visa_vsaldo_limite","Visa_msaldopesos","ccajas_transacciones","Visa_msaldototal","mv_mlimitecompra","Visa_mpagominimo"   ,"ctransferencias_recibidas","internet",
"thomebanking","mvr_Visa_mlimitecompra","mvr_Master_mlimitecompra","Visa_mconsumototal","mvr_mpagospesos","mv_mfinanciacion_limite","ccallcenter_transacciones",
"mvr_mconsumospesos" ,"mvr_msaldopesos" ,"mttarjeta_visa_debitos_automaticos","tmobile_app","Visa_status","tx_payroll","payroll_saldo_ratio","mvr_mconsumototal",
"mpagomiscuentas","Master_mlimitecompra","visa_payroll_saldo","tcallcenter","mtransferencias_emitidas","ctransferencias_emitidas","cmobile_app_trx","mv_mconsumospesos",
"mvr_msaldototal","Master_mpagospesos","ctarjeta_visa_debitos_automaticos") 
best_marzo = marzo[,..best_cols]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix( marzo ),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayes√iana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )
