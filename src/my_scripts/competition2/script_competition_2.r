# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

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



#################################
#################################


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE ) {
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
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA ) {
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol
global_model <- c()

ArbolSimple  <- function( fold_test, data, param ) {
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart(formula = Formulas["formula_selected"],
    #"clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )

  #print(modelo$variable.importance)
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]

  #cat(paste0(ganancia_testing, "/"))

  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa ) {
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 5 )   #debe ir 1 si es Windows

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x ) {
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain[,..filtered_cols], param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 5 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}



####################################################################################################################
#-------------------------------------START FUNCTIONS--------------------------------------------------------#
####################################################################################################################


initial_imputation_vars_neg <- c("Master_delinquency","Master_status","Visa_delinquency","Visa_status","Master_Fvencimiento","Visa_Fvencimiento")
initial_imputation_vars_zero <- c("mtarjeta_visa_descuentos","mtarjeta_master_descuentos")



first_variable_selection = c("ctrx_quarter","mcuentas_saldo","active_quarter","mcuenta_corriente",
"mprestamos_personales","cprestamos_personales","mcaja_ahorro","ccomisiones_otras","Master_Fvencimiento","Visa_Fvencimiento",
"mactivos_margen","Visa_delinquency","Master_delinquency","Master_status","ctarjeta_master",
"mcomisiones_mantenimiento","cproductos","internet","mtarjeta_visa_consumo","mpasivos_margen","mrentabilidad_annual",
"cdescubierto_preacordado","mpayroll","cpayroll_trx","ctarjeta_visa_transacciones","Visa_msaldopesos",
"Visa_msaldototal","tcuentas","ctarjeta_visa","cliente_antiguedad","Visa_status")



mis_variables_a_binear <- c("ctrx_quarter","mprestamos_personales","mcuentas_saldo","mactivos_margen","mcaja_ahorro","mcuenta_corriente",
                            "mpayroll","mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen", "mpasivos_margen",
                            "payroll_saldo_ratio","personal_loan_payroll_ratio", "personal_loan_saldo_ratio" , "tx_saldo","tx_payroll",
"commissions_qtx_ratio","commissions_saldo_ratio","commissions_payroll_ratio","commissions_qtx_ratio","mpasivos_margen_payroll_ratio",
"mpasivos_margen_saldo_ratio","mpasivos_margen_qtx_ratio","mcuenta_corriente","mtarjeta_visa_consumo","mpasivos_margen","mrentabilidad_annual",
"mprestamos_personales","cprestamos_personales","mcaja_ahorro","ccomisiones_otras","mpayroll","ctarjeta_visa_transacciones","Visa_msaldopesos",
"Visa_msaldototal")
  
new_created_features <- c("payroll_saldo_ratio","personal_loan_payroll_ratio", "personal_loan_saldo_ratio" , "tx_saldo","tx_payroll",
"commissions_qtx_ratio","commissions_saldo_ratio","commissions_payroll_ratio","mpasivos_margen_payroll_ratio",
"mpasivos_margen_saldo_ratio","mpasivos_margen_qtx_ratio")

new_master_vars <- c("mmaster_consumo_transacciones_ratio","cmaster_descuentos_transacciones_ratio","mmaster_descuentos_transacciones_ratio",
                    "mmaster_consumo_limite_ratio","mmaster_consumo_limitef_ratio","mmaster_payroll_limite_ratio","mmaster_consumo_limite_ratio",
                    "mmaster_inicio_mora_s","mmaster_inicio_mora_a","mmaster_falta_s","mmaster_falta_a","mmaster_fvencimiento_q"
                    )

new_visa_vars <- c("mvisa_consumo_transacciones_ratio", "cvisa_descuentos_transacciones_ratio","mvisa_descuentos_transacciones_ratio",
                  "mvisa_consumo_limite_ratio","mvisa_consumo_limitef_ratio","mvisa_payroll_limite_ratio","mvisa_consumo_limite_ratio",
                  "mvisa_inicio_mora_s","mvisa_inicio_mora_a","mvisa_falta_s","mvisa_falta_a","mvisa_fvencimiento_q"
)



customize_formulas <- function(vars) {
  campos_train <- paste(vars, collapse = " + ")
  Formulas["formula_binary_selected"] <<- paste0( "clase_binaria ~ - clase_ternaria -Visa_mpagado -mcomisiones_mantenimiento +", campos_train )
  Formulas["formula_binary_b1b2_selected"] <<- paste0( "clase_binaria_b1b2 ~ - clase_ternaria -Visa_mpagado -mcomisiones_mantenimiento +", campos_train )
}

create_canaries <- function(ds, n){
  for( i in 1:n ) {
    ds[ , paste0("canarito", i ) :=  runif( nrow(ds)) ]
    config$canarito_vars <<- c(config$canarito_vars, paste0("canarito", i ))
  }
}

impute_missing_initial <- function(ds) {
  replace_na_inf(ds, initial_imputation_vars_neg, -1, TRUE)
  replace_na_inf(ds, initial_imputation_vars_zero, 0, TRUE)
  return (ds)
}

replace_na_inf <- function(ds, attrs, val, replace) {
  for(attr in attrs) {
    suffix = ""
    if(replace == FALSE) {
      suffix = "_imp"
      print(paste0(attr,"_imp"))
      config$autogenerated_vars <<- c(config$autogenerated_vars,paste0(attr,"_imp"))
    }
    ds[, paste0(attr,suffix) := ifelse((is.infinite(get(attr)) |  is.nan(get(attr)) |  is.na(get(attr))), val, get(attr))] 
  }
  return (ds)
}


feature_engineering <- function(ds) {
  #ds <- expliod_variables(ds)
  ds <- create_new_variables(ds)
  ds <- credit_card_new_variables(ds)
  ds <- binning_variables(ds)
  return(ds)
}

binning_variables <- function(ds) {
  # Supongamos que tenemos una lista de variables a las que queremos transformar
  # A todas las vamos a rankear
  prefix <- "binned_"
  for (var in mis_variables_a_binear) {
      ds[, (paste(prefix, var, sep = "")) := ntile(get(var), 5)]
      config$autogenerated_vars <<- c(config$autogenerated_vars,paste(prefix, var, sep = ""))
  }
  return(ds)
}



leaves_table <- function(model, train, target, prefix = "") {
    leaves_train_table <- data.table(
        # Devuelve en que hoja cae un caso
        leaves = rpart.predict.leaves(model, train, type = "where"),
        classes = train[, clase_ternaria],
        target = train[, get(target)]
    )
    leaves <- dcast(
            leaves_train_table,
            leaves ~ classes, length,
            value.var = "target")
    leaves <- leaves[
        dcast(
        leaves_train_table,
        leaves ~ target, length,
        value.var = "target"),
        on = .(leaves)]
    leaves[, n := SI + NO]
    leaves[, p := round(SI / n,4)]
    leaves <- leaves[order(-p),]
    leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
    leaves[, ':='(SI = NULL, NO = NULL)]
    setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
                    new = c(paste0(prefix, "b1"),
                            paste0(prefix, "b2"),
                            paste0(prefix, "cont"),
                            paste0(prefix, "n"),
                            paste0(prefix, "p"),
                            paste0(prefix, "gan")))
    leaves[]
}




#dtrain[, lapply(.SD, function(x) sum(is.na(x)))]
#dtrain[, sum(is.na(.SD))]

#dtrain[, "payroll_saldo_ratio"] = dtrain[, "mcuentas_saldo"] / dtrain[, "mpayroll"]
#replace_na_inf(dtrain, c("payroll_saldo_ratio"), 0, TRUE) 
#dtrain[, "payroll_saldo_ratio_binned" := ntile(get("payroll_saldo_ratio"), 5)]
#dtrain[,c("tenure_over_age","payroll_saldo_ratio","personal_loan_payroll_ratio","personal_loan_saldo_ratio","personal_loan_saldo_ratio",
#"mprestamos_personales","mpayroll", "loan_saldo_ratio") ][, lapply(.SD, function(x) sum(is.na(x)))]

#dtrain[, "payroll_saldo_ratio_binned"]
create_new_variables <- function(ds) {
  #replace_na_inf <- function(ds, attrs, val, replace)
  ds[, "tenure_over_age"] = (ds[, "cliente_antiguedad"] / 12) / ds[, "cliente_edad"]
  ds[, "payroll_saldo_ratio"] = ds[, "mcuentas_saldo"] / ds[, "mpayroll"]
  replace_na_inf(ds, c("payroll_saldo_ratio"), 0, TRUE) 

  ds[, "personal_loan_payroll_ratio"] = ds[, "mprestamos_personales"] / ds[, "mpayroll"]
  replace_na_inf(ds, c("personal_loan_payroll_ratio"), 0, TRUE) 

  ds[, "personal_loan_saldo_ratio"] = ds[, "mprestamos_personales"] / ds[, "mcuentas_saldo"]
  replace_na_inf(ds, c("personal_loan_saldo_ratio"), 0, TRUE) 

  ds[, "loan_saldo_ratio"] = (ds[, "mprestamos_prendarios"] + ds[, "mprestamos_hipotecarios"] + ds[, "mprestamos_personales"]) / ds[, "mcuentas_saldo"]
  replace_na_inf(ds, c("loan_saldo_ratio"), 0, TRUE) 
  ds[, "loan_payroll_ratio"] = (ds[, "mprestamos_prendarios"] + ds[, "mprestamos_hipotecarios"] + ds[, "mprestamos_personales"]) / ds[, "mpayroll"]
  replace_na_inf(ds, c("loan_payroll_ratio"), 0, TRUE) 
  #ds[, "ctrx_quarter_cut_off"] = dlearn[, 'ctrx_quarter'] < 9.5

  ds[, "tx_payroll"] = ds[, "mpayroll"] / ds[, "ctrx_quarter"]
  replace_na_inf(ds, c("tx_payroll"), 0, TRUE) 
  ds[, "tx_saldo"] = ds[, "mcuentas_saldo"] / ds[, "ctrx_quarter"]
  replace_na_inf(ds, c("tx_saldo"), 0, TRUE) 


  ds[, "commissions_payroll_ratio"] = ds[, "mcomisiones"] / ds[, "mpayroll"]
  replace_na_inf(ds, c("commissions_payroll_ratio"), 0, TRUE) 

  ds[, "commissions_saldo_ratio"] = ds[, "mcomisiones"] / ds[, "mcuentas_saldo"]
  replace_na_inf(ds, c("commissions_saldo_ratio"), 0, TRUE) 

  ds[, "commissions_qtx_ratio"] = ds[, "mcomisiones"] / ds[, "ctrx_quarter"]
  replace_na_inf(ds, c("commissions_qtx_ratio"), 0, TRUE) 

  

  ds[, "mpasivos_margen_payroll_ratio"] = ds[, "mpasivos_margen"] / ds[, "mpayroll"]
  replace_na_inf(ds, c("mpasivos_margen_payroll_ratio"), 0, TRUE) 

  ds[, "mpasivos_margen_saldo_ratio"] = ds[, "mpasivos_margen"] / ds[, "mcuentas_saldo"]
  replace_na_inf(ds, c("mpasivos_margen_saldo_ratio"), 0, TRUE) 
  
  ds[, "mpasivos_margen_qtx_ratio"] = ds[, "mpasivos_margen"] / ds[, "ctrx_quarter"]
  replace_na_inf(ds, c("mpasivos_margen_qtx_ratio"), 0, TRUE) 
  
  
#mpasivos_margen / mactivos_margen

# mcuentas_saldo
# mautoservicio / mcaja_ahorro + mcaja_ahorro_adicional
# mtarjeta_visa_consumo
# mtarjeta_master_consumo
# mprestamos_personales / payroll
# mprestamos_personales / mcuentas_saldo


# mprestamos_prendarios + mprestamos_hipotecarios + mprestamos_personales

# mplazo_fijo_dolares + mplazo_fijo_pesos

# minversion1_pesos + minversion1_dolares + minversion2

# mpayroll + mpayroll2

# mcuenta_debitos_automaticos + mtarjeta_visa_debitos_automaticos + mttarjeta_master_debitos_automaticos 
# mpagodeservicios + mpagomiscuentas

# mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos

  return (ds)
}




#dlearn[, balance_ratio_imp := ifelse((is.infinite(balance_ratio) |  is.nan(balance_ratio) |  is.na(balance_ratio)), 0, balance_ratio)] 


#dlearn[,balance_ratio_imp]
#dlearn[mmaster_falta_a == TRUE, .N]

#dlearn[dlearn[,(Master_Fvencimiento > -90)], .N]



#dtrain[,c("mvisa_consumo_limitef_ratio") ][, lapply(.SD, function(x) sum(is.na(x)))]

credit_card_new_variables <- function(ds) {
  #replace_na_inf <- function(ds, attrs, val, replace) 

  ds[,"mmaster_consumo_transacciones_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"ctarjeta_master_transacciones"]
  ds[,"cmaster_descuentos_transacciones_ratio"] = ds[,"ctarjeta_master_descuentos"] / (ds[,"ctarjeta_master_transacciones"] + ds[,"ctarjeta_master_debitos_automaticos"])
  ds[,"mmaster_descuentos_transacciones_ratio"] = ds[,"mtarjeta_master_descuentos"] / (ds[,"mtarjeta_master_consumo"] + ds[,"mttarjeta_master_debitos_automaticos"])
  ds[,"mmaster_consumo_limite_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"Master_mlimitecompra"]
  ds[,"mmaster_consumo_limitef_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"Master_mfinanciacion_limite"]

  ds[,"mmaster_inicio_mora_s"] = ds[,Master_Finiciomora < 180]
  ds[,"mmaster_inicio_mora_a"] = ds[,Master_Finiciomora < 360]
  ds[,"mmaster_falta_s"] = ds[,Master_fechaalta < 180]
  ds[,"mmaster_falta_a"] = ds[,Master_fechaalta < 360]
  ds[,"mmaster_fvencimiento_q"] = ds[,(Master_Fvencimiento > -90)]


  #ds[,"master_saldo_limite"] = ds[,"Master_msaldototal"] / ds[,"Master_mlimitecompra"]
  #ds[,"master_saldo_limite"] = ds[,"mpayroll"] / ds[,"Master_mlimitecompra"]
  #ds[,"master_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Master_mlimitecompra"]

  #ds[,"master_saldo_limite"] = ds[,"Master_msaldototal"] / ds[,"Master_mfinanciacion_limite"]
  #ds[,"master_saldo_limite"] = ds[,"mpayroll"] / ds[,"Master_mfinanciacion_limite"]
  #ds[,"master_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Master_mfinanciacion_limite"]

  #ds[,"master_minimo_saldo"] = ds[,"Master_mpagominimo"] / ds[,"Master_msaldototal"]
  #ds[,"master_minimo_saldo"] = ds[,"Master_mpagominimo"] / ds[,"Master_mlimitecompra"]

  #ds[,"master_minimo_saldo"] = ds[,"Master_madelantopesos"] / ds[,"Master_msaldototal"]
  #ds[,"master_minimo_saldo"] = ds[,"Master_madelantopesos"] / ds[,"mpayroll"]

  #ds[,"master_saldo_limite"] = ds[,"mpayroll"] / ds[,"Master_msaldototal"]


  ds[,"mvisa_consumo_transacciones_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"ctarjeta_visa_transacciones"]
  ds[,"cvisa_descuentos_transacciones_ratio"] = ds[,"ctarjeta_visa_descuentos"] / (ds[,"ctarjeta_visa_transacciones"] + ds[,"ctarjeta_visa_debitos_automaticos"])
  ds[,"mvisa_descuentos_transacciones_ratio"] = ds[,"mtarjeta_visa_descuentos"] / (ds[,"mtarjeta_visa_consumo"] + ds[,"mttarjeta_visa_debitos_automaticos"])
  ds[,"mvisa_consumo_limite_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"Visa_mlimitecompra"]
  ds[,"mvisa_consumo_limitef_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"Visa_mfinanciacion_limite"]

  ds[,"mvisa_inicio_mora_s"] = ds[,Visa_Finiciomora < 180]
  ds[,"mvisa_inicio_mora_a"] = ds[,Visa_Finiciomora < 360]
  ds[,"mvisa_falta_s"] = ds[,Visa_fechaalta < 180]
  ds[,"mvisa_falta_a"] = ds[,Visa_fechaalta < 360]
  ds[,"mvisa_fvencimiento_q"] = ds[,(Visa_Fvencimiento > -90)]


  ds[,"visa_vsaldo_limite"] = ds[,"Visa_msaldototal"] / ds[,"Visa_mlimitecompra"]
  ds[,"visa_payroll_limite"] = ds[,"mpayroll"] / ds[,"Visa_mlimitecompra"]
  ds[,"visa_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Visa_mlimitecompra"]

  #ds[,"visa_saldo_limite"] = ds[,"Visa_msaldototal"] / ds[,"Visa_mfinanciacion_limite"]
  #ds[,"visa_saldo_limite"] = ds[,"mpayroll"] / ds[,"Visa_mfinanciacion_limite"]
  #ds[,"visa_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Visa_mfinanciacion_limite"]

  ds[,"visa_pagominimo_vsaldo"] = ds[,"Visa_mpagominimo"] / ds[,"Visa_msaldototal"]
  ds[,"visa_pagominimo_limite"] = ds[,"Visa_mpagominimo"] / ds[,"Visa_mlimitecompra"]

  ds[,"visa_adelanto_saldo"] = ds[,"Visa_madelantopesos"] / ds[,"Visa_msaldototal"]
  ds[,"visa_adelanto_payroll"] = ds[,"Visa_madelantopesos"] / ds[,"mpayroll"]

  ds[,"visa_payroll_saldo"] = ds[,"mpayroll"] / ds[,"Visa_msaldototal"]

  ds[,"master_vsaldo_limite"] = ds[,"Master_msaldototal"] / ds[,"Master_mlimitecompra"]
  ds[,"master_payroll_limite"] = ds[,"mpayroll"] / ds[,"Master_mlimitecompra"]
  ds[,"master_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Master_mlimitecompra"]

  #ds[,"master_saldo_limite"] = ds[,"Master_msaldototal"] / ds[,"Master_mfinanciacion_limite"]
  #ds[,"master_saldo_limite"] = ds[,"mpayroll"] / ds[,"Master_mfinanciacion_limite"]
  #ds[,"master_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Master_mfinanciacion_limite"]

  ds[,"master_pagominimo_vsaldo"] = ds[,"Master_mpagominimo"] / ds[,"Master_msaldototal"]
  ds[,"master_pagominimo_limite"] = ds[,"Master_mpagominimo"] / ds[,"Master_mlimitecompra"]

  ds[,"master_adelanto_saldo"] = ds[,"Master_madelantopesos"] / ds[,"Master_msaldototal"]
  ds[,"master_adelanto_payroll"] = ds[,"Master_madelantopesos"] / ds[,"mpayroll"]

  ds[,"master_payroll_saldo"] = ds[,"mpayroll"] / ds[,"Master_msaldototal"]
  return (ds)
}

#dlearn[,"Master_msaldototal"]

leaves_table <- function(model, train, target, prefix = "") {
    leaves_train_table <- data.table(
        # Devuelve en que hoja cae un caso
        leaves = rpart.predict.leaves(model, train, type = "where"),
        classes = train[, clase_ternaria],
        target = train[, get(target)]
    )
    leaves <- dcast(
            leaves_train_table,
            leaves ~ classes, length,
            value.var = "target")
    leaves <- leaves[
        dcast(
        leaves_train_table,
        leaves ~ target, length,
        value.var = "target"),
        on = .(leaves)]
    leaves[, n := SI + NO]
    leaves[, p := round(SI / n,4)]
    leaves <- leaves[order(-p),]
    leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
    leaves[, ':='(SI = NULL, NO = NULL)]
    setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
                    new = c(paste0(prefix, "b1"),
                            paste0(prefix, "b2"),
                            paste0(prefix, "cont"),
                            paste0(prefix, "n"),
                            paste0(prefix, "p"),
                            paste0(prefix, "gan")))
    leaves[]
}



#### ------------------------------------------------------------------------------------------------------------
#### UTILITARIOS INICIALES VERSION LUNES ------------------------------------------------------------------------------------
#### ------------------------------------------------------------------------------------------------------------


binary_event_join_b1b2 <- function(ds) {
  # Creamos una clase binaria
  ds[, clase_binaria := ifelse(
    clase_ternaria == "CONTINUA",
    "noevento",
    "evento"
  )]
  return (ds)
}

binary_event <- function(ds) {
  # Creamos una clase binaria
  ds[, clase_binaria := ifelse(
    clase_ternaria == "BAJA+2",
    "evento",
    "noevento"
  )]
  return (ds)
}




experimento <- function(formula) {
  gan <- c()
  auc <- c()

  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(dlearn$clase_binaria,
      p = 0.70,
      list = FALSE
    )
    train <- dlearn[in_training, ]
    test <- dlearn[-in_training, ]

    #### ACA HAGO COSAS

    r <- rpart(formula,
      data = train,
      xval = 0,
      cp = tree_params["cp"], # esto significa no limitar la complejidad de los splits
      minsplit = tree_params["minsplit"], # minima cantidad de registros para que se haga el split
      minbucket = tree_params["minbucket"], # tamaño minimo de una hoja
      maxdepth = tree_params["maxdepth"]
    )
    result <- calcular_ganancia_binaria(r, test)
    gan <- c(gan, result$profit)
    auc <- c(auc, result$auc)
    print(gan)
  }
  print(paste0("Mean profit ",mean(gan)))
  print(paste0("Mean auc ",mean(auc)))
}

TreeCVProfitPrediction <- function(ds, formula, seeds) {
  gan <- c()
  auc <- c()

  for (s in seeds) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria,
      p = 0.70,
      list = FALSE
    )
    train <- ds[in_training, ]
    test <- ds[-in_training, ]

    #### ACA HAGO COSAS

    r <- rpart(formula,
      data = train[train$clase_ternaria != "BAJA+1", ],
      xval = 0,
      cp = tree_params["cp"], # esto significa no limitar la complejidad de los splits
      minsplit = tree_params["minsplit"], # minima cantidad de registros para que se haga el split
      minbucket = tree_params["minbucket"], # tamaño minimo de una hoja
      maxdepth = tree_params["maxdepth"]
    )
    result <- calcular_ganancia_binaria(r, test)
    gan <- c(gan, result$profit)
    auc <- c(auc, result$auc)
    print(gan)
  }
  print(mean(gan))
  print(mean(auc))
  return (c(mean(gan), mean(auc)))
}

#### ------------------------------------
#### OUTPUT -----------------------------
#### ------------------------------------
#calcular_ganancia_binaria(modelo, dtest)
#calcular_ganancia_ternaria(modelo, dtest)


# Armamos una función que nos calcule la ganancia, usando el punto de corte de
# 0.025
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000)
  ))
}

expliod_variables <- function(ds) {

  mis_variables_mejor_rankeadas <- c("ctrx_quarter",
                    #"binned_mprestamos_personales",
                    "cprestamos_personales",
                    "mcuentas_saldo",
                    "active_quarter",
                    "mcuenta_corriente",
                    "mcaja_ahorro",
                    "cdescubierto_preacordado",
                    "ccomisiones_otras",
                    "mpasivos_margen",
                    "cliente_antiguedad",
                    "mrentabilidad") 

new_exploid_attributes <- c()

  for (var1 in mis_variables_mejor_rankeadas) {
    for (var2 in mis_variables_mejor_rankeadas) {
        if (var1 != var2) {
            nueva <- paste(var1, var2, sep = "___")
            ds[, (nueva) := get(var1) * get(var2)]
            new_exploid_attributes <- c(new_exploid_attributes, nueva)
        }
    }
  }
  return(ds)
}



#### ------------------------------------
#### OPTIMIZING -----------------------------
#### ------------------------------------

bay_optimization <- function(dataset, semillas){
  set.seed(semillas[1])
  obj_fun_md_ms <- function(x) {
    experimento_rpart(dataset, semillas
              , md = x$maxdepth
              , ms = x$minsplit
              , mb = x$minbucket)
  }
  obj_fun <- makeSingleObjectiveFunction(
      minimize = FALSE,
      fn = obj_fun_md_ms,
      par.set = makeParamSet(
        makeIntegerParam("maxdepth",  lower = 1L, upper = 20L),
        makeIntegerParam("minsplit",  lower = 1L, upper = 2500L),
        makeNumericParam("minbucket",  lower = 1L, upper = 800L)
        # makeNumericParam <- para parámetros continuos
      ),
      # noisy = TRUE,
      has.simple.signature = FALSE
    )
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = 400L)
  ctrl <- setMBOControlInfill(
    ctrl,
    crit = makeMBOInfillCritEI(),
    opt = "focussearch",
    # sacar parámetro opt.focussearch.points en próximas ejecuciones
    opt.focussearch.points = 500
  )

  lrn <- makeMBOLearner(ctrl, obj_fun)
  #design <- generateDesign(6L, getParamSet(obj_fun), fun = lhs::maximinLHS)
  des = generateDesign(n = 6L, getParamSet(obj_fun), fun = lhs::maximinLHS)
  surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

  run_md_ms <- mbo(obj_fun, design = des, learner = surr_km, control = ctrl, )
  print(run_md_ms)
  return(run_md_ms)
}

#dlearn[, ..to_optimize_selection][, !c("ctrx_quarter")]


#bay_optimization(dlearn[, ..to_optimize_selection],semillas)


experimento_rpart <- function(ds, semillas, cp = 0, ms = 20, mb = 1, md = 10) {
  auc <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
        list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    train_sample <- tomar_muestra(train)
    r <- modelo_rpart(train[train_sample,], test, 
                    cp = cp, ms = ms, mb = mb, md = md)
    auc <- c(auc, r)
  }
  mean(auc)
}

modelo_rpart <- function(train, test, cp =  0, ms = 20, mb = 1, md = 10) {
    modelo <- rpart(clase_binaria ~ ., data = train,
                    xval = 0,
                    cp = cp,
                    minsplit = ms,
                    minbucket = mb,
                    maxdepth = md)

    test_prediccion <- predict(modelo, test, type = "prob")
    roc_pred <-  ROCR::prediction(test_prediccion[, "evento"],
                    test$clase_binaria,
                                  label.ordering = c("noevento", "evento"))
    auc_t <-  ROCR::performance(roc_pred, "auc")

    unlist(auc_t@y.values)
}

tomar_muestra <- function(datos, resto = 10000) {
      t <- datos$clase_binaria == "evento"
      r <- rep(FALSE, length(datos$clase_binaria))
      r[!t][sample.int(resto, n = (length(t) - sum(t)))] <- TRUE
      t | r
}

ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

grip_search_optimization <- function(ds) {
  for (cp in c(-1)) {
    for (md in c(7, 10, 15, 20)) {
      for (ms in c(500, 800, 1000, 1300, 1500, 2000)) {
        for (mb in c(as.integer(ms / 2), as.integer(ms / 3), as.integer(ms / 5))) {
          
          t0 <- Sys.time()
          gan_semillas <- c()
          for (s in semillas) {
            set.seed(s)
            in_training <- caret::createDataPartition(ds[,
                                                              get("clase_binaria")],
                                                      p = 0.70, list = FALSE)
            dtrain  <-  ds[in_training, ]
            dtest   <-  ds[-in_training, ]            
            modelo <- rpart(clase_binaria ~ .,
                            data = dtrain,
                            xval = 0,
                            cp = cp,
                            minsplit = ms,
                            minbucket = mb,
                            maxdepth = md)            
            pred_testing <- predict(modelo, dtest, type = "prob")
            gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3            
            gan_semillas <- c(gan_semillas, gan)
          }
          tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
          resultados_grid_search <- c()          
          resultados_grid_search <- rbindlist(list(
            resultados_grid_search,
            data.table(
              tiempo = tiempo,
              cp = cp,
              mb = mb,
              ms = ms,
              md = md,
              gan = mean(gan_semillas))
          ))
          print("params")
          print(cp)
          print(mb)
          print(ms)
          print(md)
          print(gan)
        }
      }
    }
  }

}


#grip_search_optimization(dlearn[, ..to_optimize_selection])

calcular_ganancia_binaria <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  roc_pred <-  ROCR::prediction(pred_testing[, "evento"],
                test$clase_binaria,
                              label.ordering = c("noevento", "evento"))
  auc_t <-  ROCR::performance(roc_pred, "auc")  
  print(unlist(auc_t@y.values))
  profit = sum(
    (pred_testing[, "evento"] >= event_max_prob) * ifelse(test$clase_binaria == "evento",
      78000, -2000
    ) / 0.3
  )

  return (list("profit" = profit, "auc" = unlist(auc_t@y.values)))
}

calcular_ganancia_ternaria <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "BAJA+2"] >= 0.025) * ifelse(test$clase_ternaria == "BAJA+2",
      78000, -2000
    ) / 0.3
  )
}

############################################################################################
#############       LOGICA DE EJECUCION         ############################################
############################################################################################


#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(864379, 300647, 125707, 962303, 983363)


#Defino la  Optimizacion Bayesiana

pred_params <- c(
  "cp" = -1, 
  "minsplit" = 1367, 
  "minbucket" = 518,
  "maxdepth" = 8
)


kBO_iter  <- 75   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("minsplit" , lower=   1,   upper= 3000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit

#------------------------------------------------------------------------------
#Aqui empieza el programa
GLOBAL_iteracion  <- 0

setwd( "./" )

has_canary_creation = FALSE
Formulas <- c(
  "formula_binary_selected" = c(),
  "formula_binary" = "clase_binaria ~ . -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
  "formula_binary_b1b2_all" = "clase_binaria_b1b2 ~ . - clase_ternaria - clase_binaria -Visa_mpagado -mcomisiones_mantenimiento",
  "formula_binary_b1b2_selected" = c(),
  "formula_selected" = c(),
  "formula_custom" = c("clase_binaria ~ . - clase_ternaria")
)
configuration <- function() {
  canarito_vars <- c()
  auto_generated_vars <- c()
}
config <- configuration()

#cargo el dataset, aqui debe poner  SU RUTA
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia1_2022.csv")

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#defino los datos donde entreno
dlearn <- dataset[ foto_mes==202101, ]


filtered_cols <- c("clase_binaria","ctrx_quarter","clase_ternaria",
"payroll_saldo_ratio","personal_loan_payroll_ratio", "personal_loan_saldo_ratio" , "tx_saldo","tx_payroll","commissions_qtx_ratio",
"commissions_saldo_ratio","commissions_payroll_ratio","mpasivos_margen_payroll_ratio","mpasivos_margen_saldo_ratio","mpasivos_margen_qtx_ratio",
"mcuentas_saldo","active_quarter","mcuenta_corriente",
"mprestamos_personales","cprestamos_personales","mcaja_ahorro","ccomisiones_otras","Master_Fvencimiento","Visa_Fvencimiento",
"mactivos_margen","Visa_delinquency","Master_delinquency","Master_status","ctarjeta_master","mtarjeta_master_consumo","Master_msaldototal","ctarjeta_master_transacciones",
"mcomisiones_mantenimiento","cproductos","internet","mtarjeta_visa_consumo","mpasivos_margen","mrentabilidad_annual",
"cdescubierto_preacordado","mpayroll","cpayroll_trx","ctarjeta_visa_transacciones","Visa_msaldopesos",
"Visa_msaldototal","tcuentas","ctarjeta_visa","cliente_antiguedad","Visa_status",
"mvisa_consumo_transacciones_ratio","cvisa_descuentos_transacciones_ratio","mvisa_descuentos_transacciones_ratio","mvisa_consumo_limite_ratio",
"mvisa_consumo_limitef_ratio","mvisa_inicio_mora_s","mvisa_falta_s","mvisa_fvencimiento_q","visa_vsaldo_limite","visa_payroll_limite",
"visa_saldo_limite","visa_pagominimo_vsaldo","visa_pagominimo_limite","visa_adelanto_saldo","visa_adelanto_payroll","visa_payroll_saldo",
"mmaster_consumo_transacciones_ratio","cmaster_descuentos_transacciones_ratio","mmaster_descuentos_transacciones_ratio","mmaster_consumo_limite_ratio",
"mmaster_consumo_limitef_ratio","mmaster_inicio_mora_s","mmaster_falta_s","mmaster_fvencimiento_q","master_vsaldo_limite","master_payroll_limite",
"master_saldo_limite","master_pagominimo_vsaldo","master_pagominimo_limite","master_adelanto_saldo","master_adelanto_payroll","master_payroll_saldo",
"mautoservicio","ctarjeta_debito_transacciones","Visa_mpagominimo","Master_mpagominimo","mcomisiones","mcomisiones_otras",
"mtransferencias_recibidas","ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos","chomebanking_transacciones",
"ctransferencias_recibidas"
)

filtered_cols
if (has_canary_creation) create_canaries(dlearn, 10)

dtrain <- cbind(dlearn)
dtrain <- impute_missing_initial(dtrain)
dtrain <- feature_engineering(dtrain)
#dtrain[,"master_vsaldo_limite"]
#initial_variables = c(first_variable_selection, new_created_features, config$autogenerated_vars, config$canarito_vars)
#initial_variables_onlybinned <- initial_variables[! initial_variables %in% mis_variables_a_binear]

#formulas <- customize_formulas(initial_variables_onlybinned)
Formulas["formula_selected"] = Formulas["formula_custom"]

EstimarGanancia(pred_params)

dtrain$clase_binaria
in_training <- caret::createDataPartition(dtrain$clase_binaria, p = 0.70, list = FALSE)
#in_training <- caret::createDataPartition(dlearn$clase_ternaria, p = 0.70, list = FALSE)
dtrain_2 <- dtrain[in_training, ]
dtest_2 <- dtrain[-in_training, ]

Formulas["formula_custom"]
dtrain_2[,..filtered_cols]
dtrain_2[,"mpasivos_margen_qtx_ratio" ]
filtered_cols

modelo  <- rpart(formula = Formulas["formula_custom"],
  #"clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                  data= dtrain_2[,..filtered_cols],  #entreno en todo MENOS el fold_test que uso para testing
                  xval= 0,
                  control= pred_params )


modelo$variable.importance
#comisiones en relacion a lo gastado
#saldo y consumo de tarjeta

pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "tree.pdf"))
fancyRpartPlot(modelo)
dev.off()

train_leaves <- leaves_table(modelo, dtest_2, "clase_binaria")
train_leaves[, gan_acum := cumsum(gan) / 0.7]
train_leaves[, n_acum := cumsum(n) / 0.7]
print(train_leaves[0:25])



pruner_vars = c()


has_canary_prunning = FALSE
if (has_canary_prunning) {

  model_pruned <- prune_canaries(modelo)
  #calcular_ganancia_binaria(model_pruned, dtest, "clase_binaria")
  #print(model_pruned$variable.importance)
  model_pruned
  pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "pruned_canarito.pdf"))
  fancyRpartPlot(model_pruned)
  dev.off()

  pruner_vars = c()
  for (i in model_pruned$frame$var) {
    if(i != "<leaf>") { 
      pruner_vars = c(pruner_vars, i) 
      cat(paste0(i, ","))
    } 
  }

  modelo_pruned$cptable
  modelo
  modelo$frame[ modelo$frame$var %like% "canarito", "complexity"] <- -666
  #print(model)
  modelo_pruned  <- prune(modelo, -666)
  modelo_pruned$frame[ modelo_pruned$frame$var %like% "canarito", "complexity"]
  
  
  test_prediccion <- predict(
    object = modelo_pruned,
    newdata = dapply,
    type = "prob"
  )
  modelo_pruned$variable
  modelo_pruned$frame$var
  dapply['canarito1']

  summary(model_pruned)
  model_pruned <- prune_canaries(modelo)
  pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "pruned_canarito.pdf"))
  fancyRpartPlot(model_pruned)
  dev.off()
}

#for (i in model_pruned$frame$var) {
#  if(i != "<leaf>") { 
#    pruner_vars = c(pruner_vars, i) 
#    cat(paste0(i, ","))
#  } 
#}

prune_canaries <- function(model) {
  model$frame[ model$frame$var %like% "canarito", "complexity"] <- -666
  #print(model)
  modelo_pruned  <- prune(model, -666)
  return (modelo_pruned)
}

modelo_last  <- rpart(formula = Formulas["formula_custom"],
  #"clase_binaria ~ .  -Visa_mpagado -mcomisiones_mantenimiento -clase_ternaria",
                  data= dtrain[,..filtered_cols],  #entreno en todo MENOS el fold_test que uso para testing
                  xval= 0,
                  control= pred_params )

dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
dapply_2 <- impute_missing_initial(dapply)
dapply_2 <- feature_engineering(dapply_2)

test_prediccion <- predict(
  object = modelo_last,
  newdata = dapply_2[,..filtered_cols],
  type = "prob"
)

event_max_prob = 0.0441
# prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := test_prediccion[, "SI"]]
#dapply[, prob_baja2 := prediccion[, "BAJA+2"]]


# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > event_max_prob)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create( "./exp/" )
#dir.create( "./exp/KA2001" )

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
  file = paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/", "competencia-", Sys.time(),".csv"),
  sep = ","
)



#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT4110/", showWarnings = FALSE )
setwd("./exp/HT4110/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT4110.txt"
archivo_BO   <- "HT4110.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}

#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))
des = generateDesign(n = 6L, getParamSet(obj.fun), fun = lhs::maximinLHS)

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl,
               design = des)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista
