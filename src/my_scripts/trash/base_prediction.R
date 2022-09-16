# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot


# install.packages( "data.table", dependencies= TRUE )
# install.packages( "rpart", dependencies= TRUE )
# install.packages( "rpart.plot", dependencies= TRUE )

rm(list = ls())
gc(verbose = FALSE)

options(scipen = 100, digits = 4)


#install.packages('rattle')
#install.packages("treeClust")
#install.packages("rlist")

#### ------------------------------------
#### LOAD DATA --------------------------
#### ------------------------------------


#### ------------------------------------
# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("dplyr")
library("rattle")				
require("treeClust")
require("rlist")
require("parallel")


# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("./") # Establezco el Working Directory
semillas <- c(864379, 300647, 125707, 962303, 983363)

# cargo el dataset
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia1_2022.csv")

dlearn_raw <- dataset[foto_mes == 202101] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202103] # defino donde voy a aplicar el modelo

#GLOBALS

pred_params <- c(
  "cp" = -1, 
  "minsplit" = 1500, 
  "minbucket" = 233,
  "maxdepth" =30,
  "formula" = clase_binaria ~ .,
  "event_max_prob" = 0.025
)

has_canary_creation = FALSE
has_canary_prunning = FALSE

initial_imputation_vars_neg <- c("Master_delinquency","Master_status","Visa_delinquency","Visa_status","Master_Fvencimiento","Visa_Fvencimiento")
initial_imputation_vars_zero <- c("mtarjeta_visa_descuentos","mtarjeta_master_descuentos")

first_variable_selection = c("ctrx_quarter","mcuentas_saldo","active_quarter","mcuenta_corriente",
"mprestamos_personales","cprestamos_personales","mcaja_ahorro","ccomisiones_otras","Master_Fvencimiento","Visa_Fvencimiento",
"mactivos_margen","Visa_delinquency","Master_delinquency","Master_status","ctarjeta_master",
"mcomisiones_mantenimiento","cproductos","internet","mtarjeta_visa_consumo","mpasivos_margen","mrentabilidad_annual",
"cdescubierto_preacordado","mpayroll","cpayroll_trx","ctarjeta_visa_transacciones","Visa_msaldopesos",
"Visa_msaldototal","tcuentas","ctarjeta_visa","cliente_antiguedad","Visa_status")

pruned_variables_by_canary = c("active_quarter","cdescubierto_preacordado","binned_ctrx_quarter",
"binned_mcaja_ahorro","mtarjeta_visa_consumo","binned_mprestamos_personales","Visa_Fvencimiento","cpayroll_trx",
"cliente_antiguedad","Visa_msaldopesos","cpayroll_trx","ctarjeta_visa_transacciones","binned_mrentabilidad_annual",
"cpayroll_trx","ctarjeta_visa","Visa_msaldopesos","binned_mrentabilidad_annual","binned_mcuentas_saldo",
"binned_mpasivos_margen","mtarjeta_visa_consumo","Visa_Fvencimiento","internet","Visa_msaldototal",
"Visa_Fvencimiento","binned_mcaja_ahorro","binned_mcomisiones","cproductos")


mis_variables_a_binear <- c("ctrx_quarter","mprestamos_personales","mcuentas_saldo","mactivos_margen","mcaja_ahorro","mcuenta_corriente",
                            "mpayroll","mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen", "mpasivos_margen")
  
new_created_features <- c("tenure_over_age","balance_ratio","personal_loan_payroll_ratio", "personal_loan_saldo_ratio","loan_saldo_ratio",
                        "loan_saldo_ratio","card_tx_payroll")

new_master_vars <- c("mmaster_consumo_transacciones_ratio","cmaster_descuentos_transacciones_ratio","mmaster_descuentos_transacciones_ratio",
                    "mmaster_consumo_limite_ratio","mmaster_consumo_limitef_ratio","mmaster_payroll_limite_ratio","mmaster_consumo_limite_ratio",
                    "mmaster_inicio_mora_s","mmaster_inicio_mora_a","mmaster_falta_s","mmaster_falta_a","mmaster_fvencimiento_q"
                    )

new_visa_vars <- c("mvisa_consumo_transacciones_ratio", "cvisa_descuentos_transacciones_ratio","mvisa_descuentos_transacciones_ratio",
                  "mvisa_consumo_limite_ratio","mvisa_consumo_limitef_ratio","mvisa_payroll_limite_ratio","mvisa_consumo_limite_ratio",
                  "mvisa_inicio_mora_s","mvisa_inicio_mora_a","mvisa_falta_s","mvisa_falta_a","mvisa_fvencimiento_q"
)

Formulas <- c(
  "formula_binary_selected" = c(),
  "formula_binary_all" = "clase_binaria ~ . - clase_ternaria - clase_binaria_b1b2",
  "formula_binary_b1b2_all" = "clase_binaria_b1b2 ~ . - clase_ternaria - clase_binaria",
  "formula_binary_b1b2_selected" = c()
)

archivo_BO   <- "HT4110.RDATA"
kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana
hs  <- makeParamSet(
          makeNumericParam("minsplit" , lower=   1,   upper= 5000 ),
          makeNumericParam("minbucket", lower=   1,   upper= 1000 ),
          makeIntegerParam("maxdepth" , lower=   3L,  upper=   20L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )     



configuration <- function() {
  canarito_vars <- c()
  auto_generated_vars <- c()
}
config <- configuration()

####################################################################################################################
#-------------------------------------START MODEL SCRIPTING--------------------------------------------------------#
####################################################################################################################



dlearn <- cbind(dlearn_raw)

if (has_canary_creation) create_canaries(dlearn, 8)
dlearn <- class_modeling(dlearn)
#dlearn <- impute_missing_initial(dlearn)
#dlearn <- feature_engineering(dlearn)
#dlearn <- impute_missing_news(dlearn)



initial_variables = c(pruned_variables_by_canary, config$autogenerated_vars, config$canarito_vars)
initial_variables_onlybinned <- initial_variables[! initial_variables %in% mis_variables_a_binear]
initial_variables_onlybinned
#variables_selected <- variable_selection(dlearn)
formulas <- customize_formulas(initial_variables_onlybinned)

dlearn

TreeCVProfitPrediction(dlearn,Formulas["formula_binary_b1b2_all"],semillas, "clase_binaria_b1b2",pred_params, TRUE, FALSE)


# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dlearn$clase_binaria, p = 0.70, list = FALSE)
#in_training <- caret::createDataPartition(dlearn$clase_ternaria, p = 0.70, list = FALSE)
dtrain <- dlearn[in_training, ]
dtest <- dlearn[-in_training, ]

modelo <- rpart(
  #formula=   "clase_binaria ~ . - clase_ternaria - clase_binaria_b1b2", # quiero predecir clase_ternaria a partir de el resto de las variables
  formula = Formulas["formula_binary_selected"],
  data = dtrain,
  xval = 0,
  cp = pred_params["cp"],
  minsplit = pred_params["minsplit"], # minima cantidad de registros para que se haga el split
  minbucket = pred_params["minbucket"], # tamaño minimo de una hoja
  maxdepth = pred_params["maxdepth"]
) 


modelo$variable.importance

pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "tree.pdf"))
fancyRpartPlot(modelo)
dev.off()

dlearn[,"binned_ctrx_quarter"]

dlearn[, r_ctrx_quarter := ntile(ctrx_quarter, 10)]
summary(dlearn[,"r_ctrx_quarter"])



vimp_acum = 0
imp_vars <- c()
for (i in names(modelo$variable.importance)) {
  print(i)
  print(modelo$variable[i])
  if (vimp_acum < 99) {
    vimp_acum = vimp_acum + modelo$variable[i][1]
    imp_vars <- c(imp_vars,modelo$variable[i])
  }
  
}

modelo$variable['mtarjeta_visa_descuentos']

imp_vars
vimp_acum

modelo$variable.importance
plotcp(modelo)



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
}


summary(model_pruned)
calcular_ganancia_binaria(modelo, dtest, "clase_binaria")

print(modelo$variable.importance)
modelo$



modelo

summary(modelo)

plot(modelo$variable.importance, top = 20)

train_leaves <- leaves_table(modelo, dtrain, "clase_binaria")
train_leaves[, gan_acum := cumsum(gan) / 0.7]
train_leaves[, n_acum := cumsum(n) / 0.7]
print(train_leaves)

summary(dlearn)



dapply <- impute_missing_initial(dapply)
dapply <- feature_engineering(dapply)
#dapply <- impute_missing_news(dapply)


#calcular_ganancia_binaria(modelo, dapply)

# aplico el modelo a los datos nuevos

modelo <- rpart(
  #formula=   "clase_binaria ~ . - clase_ternaria - clase_binaria_b1b2", # quiero predecir clase_ternaria a partir de el resto de las variables
  formula = Formulas["formula_binary_selected"],
  data = dlearn,
  xval = 0,
  cp = pred_params["cp"],
  minsplit = pred_params["minsplit"], # minima cantidad de registros para que se haga el split
  minbucket = pred_params["minbucket"], # tamaño minimo de una hoja
  maxdepth = pred_params["maxdepth"]
) 

test_prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

model_pruned
# prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := test_prediccion[, "evento"]]
#dapply[, prob_baja2 := prediccion[, "BAJA+2"]]


# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > pred_params["event_max_prob"])]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create( "./exp/" )
#dir.create( "./exp/KA2001" )

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
  file = paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/", "competencia-", Sys.time(),".csv"),
  sep = ","
)



dlearn[, sum(is.na(.SD))]
dlearn[, lapply(.SD, function(x) sum(is.na(x)))]


variable_selection <- function(ds) {
  my_variable_selection <- c( "ctrx_quarter",
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

  other_selection <- c("cproductos","mpayroll","tenure_over_age","balance_ratio","tenure_over_age_imp","balance_ratio_imp",
  "Master_delinquency","Master_status",
  "Visa_delinquency","Visa_status",
  "mmaster_consumo_transacciones_ratio_imp",
  "cmaster_descuentos_transacciones_ratio_imp",
  "mmaster_descuentos_transacciones_ratio_imp",
  "mmaster_consumo_limite_ratio_imp",
  "mmaster_consumo_limitef_ratio_imp",
  "mmaster_payroll_limite_ratio_imp",
  "mmaster_inicio_mora_s_imp",
  "mmaster_inicio_mora_a_imp",
  "mmaster_falta_s_imp",
  "mmaster_falta_a_imp",
  "mmaster_fvencimiento_q_imp",
  "mvisa_consumo_transacciones_ratio_imp",
  "cvisa_descuentos_transacciones_ratio_imp",
  "mvisa_descuentos_transacciones_ratio_imp",
  "mvisa_consumo_limite_ratio_imp",
  "mvisa_consumo_limitef_ratio_imp",
  "mvisa_payroll_limite_ratio_imp",
  "mvisa_inicio_mora_s_imp",
  "mvisa_inicio_mora_a_imp",
  "mvisa_falta_s_imp",
  "mvisa_falta_a_imp",
  "mvisa_fvencimiento_q_imp", "card_tx_payroll",
  "personal_loan_saldo_ratio",
  "personal_loan_payroll_ratio",
  "loan_saldo_ratio")

  mis_variables_ds_selected <- c(my_variable_selection, other_selection) 

  return (fir)
}


prune_canaries <- function(model) {
  model$frame[ model$frame$var %like% "canarito", "complexity"] <- -666
  #print(model)
  modelo_pruned  <- prune(model, -666)
  return (modelo_pruned)
}

OptimizarGanancia <- function(x) {

  params2 <- c(
    "cp" = -1, 
    "minsplit" = x$minsplit, 
    "minbucket" = x$minbucket,
    "maxdepth" = x$maxdepth,
    "event_max_prob" = pred_params["event_max_prob"])

  mean_gan = TreeCVProfitPrediction(dlearn,Formulas["formula_binary_selected"],semillas, "clase_binaria", params2, FALSE)

  cat(paste0(" :: ", format(Sys.time(), "%Y%m%d %H%M%S"),  " ", gsub( ", ", "\t", toString( x ) ), " -> ", mean_gan, "\n" ))
  return (mean_gan)
}

#bayesian_optimization(OptimizarGanancia, hs)


bayesian_optimization <- function(function_optimize, hs) {

  configureMlr( show.learner.output= FALSE)

  #configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
  #por favor, no desesperarse por lo complejo
  obj.fun  <- makeSingleObjectiveFunction(
                fn=       function_optimize,
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
                design = des
                )

  } else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

}


TreeCVProfitPrediction <- function(ds, formula, seeds, class, params, log = TRUE, bird_prunning = FALSE) {
  gan <- c()
  auc <- c()
  for (s in seeds) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds[,get(class)],
      p = 0.70,
      list = FALSE
    )
    train <- ds[in_training, ]
    test <- ds[-in_training, ]
    #### ACA HAGO COSAS

    r <- rpart(
      formula = formula,
      data = train[train$clase_ternaria != "BAJA+1", ],
      xval = 0,
      cp = params["cp"], # esto significa no limitar la complejidad de los splits
      minsplit = params["minsplit"], # minima cantidad de registros para que se haga el split
      minbucket = params["minbucket"], # tamaño minimo de una hoja
      maxdepth = params["maxdepth"]
    )
    if(bird_prunning) {
      print("Bird prunning")
        r <- prune_canaries(r)
    }
    result <- calcular_ganancia_binaria(r, test, class)
    gan <- c(gan, result$profit)
    auc <- c(auc, result$auc)
    if(log) {
      cat(paste0(result$profit, " "))
    }
  }
    if(log) {
      cat(paste0("\n", mean(as.numeric(gan)), " - ", mean(as.numeric(auc)))) 
    }
  return (mean(as.numeric(gan)))
}

calcular_ganancia_binaria <- function(modelo, test, class) {
  pred_testing <- predict(modelo, test, type = "prob")
  roc_pred <-  ROCR::prediction(pred_testing[, "evento"],
                test[,get(class)],
                              label.ordering = c("noevento", "evento"))
  auc_t <-  ROCR::performance(roc_pred, "auc")  

  if(class == 'clase_binaria') {
    print("BINARIA")
    profit = sum(
      (pred_testing[, "evento"] >= pred_params["event_max_prob"]) * ifelse(test[,get(class)] == "evento",
        78000, -2000
      ) / 0.3
    )
  } else if (class == 'clase_binaria_b1b2') {
    print("BINARIA B1B2")
    profit = sum(
      (pred_testing[, "evento"] >= pred_params["event_max_prob"]) * ifelse(test[,'clase_ternaria'] == "BAJA+2",
        78000, -2000
      ) / 0.3
    )
  }


  profit = format(profit, scientific = FALSE)
  return (list("profit" = profit, "auc" = format(unlist(auc_t@y.values), scientific = FALSE)))
}

customize_formulas <- function(vars) {
  campos_train <- paste(vars, collapse = " + ")
  Formulas["formula_binary_selected"] <<- paste0( "clase_binaria ~ - clase_ternaria +", campos_train )
  Formulas["formula_binary_b1b2_selected"] <<- paste0( "clase_binaria_b1b2 ~ - clase_ternaria +", campos_train )
}

create_canaries <- function(ds, n){
  for( i in 1:n ) {
    ds[ , paste0("canarito", i ) :=  runif( nrow(ds)) ]
    config$canarito_vars <<- c(config$canarito_vars, paste0("canarito", i ))
  }
}

class_modeling <- function(ds) {
  if("clase_binaria_b1b2" %in% colnames(ds)) ds[, "clase_binaria_b1b2" := NULL]
  if("clase_binaria" %in% colnames(ds)) ds[, "clase_binaria" := NULL]

  model_binary_class_only_b2(ds)
  model_binary_class_join_b1b2(ds)
}

model_binary_class_join_b1b2 <- function(ds) {
  # Creamos una clase binaria
  ds[, clase_binaria_b1b2 := ifelse(
    clase_ternaria == "CONTINUA",
    "noevento",
    "evento"
  )]
  return (ds)
}

model_binary_class_only_b2 <- function(ds) {
  # Creamos una clase binaria
  ds[, clase_binaria := ifelse(
    clase_ternaria == "BAJA+2",
    "evento",
    "noevento"
  )]
  return (ds)
}


impute_missing_news <- function(ds) {  
  replace_na_inf(ds, new_master_vars, 0, FALSE)
  replace_na_inf(ds, new_visa_vars, 0, FALSE)
  replace_na_inf(ds, new_created_features, 0, FALSE)
  return (ds)
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





####################################################################################################################
#dlearn[,get(names(dlearn)["clase_binaria"])]


transform_data <- function(ds) {
  if("clase_ternaria" %in% colnames(ds)) {
    ds2 <- binary_event_join_b1b2(ds)
    #ds[, clase_ternaria := NULL]
  }
  return (ds2)
}

#dlearn[, 'ctrx_quarter'] < 50
create_new_variables <- function(ds) {
  ds[, "tenure_over_age"] = (ds[, "cliente_antiguedad"] / 12) / ds[, "cliente_edad"]
  ds[, "balance_ratio"] = ds[, "mcuentas_saldo"] / ds[, "mpayroll"]
  ds[, "personal_loan_payroll_ratio"] = ds[, "mprestamos_personales"] / ds[, "mpayroll"]
  ds[, "personal_loan_saldo_ratio"] = ds[, "mprestamos_personales"] / ds[, "mcuentas_saldo"]
  ds[, "loan_saldo_ratio"] = (ds[, "mprestamos_prendarios"] + ds[, "mprestamos_hipotecarios"] + ds[, "mprestamos_personales"]) / ds[, "mcuentas_saldo"]
  #ds[, "ctrx_quarter_cut_off"] = dlearn[, 'ctrx_quarter'] < 9.5

  ds[, "card_tx_payroll"] = (ds[, "ctrx_quarter"] + ds[, "mtarjeta_master_consumo"]) / ds[, "mcuentas_saldo"]

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

credit_card_new_variables <- function(ds) {
  
  ds[,"mvisa_consumo_transacciones_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"ctarjeta_visa_transacciones"]
  ds[,"cvisa_descuentos_transacciones_ratio"] = ds[,"ctarjeta_visa_descuentos"] / (ds[,"ctarjeta_visa_transacciones"] + ds[,"ctarjeta_visa_debitos_automaticos"])
  ds[,"mvisa_descuentos_transacciones_ratio"] = ds[,"mtarjeta_visa_descuentos"] / (ds[,"mtarjeta_visa_consumo"] + ds[,"mttarjeta_visa_debitos_automaticos"])
  ds[,"mvisa_consumo_limite_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"Visa_mlimitecompra"]
  ds[,"mvisa_consumo_limitef_ratio"] = ds[,"mtarjeta_visa_consumo"] / ds[,"Visa_mfinanciacion_limite"]
  ds[,"mvisa_payroll_limite_ratio"] = ds[,"mpayroll"] / ds[,"Visa_mlimitecompra"]
  ds[,"mvisa_consumo_limite_ratio"] = ds[,"Visa_msaldototal"] / ds[,"Visa_mlimitecompra"]

  ds[,"mvisa_inicio_mora_s"] = ds[,Visa_Finiciomora < 180]
  ds[,"mvisa_inicio_mora_a"] = ds[,Visa_Finiciomora < 360]
  ds[,"mvisa_falta_s"] = ds[,Visa_fechaalta < 180]
  ds[,"mvisa_falta_a"] = ds[,Visa_fechaalta < 360]
  ds[,"mvisa_fvencimiento_q"] = ds[,(Visa_Fvencimiento > -90)]

  ds[,"mmaster_consumo_transacciones_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"ctarjeta_master_transacciones"]
  ds[,"cmaster_descuentos_transacciones_ratio"] = ds[,"ctarjeta_master_descuentos"] / (ds[,"ctarjeta_master_transacciones"] + ds[,"ctarjeta_master_debitos_automaticos"])
  ds[,"mmaster_descuentos_transacciones_ratio"] = ds[,"mtarjeta_master_descuentos"] / (ds[,"mtarjeta_master_consumo"] + ds[,"mttarjeta_master_debitos_automaticos"])
  ds[,"mmaster_consumo_limite_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"Master_mlimitecompra"]
  ds[,"mmaster_consumo_limitef_ratio"] = ds[,"mtarjeta_master_consumo"] / ds[,"Master_mfinanciacion_limite"]
  ds[,"mmaster_payroll_limite_ratio"] = ds[,"mpayroll"] / ds[,"Master_mlimitecompra"]
  ds[,"mmaster_consumo_limite_ratio"] = ds[,"Master_msaldototal"] / ds[,"Master_mlimitecompra"]

  ds[,"mmaster_inicio_mora_s"] = ds[,Master_Finiciomora < 180]
  ds[,"mmaster_inicio_mora_a"] = ds[,Master_Finiciomora < 360]
  ds[,"mmaster_falta_s"] = ds[,Master_fechaalta < 180]
  ds[,"mmaster_falta_a"] = ds[,Master_fechaalta < 360]
  ds[,"mmaster_fvencimiento_q"] = ds[,(Master_Fvencimiento > -90)]

  return (ds)
}

feature_engineering <- function(ds) {
  ds <- binning_variables(ds)
  #ds <- expliod_variables(ds)
  ds <- create_new_variables(ds)
  ds <- credit_card_new_variables(ds)
  return(ds)
}



#### ------------------------------------------------------------------------------------------------------------
#### UTILITARIOS ------------------------------------------------------------------------------------
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

binary_event_no_b1 <- function(ds) {
  # Creamos una clase binaria
  ds = ds[, clase_binaria := ifelse(
    clase_ternaria == "BAJA+2",
    "evento",
    "noevento"
  )]
  return (ds)
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
    leaves[, n := evento + noevento]
    leaves[, p := round(evento / n,4)]
    leaves <- leaves[order(-p),]
    leaves[, gan := `BAJA+2` * 78000 - (CONTINUA + `BAJA+1`) * 2000]
    leaves[, ':='(evento = NULL, noevento = NULL)]
    setnames(leaves, old = c("BAJA+1", "BAJA+2", "CONTINUA", "n", "p", "gan"),
                    new = c(paste0(prefix, "b1"),
                            paste0(prefix, "b2"),
                            paste0(prefix, "cont"),
                            paste0(prefix, "n"),
                            paste0(prefix, "p"),
                            paste0(prefix, "gan")))
    leaves[]
}


#### ------------------------------------
#### OUTPUT -----------------------------
#### ------------------------------------
#calcular_ganancia_binaria(modelo, dtest)
#calcular_ganancia_ternaria(modelo, dtest)




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



## cutoff variables
# data = dtrain[, !c("ctrx_quarter","binned_ctrx_quarter","active_quarter","mcaja_ahorro","binned_mcaja_ahorro","mpasivos_margen","binned_mpasivos_margen","ccomisiones_otras",
#  "mtarjeta_visa_consumo","mvisa_consumo_transacciones_ratio_imp","mvisa_consumo_limitef_ratio_imp","ctarjeta_visa_transacciones","mvisa_consumo_limite_ratio_imp","ctarjeta_visa","mautoservicio",
#  "ctarjeta_debito_transacciones","Visa_delinquency","Visa_status","mcuentas_saldo","card_tx_payroll","cproductos","binned_mcuentas_saldo","cpayroll_trx","mpayroll")],



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


grip_search_optimization(dlearn[, ..to_optimize_selection])



calcular_ganancia_ternaria <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "BAJA+2"] >= 0.025) * ifelse(test$clase_ternaria == "BAJA+2",
      78000, -2000
    ) / 0.3
  )
}


