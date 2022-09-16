# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot


# install.packages( "data.table", dependencies= TRUE )
# install.packages( "rpart", dependencies= TRUE )
# install.packages( "rpart.plot", dependencies= TRUE )

rm(list = ls())
gc(verbose = FALSE)


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
install.packages('rattle')
library(rattle)				

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("./") # Establezco el Working Directory
semillas <- c(864379, 300647, 125707, 962303, 983363)

# cargo el dataset
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia1_2022.csv")

dlearn <- dataset[foto_mes == 202101] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202103] # defino donde voy a aplicar el modelo

#GLOBALS

event_max_prob <- 0.0515

tree_params <- c(
  "cp" = -1, 
  "minsplit" = 500, 
  "minbucket" = 100,
  "maxdepth" = 7,
  "formula" = clase_binaria ~ .
)

var_list_map <- c(
  "new_attr" = c(),
  "generated_attr" = c(),
  "base_attr" = c()
)


# POR EL MOMENTO NO MANEJO OUTLIERS O TRANSFORMO LAS DISTRIBUCIONES / ENTENDER COMO ES LA ASIGNACION Y EL PASO DE PARAMETROS POR VALOR O REFERENCIA EN R
dlearn <- impute_missing_initial(dlearn)
dlearn <- transform_data(dlearn)
dlearn <- feature_engineering(dlearn)
dlearn <- impute_missing_news(dlearn)

variables_selected <- variable_selection(dlearn)
campos_train <- paste(variables_selected, collapse = " + ")
formula_train <- paste0( "clase_binaria ~ - clase_ternaria +", campos_train )
campos_train
formula_train
summary(dlearn)

to_optimize_selection <- c(variables_selected,"clase_binaria")
dlearn[, ..to_optimize_selection]

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dlearn$clase_binaria, p = 0.70, list = FALSE)
#in_training <- caret::createDataPartition(dlearn$clase_ternaria, p = 0.70, list = FALSE)
dtrain <- dlearn[in_training, ]
dtest <- dlearn[-in_training, ]

modelo <- rpart(
  formula_train,
  #formula=   "clase_binaria ~ .", # quiero predecir clase_ternaria a partir de el resto de las variables
  data = dtrain,
  xval = 0,
  cp = tree_params["cp"],
  minsplit = tree_params["minsplit"], # minima cantidad de registros para que se haga el split
  minbucket = tree_params["minbucket"], # tamaño minimo de una hoja
  maxdepth = tree_params["maxdepth"]
) 

dtrain[,"ctrx_quarter"]

pdf(paste0("/Users/dfontenla/Maestria/2022C2/DMEyF/repo/exp/KA2001/img/", "tree.pdf"))
fancyRpartPlot(modelo)
dev.off()


dlearn[, ..to_optimize_selection][, !c("ctrx_quarter","binned_ctrx_quarter")]

print(modelo$variable.importance)
summary(modelo)
summary(dlearn)

dlearn[, clase_ternaria]
train_leaves <- leaves_table(modelo, dlearn, "clase_binaria")
train_leaves[, gan_acum := cumsum(gan) / 0.7]
train_leaves[, n_acum := cumsum(n) / 0.7]
print(train_leaves)



	# Fancy tree plot

binary_formula = clase_binaria ~ .
#experimento(formula_train)
TreeCVProfitPrediction(dlearn, formula_train, semillas)

# grafico el arbol
prp(modelo, extra = 101, digits = 5, branch = 1, type = 4, varlen = 0, faclen = 0)
prp(modelo, cex = 3)

dapply <- impute_missing_initial(dapply)
dapply <- feature_engineering(dapply)
dapply <- impute_missing_news(dapply)


dapply
#calcular_ganancia_binaria(modelo, dapply)

# aplico el modelo a los datos nuevos
test_prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := test_prediccion[, "evento"]]
#dapply[, prob_baja2 := prediccion[, "BAJA+2"]]


# solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > event_max_prob)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
# dir.create( "./exp/" )
# dir.create( "./exp/KA2001" )

fwrite(dapply[, list(numero_de_cliente, Predicted)], # solo los campos para Kaggle
  file = paste("./exp/KA2001/K101_001.csv", Sys.time()),
  sep = ","
)





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

other_selection <- c("cproductos","mpayroll",
"tenure_over_age_imp","balance_ratio_imp",
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
"mvisa_fvencimiento_q_imp", 
"card_tx_payroll",
"personal_loan_saldo_ratio",
"personal_loan_payroll_ratio",
"loan_saldo_ratio")

mis_variables_ds_selected <- c(my_variable_selection, other_selection) 

return (mis_variables_ds_selected)
}


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


replace_na_inf <- function(ds, attrs, val, replace) {
  for(attr in attrs) {
    suffix = ""
    if(replace == FALSE) suffix = "_imp"
    print(paste0(attr,"_imp"))
    ds[, paste0(attr,suffix) := ifelse((is.infinite(get(attr)) |  is.nan(get(attr)) |  is.na(get(attr))), val, get(attr))] 
  }
  return (ds)
}

#dlearn[, balance_ratio_imp := ifelse((is.infinite(balance_ratio) |  is.nan(balance_ratio) |  is.na(balance_ratio)), 0, balance_ratio)] 


#dlearn[,balance_ratio_imp]
#dlearn[mmaster_falta_a == TRUE, .N]

#dlearn[dlearn[,(Master_Fvencimiento > -90)], .N]

cc_new_variables <- function(ds) {
  
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
  ds <- cc_new_variables(ds)
  return(ds)
}

impute_missing_news <- function(ds) {
  new_crated_features <- c("tenure_over_age","balance_ratio") # nolint
  new_master_vars <- c("mmaster_consumo_transacciones_ratio", # nolint
  "cmaster_descuentos_transacciones_ratio",
  "mmaster_descuentos_transacciones_ratio",
  "mmaster_consumo_limite_ratio",
  "mmaster_consumo_limitef_ratio",
  "mmaster_payroll_limite_ratio",
  "mmaster_consumo_limite_ratio",
  "mmaster_inicio_mora_s",
  "mmaster_inicio_mora_a",
  "mmaster_falta_s",
  "mmaster_falta_a",
  "mmaster_fvencimiento_q"
  )


  new_visa_vars <- c("mvisa_consumo_transacciones_ratio", # nolint
  "cvisa_descuentos_transacciones_ratio",
  "mvisa_descuentos_transacciones_ratio",
  "mvisa_consumo_limite_ratio",
  "mvisa_consumo_limitef_ratio",
  "mvisa_payroll_limite_ratio",
  "mvisa_consumo_limite_ratio",
  "mvisa_inicio_mora_s",
  "mvisa_inicio_mora_a",
  "mvisa_falta_s",
  "mvisa_falta_a",
  "mvisa_fvencimiento_q"
  )
  
  replace_na_inf(ds, new_master_vars, 0, FALSE)
  replace_na_inf(ds, new_visa_vars, 0, FALSE)
  replace_na_inf(ds, new_crated_features, 0, FALSE)

  return (ds)
}

impute_missing_initial <- function(ds) {
  replace_na_inf(ds, c("Master_delinquency","Master_status","Visa_delinquency","Visa_status"), -1, TRUE)
  return (ds)
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


grip_search_optimization(dlearn[, ..to_optimize_selection])

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


binning_variables <- function(ds) {
  # Supongamos que tenemos una lista de variables a las que queremos transformar
  mis_variables_a_binear <- c("ctrx_quarter",
                      "mprestamos_personales",
                      "mcuentas_saldo",
                      "mactivos_margen",
                      "mcaja_ahorro",
                      "mcuenta_corriente",
                      "mpayroll",
                      "mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen", "mpasivos_margen")

  # A todas las vamos a rankear

  prefix <- "binned_"
  for (var in mis_variables_a_binear) {
      ds[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  }
  return(ds)
}


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


