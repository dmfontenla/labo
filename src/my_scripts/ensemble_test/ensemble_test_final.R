#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#Loading the required libraries
install.packages("RANN")
library('RANN')
install.packages('caret')
library('caret')
#Seeting the random seed
set.seed(1)

#Aca ingresar la carpeta donde estan las predicciones
base_dir = "/Users/dfontenla/Maestria/2022C2/DMEyF/c4_models/"
setwd( base_dir)

#prob_cols = c("prob_pos_21.csv", "prob_pos_04.csv", "prob_pos_13.csv","prob_pos_01.csv", "prob_pos_35.csv","prob_pos_25.csv",
#"prob_pos_02.csv", "prob_pos_15.csv", "prob_pos_31.csv")


#rank_cols = c("rank_prob_pos_21.csv","rank_prob_pos_04.csv","rank_prob_pos_13.csv","rank_prob_pos_01.csv", 
#"rank_prob_pos_35.csv", "rank_prob_pos_25.csv","rank_prob_pos_02.csv","rank_prob_pos_15.csv","rank_prob_pos_31.csv")

prob_cols = c()
rank_cols = c()

files = list.files(base_dir)
files
models = data.table()

for (i in files){
  print(i)
  probs = read.csv(i, sep="\t")
  setorder(  probs, -numero_de_cliente)
  models$numero_de_cliente = probs$numero_de_cliente
  models[,paste0('prob_', i)] = probs$prob
  prob_cols <- c(prob_cols,paste0("prob_",i))
  rank_cols <- c(rank_cols,paste0("rank_prob_",i))
}

models
models$prob_avg = rowMeans(models[,..prob_cols])
models$prob_max <- apply(models[,..prob_cols], 1, max, na.rm=TRUE)

setorder(models, -num_numero_de_clientecli)

july = 202107

dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competenciaFINAL_2022.csv.gz")
djuly = dataset[ foto_mes %in% july]
djuly

cortes  <- seq( from=  8500,
                  to=   10500,
                  by=     500 )


str(models)
dscheck = data.table()
dscheck$numero_de_cliente = models$numero_de_cliente
col = prob_cols[1]
dscheck$prob = models[,..col]
dscheck

results = data.table()

for( corte in cortes ) {
  new_row <- data.table("envios" = corte)
  results <- rbindlist(list(results, new_row),fill=TRUE)
  for(model in c(prob_cols,"prob_avg","prob_max")) {
    if(!model %in% colnames(results)){
      results[, eval(model)] <- 0
    }
    dscheck = data.table()
    dscheck$numero_de_cliente = models$numero_de_cliente
    dscheck$prob = models[,..model]
    setorder( dscheck, -prob )
    dscheck[  , Predicted := 0L ]
    dscheck[ 1:corte, Predicted := 1L ]
    print(model)
    ganancia = calcular_ganancia(dscheck[Predicted == 1L])
    print(ganancia)

    results[envios == corte, eval(model)] <- ganancia

  }
  print(results)
}

models


library("ggplot2") 
results
data_long <- melt(results, id = "envios")
gfg_plot <- ggplot(data_long,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

str(results)



data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot


# POSITIONS COMPETANCIA 3
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple") + labs()
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS AVG ALL 3

gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_avg), color = "red") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv","prob_avg")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS RANK ALL 3

gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_rank_avg), color = "red") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv","prob_rank_avg")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS MAX 3

gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_max), color = "red") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv","prob_max")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS ENSABLES + POS 01 / 02
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_rank_avg), color = "blue") +
    geom_line(aes(y = prob_max), color = "red") +
    geom_line(aes(y = prob_avg), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_rank_avg",
"prob_max","prob_avg")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS COMPETANCIA BESTS
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_pos_01.csv), color = "black") +
     geom_line(aes(y = prob_pos_02.csv), color = "yellow") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_avg_bests), color = "orange") +
    geom_line(aes(y = prob_max_bests), color = "pink")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_02.csv","prob_pos_04.csv",
"prob_avg_bests","prob_max_bests")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot
# POSITIONS COMPETANCIA LOWS
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_avg_lows), color = "black") +
    geom_line(aes(y = prob_max_lows), color = "yellow") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv","prob_max_lows","prob_avg_lows")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS COMPETANCIA Sin el mejor
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_avg_without_best), color = "black") +
    geom_line(aes(y = prob_max_without_best), color = "yellow") +
    geom_line(aes(y = prob_pos_02.csv), color = "red") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_25.csv), color = "pink") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue") +
    geom_line(aes(y = prob_pos_15.csv), color = "purple")
gfg_plot

data_base <- melt(results[,c("envios", "prob_max_without_best","prob_pos_02.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_pos_15.csv","prob_pos_31.csv","prob_avg_without_best")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot

# POSITIONS COMPETANCIA diferentes
gfg_plot <- ggplot(results, aes(envios)) +  
    geom_line(aes(y = prob_avg_differents), color = "black") +
    geom_line(aes(y = prob_max_differents), color = "yellow") +
    geom_line(aes(y = prob_pos_01.csv), color = "red") +
    geom_line(aes(y = prob_pos_04.csv), color = "green") +
    geom_line(aes(y = prob_pos_21.csv), color = "orange") +
    geom_line(aes(y = prob_pos_35.csv), color = "grey") +
    geom_line(aes(y = prob_pos_13.csv), color = "blue")
gfg_plot

data_base <- melt(results[,c("envios", "prob_pos_01.csv","prob_pos_04.csv","prob_pos_21.csv","prob_pos_25.csv",
"prob_pos_35.csv","prob_pos_13.csv","prob_avg_differents","prob_max_differents")], id = "envios")
gfg_plot <- ggplot(data_base,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot


ggplot(results, aes(x = envios, y = prob_avg)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_max)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_avg_bests)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_max_bests)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_avg_lows)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_max_lows)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_avg_without_best)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_max_without_best)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_avg_differents)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_max_differents)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_rank_avg)) +  geom_line()
ggplot(results, aes(x = envios, y = prob_rank_avg)) +  geom_line()

for (i in results) {
   gfg_plot <- ggplot(results, aes(x = envios, y = i, color = variable)) +  geom_line()
   gfg_plot
}



results[, maximum_element := do.call(pmax, .SD), .SDcols = 1:4]
results
# gfg_plot <- ggplot(results, aes(envios)) +  
#     geom_line(aes(y = prob_avg), color = "black") +
#      geom_line(aes(y = prob_exp_ZZ9410_pred_01_024.csv), color = "red") +
#     geom_line(aes(y = prob_exp_ZZ9420_TRAINING2019202_masenvios_pred_01_072.csv), color = "green") +
#     geom_line(aes(y = prob_pred_01_069.csv), color = "blue") +
#     geom_line(aes(y = prob_exp_ZZ9410_lag6_39_under_pred_01_025.csv), color = "purple")
# gfg_plot

results
for(i in colnames(results)){
  print(i)
  print(max(results[,..i]))
}




results
str(dscheck)
dscheck$numero_de_cliente
str(djuly)
str(results)

calcular_ganancia = function(ds) {
  ganancia = 0
  for(nc in ds$numero_de_cliente){
     if(djuly[numero_de_cliente == nc]$clase_ternaria == 'BAJA+2') {
      ganancia = ganancia + 78000
     }else{
      ganancia = ganancia - 2000
     } 
  }
  return (ganancia)
}

#calcular_ganancia = function(ds) {
#  sum(ifelse(djuly[ds$num_cli]$clase_ternaria == 'BAJA+2', 78000, -2000))
#}


 # for( i in ds$num_cli){
    #if(djuly[numero_de_cliente = i][numero_de_cliente == i]$clase_ternaria == 'BAJA+2') {
#    sum(ifelse(djuly[ds$num_cli]$clase_ternaria == 'BAJA+2', 78000, -2000))
     # print("gano")
    #}
  #}
#}
#df2 <- melt(leaderboad2, measure.vars =  c("publico", "publico2"))
#ggplot(df2, aes(x = value, color = variable)) + geom_density()


str(djuly)

as.list(prob_cols)
models[,..prob_cols]