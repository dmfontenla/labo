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


base_dir = "/Users/dfontenla/Maestria/2022C2/DMEyF/experimento/"
setwd( base_dir)

prob_cols = c("prob_pos_21.csv", "prob_pos_04.csv", "prob_pos_13.csv","prob_pos_01.csv", "prob_pos_35.csv","prob_pos_25.csv",
"prob_pos_02.csv", "prob_pos_15.csv")


rank_cols = c("rank_prob_pos_21.csv","rank_prob_pos_04.csv","rank_prob_pos_13.csv","rank_prob_pos_01.csv", 
"rank_prob_pos_35.csv", "rank_prob_pos_25.csv","rank_prob_pos_02.csv","rank_prob_pos_15.csv")

files = list.files(base_dir)
files
models = data.table()

for (i in files){
  print(i)
  probs = read.csv(i, sep="\t")
  setorder(  probs, -numero_de_cliente)
  models$num_cli = probs$numero_de_cliente
  models[,paste0('prob_', i)] = probs$prob
}
models
models$prob_avg = rowMeans(models[,..prob_cols])
models$prob_max <- apply(models[,..prob_cols], 1, max, na.rm=TRUE)

models$prob_avg_bests = rowMeans(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_02.csv")])
models$prob_max_bests <- apply(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_02.csv")], 1, max, na.rm=TRUE)

models$prob_avg_lows = rowMeans(models[,c("prob_pos_13.csv", "prob_pos_15.csv", "prob_pos_02.csv")])
models$prob_max_lows <- apply(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_02.csv")], 1, max, na.rm=TRUE)

setorder(models, -prob_pos_21.csv)
models$rank_prob_pos_21.csv = 1:nrow(models)
models
setorder(models, -prob_pos_04.csv)
models$rank_prob_pos_04.csv = 1:nrow(models)

setorder(models, -prob_pos_13.csv)
models$rank_prob_pos_13.csv = 1:nrow(models)

setorder(models, -prob_pos_01.csv)
models$rank_prob_pos_01.csv = 1:nrow(models)

setorder(models, -prob_pos_35.csv)
models$rank_prob_pos_35.csv = 1:nrow(models)

setorder(models, -prob_pos_25.csv)
models$rank_prob_pos_25.csv = 1:nrow(models)


#--------

setorder(models, -prob_pos_02.csv)
models$rank_prob_pos_02.csv = 1:nrow(models)

setorder(models, -prob_pos_15.csv)
models$rank_prob_pos_15.csv = 1:nrow(models)


models

models$prob_rank_avg = rowMeans(models[,..rank_cols])
models
models[1:10]

setorder(models, -num_cli)





# for(i in prob_cols) {
#   setorder(models, -..i)
#   models <- cbind(ID = 1:nrow(data), data)
# }
#data1 <- cbind(ID = 1:nrow(data), data)
#

july = 202107

dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competenciaFINAL_2022.csv.gz")
djuly = dataset[ foto_mes %in% july]
djuly

cortes  <- seq( from=  9000,
                  to=   12500,
                  by=     500 )


str(models)
dscheck = data.table()
dscheck$num_cli = models$num_cli
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
    dscheck$num_cli = models$num_cli
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
results[, "prob_rank_avg"] <- 0
results
for( corte in cortes ) {
  dscheck = data.table()
  dscheck$num_cli = models$num_cli
  dscheck$prob = models[,"prob_rank_avg"]
  setorder( dscheck, prob )
  dscheck[  , Predicted := 0L ]
  dscheck[ 1:corte, Predicted := 1L ]
  ganancia = calcular_ganancia(dscheck[Predicted == 1L])
  print(ganancia)
  results[envios == corte, "prob_rank_avg"] <- ganancia
  print(results)

}
results


library("ggplot2") 
results
data_long <- melt(results, id = "envios")
gfg_plot <- ggplot(data_long,            
               aes(x = envios,
                   y = value,
                   color = variable)) +  geom_line()
gfg_plot


# gfg_plot <- ggplot(results, aes(envios)) +  
#     geom_line(aes(y = prob_avg), color = "black") +
#      geom_line(aes(y = prob_exp_ZZ9410_pred_01_024.csv), color = "red") +
#     geom_line(aes(y = prob_exp_ZZ9420_TRAINING2019202_masenvios_pred_01_072.csv), color = "green") +
#     geom_line(aes(y = prob_pred_01_069.csv), color = "blue") +
#     geom_line(aes(y = prob_exp_ZZ9410_lag6_39_under_pred_01_025.csv), color = "purple")
# gfg_plot

results



results
str(dscheck)
dscheck$num_cli
str(djuly)
str(results)

calcular_ganancia = function(ds) {
  ganancia = 0
  for(nc in ds$num_cli){
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