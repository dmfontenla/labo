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
base_dir = "/Users/dfontenla/Maestria/2022C2/DMEyF/experimento/"
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
  models$num_cli = probs$numero_de_cliente
  models[,paste0('prob_', i)] = probs$prob
  prob_cols <- c(prob_cols,paste0("prob_",i))
  rank_cols <- c(rank_cols,paste0("rank_",i))
}

models
models$prob_avg = rowMeans(models[,..prob_cols])
models$prob_max <- apply(models[,..prob_cols], 1, max, na.rm=TRUE)

models$prob_avg_bests = rowMeans(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_02.csv")])
models$prob_max_bests <- apply(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_02.csv")], 1, max, na.rm=TRUE)

models$prob_avg_lows = rowMeans(models[,c("prob_pos_13.csv", "prob_pos_15.csv", "prob_pos_21.csv", "prob_pos_25.csv", "prob_pos_35.csv", "prob_pos_31.csv")])
models$prob_max_lows <- apply(models[,c("prob_pos_13.csv", "prob_pos_15.csv", "prob_pos_21.csv", "prob_pos_25.csv", "prob_pos_35.csv", "prob_pos_31.csv")], 1, max, na.rm=TRUE)

models$prob_avg_without_best = rowMeans(models[,c("prob_pos_21.csv", "prob_pos_04.csv", "prob_pos_13.csv", "prob_pos_35.csv","prob_pos_25.csv", "prob_pos_02.csv", "prob_pos_15.csv", "prob_pos_31.csv")])
models$prob_max_without_best <- apply(models[,c("prob_pos_21.csv", "prob_pos_04.csv", "prob_pos_13.csv", "prob_pos_35.csv","prob_pos_25.csv", "prob_pos_02.csv", "prob_pos_15.csv", "prob_pos_31.csv")], 1, max, na.rm=TRUE)

models$prob_avg_differents = rowMeans(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_13.csv", "prob_pos_35.csv","prob_pos_21.csv")])
models$prob_max_differents <- apply(models[,c("prob_pos_01.csv", "prob_pos_04.csv", "prob_pos_13.csv", "prob_pos_35.csv","prob_pos_21.csv", "prob_pos_35.csv")], 1, max, na.rm=TRUE)


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

setorder(models, -prob_pos_31.csv)
models$rank_prob_pos_31.csv = 1:nrow(models)


models

setorder(models, -num_cli)

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

september = 202109

dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competenciaFINAL_2022.csv.gz")
dseptember = dataset[ foto_mes %in% september]
dseptember

cortes  <- seq( from=  8500,
                  to=   10500,
                  by=     500 )


str(models)
dscheck = data.table()
dscheck$num_cli = models$num_cli
col = prob_cols[1]
dscheck$prob = models[,..col]
dscheck

experimento <- 'ensamble'

results = data.table()
base_dir_results = "/Users/dfontenla/Maestria/2022C2/DMEyF/final_results"
for( corte in cortes ) {
  new_row <- data.table("envios" = corte)
  results <- rbindlist(list(results, new_row),fill=TRUE)
  for(model in c(prob_cols,"prob_avg","prob_max","prob_avg_bests","prob_max_bests","prob_avg_lows","prob_max_lows",
  "prob_avg_without_best","prob_max_without_best","prob_avg_differents","prob_max_differents")) {
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

    nom_submit  <- paste0( base_dir_results, "/", experimento, 
                        "_",
                        sprintf( "%s", model ),
                        "_",
                        sprintf( "%05d", corte ),
                        ".csv" )

    fwrite(  dscheck[ , list( num_cli, Predicted ) ],
             file= nom_submit,
             sep= "," )

    #ganancia = calcular_ganancia(dscheck[Predicted == 1L])
    #print(ganancia)

    #results[envios == corte, eval(model)] <- ganancia

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
  #ganancia = calcular_ganancia(dscheck[Predicted == 1L])
  #print(ganancia)
  #results[envios == corte, "prob_rank_avg"] <- ganancia
  #print(results)

}
results



str(dseptember)

as.list(prob_cols)
models[,..prob_cols]