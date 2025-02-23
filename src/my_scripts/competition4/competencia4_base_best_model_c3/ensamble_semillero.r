# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# Parametros del script
PARAM <- list()
PARAM$experimento <- "ZZ1292_semillerio_kaggle_bmc3"
PARAM$exp_input <- "ZZ9410_semillerio_ensamble_bmc3"

# Genera los csv de los experimentos de cada semilla
# Es posible que no se desee activar porque va a crear muchos csv que posiblemente no se utilicen
PARAM$generar_salidas_individuales <- TRUE

# Decide si para finalizar la predicción usa el ranking o se queda con las probabilidades
PARAM$use_rank_final <- FALSE

# cantidad de envios
PARAM$corte <- 11000
# FIN Parametros del script

options(error = function() {
    traceback(20)
    options(error = NULL)
    stop("exiting after script error")
})

base_dir <- "~/buckets/b1/"

# creo la carpeta donde va el experimento
dir.create(paste0(base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0(base_dir, "exp/", PARAM$experimento, "/")) # Establezco el Working Directory DEL EXPERIMENTO

path_experimento_semillerio <- paste0(base_dir, "exp/", PARAM$exp_input)
path_experimento_semillerio
archivos <- list.files(path = path_experimento_semillerio, pattern = "_resultados.csv")

# Esto es MUY dependiente del formato del nombre de los experimentos z992, se puede romper muy facil
ksemillas <- lapply(strsplit(archivos, "_"), function(partes_nombre_archivo) {
    # 1      2          3        4  5   6   7      8
    # ZZ9410_semillerio_ensamble_M1_S61_S80_683257_resultados.csv
    return(partes_nombre_archivo[7]) # la posicion de la semilla en el nombre es 7
})


PARAM$exp_input_ht  <- "HT9410_4_BMC3"
#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input_ht, "/TrainingStrategy.txt" )
arch_TS

TS  <- readLines( arch_TS, warn=FALSE )
# Levantar dataset C4
# leo el dataset a partir del cual voy a calcular las ganancias
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dataset_septiembre <- fread(arch_future)


# Tabla que contendrá los rankings de todos los clientes para todas las semillas
tb_ranking_semillerio <- data.table(numero_de_cliente = dataset_septiembre[, numero_de_cliente])
archivos
for (archivo in archivos) {

    ksemilla <- strtoi(sapply(strsplit(archivo, "_"), "[", 3))

    # cols: numero_de_cliente,foto_mes,prob,rank
    tb_prediccion <- fread(paste0(path_experimento_semillerio, "/", archivo))
    setorder(tb_prediccion, numero_de_cliente)
    setorder(tb_ranking_semillerio, numero_de_cliente)
    print(PARAM$use_rank_final)
    if (FALSE) {
        # Generamos predicción del semillerio en base al rank
        tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$rank]

        # Generamos predicción individual
        setorder(tb_prediccion, rank)
    } else {
        print("usemos probabilidades")
        # usamos la probabilidad, no el rank
        # Generamos predicción del semillerio en base al rank
        tb_ranking_semillerio[, paste0("rank_", ksemilla) := tb_prediccion$prob]

        # Generamos predicción individual
        setorder(tb_prediccion, prob)
    }

    tb_prediccion[, Predicted := 0]
    tb_prediccion[1:PARAM$corte, Predicted := 1L]
    if (PARAM$generar_salidas_individuales) {
        nombre_arch_individual <- paste0(
            PARAM$experimento,
            "_",
            "individual",
            "_",
            sprintf("S%d", ksemilla),
            "_",
            ifelse(PARAM$use_rank_final, "rank", "proba"),
            "_",
            sprintf("C%d", PARAM$corte),
            ".csv"
            )
        fwrite(
            tb_prediccion[, list(numero_de_cliente, Predicted)],
            file = nombre_arch_individual,
            sep = ","
        )
    }

    # Esta es la predicción del semillerio para la semilla i-esima
    tb_prediccion_semillerio <- data.table(
        tb_ranking_semillerio[, list(numero_de_cliente)],
        prediccion = rowMeans(tb_ranking_semillerio[, c(-1)]) # excluye el numero_de_cliente del cálculo de la media
    )
    setorder(tb_prediccion_semillerio, prediccion) # Esto es un ranking, entonces de menor a mayor
    tb_prediccion_semillerio[, Predicted := 0]
    tb_prediccion_semillerio[1:PARAM$corte, Predicted := 1L]
}

tb_prediccion_semillerio
nombre_arch_ensamble <- paste0(
    PARAM$experimento,
    "_",
    "ensamble",
    "_",
    ifelse(PARAM$use_rank_final, "rank", "proba"),
    "_",
    sprintf("C%d", PARAM$corte),
    ".csv"
)
fwrite(
    tb_prediccion_semillerio[, list(numero_de_cliente, Predicted)],
    file = nombre_arch_ensamble,
    sep = ","
)

nombre_arch_ensamble_proba <- paste0(
  PARAM$experimento,
  "_",
  "ensamble",
  "_",
  ifelse(PARAM$use_rank_final, "rank", "proba_all"),
  "_",
  sprintf("C%d", PARAM$corte),
  ".csv"
)

setorder(tb_prediccion_semillerio, numero_de_cliente)

fwrite(
  tb_prediccion_semillerio[, list(numero_de_cliente, prediccion)],
  file = nombre_arch_ensamble_proba,
  sep = ","
)
