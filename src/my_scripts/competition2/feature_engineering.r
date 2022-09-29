
do_feature_engineering <- function(data) {
    data = create_new_variables(data)    
    data = credit_card_new_variables(data)    

    #valvula de seguridad para evitar valores infinitos
    #paso los infinitos a NULOS
    infinitos      <- lapply(names(data),function(.name) data[ , sum(is.infinite(get(.name)))])
    infinitos_qty  <- sum( unlist( infinitos) )
    if( infinitos_qty > 0 )
    {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    data[mapply(is.infinite, data)] <- NA
    }


    #valvula de seguridad para evitar valores NaN  que es 0/0
    #paso los NaN a 0 , decision polemica si las hay
    #se invita a asignar un valor razonable segun la semantica del campo creado
    nans      <- lapply(names(data),function(.name) data[ , sum(is.nan(get(.name)))])
    nans_qty  <- sum( unlist( nans) )
    if( nans_qty > 0 )
    {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    data[mapply(is.nan, data)] <- 0
    }
    return (data)
}


create_new_variables <- function(ds) {
    ds[, "tenure_over_age"] = (ds[, "cliente_antiguedad"] / 12) / ds[, "cliente_edad"]
    ds[, "payroll_saldo_ratio"] = ds[, "mcuentas_saldo"] / ds[, "mpayroll"]
    #  replace_na_inf(ds, c("payroll_saldo_ratio"), 0, TRUE) 

    ds[, "personal_loan_payroll_ratio"] = ds[, "mprestamos_personales"] / ds[, "mpayroll"]
    #  replace_na_inf(ds, c("personal_loan_payroll_ratio"), 0, TRUE) 

    ds[, "personal_loan_saldo_ratio"] = ds[, "mprestamos_personales"] / ds[, "mcuentas_saldo"]
    # replace_na_inf(ds, c("personal_loan_saldo_ratio"), 0, TRUE) 

    ds[, "loan_saldo_ratio"] = (ds[, "mprestamos_prendarios"] + ds[, "mprestamos_hipotecarios"] + ds[, "mprestamos_personales"]) / ds[, "mcuentas_saldo"]
    #  replace_na_inf(ds, c("loan_saldo_ratio"), 0, TRUE) 
    ds[, "loan_payroll_ratio"] = (ds[, "mprestamos_prendarios"] + ds[, "mprestamos_hipotecarios"] + ds[, "mprestamos_personales"]) / ds[, "mpayroll"]
    #  replace_na_inf(ds, c("loan_payroll_ratio"), 0, TRUE) 

    ds[, "tx_payroll"] = ds[, "mpayroll"] / ds[, "ctrx_quarter"]
    #  replace_na_inf(ds, c("tx_payroll"), 0, TRUE) 
    ds[, "tx_saldo"] = ds[, "mcuentas_saldo"] / ds[, "ctrx_quarter"]
    #  replace_na_inf(ds, c("tx_saldo"), 0, TRUE) 


    ds[, "commissions_payroll_ratio"] = ds[, "mcomisiones"] / ds[, "mpayroll"]
    #  replace_na_inf(ds, c("commissions_payroll_ratio"), 0, TRUE) 

    ds[, "commissions_saldo_ratio"] = ds[, "mcomisiones"] / ds[, "mcuentas_saldo"]
    #  replace_na_inf(ds, c("commissions_saldo_ratio"), 0, TRUE) 

    ds[, "commissions_qtx_ratio"] = ds[, "mcomisiones"] / ds[, "ctrx_quarter"]
    #  replace_na_inf(ds, c("commissions_qtx_ratio"), 0, TRUE) 

    ds[, "mpasivos_margen_payroll_ratio"] = ds[, "mpasivos_margen"] / ds[, "mpayroll"]
    #  replace_na_inf(ds, c("mpasivos_margen_payroll_ratio"), 0, TRUE) 

    ds[, "mpasivos_margen_saldo_ratio"] = ds[, "mpasivos_margen"] / ds[, "mcuentas_saldo"]
    #  replace_na_inf(ds, c("mpasivos_margen_saldo_ratio"), 0, TRUE) 

    ds[, "mpasivos_margen_qtx_ratio"] = ds[, "mpasivos_margen"] / ds[, "ctrx_quarter"]
    #  replace_na_inf(ds, c("mpasivos_margen_qtx_ratio"), 0, TRUE) 


    #INICIO de la seccion donde se deben hacer cambios con variables nuevas

    #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
    ds[  , ctrx_quarter_normalizado := ctrx_quarter ]
    ds[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
    ds[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
    ds[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

    #variable extraida de una tesis de maestria de Irlanda
    ds[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]
    return (ds)
}


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

    ds[,"visa_pagominimo_vsaldo"] = ds[,"Visa_mpagominimo"] / ds[,"Visa_msaldototal"]
    ds[,"visa_pagominimo_limite"] = ds[,"Visa_mpagominimo"] / ds[,"Visa_mlimitecompra"]

    ds[,"visa_adelanto_saldo"] = ds[,"Visa_madelantopesos"] / ds[,"Visa_msaldototal"]
    ds[,"visa_adelanto_payroll"] = ds[,"Visa_madelantopesos"] / ds[,"mpayroll"]

    ds[,"visa_payroll_saldo"] = ds[,"mpayroll"] / ds[,"Visa_msaldototal"]

    ds[,"master_vsaldo_limite"] = ds[,"Master_msaldototal"] / ds[,"Master_mlimitecompra"]
    ds[,"master_payroll_limite"] = ds[,"mpayroll"] / ds[,"Master_mlimitecompra"]
    ds[,"master_saldo_limite"] = ds[,"mcuentas_saldo"] / ds[,"Master_mlimitecompra"]

    ds[,"master_pagominimo_vsaldo"] = ds[,"Master_mpagominimo"] / ds[,"Master_msaldototal"]
    ds[,"master_pagominimo_limite"] = ds[,"Master_mpagominimo"] / ds[,"Master_mlimitecompra"]

    ds[,"master_adelanto_saldo"] = ds[,"Master_madelantopesos"] / ds[,"Master_msaldototal"]
    ds[,"master_adelanto_payroll"] = ds[,"Master_madelantopesos"] / ds[,"mpayroll"]

    ds[,"master_payroll_saldo"] = ds[,"mpayroll"] / ds[,"Master_msaldototal"]



    #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
    #varias formas de combinar Visa_status y Master_status
    ds[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
    ds[ , mv_status02       := Master_status +  Visa_status ]
    ds[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
    ds[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
    ds[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

    ds[ , mv_status06       := ifelse( is.na(Visa_status), 
                                            ifelse( is.na(Master_status), 10, Master_status), 
                                            Visa_status)  ]

    ds[ , mv_status07       := ifelse( is.na(Master_status), 
                                            ifelse( is.na(Visa_status), 10, Visa_status), 
                                            Master_status)  ]


#combino MasterCard y Visa
    ds[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

    ds[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
    ds[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
    ds[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
    ds[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
    ds[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
    ds[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
    ds[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
    ds[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
    ds[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
    ds[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
    ds[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
    ds[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
    ds[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
    ds[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
    ds[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
    ds[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
    ds[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
    ds[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
    ds[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
    ds[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
    ds[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
    ds[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
    ds[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
    ds[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
    ds[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
    ds[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
    ds[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
    ds[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
    ds[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
    ds[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
    ds[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
    ds[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
    ds[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
    ds[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
    ds[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  return (ds)
}



