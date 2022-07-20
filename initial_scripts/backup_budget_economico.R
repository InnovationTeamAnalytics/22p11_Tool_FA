

# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_budget_eco_current = readRDS(file.path('processed', 'tab_budget_current.rds'))
dt_t_cliente_tipo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE)

setDT(dt_t_cliente_tipo)


### Month Vector
kc_months = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")



# RICAVI CALCULATION =========================================================================================

kc_bdgeco_ricavi_id = c('ter_cod_adj', kc_months)

## Income by Line function: Budget_2022 -----------------------------------------

ricavi_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_ricavi = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi' , ..kc_bdgeco_ricavi_id]
    
    if(nrow(dt_budget_eco_current_ricavi) == 0) {
        
        dt_budget_eco_current_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_ricavi_id)
        
    } else {
        
        dt_budget_eco_current_ricavi = melt(dt_budget_eco_current_ricavi, id.vars = 'ter_cod_adj', measure.vars = kc_months, variable.name = 'date', 'ricavi')
        dt_budget_eco_current_ricavi = dt_budget_eco_current_ricavi[, .(budget_current = sum(ricavi, na.rm = TRUE)), by = c('ter_cod_adj', 'date')]
        
        dt_budget_eco_current_ricavi = dcast(dt_budget_eco_current_ricavi, ter_cod_adj ~ date, value.var = 'budget_current')
        
    }
    dt_budget_eco_current_ricavi[, id := line]
    
    return(dt_budget_eco_current_ricavi)
    
}


## Function Test ------------------------

ricavi_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), ricavi_line)
names(ricavi_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)


dt_ricavi_list = rbindlist(ricavi_list)


dt_ricavi_list_tot = dt_ricavi_list[, .(ter_cod_adj = 'total', value = lapply(.SD, sum), months = kc_months), .SDcols = kc_months, by = id] |> 
    dcast(id + ter_cod_adj ~ months, value.var = 'value')





# ALTRI RICAVI ---------------------------------

kc_bdgeco_altri_ricavi_id = c('ter_cod_adj', kc_months)


altri_ricavi = function(data = dt_budget_eco_current) {
    
    dt_budget_eco_current_altri_ricavi = dt_budget_eco_current[cdc_raggruppamenti_adj == "Ricavi / Costi indiretti" 
                                       
                                       & tipo_voce == 'Altri ricavi' & con_unlg_liv_2_adj == 'Altri ricavi' , ..kc_bdgeco_altri_ricavi_id]
    
    if(nrow(dt_budget_eco_current_altri_ricavi) == 0) {
        
        dt_budget_eco_current_altri_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_ricavi_id)
        
    } else {
        
        dt_budget_eco_current_altri_ricavi = melt(dt_budget_eco_current_altri_ricavi, id.vars = 'ter_cod_adj', measure.vars = kc_months, variable.name = 'date', 'altri_ricavi')
        dt_budget_eco_current_altri_ricavi = dt_budget_eco_current_altri_ricavi[, .(budget_current = round(sum(altri_ricavi, na.rm = TRUE))), by = c('ter_cod_adj', 'date')]
        
        dt_budget_eco_current_altri_ricavi = dcast(dt_budget_eco_current_altri_ricavi, ter_cod_adj ~ date, value.var = 'budget_current')
        
    }
    
    
    return(dt_budget_eco_current_altri_ricavi)
    
}



### Function test ----------------------

dt_altri_ricavi_list = altri_ricavi()

subtotal = colSums(as.matrix(dt_altri_ricavi_list[,  ..kc_months]))
subtotal = transpose(as.data.table(subtotal))
names(subtotal) = kc_months



# COSTI VARIABILI CALCULATION =========================================================================================

## Costi variabili by Line function:  -----------------------------------------

kc_bdgeco_costi_var_id = c('con_unlg_liv_2_adj', kc_months)


costi_variabili_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_costi_variabili = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Variabile', ..kc_bdgeco_costi_var_id] 
    
    if(nrow(dt_budget_eco_current_costi_variabili) == 0) {
        
        dt_budget_eco_current_costi_variabili = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_var_id)
        
    } else {
        
        dt_budget_eco_current_costi_variabili = melt(dt_budget_eco_current_costi_variabili, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_months, variable.name = 'date', 'costi_variabili')
        
        dt_budget_eco_current_costi_variabili = dt_budget_eco_current_costi_variabili[, .(budget_2022 = sum(costi_variabili, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_eco_current_costi_variabili = dcast(dt_budget_eco_current_costi_variabili, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    dt_budget_eco_current_costi_variabili[, id := line]
    
    return(dt_budget_eco_current_costi_variabili)
    
}



## Function Test --------------------------------------

costi_variabili_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), costi_variabili_line)
names(costi_variabili_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)

dt_costi_variabili_list = rbindlist(costi_variabili_list)

dt_costi_variabili_list_tot = dt_costi_variabili_list[, .(ter_cod_adj = 'total', value = lapply(.SD, sum), months = kc_months), .SDcols = kc_months, by = c('id')] |> 
    dcast(id + ter_cod_adj ~ months, value.var = 'value')





# COSTI FISSI CALCULATION =========================================================================================

## Costi fissi by Line function:  -----------------------------------------

kc_bdgeco_costi_fissi_id = c('con_unlg_liv_2_adj', kc_months)

costi_fissi_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_costi_fissi = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Fisso', ..kc_bdgeco_costi_fissi_id] 
    
    if(nrow(dt_budget_eco_current_costi_fissi) == 0) {
        
        dt_budget_eco_current_costi_fissi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_fissi_id)
        
    } else {
        
        dt_budget_eco_current_costi_fissi = melt(dt_budget_eco_current_costi_fissi, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_months, variable.name = 'date', 'costi_fissi')
        
        dt_budget_eco_current_costi_fissi = dt_budget_eco_current_costi_fissi[, .(budget_current = sum(costi_fissi, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_eco_current_costi_fissi = dcast(dt_budget_eco_current_costi_fissi, con_unlg_liv_2_adj ~ date, value.var = 'budget_current')
        
    }
    
    dt_budget_eco_current_costi_fissi[, id := line]
    
    return(dt_budget_eco_current_costi_fissi)
    
}



## Function Test --------------------------------------

costi_fissi_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), costi_fissi_line)
names(costi_fissi_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)

dt_costi_fissi_list = rbindlist(costi_fissi_list)

dt_costi_fissi_list_tot = dt_costi_fissi_list[, .(ter_cod_adj = 'total', value = lapply(.SD, sum), months = kc_months), .SDcols = kc_months, by = c('id')] |> 
    dcast(id + ter_cod_adj ~ months, value.var = 'value')







# COSTI INDIRETTI CALCULATION =========================================================================================

kc_bdgeco_costi_indir_id = c('con_unlg_liv_2_adj', kc_months)

costi_indir = {
        
        dt_budget_eco_current_costi_fissi = dt_budget_eco_current[tipo_voce == 'Ricavi / Costi indiretti', ..kc_bdgeco_costi_fissi_id] 
        dt_budget_eco_current_costi_fissi = melt(dt_budget_eco_current_costi_fissi, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_months, variable.name = 'date', 'costi_fissi')
        dt_budget_eco_current_costi_fissi = dt_budget_eco_current_costi_fissi[, .(budget_current = sum(costi_fissi, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        dt_budget_eco_current_costi_fissi = dcast(dt_budget_eco_current_costi_fissi, con_unlg_liv_2_adj ~ date, value.var = 'budget_current')
            
        dt_budget_eco_current_costi_fissi[, id := 'costi_indiretti']
        
}


costi_indir_long = melt(costi_indir, id.vars = c('id', 'con_unlg_liv_2_adj'), variable.name = 'months', value.name = 'costi_indiretti')
  


### GROUPAE
vec_ricavi_groupage_perc = {
    
    ricavi_groupage = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Groupage' & tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE) 
    ricavi_totale = colSums(dt_budget_eco_current[tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE) 
    ricavi_groupage_perc = ricavi_groupage / ricavi_totale
    ricavi_groupage_perc = data.table(months = names(ricavi_groupage_perc),
                                      ricavi_groupage_perc)
    
}



costi_indir_groupage = merge(costi_indir_long, vec_ricavi_groupage_perc, by = 'months')
costi_indir_groupage_long = costi_indir_groupage[, .(id, con_unlg_liv_2_adj, months, costi_indiretti_groupage = costi_indiretti * ricavi_groupage_perc)]
costi_indir_groupage = dcast(costi_indir_groupage_long, id + con_unlg_liv_2_adj ~ months, value.var = 'costi_indiretti_groupage')

costi_indir_groupage_tot = costi_indir_groupage_long[, .(id = 'Groupage', costi_indiretti_groupage = sum(costi_indiretti_groupage)), by = months]
costi_indir_groupage_tot = dcast(costi_indir_groupage_tot, id ~ months, value.var = 'costi_indiretti_groupage')


### TRASPORTO
vec_ricavi_trasporto_perc = {
    
    ricavi_trasporto = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Trasporto' & tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE) 
    ricavi_totale = colSums(dt_budget_eco_current[tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE) 
    ricavi_trasporto_perc = ricavi_trasporto / ricavi_totale
    ricavi_trasporto_perc = data.table(months = names(ricavi_trasporto_perc),
                                      ricavi_trasporto_perc)
    
}


costi_indir_trasporto = merge(costi_indir_long, vec_ricavi_trasporto_perc, by = 'months')
costi_indir_trasporto_long = costi_indir_trasporto[, .(id, con_unlg_liv_2_adj, months, costi_indiretti_trasporto = costi_indiretti * ricavi_trasporto_perc)]
costi_indir_trasporto = dcast(costi_indir_trasporto_long, id + con_unlg_liv_2_adj ~ months, value.var = 'costi_indiretti_trasporto')

costi_indir_trasporto_tot = costi_indir_trasporto_long[, .(id = 'Trasporto', costi_indiretti_trasporto = sum(costi_indiretti_trasporto)), by = months]
costi_indir_trasporto_tot = dcast(costi_indir_trasporto_tot, id ~ months, value.var = 'costi_indiretti_trasporto')



# CALCOLI CONTABILI ==================================================================

## MARGINE -------------------------------------------

### GROUPAGE
ricavi_groupage_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Groupage' & tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE)
costivariabili_groupage_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Groupage' & tipo_voce == 'Variabile', ..kc_months], na.rm = TRUE)
costifissi_groupage_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Groupage' & tipo_voce == 'Fisso', ..kc_months], na.rm = TRUE)

primo_groupage_margine = ricavi_groupage_tot + costivariabili_groupage_tot
secondo_groupage_margine = primo_groupage_margine + costifissi_groupage_tot


### TRASPORTO
ricavi_trasporto_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Trasporto' & tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE)
costivariabili_trasporto_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Trasporto' & tipo_voce == 'Variabile', ..kc_months], na.rm = TRUE)
costifissi_trasporto_tot = colSums(dt_budget_eco_current[cdc_raggruppamenti_adj == 'Trasporto' & tipo_voce == 'Fisso', ..kc_months], na.rm = TRUE)

primo_trasporto_margine = ricavi_trasporto_tot + costivariabili_trasporto_tot
secondo_trasporto_margine = primo_trasporto_margine + costifissi_trasporto_tot


### TOTAL
ricavi_tot = colSums(dt_budget_eco_current[ tipo_voce == 'Ricavi', ..kc_months], na.rm = TRUE)
costivariabili_tot = colSums(dt_budget_eco_current[tipo_voce == 'Variabile', ..kc_months], na.rm = TRUE)
costifissi_tot = colSums(dt_budget_eco_current[tipo_voce == 'Fisso', ..kc_months], na.rm = TRUE)

primo_margine = ricavi_tot + costivariabili_tot
secondo_margine = primo_margine + costifissi_tot