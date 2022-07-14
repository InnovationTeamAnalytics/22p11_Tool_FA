# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_budget_2022 <- readRDS(file.path('processed', 'tab_budget_2022.rds'))


# RICAVI CALCULATION =========================================================================================

kc_bdgeco_ricavi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_ricavi_id = c('ter_cod_adj', kc_bdgeco_ricavi)


## Income by Line function: Budget_2022 -----------------------------------------


ricavi_line = function(line, data = dt_budget_2022) {
    
    dt_budget_2022_ricavi = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', ..kc_bdgeco_ricavi_id]
    
    if(nrow(dt_budget_2022_ricavi) == 0) {
        
        dt_budget_2022_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_ricavi_id)
        
    } else {
        
        dt_budget_2022_ricavi = melt(dt_budget_2022_ricavi, id.vars = 'ter_cod_adj', measure.vars = kc_bdgeco_ricavi, variable.name = 'date', 'ricavi')
        dt_budget_2022_ricavi = dt_budget_2022_ricavi[, .(budget_2022 = sum(ricavi, na.rm = TRUE)), by = c('ter_cod_adj', 'date')]
        
        dt_budget_2022_ricavi = dcast(dt_budget_2022_ricavi, ter_cod_adj ~ date, value.var = 'budget_2022')
        
    }
    
    return(dt_budget_2022_ricavi)
    
}



### Function Test --------

ricavi_line('Groupage')
apply(ricavi_line('Deposito'), 2, sum) 


ricavi_list = lapply(unique(dt_budget_2022$cdc_raggruppamenti_adj), ricavi_line)
names(ricavi_list) = unique(dt_budget_2022$cdc_raggruppamenti_adj)







## Costi variabili by Line function:  -----------------------------------------

kc_bdgeco_costi_var = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_var_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_var)





costi_variabili_line = function(line, data = dt_budget_2022) {
    
    dt_budget_2022_costi_variabili = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Variabile' & con_unlg_liv_2_adj != '(Altri costi fissi)', ..kc_bdgeco_costi_var_id] 
    
    if(nrow(dt_budget_2022_costi_variabili) == 0) {
        
        dt_budget_2022_costi_variabili = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_var_id)
        
    } else {
        
        dt_budget_2022_costi_variabili = melt(dt_budget_2022_costi_variabili, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_bdgeco_costi_var, variable.name = 'date', 'costi_variabili')
        
        dt_budget_2022_costi_variabili = dt_budget_2022_costi_variabili[, .(budget_2022 = sum(costi_variabili, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_2022_costi_variabili = dcast(dt_budget_2022_costi_variabili, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    return(dt_budget_2022_costi_variabili)
    
}



### Function Test --------

costi_variabili_groupage <- costi_variabili_line('Groupage')

costi_variabili_trasporto <- costi_variabili_line('Trasporto')



costi_variabili_list = lapply(unique(dt_budget_2022$cdc_raggruppamenti_adj), costi_variabili_line)


names(costi_variabili_list) = unique(dt_budget_2022$cdc_raggruppamenti_adj)








## Costi fissi by Line function:  -----------------------------------------

kc_bdgeco_costi_fissi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_fissi_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_fissi)





costi_fissi_line = function(line, data = dt_budget_2022) {
    
    dt_budget_2022_costi_fissi = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Fisso' & con_unlg_liv_2_adj != '(Altri costi fissi)', ..kc_bdgeco_costi_fissi_id] 
    
    if(nrow(dt_budget_2022_costi_fissi) == 0) {
        
        dt_budget_2022_costi_fissi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_fissi_id)
        
    } else {
        
        dt_budget_2022_costi_fissi = melt(dt_budget_2022_costi_fissi, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_bdgeco_costi_fissi, variable.name = 'date', 'costi_fissi')
        
        dt_budget_2022_costi_fissi = dt_budget_2022_costi_fissi[, .(budget_2022 = sum(costi_fissi, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_2022_costi_fissi = dcast(dt_budget_2022_costi_fissi, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    return(dt_budget_2022_costi_fissi)
    
}



### Function Test --------

costi_fissi_groupage <- costi_fissi_line('Groupage')

costi_fissi_trasporto <- costi_fissi_line('Trasporto')



costi_fissi_list = lapply(unique(dt_budget_2022$cdc_raggruppamenti_adj), costi_fissi_line)


names(costi_fissi_list) = unique(dt_budget_2022$cdc_raggruppamenti_adj)












## Costi indiretti by Line function:  -----------------------------------------


kc_bdgeco_costi_indir = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_indir_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_indir)





costi_indir_line = function(line, data = dt_budget_2022) {
    
    dt_budget_2022_costi_indir = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi / Costi indiretti' , ..kc_bdgeco_costi_indir_id] # & con_unlg_liv_2_adj != '(Altri costi fissi)' 
    
    if(nrow(dt_budget_2022_costi_indir) == 0) {
        
        dt_budget_2022_costi_indir = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_indir_id)
        
    } else {
        
        dt_budget_2022_costi_indir = melt(dt_budget_2022_costi_indir, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_bdgeco_costi_indir, variable.name = 'date', 'costi_indir')
        
        dt_budget_2022_costi_indir = dt_budget_2022_costi_indir[, .(budget_2022 = sum(costi_indir, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_2022_costi_indir = dcast(dt_budget_2022_costi_indir, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    return(dt_budget_2022_costi_indir)
    
}



### Function Test --------

costi_indir_groupage <- costi_indir_line('Groupage')

costi_indir_trasporto <- costi_indir_line('Trasporto')



costi_indir_list = lapply(unique(dt_budget_2022$cdc_raggruppamenti_adj), costi_indir_line)


names(costi_indir_list) = unique(dt_budget_2022$cdc_raggruppamenti_adj)
