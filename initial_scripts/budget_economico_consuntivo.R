

# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_consbe = readRDS(file.path('processed', 'tab_BudgetEconomico_consuntivo.rds'))


# PRECONS CALCULATION =========================================================================================

kc_precons = keep_cols(dt_consbe, '2022')
kc_precons_id = c('soggetti_adj', kc_precons)


## Income by Line function: Consuntivato -----------------------------------------


ricavi_line = function(line, data = dt_consbe) {
    
    dt_consbe_ricavi = dt_consbe[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', ..kc_precons_id]
    
    if(nrow(dt_consbe_ricavi) == 0) {
        
        dt_consbe_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_precons_id)
        
    } else {
        
        dt_consbe_ricavi = melt(dt_consbe_ricavi, id.vars = 'soggetti_adj', measure.vars = kc_precons, variable.name = 'date', 'ricavi')
        dt_consbe_ricavi = dt_consbe_ricavi[, .(precons = sum(ricavi, na.rm = TRUE)), by = c('soggetti_adj', 'date')]
        
        dt_consbe_ricavi = dcast(dt_consbe_ricavi, soggetti_adj ~ date, value.var = 'precons')
        
    }
    
    return(dt_consbe_ricavi)
    
}



### Function Test --------

ricavi_line('Deposito')
apply(ricavi_line('Deposito'), 2, sum) 


ricavi_list = lapply(unique(dt_consbe$cdc_raggruppamenti_adj), ricavi_line)
names(ricavi_list) = unique(dt_consbe$cdc_raggruppamenti_adj)


ricavi_list




## Costs by Line function: Consuntivato -----------------------------------------
