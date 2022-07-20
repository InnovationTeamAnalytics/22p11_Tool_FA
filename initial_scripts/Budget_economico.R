

# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_budget_eco_current = readRDS(file.path('processed', 'tab_budget_current_long.rds'))
dt_t_cliente_tipo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE)

setDT(dt_t_cliente_tipo)


### Month Vector
kc_months = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")



# RICAVI CALCULATION =========================================================================================

kc_bdgeco_ricavi_id = c('ter_cod_adj', kc_months)


## Income:  -----------------------------------------

dt_budget_eco_current_ricavi_long = dt_budget_eco_current[tipo_voce %chin% c('Ricavi', 'Altri ricavi')][, .(budget_current = sum(valori, na.rm = TRUE)), by = .(cdc_raggruppamenti_adj, tipo_voce, ter_cod_adj, months)]

dt_budget_eco_current_ricavi_tot_long = dt_budget_eco_current_ricavi_long[, .(subtotal = sum(budget_current, na.rm = TRUE)), by = .(cdc_raggruppamenti_adj, tipo_voce, months)]
dt_budget_eco_current_ricavi_tot_long[, total := sum(subtotal, na.rm = TRUE), by = .(months)]
dt_budget_eco_current_ricavi_tot_long[, perc_total := subtotal / sum(subtotal, na.rm = TRUE), by = .(months)]


## Costs:  -----------------------------------------

dt_budget_eco_current_costi_long = dt_budget_eco_current[!tipo_voce %chin% c('Ricavi', 'Altri ricavi') & cdc_raggruppamenti_adj != 'Ricavi / Costi indiretti'][, .(budget_current = sum(valori, na.rm = TRUE)), by = .(cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2_adj, months)]

dt_budget_eco_current_costi_ind_long = dt_budget_eco_current[!tipo_voce %chin% c('Ricavi', 'Altri ricavi', 'Sotto EBITDA') & cdc_raggruppamenti_adj == 'Ricavi / Costi indiretti'][, .(budget_current = sum(valori, na.rm = TRUE)), by = .(tipo_voce, con_unlg_liv_2_adj, months)]

dt_budget_eco_current_costi_ind_long = merge(dt_budget_eco_current_costi_ind_long, dt_budget_eco_current_ricavi_tot_long[tipo_voce == 'Ricavi', .(cdc_raggruppamenti_adj, months, perc_total)],
                                             by =  c('months'), all.x = TRUE, allow.cartesian = TRUE)

dt_budget_eco_current_costi_ind_long[, budget_current := budget_current * perc_total]
dt_budget_eco_current_costi_ind_long[, perc_total := NULL]


