# Packages ------------------------------

library(data.table)
library(openxlsx)
library(janitor)
library(innteamUtils)



# Upload Input Data -----------------------

dt_budget_current = read.xlsx(file.path( "inputs", "budget_2022.xlsx"))
dt_budget_current = clean_names(dt_budget_current)
setDT(dt_budget_current)


dt_t_cdc = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_CdC', detectDates = TRUE)
dt_t_analitico = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Analitico_Bdgt_Eco', detectDates = TRUE)
dt_t_personale = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Costo_Personale', detectDates = TRUE)
dt_t_cliente_tipo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE)
dt_t_cliente_expo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Esportatori', detectDates = TRUE)
dt_t_aliquote = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Aliquota', detectDates = TRUE)
dt_t_condcom = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Condizioni_Commerciali', detectDates = TRUE)
dt_t_condcom_t = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Condizioni_comm_li', detectDates = TRUE)


dt_t_cdc = janitor::clean_names(dt_t_cdc)
dt_t_analitico = janitor::clean_names(dt_t_analitico)
dt_t_personale = janitor::clean_names(dt_t_personale)
dt_t_cliente_tipo = janitor::clean_names(dt_t_cliente_tipo)
dt_t_cliente_expo = janitor::clean_names(dt_t_cliente_expo)
dt_t_aliquote = janitor::clean_names(dt_t_aliquote)
dt_t_condcom = janitor::clean_names(dt_t_condcom)
dt_t_condcom_t = janitor::clean_names(dt_t_condcom_t)


setDT(dt_t_cdc)
setDT(dt_t_analitico)
setDT(dt_t_personale)
setDT(dt_t_cliente_tipo)
setDT(dt_t_cliente_expo)
setDT(dt_t_aliquote)
setDT(dt_t_condcom)
setDT(dt_t_condcom_t)



# Ricostruzione colonne excel calcolate ----------------------------------------------------

dt_budget_current[, con_codice_e_descrizione := paste(con_codice, con_descrizione, sep = " - ") ]
dt_budget_current[, ter_cod_adj := fifelse(ter_cod == "", 0, as.numeric(ter_cod))]
dt_budget_current[, ter_cod_adj := fifelse(is.na(ter_cod_adj), 0, ter_cod_adj)]
dt_budget_current = merge(x = dt_budget_current, y = dt_t_cdc, by = "cdc_raggruppamenti", all.x = T)
setnames(x = dt_budget_current, old = "riclassifica", new = "cdc_raggruppamenti_adj")



dt_budget_current = merge(x = dt_budget_current, y = dt_t_analitico, by = c("cdc_raggruppamenti_adj", "con_unlg_liv_2"), all.x = T )
setnames(x = dt_budget_current, old = "trascodifica", "con_unlg_liv_2_adj" )

dt_budget_current[, c("chiave_trascodifica", "note.y") := NULL]


dt_budget_current = merge(x = dt_budget_current, y = dt_t_personale, by = "con_codice_e_descrizione", all.x = T)
setnames(x = dt_budget_current, 'tipo_costo', 'tipo_costo_personale', skip_absent = TRUE)




dt_budget_current = merge(x = dt_budget_current, y = dt_t_cliente_tipo, by.x = "ter_cod_adj", by.y ='soggetto', all.x = TRUE)
dt_budget_current[, tipo_cliente := fifelse(is.na(tipo_cliente), 'Cliente Regolare', tipo_cliente)]
dt_budget_current[, c("descrizione") := NULL]



dt_budget_current = merge(x = dt_budget_current, y = dt_t_condcom, by.x = 'ter_cod_adj', by.y = "soggetto", all.x = TRUE)
setnames( x = dt_budget_current, "condizioni_di_pagamento_2021", "condizioni_commerciali_nominali")
dt_budget_current[, condizioni_commerciali_nominali := fifelse(is.na(condizioni_commerciali_nominali), 'ND', condizioni_commerciali_nominali)]
dt_budget_current = dt_budget_current[, c("ragione_sociale") := NULL]




dt_budget_current = merge(x = dt_budget_current, y = dt_t_condcom_t, by = "condizioni_commerciali_nominali", all.x = TRUE)


dt_t_cliente_expo = setnames(dt_t_cliente_expo, "ter_descr", "esportatore")


dt_budget_current = merge(x = dt_budget_current, y = dt_t_cliente_expo, by.x = 'ter_cod_adj', by.y = "soggetto", all.x = TRUE)


dt_budget_current = dt_budget_current[, esportatore := fifelse(is.na(esportatore), "NO", "SI")]




### ALIQUOTE
dt_budget_current = merge(x = dt_budget_current, y = dt_t_aliquote[, c("soggetto","iva_media_2021" )], 
                        
                        by.x = 'ter_cod_adj', by.y = 'soggetto', all.x = TRUE) 

dt_budget_current = setnames(dt_budget_current, "iva_media_2021", "aliquota"  )


dt_budget_current[, aliquota := fcase(esportatore == 'SI', 0, 
                              esportatore == 'NO' & !is.na(aliquota), aliquota,
                              default = 0.22)]


### DATA + IVA

kc_iva = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

dt_budget_current[, (paste0(kc_iva, '_lordo_iva')) := lapply(.SD, function(x) { (1 + aliquota) * x }), .SDcols = kc_iva]




# EXPORT ==================================================================================

saveRDS(dt_budget_current, file.path('processed', 'tab_budget_2022.rds'))






      




     