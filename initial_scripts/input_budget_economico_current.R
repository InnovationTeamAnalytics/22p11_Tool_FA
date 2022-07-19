

# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(janitor)
library(innteamUtils)



# UPLOAD DATA =================================================================================================

dt_budget_current = read.xlsx(file.path( "inputs_mod", "budget_current.xlsx"))
dt_budget_current = clean_names(dt_budget_current)
setDT(dt_budget_current)


dt_t_cdc = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_CdC', detectDates = TRUE)
dt_t_analitico = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Analitico_Bdgt_Eco', detectDates = TRUE)
dt_t_personale = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Costo_Personale', detectDates = TRUE)
dt_t_cliente_tipo = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE)
dt_t_cliente_expo = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Esportatori', detectDates = TRUE)
dt_t_aliquote = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Aliquota', detectDates = TRUE)
dt_t_condcom = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Condizioni_Commerciali', detectDates = TRUE)
dt_t_condcom_t = read.xlsx(file.path('inputs_mod', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Condizioni_comm_li', detectDates = TRUE)


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



# DATA TRANSFORMATION =========================================================================================

### CODICI 
dt_budget_current[, id_contabile := paste(con_codice, con_descrizione, sep = " - ") ]
dt_budget_current[, ter_cod_adj := fifelse(ter_cod == "", 'Blank', ter_cod)]
dt_budget_current[, ter_cod_adj := fifelse(ter_cod == "---", 'Blank', ter_cod)]
dt_budget_current[, ter_cod_adj := fifelse(is.na(ter_cod_adj), 'Blank', ter_cod_adj)]
dt_budget_current = merge(x = dt_budget_current, y = dt_t_cdc, by = "cdc_raggruppamenti", all.x = T)
setnames(x = dt_budget_current, old = "riclassifica", new = "cdc_raggruppamenti_adj")


### ID CONTABILE AND CODING
dt_budget_current = merge(x = dt_budget_current, y = dt_t_analitico[, .(cdc_raggruppamenti_adj, con_unlg_liv_2, trascodifica, tipo_voce)], by = c("cdc_raggruppamenti_adj", "con_unlg_liv_2"), all.x = T )
setnames(x = dt_budget_current, old = "trascodifica", "con_unlg_liv_2_adj" )


### COSTO PERSONALE
dt_budget_current = merge(x = dt_budget_current, y = dt_t_personale, by = "id_contabile", all.x = T)
setnames(x = dt_budget_current, 'tipo_costo', 'tipo_costo_personale', skip_absent = TRUE)


### TIPO CLIENTE
dt_budget_current = merge(x = dt_budget_current, y = dt_t_cliente_tipo[, .(ter_cod_adj = soggetto, tipo_cliente)], by = "ter_cod_adj", all.x = TRUE)
dt_budget_current[, tipo_cliente := fifelse(is.na(tipo_cliente), 'Cliente Regolare', tipo_cliente)]


### CONDIZIONI COMMERCIALI
dt_budget_current = merge(x = dt_budget_current, y = dt_t_condcom[, .(ter_cod_adj = soggetto, condizioni_di_pagamento)], by = 'ter_cod_adj', all.x = TRUE)
setnames( x = dt_budget_current, "condizioni_di_pagamento", "condizioni_commerciali_nominali")
dt_budget_current[, condizioni_commerciali_nominali := fifelse(is.na(condizioni_commerciali_nominali), 'ND', condizioni_commerciali_nominali)]



### CONDIZIONI COMMERCIALI NOMINALI
dt_budget_current = merge(x = dt_budget_current, y = dt_t_condcom_t, by = "condizioni_commerciali_nominali", all.x = TRUE)
setnames(dt_t_cliente_expo, "ter_descr", "esportatore")


### CLIENTI EXPORT
dt_budget_current = merge(x = dt_budget_current, y = dt_t_cliente_expo, by.x = 'ter_cod_adj', by.y = "soggetto", all.x = TRUE)
dt_budget_current = dt_budget_current[, esportatore := fifelse(is.na(esportatore), "NO", "SI")]




### ALIQUOTE
dt_budget_current = merge(x = dt_budget_current, y = dt_t_aliquote[, .(ter_cod_adj = soggetto, aliquota = iva_media)], by = 'ter_cod_adj', all.x = TRUE) 



dt_budget_current[, aliquota := fcase(esportatore == 'SI', 0, 
                                      esportatore == 'NO' & !is.na(aliquota), aliquota,
                                      default = 0.22)]


### DATA + IVA
kc_iva = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

dt_budget_current[, (paste0(kc_iva, '_lordo_iva')) := lapply(.SD, function(x) { (1 + aliquota) * x }), .SDcols = kc_iva]




# EXPORT ==================================================================================

saveRDS(dt_budget_current, file.path('processed', 'tab_budget_current.rds'))






      




     