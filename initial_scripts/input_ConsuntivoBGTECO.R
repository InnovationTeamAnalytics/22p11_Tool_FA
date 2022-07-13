
# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_consbe = read.xlsx(file.path('inputs', 'consuntivo_budget_economico.xlsx'), detectDates = TRUE)
dt_consbe = janitor::clean_names(dt_consbe)
setDT(dt_consbe)


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



# DATA TRANSFORMATION =========================================================================================

### SOGGETTI ADJ
dt_consbe[, soggetti_adj := fifelse(soggetti == '---', 0, as.numeric(soggetti))]

### CDC RAGGRUPPAMENTI ADJ
dt_consbe = merge(dt_consbe, dt_t_cdc, by = 'cdc_raggruppamenti', all.x = TRUE) |>
    setnames('riclassifica', 'cdc_raggruppamenti_adj', skip_absent=TRUE)

### ANALITICO
dt_consbe = merge(dt_consbe, dt_t_analitico[, .(cdc_raggruppamenti_adj, con_unlg_liv_2, trascodifica, tipo_voce)], by = c('cdc_raggruppamenti_adj', 'con_unlg_liv_2'), all.x = TRUE) |>
    setnames('trascodifica', 'con_unlg_liv_2_adj', skip_absent = TRUE)

### COSTO PERSONALE
dt_consbe = merge(dt_consbe, dt_t_personale, by.x = 'vdc', by.y = 'con_codice_e_descrizione', all.x = TRUE) |>
    setnames('tipo_costo', 'tipo_costo_personale', skip_absent = TRUE)

### TIPO CLIENTE
dt_consbe = merge(dt_consbe, dt_t_cliente_tipo[, .(soggetti_adj = soggetto, tipo_cliente)], by = 'soggetti_adj', all.x = TRUE) 
dt_consbe[, tipo_cliente := fifelse(is.na(tipo_cliente), 'Cliente Regolare', tipo_cliente)]

### CONDIZIONI COMMERCIALI
dt_consbe = merge(dt_consbe, dt_t_condcom[, .(soggetti_adj = soggetto, condizioni_commerciali_nominali = condizioni_di_pagamento_2021)], by = 'soggetti_adj', all.x = TRUE) 
dt_consbe[, condizioni_commerciali_nominali := fifelse(is.na(condizioni_commerciali_nominali), 'ND', condizioni_commerciali_nominali)]

dt_consbe = merge(dt_consbe, dt_t_condcom_t[, .(condizioni_commerciali = condizioni_commerciali_nominali_riclassificato, condizioni_commerciali_nominali, modalita_pagamento)], by = 'condizioni_commerciali_nominali', all.x = TRUE) 

### ESPORTATORI
dt_consbe = merge(dt_consbe, dt_t_cliente_expo[, .(soggetti_adj = soggetto, esportatore = ter_descr)], by = 'soggetti_adj', all.x = TRUE) 
dt_consbe[, esportatore := fifelse(is.na(esportatore), 'NO', 'SI')]

### ALIQUOTE
dt_consbe = merge(dt_consbe, dt_t_aliquote[, .(soggetti_adj = soggetto, aliquota = iva_media_2021)], by = 'soggetti_adj', all.x = TRUE) 
dt_consbe[, aliquota := fcase(esportatore == 'SI', 0, 
                              esportatore == 'NO' & !is.na(aliquota), aliquota,
                              default = 0.22)]


### DATA + IVA
kc_iva = keep_cols(dt_consbe, '2021')
dt_consbe[, (paste0(kc_iva, '_iva')) := lapply(.SD, function(x) { (1 + aliquota) * x }), .SDcols = kc_iva]





