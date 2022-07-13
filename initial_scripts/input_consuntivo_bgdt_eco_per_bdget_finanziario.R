### Calcoli su input Finanziario ###

# Libraries
## install.packages("remotes")
## library(remotes)
## remotes::install_github("aba-innovationteam/innteamUtils", build_vignettes = F)

library(openxlsx)
library(data.table)
library(janitor)
library(innteamUtils)


# Nel file originale è il foglio chiamato "Input consuntivo BGT ECO"
input_fin <- read.xlsx(file.path("inputs", "input_finanziario.xlsx"), detectDates = T) |>
    clean_names() |>
    as.data.table()

dt_t_cdc <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_CdC', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

dt_t_ana <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Analitico_Bdgt_Eco', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

dt_t_cliente_tipo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

dt_t_personale <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Costo_Personale', detectDates = TRUE)  |>
    as.data.table() |>
    clean_names()

dt_t_condcom <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Condizioni_Commerciali', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

dt_t_condcom_t <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Trascodifica_Condizioni_comm_li', detectDates = TRUE)  |>
    as.data.table() |>
    clean_names()

dt_t_cliente_expo <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Esportatori', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

dt_t_aliquote <- read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Aliquota', detectDates = TRUE) |>
    as.data.table() |>
    clean_names()

# Creazione delle nuove variabili

# - soggetti_adj
mod_fin <- input_fin[, ':=' (soggetti_adj = ifelse(soggetti == "---" |
                                                       is.na(soggetti)|
                                                       soggetti == " ",
                                                   0,
                                                   as.numeric(soggetti)))]

# - cdc_raggruppamenti_adj
mod_fin = merge(mod_fin, dt_t_cdc, by = 'cdc_raggruppamenti', all.x = TRUE) |>
    setnames('riclassifica', 'cdc_raggruppamenti_adj', skip_absent = TRUE)

# - con_unlg_liv_2_adj
mod_fin = merge(mod_fin, dt_t_ana[,c(2, 3, 4)], by = c('cdc_raggruppamenti_adj', 'con_unlg_liv_2'), all.x = TRUE, allow.cartesian = TRUE) |>
    setnames('trascodifica', 'con_unlg_liv_2_adj', skip_absent = TRUE)

# - tipo_voce
mod_fin = merge(mod_fin, dt_t_ana[,c(2, 3, 5)], by = c('cdc_raggruppamenti_adj', 'con_unlg_liv_2'), all.x = TRUE, allow.cartesian = TRUE)

# - tipo_costo_del_personale
## Se con_unlg_liv_2_adj != (Personale) allora "-" (NA),
## se no inserisci il valore del costo del personale (Tipo costo)
## che trovi in dt_t_personale facendo merge tramite "VDC" anche chiamato "$CON: Codice e descrizione",
## se dà errore (perchè non trova niente) allora il valore è "COSTO STANDARD DEL PERSONALE"
mod_fin = merge(mod_fin, dt_t_personale[, ':=' (vdc = con_codice_e_descrizione,
                                                con_codice_e_descrizione = NULL)],
                by = 'vdc', all.x = TRUE, allow.cartesian = TRUE)

mod_fin <- mod_fin[con_unlg_liv_2_adj == "(Personale)" & is.na(tipo_costo), tipo_costo := "COSTO STANDARD DEL PERSONALE"]

# - tipo_cliente
mod_fin = merge(mod_fin, dt_t_cliente_tipo[, ':=' (soggetti_adj = soggetto)][, .(soggetti_adj, tipo_cliente)],
                by = 'soggetti_adj', all.x = TRUE, allow.cartesian = TRUE)

mod_fin <- mod_fin[is.na(tipo_cliente), tipo_cliente := "Cliente standard"]

# - condizioni_commerciali_nominali
mod_fin = merge(mod_fin, dt_t_condcom[, ':=' (soggetti_adj = soggetto)][, .(soggetti_adj, condizioni_di_pagamento_2021)],
                by = 'soggetti_adj', all.x = TRUE, allow.cartesian = TRUE) |>
    setnames('condizioni_di_pagamento_2021', 'condizioni_commerciali_nominali', skip_absent = TRUE)

mod_fin <- mod_fin[is.na(condizioni_commerciali_nominali), condizioni_commerciali_nominali := "ND"]

# - condizioni_commerciali_adj
mod_fin = merge(mod_fin, dt_t_condcom_t[,c(1, 2)], by = 'condizioni_commerciali_nominali', all.x = TRUE, allow.cartesian = TRUE) |>
    setnames('condizioni_commerciali_nominali_riclassificato', 'condizioni_commerciali_adj', skip_absent = TRUE)

# - modalita_pagamento
mod_fin = merge(mod_fin, dt_t_condcom_t[,c(1, 3)], by = 'condizioni_commerciali_nominali', all.x = TRUE, allow.cartesian = TRUE)

# - esportatore_si_no
mod_fin = merge(mod_fin, dt_t_cliente_expo[, ':=' (soggetti_adj = soggetto,
                                                   soggetto = NULL)], by = "soggetti_adj", all.x = TRUE, allow.cartesian = TRUE) |>
    setnames('ter_descr', 'esportatore_si_no', skip_absent = TRUE)

mod_fin <- mod_fin[, esportatore_si_no := ifelse(is.na(esportatore_si_no), "NO", "SI")]

# - aliquota
mod_fin <- merge(mod_fin, dt_t_aliquote[, .(soggetti_adj = soggetto, aliquota = iva_media_2021)], by = 'soggetti_adj', all.x = TRUE) 
mod_fin[, aliquota := fcase(esportatore_si_no == 'SI', 0, 
                              esportatore_si_no == 'NO' & !is.na(aliquota), aliquota,
                              default = 0.22)]

# mesi_2021_lordo_iva
### DATA + IVA
kc_iva = keep_cols(mod_fin, '2021')
mod_fin[, (paste0(kc_iva, '_iva')) := lapply(.SD, function(x) { (1 + aliquota) * x }), .SDcols = kc_iva]

# Export----
saveRDS(mod_fin, file.path('processed', 'tab_BudgetFinanziario_consuntivo.rds'))



