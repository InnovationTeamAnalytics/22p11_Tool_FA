#BUDGET FINANZIARIO

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)

options(scipen = 999, digits = 3)

# UPLOAD DATA----

## Consuntivo
dt_consbe = readRDS(file.path('processed', 'tab_BudgetEconomico_consuntivo.rds'))

## Budget 2022

dt_budget_2022 = readRDS(file.path('processed', 'tab_budget_2022.rds')) |>
    setnames("condizioni_commerciali_nominali_riclassificato", "condizioni_commerciali") |>
    setnames("ter_cod_adj", "soggetti_adj")

## Supporti


# Colonne mesi
mesi <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

col_mesi_id <- c("soggetti_adj", keep_cols(dt_consbe, '2022'))

condizioni_commerciali <- dt_consbe[, .(soggetti_adj, condizioni_commerciali)]

differenza <- match("mese_x", mesi)-as.numeric(gsub("\\D", "", condizioni_commerciali[soggetti_adj]))

#Funzione che trova la differenza fra contabilizzazione
# calc_diff<- function(x){
#     mesi <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")
#     diff <- match(x, mesi)-as.numeric(gsub("\\D", "", "Dopo 4 mesi"))
#     return(diff)
# }

calc_diff_contab <- function(x, data = dt_consbe){
    mesi <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")
    diff <- match(x, mesi)-as.numeric(gsub("\\D", "", "Dopo 4 mesi"))
    mesi_new <- mesi[match(x, mesi)+diff]
    
    
    if(diff > 0){
        dt_entrate <- dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', ..mesi[match(x, mesi)+diff]]
    } else {
        dt_entrate <- dt_consbe[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', ..mesi[match(x, mesi) + 12 - diff]]
    }
    return(dt_entrate)
}

calc_diff_contab("dicembre")


di <- calc_diff("dicembre")
mesi[di]

lapply(mesi, calc_diff_contab)


if(differenza >=0){
    dt_entrate = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi',..x]
}

mesi_new <- mesi[match("gennaio", mesi)+diff]
#se differenza >= 0 allora budget, se < 0 allora consuntivo
if(differenza >=0){
    dt_entrate = dt_budget_2022
}


if(mesi[match("x", mesi)-as.numeric(gsub("\\D", "", condizioni_commerciali))]>0){
    dt_entrate = dt_budget_2022[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', match(mesi, (12 - ( match("gennaio", mesi)-as.numeric(gsub("\\D", "", condizioni_commerciali)))))]
} else {
    dt_entrate = dt_consbe[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi', match(mesi, (1 + ( match("gennaio", mesi)-as.numeric(gsub("\\D", "", condizioni_commerciali)))))]
}

# ENTRATE GESTIONE CORRENTE by line----

# Se contabilizzazione prima di gennaio 2022, prendi da consuntivo, se no da budget
# colonna che indica contabilizzazione è condizioni_commerciali

# Tutte le condizioni commerciali sono Dopo "n" mesi (in questo caso - potrebbe capitare che sia prima?)
n_mesi_contabilizzazione <- as.numeric(gsub("\\D", "", condizioni_commerciali))





stringdist_join()

### Function Test --------

ricavi_line('Deposito')
apply(ricavi_line('Deposito'), 2, sum) 


ricavi_list = lapply(unique(dt_consbe$cdc_raggruppamenti_adj), ricavi_line)
names(ricavi_list) = unique(dt_consbe$cdc_raggruppamenti_adj)


ricavi_list

