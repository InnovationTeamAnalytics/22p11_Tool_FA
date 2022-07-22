#BUDGET FINANZIARIO

# LIBRARIES
library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)
library(stringr)

options(scipen = 999)

# UPLOAD DATA----
## Consuntivo
dt_consbe = readRDS(file.path('processed', 'tab_BudgetEconomico_consuntivo.rds'))
dt_consbe = janitor::clean_names(dt_consbe)
setDT(dt_consbe)

## Budget 2022
dt_budget_current = readRDS(file.path('processed', 'tab_budget_current.rds')) |>
    setnames("condizioni_commerciali_nominali_riclassificato", "condizioni_commerciali") |>
    setnames("ter_cod_adj", "soggetti_adj")
dt_budget_current = janitor::clean_names(dt_budget_current)
setDT(dt_budget_current)

## Supporti

dt_t_ipotesi = read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Ipotesi', detectDates = TRUE)
dt_assicurazioni <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Dettaglio_assicurazioni', detectDates = TRUE)

setDT(dt_t_ipotesi)
setDT(dt_assicurazioni)



################################################################################

dt_consbe_fin_parziale <- dt_consbe[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2, con_unlg_liv_2_adj,
                        gennaio_2021_iva, febbraio_2021_iva, marzo_2021_iva, aprile_2021_iva, maggio_2021_iva, giugno_2021_iva,
                        luglio_2021_iva, agosto_2021_iva, settembre_2021_iva, ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva)]

dt_budget_current_parziale <- dt_budget_current[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2, con_unlg_liv_2_adj,
                          gennaio_lordo_iva, febbraio_lordo_iva, marzo_lordo_iva, aprile_lordo_iva, maggio_lordo_iva, giugno_lordo_iva,
                          luglio_lordo_iva, agosto_lordo_iva, settembre_lordo_iva, ottobre_lordo_iva, novembre_lordo_iva, dicembre_lordo_iva)]

dt_input_budget_fin <- rbind(dt_consbe_fin_parziale, dt_budget_current_parziale, fill = T)

#dt_input_budget_fin[soggetti_adj = "0000000302", condizioni_commerciali := "Dopo 2 mesi"]

# 0 mesi
# dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Entro mese in corso", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(gennaio_lordo_iva:dicembre_lordo_iva)]

#
dt_input_budget_fin[condizioni_commerciali == "Entro mese in corso", ':=' (gennaio = gennaio_lordo_iva,
                                                                        febbraio = febbraio_lordo_iva,
                                                                        marzo = marzo_lordo_iva,
                                                                        aprile = aprile_lordo_iva,
                                                                        maggio = maggio_lordo_iva,
                                                                        giugno = giugno_lordo_iva,
                                                                        luglio = luglio_lordo_iva,
                                                                        agosto = agosto_lordo_iva,
                                                                        settembre = settembre_lordo_iva,
                                                                        ottobre = ottobre_lordo_iva,
                                                                        novembre = novembre_lordo_iva,
                                                                        dicembre = dicembre_lordo_iva)]

# -1 mese
# dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Mese successivo", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(dicembre_2021_iva, gennaio_lordo_iva:novembre_lordo_iva)]

dt_input_budget_fin[condizioni_commerciali == "Mese successivo", ':=' (gennaio = dicembre_2021_iva,
                                                                    febbraio = gennaio_lordo_iva,
                                                                    marzo = febbraio_lordo_iva,
                                                                    aprile = marzo_lordo_iva,
                                                                    maggio = aprile_lordo_iva,
                                                                    giugno = maggio_lordo_iva,
                                                                    luglio = giugno_lordo_iva,
                                                                    agosto = luglio_lordo_iva,
                                                                    settembre = agosto_lordo_iva,
                                                                    ottobre = settembre_lordo_iva,
                                                                    novembre = ottobre_lordo_iva,
                                                                    dicembre = novembre_lordo_iva)]
# -2 mesi
#dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Dopo 2 mesi", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:ottobre_lordo_iva)]


dt_input_budget_fin[condizioni_commerciali == "Dopo 2 mesi", ':=' (gennaio = novembre_2021_iva,
                                                                    febbraio = dicembre_2021_iva,
                                                                    marzo = gennaio_lordo_iva,
                                                                    aprile = febbraio_lordo_iva,
                                                                    maggio = marzo_lordo_iva,
                                                                    giugno = aprile_lordo_iva,
                                                                    luglio = maggio_lordo_iva,
                                                                    agosto = giugno_lordo_iva,
                                                                    settembre = luglio_lordo_iva,
                                                                    ottobre = agosto_lordo_iva,
                                                                    novembre = settembre_lordo_iva,
                                                                    dicembre = ottobre_lordo_iva)]

# -3 mesi
#dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Dopo 3 mesi", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:settebre_lordo_iva)]

dt_input_budget_fin[condizioni_commerciali == "Dopo 3 mesi", ':=' (gennaio = ottobre_2021_iva,
                                                                    febbraio = novembre_2021_iva,
                                                                    marzo = dicembre_2021_iva,
                                                                    aprile = gennaio_lordo_iva,
                                                                    maggio = febbraio_lordo_iva,
                                                                    giugno = marzo_lordo_iva,
                                                                    luglio = aprile_lordo_iva,
                                                                    agosto = maggio_lordo_iva,
                                                                    settembre = giugno_lordo_iva,
                                                                    ottobre = luglio_lordo_iva,
                                                                    novembre = agosto_lordo_iva,
                                                                    dicembre = settembre_lordo_iva)]

# -4 mesi
#dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Dopo 4 mesi", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(settembre_2021_iva, ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:agosto_lordo_iva)]


dt_input_budget_fin[condizioni_commerciali == "Dopo 4 mesi", ':=' (gennaio = ottobre_2021_iva,
                                                                febbraio = ottobre_2021_iva,
                                                                marzo = novembre_2021_iva,
                                                                aprile = dicembre_2021_iva,
                                                                maggio = gennaio_lordo_iva,
                                                                giugno = febbraio_lordo_iva,
                                                                luglio = marzo_lordo_iva,
                                                                agosto = aprile_lordo_iva,
                                                                settembre = maggio_lordo_iva,
                                                                ottobre = giugno_lordo_iva,
                                                                novembre = luglio_lordo_iva,
                                                                dicembre = agosto_lordo_iva)]



# ENTRATE----
kc_months = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_months_id = c('soggetti_adj', kc_months)


entrate_line = function(line, data = dt_input_budget_fin) {
    
    dt_budget_current_entrate = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi', ..kc_months_id]
    if(nrow(dt_budget_current_entrate) == 0) {
        
        dt_budget_current_entrate = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_months_id)
        
    } else {
        
        dt_budget_current_entrate = melt(dt_budget_current_entrate, id.vars = 'soggetti_adj', measure.vars = kc_months, variable.name = 'date', 'entrate')
        dt_budget_current_entrate = dt_budget_current_entrate[, .(budget_current = sum(entrate, na.rm = TRUE)), by = c('soggetti_adj', 'date')]
        dt_budget_current_entrate = dcast(dt_budget_current_entrate, soggetti_adj ~ date, value.var = 'budget_current')
        
    }
    
    dt_budget_current_entrate[, id := line]
    
    return(dt_budget_current_entrate)
    
}


entrate_list = lapply(unique(dt_budget_current$cdc_raggruppamenti_adj), entrate_line)
names(entrate_list) = unique(dt_budget_current$cdc_raggruppamenti_adj)
dt_entrate_list <- rbindlist(entrate_list)

dt_entrate_list_tot <- dt_entrate_list[, (lapply(.SD, sum)), .SDcols = kc_months, by = id]

rbind(dt_entrate_list, dt_entrate_list_tot, fill = T)

## Export ----
write.xlsx(dt_entrate_list, file = file.path('processed', 'entrate_tab_budget_fin.xlsx'))



# ALTRE ENTRATE----

altre_entrate = function(data = dt_input_budget_fin) {
    
    dt_budget_current_altre_entrate = data[cdc_raggruppamenti_adj == "Ricavi / Costi indiretti" 
                                       
                                       & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj == 'Altri ricavi' , ..kc_months_id]
    
    if(nrow(dt_budget_current_altre_entrate) == 0) {
        
        dt_budget_current_altre_entrate = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_months_id)
        
    } else {
        
        dt_budget_current_altre_entrate = melt(dt_budget_current_altre_entrate, id.vars = 'soggetti_adj', measure.vars = kc_months, variable.name = 'date', 'altre_entrate')
        dt_budget_current_altre_entrate = dt_budget_current_altre_entrate[, .(budget_current = sum(altre_entrate, na.rm = TRUE)), by = c('soggetti_adj', 'date')]
        
        dt_budget_current_altre_entrate = dcast(dt_budget_current_altre_entrate, soggetti_adj ~ date, value.var = 'budget_current')
        
    }
    
    
    return(dt_budget_current_altre_entrate)
    
}


dt_altre_entrate <- altre_entrate()

dt_altre_entrate_tot <- dt_altre_entrate[, (lapply(.SD, sum)), .SDcols = kc_months]
dt_altre_entrate_tot[, id := "altre_entrate"]
dt_altre_entrate_tot <- setcolorder(dt_altre_entrate_tot, c("id", "gennaio" , "febbraio", "marzo" , "aprile",
                                                          
                                                          "maggio", "giugno",  "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre"))


dt_entrate_totali_full <- rbind(dt_entrate_list_tot, dt_altre_entrate_tot, fill = T)

dt_entrate_totali_percentuali_full <- copy(dt_entrate_totali_full)
dt_entrate_totali_percentuali_full[, (kc_months) := lapply(.SD, function(x) {x / sum(x)}), .SDcols = kc_months]


dt_tot_entrate_gestione_corrente <- dt_entrate_totali_full[, lapply(.SD, sum), .SDcols = kc_months]

dt_tot_entrate_gestione_corrente[, id := "entrate"]

#Export
write.xlsx(dt_entrate_totali_full, file = file.path('processed', 'altre_entrate_tot_tab_budget_fin.xlsx'))
write.xlsx(dt_entrate_totali_percentuali_full, file = file.path('processed', 'altre_entrate_perc_tab_budget_fin.xlsx'))


# USCITE----

kc_months_id_adj = c(kc_months_id, "con_unlg_liv_2_adj")

kc_months_tipo <- c(kc_months, "con_unlg_liv_2_adj")


uscite_tipo = function(tipo, data = dt_input_budget_fin) {
    
    dt_budget_current_uscite = data[con_unlg_liv_2 == tipo, ..kc_months_id_adj]
    if(nrow(dt_budget_current_uscite) == 0) {
        
        dt_budget_current_uscite = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_months_id_adj)
        
    } else {
        
        dt_budget_current_uscite = melt(dt_budget_current_uscite, id.vars = c('soggetti_adj',"con_unlg_liv_2_adj"), measure.vars = kc_months, variable.name = 'date', 'uscite')
        dt_budget_current_uscite = dt_budget_current_uscite[, .(budget_current = sum(uscite, na.rm = TRUE)), by = c('soggetti_adj', 'con_unlg_liv_2_adj', 'date')]

        dt_budget_current_uscite = dcast(dt_budget_current_uscite, ... ~ date, value.var = 'budget_current')
        
    }
    
    dt_budget_current_uscite[, ':=' (id = tipo)]
    
    return(dt_budget_current_uscite)
    
}


raggr_uscite = unique(dt_input_budget_fin$con_unlg_liv_2)

uscite_list = lapply(raggr_uscite, uscite_tipo)

raggr_uscite_adj = str_subset(unique(dt_input_budget_fin$con_unlg_liv_2_adj), '\\(', negate = F)
dt_uscite_list <- rbindlist(uscite_list, fill = T)[con_unlg_liv_2_adj %in% raggr_uscite_adj]

dt_uscite_list_tot <- dt_uscite_list[, (lapply(.SD, sum)), .SDcols = kc_months, by = "con_unlg_liv_2_adj"]


dt_tot_uscite_gestione_corrente <- dt_uscite_list_tot[, lapply(.SD, sum), .SDcols = kc_months]

dt_tot_uscite_gestione_corrente[, id := "uscite"]

# saldo gestione


dt_saldo_gestione_corrente <- rbind(dt_tot_entrate_gestione_corrente, dt_tot_uscite_gestione_corrente)

dt_diff_tot <- copy(dt_saldo_gestione_corrente)
dt_diff_tot <- dt_diff_tot[, lapply(.SD, sum), .SDcols = kc_months]
dt_diff_tot[, id := "saldo"]


dt_saldo_tot <- rbind(dt_saldo_gestione_corrente, dt_diff_tot, fill = T)

#Export
write.xlsx(dt_uscite_list, file = file.path('processed', 'uscite_tab_budget_fin.xlsx'))
write.xlsx(dt_uscite_list_tot, file = file.path('processed', 'uscite_tot_tab_budget_fin.xlsx'))















# TRASPORTO GOOD TRACK --------------------------------------

dt_input_budget_fin
dt_t_ipotesi

dt_t_ipotesi_goodtruck <- dt_t_ipotesi[id == "Uscite Good Truck"]
dt_trasporto_goodtruck <- dt_input_budget_fin[soggetti_adj == "0000000302" & con_unlg_liv_2_adj == "(Trasporto)"]



dt_trasporto_goodtruck[, ..kc_months]


dt_trasporto_goodtruck_tot <- dt_trasporto_goodtruck[, (lapply(.SD, function(x){sum(x,na.rm = T)})), .SDcols = kc_months]

dt_trasporto_goodtruck_tot[, soggetti_adj := "0000000302"]
dt_trasporto_goodtruck_tot[, condizioni_commerciali := "Dopo 4 mesi"]


dt_t_ipotesi_goodtruck <- dcast(melt(dt_t_ipotesi_goodtruck, id.vars = "item"), value ~ item)
dt_t_ipotesi_goodtruck <- dt_t_ipotesi_goodtruck[value != "Uscite Good Truck"]
dt_t_ipotesi_goodtruck <- clean_names(dt_t_ipotesi_goodtruck)


dt_t_ipotesi_goodtruck[, value := as.numeric(value)]
dt_t_ipotesi_goodtruck[, quota_parte_ex_cta := as.numeric(quota_parte_ex_cta)]
dt_t_ipotesi_goodtruck[, quota_parte_ex_ctl := as.numeric(quota_parte_ex_ctl)]
dt_t_ipotesi_goodtruck[, quota_parte_good_truck := as.numeric(quota_parte_good_truck)]
col_ipotesi <- c("value", "quota_parte_ex_cta", "quota_parte_ex_ctl", "quota_parte_good_truck")
dt_t_ipotesi_goodtruck <- dt_t_ipotesi_goodtruck[, (lapply(.SD, function(x){sum(x,na.rm = T)})), .SDcols = col_ipotesi]




dt_trasporto_goodtruck_excta <- dt_trasporto_goodtruck_tot[, (lapply(.SD, function(x){x*dt_t_ipotesi_goodtruck$quota_parte_ex_cta})), .SDcols = kc_months]
dt_trasporto_goodtruck_exctl <- dt_trasporto_goodtruck_tot[, (lapply(.SD, function(x){x*dt_t_ipotesi_goodtruck$quota_parte_ex_ctl})), .SDcols = kc_months]
dt_trasporto_goodtruck_quota_goodtrack <- dt_trasporto_goodtruck_tot[, (lapply(.SD, function(x){x*dt_t_ipotesi_goodtruck$quota_parte_good_truck})), .SDcols = kc_months]


dt_trasporto_goodtruck_tot_full <- rbind(dt_trasporto_goodtruck_tot,dt_trasporto_goodtruck_excta, dt_trasporto_goodtruck_exctl, dt_trasporto_goodtruck_quota_goodtrack, fill = TRUE )






# ASSICURAZIONI --------------------------

dt_assicurazioni <- clean_names(dt_assicurazioni)



dt_assicurazioni <- dt_assicurazioni[, .(importo = sum(importo)), by = "periodicita"]



assicurazioni_gennaio <- - dt_assicurazioni[periodicita == "TRIMESTRALE"]$importo - dt_assicurazioni[periodicita == "ANNUALE"]$importo 

assicurazioni_marzo <- dt_assicurazioni[periodicita == "MARZO-APRILE"]$importo /2 

assicurazioni_aprile <- - dt_assicurazioni[periodicita == "TRIMESTRALE"]$importo - dt_assicurazioni[periodicita == "MARZO-APRILE"]$importo /2
    
assicurazioni_luglio <- - dt_assicurazioni[periodicita == "SEMESTRALE"]$importo - dt_assicurazioni[periodicita == "TRIMESTRALE"]$importo

assicurazioni_ottobre <- - dt_assicurazioni[periodicita == "TRIMESTRALE"]$importo








# GESTIONE FINANZIAMENTI DI MEDIO/LUNGO TERMINE ------------------------------------

dt_muto_unicredit  <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_unicredit', detectDates = TRUE)
dt_mutuo_bper  <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_BPER', detectDates = TRUE)
dt_mutuo_intesa  <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_intesa', detectDates = TRUE)

setDT(dt_muto_unicredit)
setDT(dt_mutuo_bper)
setDT(dt_mutuo_intesa)



colnames <- c("banca/class", "category", kc_months)


dt_gestione_fin_mediolungo <- data.frame(banca = c("unicredit", "unicredit", "unicredit", 
                                                   "bper","bper", "bper", 
                                                   "intesa", "intesa","intesa",
                                                    "parfinco", "parfinco", "parfinco",
                                                   "nuovo_finanziamento", "nuovo_finanziamento", "nuovo_finanziamento"), 
                                         category = c("erogazione_mutuo", "rimborso_rata_mutuo", "interessi_su_mutuo", 
                                                      "erogazione_mutuo", "rimborso_rata_mutuo", "interessi_su_mutuo",
                                                      "erogazione_mutuo", "rimborso_rata_mutuo", "interessi_su_mutuo",
                                                      "erogazione_mutuo", "rimborso_rata_mutuo", "interessi_su_mutuo",
                                                      "nuova_erogazione_mutuo_MLT", "rimborso_rata_nuovo_mutuo_MLT", "interessi_nuovo_mutuo_MLT"),
                                         gennaio = NA, febbraio = NA, marzo = NA, aprile = NA,
                                         maggio = NA, giugno = NA, luglio = NA, agosto = NA, 
                                         setembre = NA, ottobre = NA, novembre = NA, dicembre = NA)



setDT(dt_gestione_fin_mediolungo)
