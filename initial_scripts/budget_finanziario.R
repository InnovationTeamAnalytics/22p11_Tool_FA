#BUDGET FINANZIARIO

# LIBRARIES
library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)
library(stringr)
library(lubridate)
library(xts)

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
dt_t_ipotesi = janitor::clean_names(dt_t_ipotesi)
setDT(dt_t_ipotesi)

dt_t_consulenze = read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Dettaglio_consulenze', detectDates = TRUE)
dt_t_consulenze = janitor::clean_names(dt_t_consulenze)
setDT(dt_t_consulenze)

## Mutui
dt_t_bper <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_BPER', detectDates = TRUE)
dt_t_bper = janitor::clean_names(dt_t_bper)
setDT(dt_t_bper)
dt_t_bper[, banca := "BPER"]

dt_t_unicredit <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_unicredit', detectDates = TRUE)
dt_t_unicredit = janitor::clean_names(dt_t_unicredit)
setDT(dt_t_unicredit)
dt_t_unicredit[, banca := "Unicredit"]

dt_t_intesa <- read.xlsx(file.path('inputs', 'support_fin.xlsx'), sheet = 'Mutuo_intesa', detectDates = TRUE)
dt_t_intesa = janitor::clean_names(dt_t_intesa)
setDT(dt_t_intesa)
dt_t_intesa[, banca := "Intesa"]

dt_t_mutui <- rbind(dt_t_bper, dt_t_intesa, dt_t_unicredit)

################################################################################

dt_consbe_fin_parziale <- dt_consbe[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2, con_unlg_liv_2_adj, tipo_costo_personale,
                        gennaio_2021_iva, febbraio_2021_iva, marzo_2021_iva, aprile_2021_iva, maggio_2021_iva, giugno_2021_iva,
                        luglio_2021_iva, agosto_2021_iva, settembre_2021_iva, ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva)]

dt_budget_current_parziale <- dt_budget_current[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2, con_unlg_liv_2_adj, tipo_costo_personale,
                          gennaio_lordo_iva, febbraio_lordo_iva, marzo_lordo_iva, aprile_lordo_iva, maggio_lordo_iva, giugno_lordo_iva,
                          luglio_lordo_iva, agosto_lordo_iva, settembre_lordo_iva, ottobre_lordo_iva, novembre_lordo_iva, dicembre_lordo_iva)]

dt_input_budget_fin <- rbind(dt_consbe_fin_parziale, dt_budget_current_parziale, fill = T)

#dt_input_budget_fin[soggetti_adj = "0000000302", condizioni_commerciali := "Dopo 2 mesi"]

# 0 mesi
# dt_input_budget_fin <- dt_input_budget_fin[condizioni_commerciali == "Entro mese in corso", c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio","agosto","settembre","ottobre","novembre","dicembre") := list(gennaio_lordo_iva:dicembre_lordo_iva)]

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
# Attenzione: da fare a parte (specifiche) ci sono:
## Trasporto E
## Personale S
## Assicurazioni E
## consulenze S

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

#SPECIFICHE USCITE----
## Personale----

### Costo standard del personale e contributi inps e inail
personale_line = function(tipo_costo, data = dt_input_budget_fin) {
    
    dt_budget_current_personale = data[con_unlg_liv_2_adj == "(Personale)" & tipo_costo_personale == tipo_costo, ..kc_months_id]
    if(nrow(dt_budget_current_personale) == 0) {
        
        dt_budget_current_personale = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_months_id)
        
    } else {
        
        dt_budget_current_personale = melt(dt_budget_current_personale, id.vars = 'soggetti_adj', measure.vars = kc_months, variable.name = 'date', 'personale')
        dt_budget_current_personale = dt_budget_current_personale[, .(budget_current = sum(personale, na.rm = TRUE)), by = c('soggetti_adj', 'date')]
        dt_budget_current_personale = dcast(dt_budget_current_personale, soggetti_adj ~ date, value.var = 'budget_current')
        
    }
    
    dt_budget_current_personale[, id := tipo_costo]
    
    return(dt_budget_current_personale)
    
}


personale_list = lapply(unique(dt_budget_current$tipo_costo_personale, na.rm = T), personale_line)
names(personale_list) = unique(dt_budget_current$tipo_costo_personale, na.rm = T)
dt_personale_list <- rbindlist(personale_list)

### Contributi irpef
dt_irpef_line <- dt_personale_list[id != "CONTRIBUTI INPS E INAIL", lapply(.SD, function(x) {x * dt_t_ipotesi[item %like% "IRPEF", value]}), .SDcols = kc_months, by = "soggetti_adj"][, id := "CONTRIBUTI IRPEF"]

dt_personale_part <- rbind(dt_personale_list, dt_irpef_line)

dt_personale_part_tot <- dt_personale_part[, lapply(.SD, sum), .SDcols = kc_months, by = "id"]

### Plafond
dt_plafond_line <- data.table(id = "Plafond",
                              gennaio = dt_t_ipotesi[item %like% "Plafond", value]) 

dt_plafond_line <- rbind(dt_personale_part_tot, dt_plafond_line, fill = T)

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (febbraio = gennaio)]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (marzo = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", marzo] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", marzo] +
                                                                                 dt_plafond_line[id == "Plafond", febbraio] < 0,
                                                                             0,
                                                                             dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", marzo] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", marzo] +
                                                                                 dt_plafond_line[id == "Plafond", febbraio]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (aprile = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", aprile] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", aprile] +
                                                                                 dt_plafond_line[id == "Plafond", marzo] < 0,
                                                                             0,
                                                                             dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", aprile] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", aprile] +
                                                                                 dt_plafond_line[id == "Plafond", marzo]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (maggio = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", maggio] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", maggio] +
                                                                                  dt_plafond_line[id == "Plafond", aprile] < 0,
                                                                              0,
                                                                              dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", maggio] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", maggio] +
                                                                                  dt_plafond_line[id == "Plafond", aprile]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (giugno = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", giugno] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", giugno] +
                                                                                  dt_plafond_line[id == "Plafond", maggio] < 0,
                                                                              0,
                                                                              dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", giugno] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", giugno] +
                                                                                  dt_plafond_line[id == "Plafond", maggio]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (luglio = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", luglio] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", luglio] +
                                                                                 dt_plafond_line[id == "Plafond", giugno] < 0,
                                                                             0,
                                                                             dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", luglio] + 
                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", luglio] +
                                                                                 dt_plafond_line[id == "Plafond", giugno]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (agosto = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", agosto] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", agosto] +
                                                                                  dt_plafond_line[id == "Plafond", luglio] < 0,
                                                                              0,
                                                                              dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", agosto] + 
                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", agosto] +
                                                                                  dt_plafond_line[id == "Plafond", luglio]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (settembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", settembre] + 
                                                                                     dt_plafond_line[id == "CONTRIBUTI IRPEF", settembre] +
                                                                                     dt_plafond_line[id == "Plafond", agosto] < 0,
                                                                                 0,
                                                                                 dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", settembre] + 
                                                                                     dt_plafond_line[id == "CONTRIBUTI IRPEF", settembre] +
                                                                                     dt_plafond_line[id == "Plafond", agosto]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (ottobre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", ottobre] + 
                                                                                   dt_plafond_line[id == "CONTRIBUTI IRPEF", ottobre] +
                                                                                   dt_plafond_line[id == "Plafond", settembre] < 0,
                                                                               0,
                                                                               dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", ottobre] + 
                                                                                   dt_plafond_line[id == "CONTRIBUTI IRPEF", ottobre] +
                                                                                   dt_plafond_line[id == "Plafond", settembre]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (novembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", novembre] + 
                                                                                    dt_plafond_line[id == "CONTRIBUTI IRPEF", novembre] +
                                                                                    dt_plafond_line[id == "Plafond", ottobre] < 0,
                                                                                0,
                                                                                dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", novembre] + 
                                                                                    dt_plafond_line[id == "CONTRIBUTI IRPEF", novembre] +
                                                                                    dt_plafond_line[id == "Plafond", ottobre]))]

dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (dicembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", dicembre] + 
                                                                                    dt_plafond_line[id == "CONTRIBUTI IRPEF", dicembre] +
                                                                                    dt_plafond_line[id == "Plafond", novembre] < 0,
                                                                                0,
                                                                                dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", dicembre] + 
                                                                                    dt_plafond_line[id == "CONTRIBUTI IRPEF", dicembre] +
                                                                                    dt_plafond_line[id == "Plafond", novembre]))]


# Unito non funziona
# dt_plafond_line_tot <- dt_plafond_line[id == "Plafond", ':=' (febbraio = gennaio,
#                                        marzo = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", marzo] + 
#                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", marzo] +
#                                                               dt_plafond_line[id == "Plafond", febbraio] < 0,
#                                                           0,
#                                                           dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", marzo] + 
#                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", marzo] +
#                                                               dt_plafond_line[id == "Plafond", febbraio]),
#                                        aprile = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", aprile] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", aprile] +
#                                                                               dt_plafond_line[id == "Plafond", marzo] < 0,
#                                                        0,
#                                                        dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", aprile] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", aprile] +
#                                                                               dt_plafond_line[id == "Plafond", marzo]),
#                                        maggio = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", maggio] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", maggio] +
#                                                                               dt_plafond_line[id == "Plafond", aprile] < 0,
#                                                        0,
#                                                        dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", maggio] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", maggio] +
#                                                                               dt_plafond_line[id == "Plafond", aprile]),
#                                        giugno = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", giugno] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", giugno] +
#                                                                               dt_plafond_line[id == "Plafond", maggio] < 0,
#                                                        0,
#                                                        dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", giugno] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", giugno] +
#                                                                               dt_plafond_line[id == "Plafond", maggio]),
#                                        luglio = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", luglio] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", luglio] +
#                                                                               dt_plafond_line[id == "Plafond", giugno] < 0,
#                                                        0,
#                                                        dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", luglio] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", luglio] +
#                                                                               dt_plafond_line[id == "Plafond", giugno]),
#                                        agosto = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", agosto] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", agosto] +
#                                                                               dt_plafond_line[id == "Plafond", luglio] < 0,
#                                                        0,
#                                                        dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", agosto] + 
#                                                                               dt_plafond_line[id == "CONTRIBUTI IRPEF", agosto] +
#                                                                               dt_plafond_line[id == "Plafond", luglio]),
#                                        settembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", settembre] + 
#                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", settembre] +
#                                                                                  dt_plafond_line[id == "Plafond", agosto] < 0,
#                                                           0,
#                                                           dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", settembre] + 
#                                                                                  dt_plafond_line[id == "CONTRIBUTI IRPEF", settembre] +
#                                                                                  dt_plafond_line[id == "Plafond", agosto]),
#                                        ottobre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", ottobre] + 
#                                                                                dt_plafond_line[id == "CONTRIBUTI IRPEF", ottobre] +
#                                                                                dt_plafond_line[id == "Plafond", settembre] < 0,
#                                                         0,
#                                                         dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", ottobre] + 
#                                                                                dt_plafond_line[id == "CONTRIBUTI IRPEF", ottobre] +
#                                                                                dt_plafond_line[id == "Plafond", settembre]),
#                                        novembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", novembre] + 
#                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", novembre] +
#                                                                                 dt_plafond_line[id == "Plafond", ottobre] < 0,
#                                                          0,
#                                                          dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", novembre] + 
#                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", novembre] +
#                                                                                 dt_plafond_line[id == "Plafond", ottobre]),
#                                        dicembre = ifelse(dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", dicembre] + 
#                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", dicembre] +
#                                                                                 dt_plafond_line[id == "Plafond", novembre] < 0,
#                                                          0,
#                                                          dt_plafond_line[id == "CONTRIBUTI INPS E INAIL", dicembre] + 
#                                                                                 dt_plafond_line[id == "CONTRIBUTI IRPEF", dicembre] +
#                                                                                 dt_plafond_line[id == "Plafond", novembre]))]

dt_plafond_line_tot <- dt_plafond_line_tot[, category := ifelse(id %like% "CONTRIBUTI" | id %like% "Plafond", "CONTRIBUTI", "COSTO STANDARD DEL PERSONALE")]

dt_plafond_single <- dt_plafond_line_tot[id == "Plafond",]

# Consulenze----
# Eliminare Cerved da uscite_list perchè i calcoli per Cerved sono diversi

# Calcoli dataset consulenze

consulenza_tesoreria <- dt_t_consulenze[, ':=' (scadenza = as.POSIXct(scadenza),
                        iva = imponibile * 0.22)][, ':=' (totale = - imponibile - iva)][, mese_scadenza := format(scadenza,"%B")][!progetto %like% "Credito" & !progetto %like% "Rating"]
#dt_consulenza_grouped <- dt_t_consulenze[, lapply(.SD, sum), by = "mese_scadenza", .SDcols = "totale"]
dt_consulenza_months <- merge(consulenza_tesoreria, data.table(mese = c(kc_months)), by.x = "mese_scadenza", by.y = "mese", all.y = T)

dt_uscite_tesoreria <- dt_uscite_list[soggetti_adj == "0000002035" & con_unlg_liv_2_adj == "(Consulenze)"][, ':=' (gennaio = dt_consulenza_months[mese_scadenza == "gennaio", sum(totale), by = "mese_scadenza"][, V1],
                                                                        febbraio = dt_consulenza_months[mese_scadenza == "febbraio", sum(totale), by = "mese_scadenza"][, V1],
                                                                        marzo = dt_consulenza_months[mese_scadenza == "marzo", sum(totale), by = "mese_scadenza"][, V1],
                                                                        aprile = dt_consulenza_months[mese_scadenza == "aprile", sum(totale), by = "mese_scadenza"][, V1],
                                                                        maggio = dt_consulenza_months[mese_scadenza == "maggio", sum(totale), by = "mese_scadenza"][, V1],
                                                                        giugno = dt_consulenza_months[mese_scadenza == "giugno", sum(totale), by = "mese_scadenza"][, V1],
                                                                        luglio = dt_consulenza_months[mese_scadenza == "luglio", sum(totale), by = "mese_scadenza"][, V1],
                                                                        agosto = dt_consulenza_months[mese_scadenza == "agosto", sum(totale), by = "mese_scadenza"][, V1],
                                                                        settembre = dt_consulenza_months[mese_scadenza == "settembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                        ottobre = dt_consulenza_months[mese_scadenza == "ottobre", sum(totale), by = "mese_scadenza"][, V1],
                                                                        novembre = dt_consulenza_months[mese_scadenza == "novembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                        dicembre = dt_consulenza_months[mese_scadenza == "dicembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                        cat_cerved = "TESORERIA E CDG")]


consulenza_credito <- dt_t_consulenze[, ':=' (scadenza = as.POSIXct(scadenza),
                        iva = imponibile * 0.22)][, ':=' (totale = - imponibile - iva)][, mese_scadenza := format(scadenza,"%B")][progetto %like% "Credito"]

dt_consulenza_months_2 <- merge(consulenza_credito, data.table(mese = c(kc_months)), by.x = "mese_scadenza", by.y = "mese", all.y = T)

dt_uscite_credito <- dt_uscite_list[soggetti_adj == "0000002031" & con_unlg_liv_2_adj == "(Consulenze)"][, ':=' (gennaio = dt_consulenza_months_2[mese_scadenza == "gennaio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 febbraio = dt_consulenza_months_2[mese_scadenza == "febbraio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 marzo = dt_consulenza_months_2[mese_scadenza == "marzo", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 aprile = dt_consulenza_months_2[mese_scadenza == "aprile", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 maggio = dt_consulenza_months_2[mese_scadenza == "maggio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 giugno = dt_consulenza_months_2[mese_scadenza == "giugno", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 luglio = dt_consulenza_months_2[mese_scadenza == "luglio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 agosto = dt_consulenza_months_2[mese_scadenza == "agosto", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 settembre = dt_consulenza_months_2[mese_scadenza == "settembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 ottobre = dt_consulenza_months_2[mese_scadenza == "ottobre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 novembre = dt_consulenza_months_2[mese_scadenza == "novembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 dicembre = dt_consulenza_months_2[mese_scadenza == "dicembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                 cat_cerved = "CREDITO")]

consulenza_rating <- dt_t_consulenze[, ':=' (scadenza = as.POSIXct(scadenza),
                                              iva = imponibile * 0.22)][, ':=' (totale = - imponibile - iva)][, mese_scadenza := format(scadenza,"%B")][progetto %like% "Rating"]

dt_consulenza_months_3 <- merge(consulenza_rating, data.table(mese = c(kc_months)), by.x = "mese_scadenza", by.y = "mese", all.y = T)

dt_uscite_rating <- dt_uscite_list[soggetti_adj == "0000002031" & con_unlg_liv_2_adj == "(Consulenze)"][, ':=' (gennaio = dt_consulenza_months_3[mese_scadenza == "gennaio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                febbraio = dt_consulenza_months_3[mese_scadenza == "febbraio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                marzo = dt_consulenza_months[mese_scadenza == "marzo", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                aprile = dt_consulenza_months_3[mese_scadenza == "aprile", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                maggio = dt_consulenza_months_3[mese_scadenza == "maggio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                giugno = dt_consulenza_months_3[mese_scadenza == "giugno", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                luglio = dt_consulenza_months_3[mese_scadenza == "luglio", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                agosto = dt_consulenza_months_3[mese_scadenza == "agosto", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                settembre = dt_consulenza_months_3[mese_scadenza == "settembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                ottobre = dt_consulenza_months_3[mese_scadenza == "ottobre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                novembre = dt_consulenza_months_3[mese_scadenza == "novembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                dicembre = dt_consulenza_months_3[mese_scadenza == "dicembre", sum(totale), by = "mese_scadenza"][, V1],
                                                                                                                cat_cerved = "RATING")]

dt_uscite_consulenza <- rbind(dt_uscite_tesoreria, dt_uscite_credito, dt_uscite_rating)


# SALDO GESTIONE----

dt_saldo_gestione_corrente <- rbind(dt_tot_entrate_gestione_corrente, dt_tot_uscite_gestione_corrente)

dt_diff_tot <- copy(dt_saldo_gestione_corrente)
dt_diff_tot <- dt_diff_tot[, lapply(.SD, sum), .SDcols = kc_months]
dt_diff_tot[, id := "saldo"]


dt_saldo_tot <- rbind(dt_saldo_gestione_corrente, dt_diff_tot, fill = T)

#Export
write.xlsx(dt_uscite_list, file = file.path('processed', 'uscite_tab_budget_fin.xlsx'))
write.xlsx(dt_uscite_list_tot, file = file.path('processed', 'uscite_tot_tab_budget_fin.xlsx'))




# MUTUI----
dt_t_mutui[, data := as.Date(data, format = "%Y-%m-%d")]
current_year <- format(Sys.time(), "%Y")

dt_t_mutui_current <- dt_t_mutui[format(data, "%Y") == current_year][, mese_nome := format(data,"%B")]

kc_months = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

# Erogazione----

mutui_erogazione = function(nome_banca, data = dt_t_mutui_current) {
    
    f <- function(x) {list(0)}
    dt_mutui_new <- dt_t_mutui_current[,(kc_months) := f()][banca == nome_banca,]
    dt_mutui_new <- dt_mutui_new[order(data)]
    if (nrow(dt_mutui_new[is.na(quota_capitale) & is.na(quote_interes) & is.na(spese) & is.na(totale_rata)]) == 0) {
        dt_mutui_new <- dt_mutui_new[, id := "Erogazione mutuo"]
    } else {
        mese_erogazione <- dt_mutui_new[is.na(quota_capitale) & is.na(quote_interes) & is.na(spese) & is.na(totale_rata), mese_nome]
        importo <- dt_mutui_new[is.na(quota_capitale) & is.na(quote_interes) & is.na(spese) & is.na(totale_rata), debito_residuo]
        
        dt_mutui_new[, mese_erogazione] <- importo
        dt_mutui_new <- dt_mutui_new[, id := "Erogazione mutuo"]
    
    }

    return(dt_mutui_new)
}


mutui_erogazione = lapply(unique(dt_t_mutui_current$banca), mutui_erogazione)
names(mutui_erogazione) = unique(dt_t_mutui_current$banca)
dt_mutui_erogazione_list <- rbindlist(mutui_erogazione)

dt_mutui_erogazione_grouped <- dt_mutui_erogazione_list[, lapply(.SD, unique), .SDcols = kc_months, by = .(banca, id)]



# Rimborso rata----

mutui_rimborso = function(nome_banca, data = dt_t_mutui_current) {
    
    f <- function(x) {list(0)}
    dt_mutui_quota <- dt_t_mutui_current[,(kc_months) := f()][banca == nome_banca,]
    dt_mutui_quota <- dt_mutui_quota[order(data)]
    
    mese_quota <- dt_mutui_quota[!is.na(quota_capitale) | !is.na(quote_interes) | !is.na(spese) | !is.na(totale_rata), mese_nome]
    importo_quota <- dt_mutui_quota[!is.na(quota_capitale) | !is.na(quote_interes) | !is.na(spese) | !is.na(totale_rata), quota_capitale]
    
    assign_quota <- function(i) {
        dt_mutui_quota[, mese_quota[i]] <- importo_quota[i]
        return(dt_mutui_quota)
    }
    
    dt_mutui_quota <- lapply(c(seq(1:length(mese_quota))), assign_quota)
    dt_mutui_quota <- rbindlist(dt_mutui_quota)
    dt_mutui_quota[, id := "Rimborso rata mutuo"]
    
    dt_mutui_quota <- dt_mutui_quota[, lapply(.SD, unique), .SDcols = kc_months, by = .(banca, id)][, lapply(.SD, sum), .SDcols = kc_months, by = .(banca, id)]
    
    return(dt_mutui_quota)
}


mutui_quota_rata = lapply(unique(dt_t_mutui_current$banca), mutui_rimborso)
names(mutui_quota_rata) = unique(dt_t_mutui_current$banca)
dt_mutui_quota_rata_list <- rbindlist(mutui_quota_rata)

dt_mutui_quota_rata_grouped <- dt_mutui_quota_rata_list[, lapply(.SD, unique), .SDcols = kc_months, by = .(banca, id)]


# Interessi----

# Rimborso rata----

mutui_interesse = function(nome_banca, data = dt_t_mutui_current) {
    
    f <- function(x) {list(0)}
    dt_mutui_interess <- dt_t_mutui_current[,(kc_months) := f()][banca == nome_banca,]
    dt_mutui_interess <- dt_mutui_interess[order(data)]
    
    mese_quota <- dt_mutui_interess[!is.na(quota_capitale) | !is.na(quote_interes) | !is.na(spese) | !is.na(totale_rata), mese_nome]
    importo_quota <- dt_mutui_interess[!is.na(quota_capitale) | !is.na(quote_interes) | !is.na(spese) | !is.na(totale_rata), quote_interes]
    
    assign_interesse <- function(i) {
        dt_mutui_interess[, mese_quota[i]] <- importo_quota[i]
        return(dt_mutui_interess)
    }
    
    dt_mutui_interess <- lapply(c(seq(1:length(mese_quota))), assign_interesse)
    dt_mutui_interess <- rbindlist(dt_mutui_interess)
    dt_mutui_interess[, id := "Interessi su mutuo"]
    
    dt_mutui_interess <- dt_mutui_interess[, lapply(.SD, unique), .SDcols = kc_months, by = .(banca, id)][, lapply(.SD, sum), .SDcols = kc_months, by = .(banca, id)]
    
    return(dt_mutui_interess)
}


mutui_interess = lapply(unique(dt_t_mutui_current$banca), mutui_interesse)
names(mutui_interess) = unique(dt_t_mutui_current$banca)
dt_mutui_interess_list <- rbindlist(mutui_interess)

dt_mutui_interess_grouped <- dt_mutui_interess_list[, lapply(.SD, unique), .SDcols = kc_months, by = .(banca, id)]


dt_mutui_tot <- rbind(dt_mutui_erogazione_grouped, dt_mutui_quota_rata_grouped, dt_mutui_interess_grouped)

# Parfinco ---------------------


dt_parfinco <- dt_t_ipotesi[id == "Parfinco"]

debito_parfinco <- dt_parfinco[item == "Debito residuo", value] 
interesse_parfinco <- dt_parfinco[item == "Tasso di interesse annuo", value]

operazione <- debito_parfinco*interesse_parfinco/12

dt_parfinco_line <- data.table(  rep( NA, ncol(dt_mutui_tot)))

dt_parfinco_line <- transpose(dt_parfinco_line)

setnames(dt_parfinco_line, names(dt_mutui_tot))


dt_mutui_parfinco_tot <- rbind(dt_mutui_tot, dt_parfinco_line)

dt_mutui_parfinco_tot[is.na(banca) , banca := "Parfinco" ]

dt_mutui_parfinco_tot[banca == "Parfinco" , id := "Interessi su mutuo" ]

# 
# 
# f_parf <- function(debito, interesse ) {list(debito*interesse/12)}
# dt_mutui_parfinco_tot[banca == "Parfinco"][, (kc_months) := operazione]

# 

dt_mutui_parfinco_tot [banca == "Parfinco", ':='
                       
                       (
                           gennaio = operazione,
                           febbraio = operazione,
                           marzo = operazione,
                           aprile = operazione,
                           maggio = operazione,
                           giugno = operazione,
                           luglio = operazione,
                           agosto = operazione,
                           settembre = operazione,
                           ottobre = operazione,
                           novembre = operazione,
                           dicembre = operazione)]



# Gestione Straordinaria ----------------------------

dt_immobiliare <- dt_t_ipotesi[id == "Incasso credito FMG"]

dt_immobiliare <- dt_immobiliare[, item := as.Date(item, format = "%Y-%m-%d")][, mese_nome := format(item,"%B")]

dt_immobiliare_line <- data.table(  rep( NA, ncol(dt_mutui_parfinco_tot)))

dt_immobiliare_line <- transpose(dt_immobiliare_line)

setnames(dt_immobiliare_line, names(dt_mutui_parfinco_tot))


dt_immobiliare_line[, banca := "Gestione_straordinaria" ]

dt_immobiliare_line[, id := as.character(id) ]

dt_immobiliare_line[, id := "FMG_Immobiliare"]

    
assign_fmg <- function(i) {
    mese_nome <- dt_immobiliare[, mese_nome]
    value <- dt_immobiliare[, value]
    dt_immobiliare_line[, mese_nome[i]] <- dt_immobiliare_line[, value[i]]
        return(dt_immobiliare_line)
    }
    
dt_immobiliare_fin <- lapply(c(seq(1:nrow(dt_immobiliare))), assign_fmg)

dt_immobiliare_fin <- rbindlist(dt_immobiliare_fin)

dt_immobiliare_fin_grouped <- dt_immobiliare_fin[, lapply(.SD, function(x){sum(x, na.rm = T)}), .SDcols = kc_months, by = .(banca, id)]


imposte <- dt_t_ipotesi[item %like% "Imposte totali", value]
dt_imposte_line <- data.table(  rep( NA, ncol(dt_immobiliare_fin)))

dt_imposte_line <- transpose(dt_imposte_line)

setnames(dt_imposte_line, names(dt_immobiliare_fin))

dt_imposte_line[, id := "Imposte totali"]

dt_straordinaria_grouped <- rbind(dt_immobiliare_fin_grouped, dt_imposte_line)

dt_straordinaria_grouped[id == "Imposte totali", agosto := imposte]
