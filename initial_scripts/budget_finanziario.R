#BUDGET FINANZIARIO

# LIBRARIES
library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)

options(scipen = 999, digits = 3)

# UPLOAD DATA----
## Consuntivo
dt_consbe = readRDS(file.path('processed', 'tab_BudgetEconomico_consuntivo.rds'))
dt_consbe = janitor::clean_names(dt_consbe)
setDT(dt_consbe)

## Budget 2022
dt_budget_2022 = readRDS(file.path('processed', 'tab_budget_2022.rds')) |>
    setnames("condizioni_commerciali_nominali_riclassificato", "condizioni_commerciali") |>
    setnames("ter_cod_adj", "soggetti_adj")
dt_budget_2022 = janitor::clean_names(dt_budget_2022)
setDT(dt_budget_2022)

################################################################################

dt_tot_1 <- dt_consbe[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2_adj,
                        gennaio_2021_iva, febbraio_2021_iva, marzo_2021_iva, aprile_2021_iva, maggio_2021_iva, giugno_2021_iva,
                        luglio_2021_iva, agosto_2021_iva, settembre_2021_iva, ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva)]

dt_tot_2 <- dt_budget_2022[, .(soggetti_adj, condizioni_commerciali, cdc_raggruppamenti_adj, tipo_voce, con_unlg_liv_2_adj,
                          gennaio_lordo_iva, febbraio_lordo_iva, marzo_lordo_iva, aprile_lordo_iva, maggio_lordo_iva, giugno_lordo_iva,
                          luglio_lordo_iva, agosto_lordo_iva, settembre_lordo_iva, ottobre_lordo_iva, novembre_lordo_iva, dicembre_lordo_iva)]

dt_tot <- rbind(dt_tot_1, dt_tot_2, fill = T)

# 0 mesi
dt_tot <- dt_tot[condizioni_commerciali == "Entro mese in corso", c("gennaio_2022","febbraio_2022","marzo_2022","aprile_2022","maggio_2022","giugno_2022",
                                                                    "luglio_2022","agosto_2022","settembre_2022","ottobre_2022","novembre_2022","dicembre_2022") := list(gennaio_lordo_iva:dicembre_lordo_iva)]

#oppure
dt_tot <- dt_tot[condizioni_commerciali == "Entro mese in corso", ':=' (gennaio_2022 = gennaio_lordo_iva,
                         febbraio_2022 = febbraio_lordo_iva,
                         marzo_2022 = marzo_lordo_iva,
                         aprile_2022 = aprile_lordo_iva,
                         maggio_2022 = maggio_lordo_iva,
                         giugno_2022 = giugno_lordo_iva,
                         luglio_2022 = luglio_lordo_iva,
                         agosto_2022 = agosto_lordo_iva,
                         settembre_2022 = settembre_lordo_iva,
                         ottobre_2022 = ottobre_lordo_iva,
                         novembre_2022 = novembre_lordo_iva,
                         dicembre_2022 = dicembre_lordo_iva)]

# -1 mese
dt_tot <- dt_tot[condizioni_commerciali == "Mese successivo", c("gennaio_2022","febbraio_2022","marzo_2022","aprile_2022","maggio_2022","giugno_2022",
                                                                    "luglio_2022","agosto_2022","settembre_2022","ottobre_2022","novembre_2022","dicembre_2022") := list(dicembre_2021_iva, gennaio_lordo_iva:novembre_lordo_iva)]

# -2 mesi
dt_tot <- dt_tot[condizioni_commerciali == "Dopo 2 mesi", c("gennaio_2022","febbraio_2022","marzo_2022","aprile_2022","maggio_2022","giugno_2022",
                                                                "luglio_2022","agosto_2022","settembre_2022","ottobre_2022","novembre_2022","dicembre_2022") := list(novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:ottobre_lordo_iva)]

# -3 mesi
dt_tot <- dt_tot[condizioni_commerciali == "Dopo 3 mesi", c("gennaio_2022","febbraio_2022","marzo_2022","aprile_2022","maggio_2022","giugno_2022",
                                                            "luglio_2022","agosto_2022","settembre_2022","ottobre_2022","novembre_2022","dicembre_2022") := list(ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:settebre_lordo_iva)]

# -4 mesi
dt_tot <- dt_tot[condizioni_commerciali == "Dopo 2 mesi", c("gennaio_2022","febbraio_2022","marzo_2022","aprile_2022","maggio_2022","giugno_2022",
                                                            "luglio_2022","agosto_2022","settembre_2022","ottobre_2022","novembre_2022","dicembre_2022") := list(settembre_2021_iva, ottobre_2021_iva, novembre_2021_iva, dicembre_2021_iva, gennaio_lordo_iva:agosto_lordo_iva)]
