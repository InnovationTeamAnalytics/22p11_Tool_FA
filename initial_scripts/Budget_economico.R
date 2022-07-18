# PACKAGES ====================================================================================================

library(data.table)
library(openxlsx)
library(innteamUtils)
library(janitor)



# UPLOAD DATA =================================================================================================

dt_budget_eco_current <- readRDS(file.path('processed', 'tab_budget_2022.rds'))


dt_t_cliente_tipo = read.xlsx(file.path('inputs', 'support_trascodifiche.xlsx'), sheet = 'Tipo_Cliente', detectDates = TRUE)




# RICAVI CALCULATION =========================================================================================

kc_bdgeco_ricavi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_ricavi_id = c('ter_cod_adj', kc_bdgeco_ricavi)




## Income by Line function: Budget_2022 -----------------------------------------



ricavi_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_ricavi = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj != 'Altri ricavi' , ..kc_bdgeco_ricavi_id]
    
    if(nrow(dt_budget_eco_current_ricavi) == 0) {
        
        dt_budget_eco_current_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_ricavi_id)
        
    } else {
        
        dt_budget_eco_current_ricavi = melt(dt_budget_eco_current_ricavi, id.vars = 'ter_cod_adj', measure.vars = kc_bdgeco_ricavi, variable.name = 'date', 'ricavi')
        dt_budget_eco_current_ricavi = dt_budget_eco_current_ricavi[, .(budget_2022 = sum(ricavi, na.rm = TRUE)), by = c('ter_cod_adj', 'date')]
        
        dt_budget_eco_current_ricavi = dcast(dt_budget_eco_current_ricavi, ter_cod_adj ~ date, value.var = 'budget_2022')
        
    }
    dt_budget_eco_current_ricavi[, id := line]
    
    return(dt_budget_eco_current_ricavi)
    
}


## Function Test ------------------------



ricavi_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), ricavi_line)


names(ricavi_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)


dt_ricavi_list <- rbindlist(ricavi_list)

dt_ricavi_list_tot <- dt_ricavi_list[, .
                                   
                                   
                                      (   ter_cod_adj = "total",
                                          
                                          gennaio = sum(gennaio), 
                                       
                                          febbraio = sum(febbraio),
                                       
                                          marzo = sum(marzo),
                                       
                                          aprile = sum (aprile),
                                       
                                          maggio = sum(maggio),
                                       
                                          giugno = sum(giugno),
                                       
                                          luglio = sum(luglio),
                                       
                                          agosto = sum(agosto),
                                       
                                          settembre = sum(settembre),
                                       
                                          ottobre = sum(ottobre),
                                       
                                          novembre = sum(novembre),
                                       
                                          dicembre = sum(dicembre)) , by = id]


rbind(dt_ricavi_list, dt_ricavi_list_tot)


## Export -----------------------------------------


write.xlsx(dt_ricavi_list, file = file.path('processed', 'ricavi_tab_budget_eco.xlsx'))





# ALTRI RICAVI ---------------------------------

kc_bdgeco_altri_ricavi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_altri_ricavi_id = c('ter_cod_adj', kc_bdgeco_altri_ricavi)




altri_ricavi = function(data = dt_budget_2022) {
    
    dt_budget_2022_altri_ricavi = data[cdc_raggruppamenti_adj == "Ricavi / Costi indiretti" 
                                       
                                       & tipo_voce == 'Ricavi' & con_unlg_liv_2_adj == 'Altri ricavi' , ..kc_bdgeco_altri_ricavi_id]
    
    if(nrow(dt_budget_2022_altri_ricavi) == 0) {
        
        dt_budget_2022_altri_ricavi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_ricavi_id)
        
    } else {
        
        dt_budget_2022_altri_ricavi = melt(dt_budget_2022_altri_ricavi, id.vars = 'ter_cod_adj', measure.vars = kc_bdgeco_altri_ricavi, variable.name = 'date', 'altri_ricavi')
        dt_budget_2022_altri_ricavi = dt_budget_2022_altri_ricavi[, .(budget_2022 = sum(altri_ricavi, na.rm = TRUE)), by = c('ter_cod_adj', 'date')]
        
        dt_budget_2022_altri_ricavi = dcast(dt_budget_2022_altri_ricavi, ter_cod_adj ~ date, value.var = 'budget_2022')
        
    }
    
    
    return(dt_budget_2022_altri_ricavi)
    
}


dt_altri_ricavi <- altri_ricavi()


dt_altri_ricavi_tot <- dt_altri_ricavi[, .
                                     
                                     
                                     (   ter_cod_adj = "total",
                                         
                                         gennaio = sum(gennaio), 
                                         
                                         febbraio = sum(febbraio),
                                         
                                         marzo = sum(marzo),
                                         
                                         aprile = sum (aprile),
                                         
                                         maggio = sum(maggio),
                                         
                                         giugno = sum(giugno),
                                         
                                         luglio = sum(luglio),
                                         
                                         agosto = sum(agosto),
                                         
                                         settembre = sum(settembre),
                                         
                                         ottobre = sum(ottobre),
                                         
                                         novembre = sum(novembre),
                                         
                                         dicembre = sum(dicembre))]


dt_altri_ricavi_tot[, id := "atri_ricavi"]

dt_altri_ricavi_tot <- setcolorder(dt_altri_ricavi_tot, c("id" , "ter_cod_adj" , "gennaio" , "febbraio", "marzo" , "aprile",
                                   
                                   "maggio", "giugno",  "luglio",   "agosto", "settembre", "ottobre", "novembre",   "dicembre"))


dt_ricavi_totali_full <- rbind(dt_ricavi_list_tot, dt_altri_ricavi_tot)


dt_ricavi_totali_percentuali_full <- dt_ricavi_totali_full[, .
                                       
                                       
                                       ( id = id,
                                           
                                           gennaio = gennaio/sum(gennaio), 
                                           
                                           febbraio = febbraio/sum(febbraio),
                                           
                                           marzo = marzo/sum(marzo),
                                           
                                           aprile = aprile/sum(aprile),
                                           
                                           maggio = maggio/sum(maggio),
                                           
                                           giugno = giugno/sum(giugno),
                                           
                                           luglio = luglio/sum(luglio),
                                           
                                           agosto = agosto/sum(agosto),
                                           
                                           settembre = settembre/sum(settembre),
                                           
                                           ottobre = ottobre/sum(ottobre),
                                           
                                           novembre = novembre/sum(novembre),
                                           
                                           dicembre = dicembre/sum(dicembre))]





# COSTI VARIABILI CALCULATION =========================================================================================



## Costi variabili by Line function:  -----------------------------------------

kc_bdgeco_costi_var = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_var_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_var)





costi_variabili_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_costi_variabili = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Variabile', ..kc_bdgeco_costi_var_id] 
    
    if(nrow(dt_budget_eco_current_costi_variabili) == 0) {
        
        dt_budget_eco_current_costi_variabili = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_var_id)
        
    } else {
        
        dt_budget_eco_current_costi_variabili = melt(dt_budget_eco_current_costi_variabili, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_bdgeco_costi_var, variable.name = 'date', 'costi_variabili')
        
        dt_budget_eco_current_costi_variabili = dt_budget_eco_current_costi_variabili[, .(budget_2022 = sum(costi_variabili, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_eco_current_costi_variabili = dcast(dt_budget_eco_current_costi_variabili, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    dt_budget_eco_current_costi_variabili[, id := line]
    
    return(dt_budget_eco_current_costi_variabili)
    
}



## Function Test --------------------------------------



costi_variabili_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), costi_variabili_line)


names(costi_variabili_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)


dt_costi_variabili_list <- rbindlist(costi_variabili_list)


dt_costi_variabili_list_tot <- dt_costi_variabili_list[, .
                                     
                                     
                                     (   ter_cod_adj = "total",
                                         
                                         gennaio = sum(gennaio), 
                                         
                                         febbraio = sum(febbraio),
                                         
                                         marzo = sum(marzo),
                                         
                                         aprile = sum (aprile),
                                         
                                         maggio = sum(maggio),
                                         
                                         giugno = sum(giugno),
                                         
                                         luglio = sum(luglio),
                                         
                                         agosto = sum(agosto),
                                         
                                         settembre = sum(settembre),
                                         
                                         ottobre = sum(ottobre),
                                         
                                         novembre = sum(novembre),
                                         
                                         dicembre = sum(dicembre)) , by = id]


dt_costi_variabili_list_primo_margine <- dt_costi_variabili_list_tot [, .
                                                           
                                                           
                                                           (   id = id,
                                                               
                                                               ter_cod_adj = "primio_margine",

                                                               gennaio = gennaio + dt_ricavi_list_tot$gennaio , 
                                                               
                                                               febbraio = febbraio + dt_ricavi_list_tot$febbraio,
                                                               
                                                               marzo = marzo + dt_ricavi_list_tot$marzo,
                                                               
                                                               aprile = aprile + dt_ricavi_list_tot$aprile,
                                                               
                                                               maggio = maggio + dt_ricavi_list_tot$maggio,
                                                               
                                                               giugno = giugno + dt_ricavi_list_tot$giugno,
                                                               
                                                               luglio = luglio + dt_ricavi_list_tot$luglio,
                                                               
                                                               agosto = agosto + dt_ricavi_list_tot$agosto,
                                                               
                                                               settembre = settembre + dt_ricavi_list_tot$settembre,
                                                               
                                                               ottobre = ottobre + dt_ricavi_list_tot$ottobre,
                                                               
                                                               novembre = novembre + dt_ricavi_list_tot$novembre,
                                                               
                                                               dicembre = dicembre + dt_ricavi_list_tot$dicembre)]


dt_costi_variabili_list_primo_margine_percentuale <- dt_costi_variabili_list_primo_margine[, .
                                                           
                                                           
                                                           ( id = id,
                                                               
                                                               ter_cod_adj = "primio_margine_%",
                                                               
                                                               gennaio = gennaio/dt_ricavi_list_tot$gennaio , 
                                                               
                                                               febbraio = febbraio/dt_ricavi_list_tot$febbraio ,
                                                               
                                                               marzo = marzo/dt_ricavi_list_tot$marzo ,
                                                               
                                                               aprile = aprile/dt_ricavi_list_tot$aprile ,
                                                               
                                                               maggio = maggio/dt_ricavi_list_tot$maggio ,
                                                               
                                                               giugno = giugno/dt_ricavi_list_tot$giugno ,
                                                               
                                                               luglio = luglio/dt_ricavi_list_tot$luglio ,
                                                               
                                                               agosto = agosto/dt_ricavi_list_tot$agosto ,
                                                               
                                                               settembre = settembre/dt_ricavi_list_tot$settembre ,
                                                               
                                                               ottobre = ottobre/dt_ricavi_list_tot$ottobre ,
                                                               
                                                               novembre = novembre/dt_ricavi_list_tot$novembre ,
                                                               
                                                               dicembre = dicembre/dt_ricavi_list_tot$dicembre)]



## Export -----------------------------------------



write.xlsx(dt_costi_variabili_list, file = file.path('processed', 'costi_variabili_tab_budget_eco.xlsx'))











# COSTI FISSI CALCULATION =========================================================================================



## Costi fissi by Line function:  -----------------------------------------

kc_bdgeco_costi_fissi = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_fissi_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_fissi)





costi_fissi_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_costi_fissi = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Fisso', ..kc_bdgeco_costi_fissi_id] 
    
    if(nrow(dt_budget_eco_current_costi_fissi) == 0) {
        
        dt_budget_eco_current_costi_fissi = setNames(data.table(matrix(nrow = 0, ncol = 13)), kc_bdgeco_costi_fissi_id)
        
    } else {
        
        dt_budget_eco_current_costi_fissi = melt(dt_budget_eco_current_costi_fissi, id.vars = 'con_unlg_liv_2_adj', measure.vars = kc_bdgeco_costi_fissi, variable.name = 'date', 'costi_fissi')
        
        dt_budget_eco_current_costi_fissi = dt_budget_eco_current_costi_fissi[, .(budget_2022 = sum(costi_fissi, na.rm = TRUE)), by = c('con_unlg_liv_2_adj', 'date')]
        
        dt_budget_eco_current_costi_fissi = dcast(dt_budget_eco_current_costi_fissi, con_unlg_liv_2_adj ~ date, value.var = 'budget_2022')
        
    }
    
    dt_budget_eco_current_costi_fissi[, id := line]
    
    return(dt_budget_eco_current_costi_fissi)
    
}



## Function Test --------------------------------------



costi_fissi_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), costi_fissi_line)


names(costi_fissi_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)



dt_costi_fissi_list <- rbindlist(costi_fissi_list)


dt_costi_fissi_list_tot <- dt_costi_fissi_list[, .
                                                       
                                                       
                                                       (   ter_cod_adj = "total",
                                                           
                                                           gennaio = sum(gennaio), 
                                                           
                                                           febbraio = sum(febbraio),
                                                           
                                                           marzo = sum(marzo),
                                                           
                                                           aprile = sum (aprile),
                                                           
                                                           maggio = sum(maggio),
                                                           
                                                           giugno = sum(giugno),
                                                           
                                                           luglio = sum(luglio),
                                                           
                                                           agosto = sum(agosto),
                                                           
                                                           settembre = sum(settembre),
                                                           
                                                           ottobre = sum(ottobre),
                                                           
                                                           novembre = sum(novembre),
                                                           
                                                           dicembre = sum(dicembre)) , by = id]


dt_costi_fissi_list_secondo_margine <- dt_costi_fissi_list_tot [, .
                                                                
                                                                
                                                                (   id = id,
                                                                    
                                                                    ter_cod_adj = "secondo_margine",
                                                                    
                                                                    gennaio = gennaio + dt_costi_variabili_list_primo_margine$gennaio , 
                                                                    
                                                                    febbraio = febbraio + dt_costi_variabili_list_primo_margine$febbraio,
                                                                    
                                                                    marzo = marzo + dt_costi_variabili_list_primo_margine$marzo,
                                                                    
                                                                    aprile = aprile + dt_costi_variabili_list_primo_margine$aprile,
                                                                    
                                                                    maggio = maggio + dt_costi_variabili_list_primo_margine$maggio,
                                                                    
                                                                    giugno = giugno + dt_costi_variabili_list_primo_margine$giugno,
                                                                    
                                                                    luglio = luglio + dt_costi_variabili_list_primo_margine$luglio,
                                                                    
                                                                    agosto = agosto + dt_costi_variabili_list_primo_margine$agosto,
                                                                    
                                                                    settembre = settembre + dt_costi_variabili_list_primo_margine$settembre,
                                                                    
                                                                    ottobre = ottobre + dt_costi_variabili_list_primo_margine$ottobre,
                                                                    
                                                                    novembre = novembre + dt_costi_variabili_list_primo_margine$novembre,
                                                                    
                                                                    dicembre = dicembre + dt_costi_variabili_list_primo_margine$dicembre)]





dt_costi_fissi_list_secondo_margine_percentuale <- dt_costi_fissi_list_secondo_margine[, .
                                                                                           
                                                                                           
                                                                                           ( id = id,
                                                                                               
                                                                                               ter_cod_adj = "secondo_margine_%",
                                                                                               
                                                                                               gennaio = gennaio/dt_ricavi_list_tot$gennaio , 
                                                                                               
                                                                                               febbraio = febbraio/dt_ricavi_list_tot$febbraio ,
                                                                                               
                                                                                               marzo = marzo/dt_ricavi_list_tot$marzo ,
                                                                                               
                                                                                               aprile = aprile/dt_ricavi_list_tot$aprile ,
                                                                                               
                                                                                               maggio = maggio/dt_ricavi_list_tot$maggio ,
                                                                                               
                                                                                               giugno = giugno/dt_ricavi_list_tot$giugno ,
                                                                                               
                                                                                               luglio = luglio/dt_ricavi_list_tot$luglio ,
                                                                                               
                                                                                               agosto = agosto/dt_ricavi_list_tot$agosto ,
                                                                                               
                                                                                               settembre = settembre/dt_ricavi_list_tot$settembre ,
                                                                                               
                                                                                               ottobre = ottobre/dt_ricavi_list_tot$ottobre ,
                                                                                               
                                                                                               novembre = novembre/dt_ricavi_list_tot$novembre ,
                                                                                               
                                                                                               dicembre = dicembre/dt_ricavi_list_tot$dicembre)]



## Export -----------------------------------------




write.xlsx(dt_costi_fissi_list, file = file.path('processed', 'costi_fissi_tab_budget_eco.xlsx'))








# COSTI INDIRETTI CALCULATION =========================================================================================





kc_bdgeco_costi_indir = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")

kc_bdgeco_costi_indir_id = c('con_unlg_liv_2_adj', kc_bdgeco_costi_indir)





costi_indir_line = function(line, data = dt_budget_eco_current) {
    
    dt_budget_eco_current_costi_indir = data[cdc_raggruppamenti_adj == line & tipo_voce == 'Ricavi / Costi indiretti' , ..kc_bdgeco_costi_indir_id] 
    
    if(nrow(dt_budget_eco_current_costi_indir) == 0) {
    
    dt_budget_eco_current_costi_indir[, id := line]
    
    return(dt_budget_eco_current_costi_indir)
    
    altri_costi_fissi = altri_costi_fissi*trasporto,
    
    altri_costi_di_funzionamento = altri_costi_di_funzionamento*trasporto,      
    
    assicurazioni = assicurazioni*trasporto,                    
    
    consulenze = consulenze*trasporto,                        
    
    costi_per_godimento_beni_di_terzi = costi_per_godimento_beni_di_terzi*trasporto, 
    
    costi_pubblicitari = costi_pubblicitari*trasporto,                
    
    manutenzioni = manutenzioni*trasporto,                     
    
    materiali = materiali*trasporto,                         
    
    oneri_diversi_di_gestione = oneri_diversi_di_gestione*trasporto,         
    
    personale = personale*trasporto,                         
    
    utenze_pulizie_e_vigilanza = utenze_pulizie_e_vigilanza*trasporto)]





dt_budget_2022_costi_indir_groupage <- dcast(melt(dt_budget_2022_costi_indir_groupage, 
                                                  
                                                  id.vars = "variable"), variable ~ variable)



costi_indir_list = lapply(unique(dt_budget_eco_current$cdc_raggruppamenti_adj), costi_indir_line)


names(costi_indir_list) = unique(dt_budget_eco_current$cdc_raggruppamenti_adj)
