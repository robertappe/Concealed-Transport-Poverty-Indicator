library("tidyverse")

# Carregando a base:
load(file = "Despesa_Individual.RData")
load(file = "Estratos_peso.RData")

# Criar ID Domicilio ------------------------------------------------------
require(dplyr)

despesa_ind$cod_upa=as.character(despesa_ind$cod_upa)
despesa_ind$num_dom=as.character(despesa_ind$num_dom)
despesa_ind$num_uc=as.character(despesa_ind$num_uc)

despesa_ind <- despesa_ind %>%
  dplyr::mutate(DomicilioID = paste0(cod_upa, num_dom, num_uc, sep = ""))

# Familias que NAO tem gasto com transporte

base_sem_gastos_transp <- despesa_ind %>%
  filter(!(v9001 %in% c(2300801,2300701,2302301,2302302,2300101,2300201,2300301,
                      2300401,2300402,2300403,2300403,2300404,2300502,2303101,
                      2303102,2303201,2300601,2300602,2300409,2300901,2300902,
                      2300903,2300904,2300906,2300907,2300908,2300911,2301101,
                      2301201,2301301,2302801,2302901,2303001,2301401,2301501,
                      2301502,2301601,2301801))) %>% 
  mutate(gasto_sem_transporte = ifelse(is.na(v9011), 
                                    v8000_defla*fator_anualizacao, 
                                    v8000_defla*fator_anualizacao*v9011), 
         renda_anual = ifelse(is.na(deflator),
                              renda_total*12,
                              renda_total*12*deflator))


base_sem_gastos_transp$DomicilioID=as.factor(base_sem_gastos_transp$DomicilioID)

# Deixando na base somente a informação da primeira linha do domicílio:
base =  dplyr::distinct(base_sem_gastos_transp,DomicilioID,.keep_all = TRUE)

###  Construindo o desenho amostral da base geral:
library(survey)
options(survey.lonely.psu = "adjust")
base = merge(base, post_stratification_df)

desenho_amostral <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base,
    nest = TRUE
  )
 
#library(surveytable)
#set_survey(desenho_amostral)
# set_count_int()
# options(surveytable.check_present = FALSE)
# total()

# Total de familias com gastos 
desenho_amostral$variables$Total = 1
svytotal(~ Total, desenho_amostral)

### Criar os decis de renda

decis <- svyquantile(~base$renda_anual, desenho_amostral, quantiles = seq(0.1, 1, by = 0.1))

base = base %>% 
  mutate(decil= case_when(
    renda_anual >= 0 & renda_anual <= decis[["base$renda_anual"]][1] ~ "1º Decil",
    renda_anual > decis[["base$renda_anual"]][1] & renda_anual <= decis[["base$renda_anual"]][2] ~ "2º Decil",
    renda_anual > decis[["base$renda_anual"]][2] & renda_anual <= decis[["base$renda_anual"]][3] ~ "3º Decil",
    renda_anual > decis[["base$renda_anual"]][3]  & renda_anual <= decis[["base$renda_anual"]][4] ~ "4º Decil",
    renda_anual > decis[["base$renda_anual"]][4]  & renda_anual <= decis[["base$renda_anual"]][5] ~ "5º Decil",
    renda_anual > decis[["base$renda_anual"]][5]  & renda_anual <= decis[["base$renda_anual"]][6] ~ "6º Decil",
    renda_anual > decis[["base$renda_anual"]][6]  & renda_anual <= decis[["base$renda_anual"]][7] ~ "7º Decil",
    renda_anual > decis[["base$renda_anual"]][7]  & renda_anual <= decis[["base$renda_anual"]][8] ~ "8º Decil",
    renda_anual > decis[["base$renda_anual"]][8]  & renda_anual <= decis[["base$renda_anual"]][9] ~ "9º Decil",
    renda_anual > decis[["base$renda_anual"]][9] ~ "10º Decil", 
    T ~ NA))

base$decil<-as.factor(base$decil)
base$decil<-ordered(base$decil, levels=c("1º Decil","2º Decil","3º Decil","4º Decil","5º Decil",
                                                       "6º Decil","7º Decil","8º Decil","9º Decil","10º Decil"))

# Deixando na 1º decil

base_d1 = base %>% 
  filter(decil == "1º Decil")

base_d1 = merge(base_d1, post_stratification_df)

desenho_amostral1 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d1,
    nest = TRUE
  )
desenho_amostral1$variables$Total = 1
tot_fam1=svytotal(~Total, desenho_amostral1)

# Deixando na 2º decil

base_d2 = base %>% 
  filter(decil == "2º Decil")

base_d2 = merge(base_d2, post_stratification_df)

desenho_amostral2 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d2,
    nest = TRUE
  )
desenho_amostral2$variables$Total = 1
tot_fam2=svytotal(~Total, desenho_amostral2)

# Deixando na 3º decil

base_d3 = base %>% 
  filter(decil == "3º Decil")

base_d3 = merge(base_d3, post_stratification_df)

desenho_amostral3 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d3,
    nest = TRUE
  )
desenho_amostral3$variables$Total = 1
tot_fam3=svytotal(~Total, desenho_amostral3)

# Deixando na 4º decil

base_d4 = base %>% 
  filter(decil == "4º Decil")

base_d4 = merge(base_d4, post_stratification_df)

desenho_amostral4 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d4,
    nest = TRUE
  )
desenho_amostral4$variables$Total = 1
tot_fam4=svytotal(~Total, desenho_amostral4)

# Deixando na 5º decil

base_d5 = base %>% 
  filter(decil == "5º Decil")

base_d5 = merge(base_d5, post_stratification_df)

desenho_amostral5 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d5,
    nest = TRUE
  )
desenho_amostral5$variables$Total = 1
tot_fam5=svytotal(~Total, desenho_amostral5)

# Deixando na 6º decil

base_d6 = base %>% 
  filter(decil == "6º Decil")

base_d6 = merge(base_d6, post_stratification_df)

desenho_amostral6 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d6,
    nest = TRUE
  )
desenho_amostral6$variables$Total = 1
tot_fam6=svytotal(~Total, desenho_amostral6)

# Deixando na 7º decil

base_d7 = base %>% 
  filter(decil == "7º Decil")

base_d7 = merge(base_d7, post_stratification_df)

desenho_amostral7 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d7,
    nest = TRUE
  )
desenho_amostral7$variables$Total = 1
tot_fam7=svytotal(~Total, desenho_amostral7)

# Deixando na 8º decil

base_d8 = base %>% 
  filter(decil == "8º Decil")

base_d8 = merge(base_d8, post_stratification_df)

desenho_amostral8 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d8,
    nest = TRUE
  )
desenho_amostral8$variables$Total = 1
tot_fam8=svytotal(~Total, desenho_amostral8)

# Deixando na 9º decil

base_d9 = base %>% 
  filter(decil == "9º Decil")

base_d9 = merge(base_d9, post_stratification_df)

desenho_amostral9 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d9,
    nest = TRUE
  )
desenho_amostral9$variables$Total = 1
tot_fam9=svytotal(~Total, desenho_amostral9)

# Deixando na 10º decil

base_d10 = base %>% 
  filter(decil == "10º Decil")

base_d10 = merge(base_d10, post_stratification_df)

desenho_amostral10 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d10,
    nest = TRUE
  )
desenho_amostral10$variables$Total = 1
tot_fam10=svytotal(~Total, desenho_amostral10)

# tamanho família no decil

tot_fam_decil=c(tot_fam1[1],
              tot_fam2[1],
              tot_fam3[1],
              tot_fam4[1],
              tot_fam5[1],
              tot_fam6[1],
              tot_fam7[1],
              tot_fam8[1],
              tot_fam9[1],
              tot_fam10[1])

# Pacote para salvar no excel
#install.packages("openxlsx")
library(openxlsx)

# Crie um novo workbook
wb <- createWorkbook()
# Adicione várias planilhas com diferentes resultados
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", tot_fam_decil)

saveWorkbook(wb, "resultados_analise6.xlsx", overwrite = TRUE)
