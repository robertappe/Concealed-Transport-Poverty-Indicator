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

# CÓDIGO DO PRODUTO: 2300801
# DESCRIÇÃO DO PRODUTO: TREM
# CÓDIGO DO PRODUTO: 2300701
# DESCRIÇÃO DO PRODUTO: METRO
# CÓDIGO DO PRODUTO: 2302301
# DESCRIÇÃO DO PRODUTO: BONDE, BONDINHO, ETC.
# CÓDIGO DO PRODUTO: 2302302
# DESCRIÇÃO DO PRODUTO: VLT
# CÓDIGO DO PRODUTO: 2300101
# DESCRIÇÃO DO PRODUTO: ONIBUS URBANO
# CÓDIGO DO PRODUTO: 2300201
# DESCRIÇÃO DO PRODUTO: ONIBUS INTERMUNICIPAL
# CÓDIGO DO PRODUTO: 2300301
# DESCRIÇÃO DO PRODUTO: ONIBUS INTERESTADUAL
# CÓDIGO DO PRODUTO: 2300401
# DESCRIÇÃO DO PRODUTO: TRANSPORTE ALTERNATIVO (VAN, KOMBI, LOTADA, ETC.)
# CÓDIGO DO PRODUTO: 2300402
# DESCRIÇÃO DO PRODUTO: VAN
# CÓDIGO DO PRODUTO: 2300403
# DESCRIÇÃO DO PRODUTO: KOMBI
# CÓDIGO DO PRODUTO: 2300403
# DESCRIÇÃO DO PRODUTO: TAXI
# CÓDIGO DO PRODUTO: 2300404
# DESCRIÇÃO DO PRODUTO: LOTADA
# CÓDIGO DO PRODUTO: 2300502
# DESCRIÇÃO DO PRODUTO: AUTOMOVEL DE ALUGUEL (TAXI)
# CÓDIGO DO PRODUTO: 2303101
# DESCRIÇÃO DO PRODUTO: UBER
# CÓDIGO DO PRODUTO: 2303102
# DESCRIÇÃO DO PRODUTO: CABIFY
# CÓDIGO DO PRODUTO: 2303201
# DESCRIÇÃO DO PRODUTO: TRANSPORTE ESCOLAR DIARIO
# CÓDIGO DO PRODUTO: 2300601
# DESCRIÇÃO DO PRODUTO: MOTO-TAXI
# CÓDIGO DO PRODUTO: 2300602
# DESCRIÇÃO DO PRODUTO: TAXI-MOTO
# CÓDIGO DO PRODUTO: 2300409
# DESCRIÇÃO DO PRODUTO: CARONA EM MOTO
# CÓDIGO DO PRODUTO: 2300901
# DESCRIÇÃO DO PRODUTO: TRANSPORTE AQUAVIARIO (BARCA, BALSA, NAVIO, ETC.)
# CÓDIGO DO PRODUTO: 2300902
# DESCRIÇÃO DO PRODUTO: BARCA
# CÓDIGO DO PRODUTO: 2300903
# DESCRIÇÃO DO PRODUTO: BARCO
# CÓDIGO DO PRODUTO: 2300904
# DESCRIÇÃO DO PRODUTO: BALSA
# CÓDIGO DO PRODUTO: 2300906
# DESCRIÇÃO DO PRODUTO: FERRY-BOAT
# CÓDIGO DO PRODUTO: 2300907
# DESCRIÇÃO DO PRODUTO: AEROBARCO
# CÓDIGO DO PRODUTO: 2300908
# DESCRIÇÃO DO PRODUTO: CATAMARA
# CÓDIGO DO PRODUTO: 2300911
# DESCRIÇÃO DO PRODUTO: CANOA MOTORIZADA
# 1.1.1.	Combined passenger transport (S)
# QUADRO: 23
# CÓDIGO DO PRODUTO: 2301101
# DESCRIÇÃO DO PRODUTO: INTEGRACAO TREM-METRO
# CÓDIGO DO PRODUTO: 2301201
# DESCRIÇÃO DO PRODUTO: INTEGRACAO TREM-ONIBUS
# CÓDIGO DO PRODUTO: 2301301
# DESCRIÇÃO DO PRODUTO: INTEGRACAO METRO-ONIBUS
# CÓDIGO DO PRODUTO: 2302801
# DESCRIÇÃO DO PRODUTO: INTEGRACAO BARCA-TREM
# CÓDIGO DO PRODUTO: 2302901
# DESCRIÇÃO DO PRODUTO: INTEGRACAO BARCA-ONIBUS
# CÓDIGO DO PRODUTO: 2303001
# DESCRIÇÃO DO PRODUTO: INTEGRACAO BARCA-METRO



base_gastos_transp <- despesa_ind %>%
  filter(v9001 %in% c(2300801,2300701,2302301,2302302,2300101,2300201,2300301,2300401,2300402,2300403,2300403,2300404,2300502,2303101,2303102,2303201,2300601,2300602,2300409,2300901,2300902,2300903,2300904,2300906,2300907,2300908,2300911,2301101,2301201,2301301,2302801,2302901,2303001)) %>% 
  mutate(gasto_transporte_pub = ifelse(is.na(v9011), 
                                    v8000_defla*fator_anualizacao, 
                                    v8000_defla*fator_anualizacao*v9011), 
         renda_anual = ifelse(is.na(deflator),
                              renda_total*12,
                              renda_total*12*deflator))


base_gastos_transp$DomicilioID=as.factor(base_gastos_transp$DomicilioID)

base_gastos_transp=base_gastos_transp %>% 
  group_by(DomicilioID) %>% 
  mutate(gasto_total = sum(gasto_transporte_pub)) %>% 
  ungroup()


# Deixando na base somente a informação da primeira linha do domicílio:
base =  dplyr::distinct(base_gastos_transp,DomicilioID,.keep_all = TRUE)
base <- base %>% 
  select(estrato_pof,uf,cod_upa,DomicilioID,gasto_total,renda_anual,peso,peso_final)

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

m1=svymean(~base_d1$renda_anual, desenho_amostral1)
mg1=svymean(~base_d1$gasto_total, desenho_amostral1)

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

m2=svymean(~base_d2$renda_anual, desenho_amostral2)
mg2=svymean(~base_d2$gasto_total, desenho_amostral2)

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

m3=svymean(~base_d3$renda_anual, desenho_amostral3)
mg3=svymean(~base_d3$gasto_total, desenho_amostral3)

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

m4=svymean(~base_d4$renda_anual, desenho_amostral4)
mg4=svymean(~base_d4$gasto_total, desenho_amostral4)

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

m5=svymean(~base_d5$renda_anual, desenho_amostral5)
mg5=svymean(~base_d5$gasto_total, desenho_amostral5)

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

m6=svymean(~base_d6$renda_anual, desenho_amostral6)
mg6=svymean(~base_d6$gasto_total, desenho_amostral6)

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

m7=svymean(~base_d7$renda_anual, desenho_amostral7)
mg7=svymean(~base_d7$gasto_total, desenho_amostral7)
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

m8=svymean(~base_d8$renda_anual, desenho_amostral8)
mg8=svymean(~base_d8$gasto_total, desenho_amostral8)

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

m9=svymean(~base_d9$renda_anual, desenho_amostral9)
mg9=svymean(~base_d9$gasto_total, desenho_amostral9)

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

m10=svymean(~base_d10$renda_anual, desenho_amostral10)
mg10=svymean(~base_d10$gasto_total, desenho_amostral10)


media_gasto=c(mg1[["base_d1$gasto_total"]][1],
              mg2[["base_d2$gasto_total"]][1],
              mg3[["base_d3$gasto_total"]][1],
              mg4[["base_d4$gasto_total"]][1],
              mg5[["base_d5$gasto_total"]][1],
              mg6[["base_d6$gasto_total"]][1],
              mg7[["base_d7$gasto_total"]][1],
              mg8[["base_d8$gasto_total"]][1],
              mg9[["base_d9$gasto_total"]][1],
              mg10[["base_d10$gasto_total"]][1])
media_gasto=as.data.frame(media_gasto)


# Pacote para salvar no excel
#install.packages("openxlsx")
library(openxlsx)

# Crie um novo workbook
wb <- createWorkbook()
# Adicione várias planilhas com diferentes resultados
addWorksheet(wb, "Resultado_3")
writeData(wb, "Resultado_3", media_gasto)

saveWorkbook(wb, "resultados_analise4.xlsx", overwrite = TRUE)
