library("tidyverse")

#Parte que vai baixar a base de DESPESA INDIVIDUAL:
#despesa_ind <- 
#  read.fwf("DESPESA_INDIVIDUAL.txt" 
#           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
#                        ,2,1,1,1,12,10,1,2,14,14,10,5)
#           , na.strings=c(" ")
#           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                           "COD_UPA", "NUM_DOM", "NUM_UC",
#                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
#                           "V9002", "V8000", "V9010", "V9011", "V9012",
#                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
#                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
#                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
#           , dec="."
#  )   
#names(despesa_ind) <- tolower(names(despesa_ind)) # colocando em caixa baixa os nomes das variaveis
#save(despesa_ind, file = "Despesa_Individual.RData") # salvando a base em formato R

# vamos pegar os pesos amostrais 

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

#despesa_ind <- despesa_ind %>%
#  dplyr::mutate(MoradorID = paste0(cod_upa, num_dom, num_uc,cod_informante, sep = ""))

# filtrando as famílias que possuem os seguintes gastos:
#CÓDIGO DO PRODUTO: 2301401
#DESCRIÇÃO DO PRODUTO: GASOLINA COMUM (COMBUSTIVEL DE VEICULO)
#CÓDIGO DO PRODUTO: 2301501
#DESCRIÇÃO DO PRODUTO: GASOLINA ADITIVADA (COMBUSTIVEL DE VEICULO)
#CÓDIGO DO PRODUTO: 2301502
#DESCRIÇÃO DO PRODUTO: GASOLINA ESPECIAL (COMBUSTIVEL DE VEICULO)
# CATEGORIA POF: Despesas Individuais
# DESCRIÇÃO CATEGORIAS: Transportes coletivos e próprios (não inclue compra do veículo)
# CÓDIGO DO PRODUTO: 2301601
# DESCRIÇÃO DO PRODUTO: ALCOOL (COMBUSTIVEL DE VEICULO)
# CÓDIGO DO PRODUTO: 2301801
# DESCRIÇÃO DO PRODUTO: GAS VEICULAR

base_gastos_comb <- despesa_ind %>%
  filter(v9001 %in% c(2301401,2301501,2301502,2301601,2301801)) %>% 
  mutate(gasto_combustivel = ifelse(is.na(v9011), 
                                    v8000_defla*fator_anualizacao, 
                                    v8000_defla*fator_anualizacao*v9011), 
         renda_anual = ifelse(is.na(deflator),
                              renda_total*12,
                              renda_total*12*deflator))


base_gastos_comb$DomicilioID=as.factor(base_gastos_comb$DomicilioID)

base_gastos_comb=base_gastos_comb %>% 
  group_by(DomicilioID) %>% 
  mutate(gasto_total = sum(gasto_combustivel)) %>% 
  ungroup()


# Deixando na base somente a informação da primeira linha do domicílio:
base =  dplyr::distinct(base_gastos_comb,DomicilioID,.keep_all = TRUE)
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

#svyby(~base$renda_anual, ~base$decil, desenho_amostral,svymean, na.rm=TRUE)

# calculo das medias sem a incorporação do desenho
media_por_decil <- base %>% 
  group_by(decil) %>% 
  summarise(media_renda = mean(renda_anual, na.rm=TRUE), 
            media_gasto = mean(gasto_total, na.rm=TRUE))

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

media_renda=c(m1[["base_d1$renda_anual"]][1],
                            m2[["base_d2$renda_anual"]][1],
                            m3[["base_d3$renda_anual"]][1],
                            m4[["base_d4$renda_anual"]][1],
                            m5[["base_d5$renda_anual"]][1],
                            m6[["base_d6$renda_anual"]][1],
                            m7[["base_d7$renda_anual"]][1],
                            m8[["base_d8$renda_anual"]][1],
                            m9[["base_d9$renda_anual"]][1],
                            m10[["base_d10$renda_anual"]][1])
media_renda=as.data.frame(media_renda)

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
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", media_renda)

addWorksheet(wb, "Resultado_2")
writeData(wb, "Resultado_2", media_gasto)

saveWorkbook(wb, "resultados_analise3.xlsx", overwrite = TRUE)
