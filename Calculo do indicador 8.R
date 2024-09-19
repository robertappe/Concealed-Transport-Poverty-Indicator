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

base_gastos_transp <- despesa_ind %>%
  filter(v9001 %in% c(2300801,2300701,2302301,2302302,2300101,2300201,2300301,
                      2300401,2300402,2300403,2300403,2300404,2300502,2303101,
                      2303102,2303201,2300601,2300602,2300409,2300901,2300902,
                      2300903,2300904,2300906,2300907,2300908,2300911,2301101,
                      2301201,2301301,2302801,2302901,2303001,2301401,2301501,
                      2301502,2301601,2301801)) %>% 
  mutate(gasto_transporte_pub = ifelse(is.na(v9011), 
                                    v8000_defla*fator_anualizacao, 
                                    v8000_defla*fator_anualizacao*v9011), 
         renda_anual = ifelse(is.na(deflator),
                              renda_total*12,
                              renda_total*12*deflator))


base_gastos_transp$DomicilioID=as.factor(base_gastos_transp$DomicilioID)

base_gastos_transp=base_gastos_transp %>% 
  group_by(DomicilioID) %>% 
  mutate(gasto_total = sum(gasto_transporte_pub, rm.na=T)) %>% 
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

mediana_total=svyquantile(~base$gasto_total, desenho_amostral, quantiles = 0.5)

# Base com a mediana do total dividido por 2: 

base_mediana = base %>% 
  filter(gasto_total < (mediana_total[["base$gasto_total"]][1]/2)) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_mediana = merge(base_mediana, post_stratification_df)

desenho_amostral <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_mediana,
    nest = TRUE
  )
# qtde de familia que recebem menos da metade dos gastos da mediana Brasil
desenho_amostral$variables$Total = 1
svytotal(~Total, desenho_amostral)

svymean(~base_mediana$valor_total, desenho_amostral)
svyquantile(~base_mediana$valor_total, desenho_amostral, quantiles=0.5)

svymean(~base_mediana$gasto_total, desenho_amostral)
svyquantile(~base_mediana$gasto_total, desenho_amostral, quantiles=0.5)

# Deixando na 1º decil

base_d1 = base %>% 
  filter((decil == "1º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d1 = merge(base_d1, post_stratification_df)

desenho_amostral1 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d1,
    nest = TRUE
  )

media_dif_d1=svymean(~base_d1$valor_total, desenho_amostral1)
mediana_dif_d1=svyquantile(~base_d1$valor_total, desenho_amostral1, quantiles = 0.5)

media_d1=svymean(~base_d1$gasto_total, desenho_amostral1)
mediana_d1=svyquantile(~base_d1$gasto_total, desenho_amostral1, quantiles = 0.5)

desenho_amostral1$variables$Total = 1
tot_fam1=svytotal(~Total, desenho_amostral1)

# Deixando na 2º decil

base_d2 = base %>% 
  filter((decil == "2º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d2 = merge(base_d2, post_stratification_df)

desenho_amostral2 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d2,
    nest = TRUE
  )

media_dif_d2=svymean(~base_d2$valor_total, desenho_amostral2)
mediana_dif_d2=svyquantile(~base_d2$valor_total, desenho_amostral2, quantiles = 0.5)

media_d2=svymean(~base_d2$gasto_total, desenho_amostral2)
mediana_d2=svyquantile(~base_d2$gasto_total, desenho_amostral2, quantiles = 0.5)

desenho_amostral2$variables$Total = 1
tot_fam2=svytotal(~Total, desenho_amostral2)

# Deixando na 3º decil

base_d3 = base %>% 
  filter((decil == "3º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d3 = merge(base_d3, post_stratification_df)

desenho_amostral3 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d3,
    nest = TRUE
  )

media_dif_d3=svymean(~base_d3$valor_total, desenho_amostral3)
mediana_dif_d3=svyquantile(~base_d3$valor_total, desenho_amostral3, quantiles = 0.5)

media_d3=svymean(~base_d3$gasto_total, desenho_amostral3)
mediana_d3=svyquantile(~base_d3$gasto_total, desenho_amostral3, quantiles = 0.5)

desenho_amostral3$variables$Total = 1
tot_fam3=svytotal(~Total, desenho_amostral3)

# Deixando na 4º decil

base_d4 = base %>% 
  filter((decil == "4º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d4 = merge(base_d4, post_stratification_df)

desenho_amostral4 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d4,
    nest = TRUE
  )

media_dif_d4=svymean(~base_d4$valor_total, desenho_amostral4)
mediana_dif_d4=svyquantile(~base_d4$valor_total, desenho_amostral4, quantiles = 0.5)

media_d4=svymean(~base_d4$gasto_total, desenho_amostral4)
mediana_d4=svyquantile(~base_d4$gasto_total, desenho_amostral4, quantiles = 0.5)

desenho_amostral4$variables$Total = 1
tot_fam4=svytotal(~Total, desenho_amostral4)

# Deixando na 5º decil

base_d5 = base %>% 
  filter((decil == "5º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d5 = merge(base_d5, post_stratification_df)

desenho_amostral5 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d5,
    nest = TRUE
  )

media_dif_d5=svymean(~base_d5$valor_total, desenho_amostral5)
mediana_dif_d5=svyquantile(~base_d5$valor_total, desenho_amostral5, quantiles = 0.5)

media_d5=svymean(~base_d5$gasto_total, desenho_amostral5)
mediana_d5=svyquantile(~base_d5$gasto_total, desenho_amostral5, quantiles = 0.5)

desenho_amostral5$variables$Total = 1
tot_fam5=svytotal(~Total, desenho_amostral5)

# Deixando na 6º decil

base_d6 = base %>% 
  filter((decil == "6º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d6 = merge(base_d6, post_stratification_df)

desenho_amostral6 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d6,
    nest = TRUE
  )

media_dif_d6=svymean(~base_d6$valor_total, desenho_amostral6)
mediana_dif_d6=svyquantile(~base_d6$valor_total, desenho_amostral6, quantiles = 0.5)

media_d6=svymean(~base_d6$gasto_total, desenho_amostral6)
mediana_d6=svyquantile(~base_d6$gasto_total, desenho_amostral6, quantiles = 0.5)

desenho_amostral6$variables$Total = 1
tot_fam6=svytotal(~Total, desenho_amostral6)

# Deixando na 7º decil

base_d7 = base %>% 
  filter((decil == "7º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d7 = merge(base_d7, post_stratification_df)

desenho_amostral7 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d7,
    nest = TRUE
  )

media_dif_d7=svymean(~base_d7$valor_total, desenho_amostral7)
mediana_dif_d7=svyquantile(~base_d7$valor_total, desenho_amostral7, quantiles = 0.5)

media_d7=svymean(~base_d7$gasto_total, desenho_amostral7)
mediana_d7=svyquantile(~base_d7$gasto_total, desenho_amostral7, quantiles = 0.5)

desenho_amostral7$variables$Total = 1
tot_fam7=svytotal(~Total, desenho_amostral7)

# Deixando na 8º decil

base_d8 = base %>% 
  filter((decil == "8º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d8 = merge(base_d8, post_stratification_df)

desenho_amostral8 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d8,
    nest = TRUE
  )

media_dif_d8=svymean(~base_d8$valor_total, desenho_amostral8)
mediana_dif_d8=svyquantile(~base_d8$valor_total, desenho_amostral8, quantiles = 0.5)

media_d8=svymean(~base_d8$gasto_total, desenho_amostral8)
mediana_d8=svyquantile(~base_d8$gasto_total, desenho_amostral8, quantiles = 0.5)

desenho_amostral8$variables$Total = 1
tot_fam8=svytotal(~Total, desenho_amostral8)

# Deixando na 9º decil

base_d9 = base %>% 
  filter((decil == "9º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d9 = merge(base_d9, post_stratification_df)

desenho_amostral9 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d9,
    nest = TRUE
  )

media_dif_d9=svymean(~base_d9$valor_total, desenho_amostral9)
mediana_dif_d9=svyquantile(~base_d9$valor_total, desenho_amostral9, quantiles = 0.5)

media_d9=svymean(~base_d9$gasto_total, desenho_amostral9)
mediana_d9=svyquantile(~base_d9$gasto_total, desenho_amostral9, quantiles = 0.5)

desenho_amostral9$variables$Total = 1
tot_fam9=svytotal(~Total, desenho_amostral9)

# Deixando na 10º decil

base_d10 = base %>% 
  filter((decil == "10º Decil") & (gasto_total < (mediana_total[["base$gasto_total"]][1]/2))) %>% 
  mutate(valor_total = (mediana_total[["base$gasto_total"]][1]/2) - gasto_total)

base_d10 = merge(base_d10, post_stratification_df)

desenho_amostral10 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d10,
    nest = TRUE
  )

media_dif_d10=svymean(~base_d10$valor_total, desenho_amostral10)
mediana_dif_d10=svyquantile(~base_d10$valor_total, desenho_amostral10, quantiles = 0.5)

media_d10=svymean(~base_d10$gasto_total, desenho_amostral10)
mediana_d10=svyquantile(~base_d10$gasto_total, desenho_amostral10, quantiles = 0.5)

desenho_amostral10$variables$Total = 1
tot_fam10=svytotal(~Total, desenho_amostral10)

# Media do decil

tot_decil=c(tot_fam1[1],
            tot_fam2[1],
            tot_fam3[1],
            tot_fam4[1],
            tot_fam5[1],
            tot_fam6[1],
            tot_fam7[1],
            tot_fam8[1],
            tot_fam9[1],
            tot_fam10[1])

# Media da diferença do decil

media_dif_decil=c(media_dif_d1[1],
              media_dif_d2[1],
              media_dif_d3[1],
              media_dif_d4[1],
              media_dif_d5[1],
              media_dif_d6[1],
              media_dif_d7[1],
              media_dif_d8[1],
              media_dif_d9[1],
              media_dif_d10[1])

# Mediana da diferença do decil

mediana_dif_decil=c(mediana_dif_d1[["base_d1$valor_total"]][1],
                mediana_dif_d2[["base_d2$valor_total"]][1],
                mediana_dif_d3[["base_d3$valor_total"]][1],
                mediana_dif_d4[["base_d4$valor_total"]][1],
                mediana_dif_d5[["base_d5$valor_total"]][1],
                mediana_dif_d6[["base_d6$valor_total"]][1],
                mediana_dif_d7[["base_d7$valor_total"]][1],
                mediana_dif_d8[["base_d8$valor_total"]][1],
                mediana_dif_d9[["base_d9$valor_total"]][1],
                mediana_dif_d10[["base_d10$valor_total"]][1])

# Media do decil

media_decil=c(media_d1[1],
                  media_d2[1],
                  media_d3[1],
                  media_d4[1],
                  media_d5[1],
                  media_d6[1],
                  media_d7[1],
                  media_d8[1],
                  media_d9[1],
                  media_d10[1])

# Mediana do decil

mediana_decil=c(mediana_d1[["base_d1$gasto_total"]][1],
                    mediana_d2[["base_d2$gasto_total"]][1],
                    mediana_d3[["base_d3$gasto_total"]][1],
                    mediana_d4[["base_d4$gasto_total"]][1],
                    mediana_d5[["base_d5$gasto_total"]][1],
                    mediana_d6[["base_d6$gasto_total"]][1],
                    mediana_d7[["base_d7$gasto_total"]][1],
                    mediana_d8[["base_d8$gasto_total"]][1],
                    mediana_d9[["base_d9$gasto_total"]][1],
                    mediana_d10[["base_d10$gasto_total"]][1])





# Pacote para salvar no excel
#install.packages("openxlsx")
library(openxlsx)

# Crie um novo workbook
wb <- createWorkbook()
# Adicione várias planilhas com diferentes resultados
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", tot_decil)
addWorksheet(wb, "Resultado_2")
writeData(wb, "Resultado_2", media_decil)
addWorksheet(wb, "Resultado_3")
writeData(wb, "Resultado_3", mediana_decil)
addWorksheet(wb, "Resultado_4")
writeData(wb, "Resultado_4", media_dif_decil)
addWorksheet(wb, "Resultado_5")
writeData(wb, "Resultado_5", mediana_dif_decil)

saveWorkbook(wb, "resultados_analise8.xlsx", overwrite = TRUE)
