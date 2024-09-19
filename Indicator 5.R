library("tidyverse")

# Loading a base:
load(file = "Despesa_Individual.RData")
load(file = "Estratos_peso.RData")

# Create Home ID ------------------------------------------------------
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


# Leaving only the information from the first line of the household in the database:
base =  dplyr::distinct(base_gastos_transp,DomicilioID,.keep_all = TRUE)
base <- base %>% 
  select(estrato_pof,uf,cod_upa,DomicilioID,gasto_total,renda_anual,peso,peso_final)

###  Constructing the general base sampling design:
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

mediana_total=svyquantile(~base$gasto_total, desenho_amostral, quantiles = seq(0.1, 1, by = 0.1))

base_mediana = base %>% 
  filter(gasto_total < mediana_total[["base$gasto_total"]][5]) %>% 
  mutate(valor_total = mediana_total[["base$gasto_total"]][5] - gasto_total)

base_mediana = merge(base_mediana, post_stratification_df)

desenho_amostral <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_mediana,
    nest = TRUE
  )

svymean(~base_mediana$valor_total, desenho_amostral)

total=svytotal(~base_mediana$valor_total, desenho_amostral)
write.csv2(total, file="total_dif.csv")

### Create income deciles

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
# Leaving 1st decile

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
mediana1=svyquantile(~base_d1$gasto_total, desenho_amostral1, quantiles = 0.5)

base_d1_tot = base_d1 %>%  
  filter(gasto_total < mediana1[["base_d1$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana1[["base_d1$gasto_total"]][1] - gasto_total)

base_d1_tot = merge(base_d1_tot, post_stratification_df)
desenho_amostral1 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d1_tot,
    nest = TRUE
  )
media_dif_d1=svymean(~base_d1_tot$valor_total, desenho_amostral1)

# Leaving 2nd decile
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

mediana2=svyquantile(~base_d2$gasto_total, desenho_amostral2, quantiles = 0.5)

base_d2_tot = base_d2 %>%  
  filter(gasto_total < mediana2[["base_d2$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana2[["base_d2$gasto_total"]][1] - gasto_total)

base_d2_tot = merge(base_d2_tot, post_stratification_df)
desenho_amostral2<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d2_tot,
    nest = TRUE
  )
media_dif_d2=svymean(~base_d2_tot$valor_total, desenho_amostral2)

# Leaving 3rd decile

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
mediana3=svyquantile(~base_d3$gasto_total, desenho_amostral3, quantiles = 0.5)

base_d3_tot = base_d3 %>%  
  filter(gasto_total < mediana3[["base_d3$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana3[["base_d3$gasto_total"]][1] - gasto_total)

base_d3_tot = merge(base_d3_tot, post_stratification_df)
desenho_amostral3<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d3_tot,
    nest = TRUE
  )
media_dif_d3=svymean(~base_d3_tot$valor_total, desenho_amostral3)

# Leaving 4th decile

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
mediana4=svyquantile(~base_d4$gasto_total, desenho_amostral4, quantiles = 0.5)

base_d4_tot = base_d4 %>%  
  filter(gasto_total < mediana4[["base_d4$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana4[["base_d4$gasto_total"]][1] - gasto_total)

base_d4_tot = merge(base_d4_tot, post_stratification_df)
desenho_amostral4<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d4_tot,
    nest = TRUE
  )
media_dif_d4=svymean(~base_d4_tot$valor_total, desenho_amostral4)

# Leaving in the 5th decile

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
mediana5=svyquantile(~base_d5$gasto_total, desenho_amostral5, quantiles = 0.5)

base_d5_tot = base_d5 %>%  
  filter(gasto_total < mediana5[["base_d5$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana5[["base_d5$gasto_total"]][1] - gasto_total)

base_d5_tot = merge(base_d5_tot, post_stratification_df)
desenho_amostral5<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d5_tot,
    nest = TRUE
  )
media_dif_d5=svymean(~base_d5_tot$valor_total, desenho_amostral5)

# Leaving 6th decile

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
mediana6=svyquantile(~base_d6$gasto_total, desenho_amostral6, quantiles = 0.5)

base_d6_tot = base_d6 %>%  
  filter(gasto_total < mediana6[["base_d6$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana6[["base_d6$gasto_total"]][1] - gasto_total)

base_d6_tot = merge(base_d6_tot, post_stratification_df)
desenho_amostral6<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d6_tot,
    nest = TRUE
  )
media_dif_d6=svymean(~base_d6_tot$valor_total, desenho_amostral6)

# Leaving 7th decile

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

mediana7=svyquantile(~base_d7$gasto_total, desenho_amostral7, quantiles = 0.5)

base_d7_tot = base_d7 %>%  
  filter(gasto_total < mediana7[["base_d7$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana7[["base_d7$gasto_total"]][1] - gasto_total)

base_d7_tot = merge(base_d7_tot, post_stratification_df)
desenho_amostral7<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d7_tot,
    nest = TRUE
  )
media_dif_d7=svymean(~base_d7_tot$valor_total, desenho_amostral7)

# Leaving 8th decile

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

mediana8=svyquantile(~base_d8$gasto_total, desenho_amostral8, quantiles = 0.5)

base_d8_tot = base_d8 %>%  
  filter(gasto_total < mediana8[["base_d8$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana8[["base_d8$gasto_total"]][1] - gasto_total)

base_d8_tot = merge(base_d8_tot, post_stratification_df)
desenho_amostral8<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d8_tot,
    nest = TRUE
  )
media_dif_d8=svymean(~base_d8_tot$valor_total, desenho_amostral8)


# Leaving 9th decile

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

mediana9=svyquantile(~base_d9$gasto_total, desenho_amostral9, quantiles = 0.5)

base_d9_tot = base_d9 %>%  
  filter(gasto_total < mediana9[["base_d9$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana9[["base_d9$gasto_total"]][1] - gasto_total)

base_d9_tot = merge(base_d9_tot, post_stratification_df)
desenho_amostral9<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d9_tot,
    nest = TRUE
  )
media_dif_d9=svymean(~base_d9_tot$valor_total, desenho_amostral9)

# Leaving 10th decile

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

mediana10=svyquantile(~base_d10$gasto_total, desenho_amostral10, quantiles = 0.5)
base_d10_tot = base_d10 %>%  
  filter(gasto_total < mediana10[["base_d10$gasto_total"]][1]) %>% 
  mutate(valor_total = mediana10[["base_d10$gasto_total"]][1] - gasto_total)

base_d10_tot = merge(base_d10_tot, post_stratification_df)
desenho_amostral10<- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_d10_tot,
    nest = TRUE
  )
media_dif_d10=svymean(~base_d10_tot$valor_total, desenho_amostral10)

# Decile average

media_decil=c(media_dif_d1[1],
              media_dif_d2[1],
              media_dif_d3[1],
              media_dif_d4[1],
              media_dif_d5[1],
              media_dif_d6[1],
              media_dif_d7[1],
              media_dif_d8[1],
              media_dif_d9[1],
              media_dif_d10[1])
# Package to save in excel
#install.packages("openxlsx")
library(openxlsx)

# Create a new workbook
wb <- createWorkbook()
# Add multiple sheets with different results
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", media_decil)
saveWorkbook(wb, "resultados_analise7.xlsx", overwrite = TRUE)
