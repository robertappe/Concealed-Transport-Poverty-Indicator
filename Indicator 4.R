library("tidyverse")

# Loading the base:
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
 
#library(surveytable)
#set_survey(desenho_amostral)
# set_count_int()
# options(surveytable.check_present = FALSE)
# total()

# Total number of families with expenses
desenho_amostral$variables$Total = 1
svytotal(~ Total, desenho_amostral)

# Median expenditure for all households
mediana_total=svyquantile(~base$gasto_total, desenho_amostral, quantiles = 0.5)

base_mediana = base %>% 
  filter(gasto_total < mediana_total[["base$gasto_total"]][1])

base_mediana = merge(base_mediana, post_stratification_df)

desenho_amostralmed <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_mediana,
    nest = TRUE
  )

desenho_amostralmed$variables$Total = 1
svytotal(~ Total, desenho_amostralmed)
svymean(~gasto_total, desenho_amostralmed)


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
desenho_amostral1$variables$Total = 1
tot_fam1=svytotal(~Total, desenho_amostral1)

mediana1=svyquantile(~base_d1$gasto_total, desenho_amostral1, quantiles = 0.5)

base_d1 = base_d1 %>%  
  filter(gasto_total < mediana1[["base_d1$gasto_total"]][1])

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
tot_fam_med1=svytotal(~Total, desenho_amostral1)

gastomedio_fam_med1=svymean(~gasto_total, desenho_amostral1)

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
desenho_amostral2$variables$Total = 1
tot_fam2=svytotal(~Total, desenho_amostral2)

mediana2=svyquantile(~base_d2$gasto_total, desenho_amostral2, quantiles = 0.5)

base_d2 = base_d2 %>%  
  filter(gasto_total < mediana2[["base_d2$gasto_total"]][1])

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
tot_fam_med2=svytotal(~Total, desenho_amostral2)

gastomedio_fam_med2=svymean(~gasto_total, desenho_amostral2)

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
desenho_amostral3$variables$Total = 1
tot_fam3=svytotal(~Total, desenho_amostral3)

mediana3=svyquantile(~base_d3$gasto_total, desenho_amostral3, quantiles = 0.5)

base_d3 = base_d3 %>%  
  filter(gasto_total < mediana3[["base_d3$gasto_total"]][1])

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
tot_fam_med3=svytotal(~Total, desenho_amostral3)

gastomedio_fam_med3=svymean(~gasto_total, desenho_amostral3)

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
desenho_amostral4$variables$Total = 1
tot_fam4=svytotal(~Total, desenho_amostral4)

mediana4=svyquantile(~base_d4$gasto_total, desenho_amostral4, quantiles = 0.5)

base_d4 = base_d4 %>%  
  filter(gasto_total < mediana4[["base_d4$gasto_total"]][1])

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
tot_fam_med4=svytotal(~Total, desenho_amostral4)

gastomedio_fam_med4=svymean(~gasto_total, desenho_amostral4)

# Leaving 5th decile

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

mediana5=svyquantile(~base_d5$gasto_total, desenho_amostral5, quantiles = 0.5)

base_d5 = base_d5 %>%  
  filter(gasto_total < mediana5[["base_d5$gasto_total"]][1])

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
tot_fam_med5=svytotal(~Total, desenho_amostral5)

gastomedio_fam_med5=svymean(~gasto_total, desenho_amostral5)

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
desenho_amostral6$variables$Total = 1
tot_fam6=svytotal(~Total, desenho_amostral6)

mediana6=svyquantile(~base_d6$gasto_total, desenho_amostral6, quantiles = 0.5)

base_d6 = base_d6 %>%  
  filter(gasto_total < mediana6[["base_d6$gasto_total"]][1])

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
tot_fam_med6=svytotal(~Total, desenho_amostral6)

gastomedio_fam_med6=svymean(~gasto_total, desenho_amostral6)

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
desenho_amostral7$variables$Total = 1
tot_fam7=svytotal(~Total, desenho_amostral7)

mediana7=svyquantile(~base_d7$gasto_total, desenho_amostral7, quantiles = 0.5)

base_d7 = base_d7 %>%  
  filter(gasto_total < mediana7[["base_d7$gasto_total"]][1])

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
tot_fam_med7=svytotal(~Total, desenho_amostral7)

gastomedio_fam_med7=svymean(~gasto_total, desenho_amostral7)


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
desenho_amostral8$variables$Total = 1
tot_fam8=svytotal(~Total, desenho_amostral8)

mediana8=svyquantile(~base_d8$gasto_total, desenho_amostral8, quantiles = 0.5)

base_d8 = base_d8 %>%  
  filter(gasto_total < mediana8[["base_d8$gasto_total"]][1])

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
tot_fam_med8=svytotal(~Total, desenho_amostral8)

gastomedio_fam_med8=svymean(~gasto_total, desenho_amostral8)


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
desenho_amostral9$variables$Total = 1
tot_fam9=svytotal(~Total, desenho_amostral9)

mediana9=svyquantile(~base_d9$gasto_total, desenho_amostral9, quantiles = 0.5)

base_d9 = base_d9 %>%  
  filter(gasto_total < mediana9[["base_d9$gasto_total"]][1])
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
tot_fam_med9=svytotal(~Total, desenho_amostral9)

gastomedio_fam_med9=svymean(~gasto_total, desenho_amostral9)

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
desenho_amostral10$variables$Total = 1
tot_fam10=svytotal(~Total, desenho_amostral10)

mediana10=svyquantile(~base_d10$gasto_total, desenho_amostral10, quantiles = 0.5)

base_d10 = base_d10 %>%  
  filter(gasto_total > mediana10[["base_d10$gasto_total"]][1])

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
tot_fam_med10=svytotal(~Total, desenho_amostral10)

gastomedio_fam_med10=svymean(~gasto_total, desenho_amostral10)

# family size in decile

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

# Decile median

mediana_decil=c(mediana1[["base_d1$gasto_total"]][1],
                mediana2[["base_d2$gasto_total"]][1],
                mediana3[["base_d3$gasto_total"]][1],
                mediana4[["base_d4$gasto_total"]][1],
                mediana5[["base_d5$gasto_total"]][1],
                mediana6[["base_d6$gasto_total"]][1],
                mediana7[["base_d7$gasto_total"]][1],
                mediana8[["base_d8$gasto_total"]][1],
                mediana9[["base_d9$gasto_total"]][1],
                mediana10[["base_d10$gasto_total"]][1])

# Number of families below the median

tot_fam_abaixo_decil=c(tot_fam_med1[1],
                       tot_fam_med2[1],
                       tot_fam_med3[1],
                       tot_fam_med4[1],
                       tot_fam_med5[1],
                       tot_fam_med6[1],
                       tot_fam_med7[1],
                       tot_fam_med8[1],
                       tot_fam_med9[1],
                       tot_fam_med10[1])
                       
# Average family transportation spending that is below the median

gasto_med_abaixo_decil=c(gastomedio_fam_med1[1],
                         gastomedio_fam_med2[1],
                         gastomedio_fam_med3[1],
                         gastomedio_fam_med4[1],
                         gastomedio_fam_med5[1],
                         gastomedio_fam_med6[1],
                         gastomedio_fam_med7[1],
                         gastomedio_fam_med8[1],
                         gastomedio_fam_med9[1],
                         gastomedio_fam_med10[1])

# Package to save in excel
#install.packages("openxlsx")
library(openxlsx)

# Create a new workbook
wb <- createWorkbook()
# Add multiple sheets with different results
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", tot_fam_decil)

addWorksheet(wb, "Resultado_2")
writeData(wb, "Resultado_2", mediana_decil)

addWorksheet(wb, "Resultado_3")
writeData(wb, "Resultado_3", tot_fam_abaixo_decil)

addWorksheet(wb, "Resultado_4")
writeData(wb, "Resultado_4", gasto_med_abaixo_decil)

saveWorkbook(wb, "resultados_analise5.xlsx", overwrite = TRUE)
