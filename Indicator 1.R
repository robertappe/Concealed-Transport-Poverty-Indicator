#Part that will lower the LIVING CONDITION base
CONDICOES_VIDA <- 
  read.fwf("CONDICOES_VIDA.txt" 
           , widths = c(2,4,1,9,2,1,2,1,6,5,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
                           "V6101", "V6102", "V6103", "V61041", "V61042",
                           "V61043", "V61044", "V61045", "V61046", 
                           "V61051", "V61052", "V61053", "V61054",
                           "V61055", "V61056", "V61057", "V61058",
                           "V61061", "V61062", "V61063", "V61064",
                           "V61065", "V61066", "V61067", "V61068",
                           "V61069", "V610610", "V610611", "V61071",
                           "V61072", "V61073", "V6108", "V6109",
                           "V6110", "V6111", "V6112", "V6113", "V6114",
                           "V6115", "V6116", "V6117", "V6118", "V6119",
                           "V6120", "V6121", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL")
           , dec="."
  )   

save(condicao, file = "Condicoes_Vida.RData")
condicao <- CONDICOES_VIDA
names(condicao) <- tolower( names(condicao))
save(post_stratification_df, file = "Estratos_peso.RData")

# Loading the base:
load(file = "Condicoes_Vida.RData")
load(file = "Estratos_peso.RData")
# Calculating % of housing condition:
names(condicao)

# V61058: How do you rate your family's housing conditions in relation to the
# public transportation service?

#1 – Good
#2 – Satisfactory
#3 – Bad
#4 – Not have

table(condicao$v61058, useNA = "always")

# Create Home ID ------------------------------------------------------
require(dplyr)

condicao <- condicao %>%
  dplyr::mutate(DomicilioID = paste0(cod_upa, num_dom, num_uc, sep = "")) 

condicao_vida <- condicao %>% 
  select(estrato_pof,uf,cod_upa,DomicilioID,v61058,peso,peso_final,renda_total)

### Building the sample design
library(survey)
options( survey.lonely.psu = "adjust" )

base = merge(condicao_vida, post_stratification_df)

# Leaving only the information from the first line of the domicile in the database:
#base_condicao_vida = dplyr::distinct(base,DomicilioID,.keep_all = TRUE) %>% 
# rename(condicao_transp_coletivo = v61058)

desenho_amostral <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base,
    nest = TRUE
  )

total_populacao <-aggregate( peso_final ~ pos_estrato , data = base , sum )

names( total_populacao ) <- c( 'pos_estrato' , 'Freq' )

desenho_condicao<-postStratify(desenho_amostral , ~ pos_estrato , total_populacao)

### Create income deciles
decis <- svyquantile(~base$renda_total, desenho_amostral, quantiles = seq(0.1, 1, by = 0.1))


base = base %>% 
  mutate(decil= case_when(
    renda_total >= 0 & renda_total <= 1232.61 ~ "1º Decil",
    renda_total > 1232.61 & renda_total <= 1697.72 ~ "2º Decil",
    renda_total > 1697.72  & renda_total <= 2227.73 ~ "3º Decil",
    renda_total > 2227.73  & renda_total <= 2706.64 ~ "4º Decil",
    renda_total > 2706.64  & renda_total <= 3323.68 ~ "5º Decil",
    renda_total > 3323.68  & renda_total <= 4098.24 ~ "6º Decil",
    renda_total > 4098.24  & renda_total <= 5239.78 ~ "7º Decil",
    renda_total > 5239.78  & renda_total <= 6944.56 ~ "8º Decil",
    renda_total > 6944.56  & renda_total <= 10941.71 ~ "9º Decil",
    renda_total > 10941.71 ~ "10º Decil", 
    T ~ NA))
base$decil<-as.factor(base$decil)
base$decil<-ordered(base$decil, levels=c("1º Decil","2º Decil","3º Decil","4º Decil","5º Decil",
                                                       "6º Decil","7º Decil","8º Decil","9º Decil","10º Decil"))


## Varying the number of families by income decile:

freq_decil<-svytable( ~ base$decil , desenho_amostral )

svytable( ~ base$v61058 , desenho_amostral )

#### Doing it without incorporating the drawing, it would be like this:
table(base$v61058, base$decil)

# To incorporate the drawing, I need to separate by answer:
# V61058: How do you evaluate your family's housing conditions in relation to the
# public transportation service?
#1 – Good
#2 – Satisfactory
#3 – Bad
#4 – Not have 

# Selecting GOOD response:

# leaving only 1 at the base:
base_bom <- subset(base, v61058==1)
# Redoing the drawing:
desenho_amostral2 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_bom,
    nest = TRUE
  )
total_populacao <-aggregate( peso_final ~ pos_estrato , data = base_bom , sum )
names( total_populacao ) <- c( 'pos_estrato' , 'Freq' )
desenho_bom<-postStratify(desenho_amostral2 , ~ pos_estrato , total_populacao)
# Calculating the amount of GOOD responses by decile:
freq_bom<-svytable( ~ base_bom$decil , desenho_bom )

# Selecting SATISFACTORY answer:

# leaving only 2 at the base:
base_sat <- subset(base, v61058==2)
# Redoing the drawing:
desenho_amostral3 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_sat,
    nest = TRUE
  )
total_populacao <-aggregate( peso_final ~ pos_estrato , data = base_sat , sum )
names( total_populacao ) <- c( 'pos_estrato' , 'Freq' )
desenho_sat<-postStratify(desenho_amostral3 , ~ pos_estrato , total_populacao)

# Calculating the amount of SAT responses by decile:
freq_sat<-svytable( ~ base_sat$decil , desenho_sat )

# Selecting BAD response:

# leaving only 3 at the base:
base_ruim <- subset(base, v61058==3)
# Redoing the drawing:
desenho_amostral4 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_ruim,
    nest = TRUE
  )
total_populacao <-aggregate( peso_final ~ pos_estrato , data = base_ruim , sum )
names( total_populacao ) <- c( 'pos_estrato' , 'Freq' )
desenho_ruim<-postStratify(desenho_amostral4 , ~ pos_estrato , total_populacao)

# Calculating the amount of BAD responses by decile:
freq_ruim<-svytable( ~ base_ruim$decil , desenho_ruim )

# Selecting answer DOES NOT HAVE:

# leaving at the base only 4:
base_naotem <- subset(base, v61058==4)

# Redoing the drawing:
desenho_amostral5 <- 
  svydesign(
    id = ~ cod_upa , 
    strata = ~ estrato_pof ,
    weights = ~ peso ,
    data = base_naotem,
    nest = TRUE
  )
total_populacao <-aggregate( peso_final ~ pos_estrato , data = base_naotem , sum )
names( total_populacao ) <- c( 'pos_estrato' , 'Freq' )
desenho_naotem<-postStratify(desenho_amostral5 , ~ pos_estrato , total_populacao)

# Calculating the number of NO responses per decile:
freq_naotem<-svytable( ~ base_naotem$decil , desenho_naotem )
# Package to save in excel
#install.packages("openxlsx")
library(openxlsx)

# Create a new workbook
wb <- createWorkbook()
# Add multiple sheets with different results
addWorksheet(wb, "Resultado_1")
writeData(wb, "Resultado_1", freq_decil)

addWorksheet(wb, "Resultado_2")
writeData(wb, "Resultado_2", freq_bom)

addWorksheet(wb, "Resultado_3")
writeData(wb, "Resultado_3", freq_sat)

addWorksheet(wb, "Resultado_4")
writeData(wb, "Resultado_4", freq_ruim)

addWorksheet(wb, "Resultado_5")
writeData(wb, "Resultado_5", freq_naotem) 

saveWorkbook(wb, "resultados_analise.xlsx", overwrite = TRUE)
