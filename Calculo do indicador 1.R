#Parte que vai baixar a base de CONDICAO DE VIDA
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

# Carregando a base:
load(file = "Condicoes_Vida.RData")
load(file = "Estratos_peso.RData")
# Calculando % de condicao de moradia:

names(condicao)
# V61058: Como avalia as condicoes de moradia da sua família em relacao ao 
# servico de transporte coletivo?

#1 – Bom
#2 – Satisfatório
#3 – Ruim
#4 – Não tem 

table(condicao$v61058, useNA = "always")

# Criar ID Domicilio ------------------------------------------------------
require(dplyr)

condicao <- condicao %>%
  dplyr::mutate(DomicilioID = paste0(cod_upa, num_dom, num_uc, sep = "")) 

condicao_vida <- condicao %>% 
  select(estrato_pof,uf,cod_upa,DomicilioID,v61058,peso,peso_final,renda_total)

###  Construindo o desenho amostral
library(survey)
options( survey.lonely.psu = "adjust" )

base = merge(condicao_vida, post_stratification_df)

# Deixando na base somente a informação da primeira linha do domicílio:
#base_condicao_vida =  dplyr::distinct(base,DomicilioID,.keep_all = TRUE) %>%  
#  rename(condicao_transp_coletivo = v61058)

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

### Criar os decis de renda
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


## Varificando a quantidade de famílias por decil de renda:
freq_decil<-svytable( ~ base$decil , desenho_amostral )

svytable( ~ base$v61058 , desenho_amostral )

#### Fazendo sem a incorporaccao do desenho, seria assim:
table(base$v61058, base$decil)

# Para incorporar o desenho, preciso separar por resposta:
# V61058: Como avalia as condicoes de moradia da sua família em relacao ao 
# servico de transporte coletivo?
#1 – Bom
#2 – Satisfatório
#3 – Ruim
#4 – Não tem 

# Selecionando resposta BOM:

# deixando na base somente 1:
base_bom <- subset(base, v61058==1)
# Refazendo o desenho:
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
# Calculando as quantidade de resposta BOM por decil:
freq_bom<-svytable( ~ base_bom$decil , desenho_bom )

# Selecionando resposta SATISFATORIO:

# deixando na base somente 2:
base_sat <- subset(base, v61058==2)
# Refazendo o desenho:
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
# Calculando as quantidade de resposta BOM por decil:
freq_sat<-svytable( ~ base_sat$decil , desenho_sat )

# Selecionando resposta RUIM:

# deixando na base somente 3:
base_ruim <- subset(base, v61058==3)
# Refazendo o desenho:
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
# Calculando as quantidade de resposta BOM por decil:
freq_ruim<-svytable( ~ base_ruim$decil , desenho_ruim )

# Selecionando resposta NAO TEM:

# deixando na base somente 4:
base_naotem <- subset(base, v61058==4)
# Refazendo o desenho:
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
# Calculando as quantidade de resposta BOM por decil:
freq_naotem<-svytable( ~ base_naotem$decil , desenho_naotem )
# Pacote para salvar no excel
#install.packages("openxlsx")
library(openxlsx)

# Crie um novo workbook
wb <- createWorkbook()
# Adicione várias planilhas com diferentes resultados
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
