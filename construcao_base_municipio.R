# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(reshape2)
library(corrplot)
library(grf)
library(scaler)
library(nlme)
library(lme4)
library(merTools)
library(lattice)
library(zoo)
library(readxl)
library(DescTools)

# Bases -------------------------------------------------------------------
#População por município proj tcu - http://tabnet.datasus.gov.br/cgi/tabcgi.exe?ibge/cnv/poptbr.def
est_pop <- read_delim("dados/bases_ministerio_saude/est_pop_municipio_2016_2017.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE)
names(est_pop) <- c("MUNICIPIO", "2016", "2017")
est_pop <- melt(est_pop)
names(est_pop) <- c("MUNICIPIO", "ANO", "POPULACAO")
est_pop$ANO <- as.numeric(as.character(est_pop$ANO))
est_pop$COD_MUNICIPIO <- substr(est_pop$MUNICIPIO, 0,6)
est_pop$MUNICIPIO <- substr(est_pop$MUNICIPIO, 8,100)


##População por faixa etária e sexo censo - 2010 #https://sidra.ibge.gov.br/tabela/200
faixa_etaria_sexo <- read_delim("dados/bases_ibge/faixa_etaria_municipio_censo_2010.csv", 
    ";", escape_double = FALSE, trim_ws = TRUE)
names(faixa_etaria_sexo) <- c("MUNICIPIO", "POP_FAIXA_ETARIA", "POP_MASC", "POP_FEM")
faixa_etaria_sexo$FAIXA_ETARIA <- sub("^[^;]*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$FAIXA_ETARIA <- substr(faixa_etaria_sexo$FAIXA_ETARIA, 4,100)
faixa_etaria_sexo$FAIXA_ETARIA <- substr(faixa_etaria_sexo$FAIXA_ETARIA, 0,nchar(faixa_etaria_sexo$FAIXA_ETARIA)-2)
faixa_etaria_sexo$MUNICIPIO <- sub("\\;.*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$MUNICIPIO <- substr(faixa_etaria_sexo$MUNICIPIO, 2,100) 
faixa_etaria_sexo$ESTADO <- sub(".*(?:\\((.*)\\)).*|.*", "\\1", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$MUNICIPIO <- sub(" *\\(.*", "", faixa_etaria_sexo$MUNICIPIO)
faixa_etaria_sexo$POP_FAIXA_ETARIA <- substr(faixa_etaria_sexo$POP_FAIXA_ETARIA, 3,100)
faixa_etaria_sexo$POP_FAIXA_ETARIA <- substr(faixa_etaria_sexo$POP_FAIXA_ETARIA, 0,nchar(faixa_etaria_sexo$POP_FAIXA_ETARIA)-2)
faixa_etaria_sexo$POP_MASC <- substr(faixa_etaria_sexo$POP_MASC, 3,100)
faixa_etaria_sexo$POP_MASC <- substr(faixa_etaria_sexo$POP_MASC, 0,nchar(faixa_etaria_sexo$POP_MASC)-2)
faixa_etaria_sexo$POP_FEM <- substr(faixa_etaria_sexo$POP_FEM, 3,100)
faixa_etaria_sexo$POP_FEM <- substr(faixa_etaria_sexo$POP_FEM, 0,nchar(faixa_etaria_sexo$POP_FEM)-6)
faixa_etaria_sexo[which(faixa_etaria_sexo$ESTADO == ""), colnames(faixa_etaria_sexo) == "ESTADO"] <- "BR"
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FAIXA_ETARIA == "..."),colnames(faixa_etaria_sexo) =="POP_FAIXA_ETARIA"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_MASC == "..."),colnames(faixa_etaria_sexo) =="POP_MASC"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FEM == "..."),colnames(faixa_etaria_sexo) =="POP_FEM"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FAIXA_ETARIA == "-"),colnames(faixa_etaria_sexo) =="POP_FAIXA_ETARIA"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_MASC == "-"),colnames(faixa_etaria_sexo) =="POP_MASC"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo[which(faixa_etaria_sexo$POP_FEM == "-"),colnames(faixa_etaria_sexo) =="POP_FEM"] <- 1 #inserido 1 para evitar divisáo por 0 no cálculo das taxas
faixa_etaria_sexo$POP_FAIXA_ETARIA <- as.numeric(as.character(faixa_etaria_sexo$POP_FAIXA_ETARIA))
faixa_etaria_sexo$POP_MASC <- as.numeric(as.character(faixa_etaria_sexo$POP_MASC))
faixa_etaria_sexo$POP_FEM <- as.numeric(as.character(faixa_etaria_sexo$POP_FEM))
faixa_etaria <- faixa_etaria_sexo[,colnames(faixa_etaria_sexo) %in% c("ESTADO", "MUNICIPIO", "FAIXA_ETARIA", "POP_FAIXA_ETARIA")]
sexo <- faixa_etaria_sexo[,colnames(faixa_etaria_sexo) %in% c("ESTADO", "MUNICIPIO", "POP_MASC", "POP_FEM", "FAIXA_ETARIA")]
faixa_etaria <- dcast(faixa_etaria,formula = ESTADO + MUNICIPIO ~ FAIXA_ETARIA, value.var = "POP_FAIXA_ETARIA", fun.aggregate = sum, na.rm = T)
faixa_etaria$Var.3 <- NULL
faixa_etaria$`Idade ignorada` <- NULL
faixa_etaria$`Grupo de idade` <- NULL


faixa_etaria$Total <- NULL
faixa_etaria <- faixa_etaria[complete.cases(faixa_etaria),]
faixa_etaria <- melt(faixa_etaria, id.vars = c("ESTADO", "MUNICIPIO"))
names(faixa_etaria) <- c("ESTADO", "MUNICIPIO", "FAIXA_ETARIA", "POP_FAIXA_ETARIA")

faixa_etaria_0a4 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("0 a 4 anos"))

faixa_etaria_5a14 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("5 a 9 anos", "10 a 14 anos"))	
faixa_etaria_5a14 <- faixa_etaria_5a14 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_5a14$FAIXA_ETARIA <- "5 a 14 anos"
faixa_etaria_5a14 <- as.data.frame(faixa_etaria_5a14)

faixa_etaria_15a24 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("15 a 19 anos", "20 a 24 anos"))	
faixa_etaria_15a24 <- faixa_etaria_15a24 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_15a24$FAIXA_ETARIA <- "15 a 24 anos"
faixa_etaria_15a24 <- as.data.frame(faixa_etaria_15a24)

faixa_etaria_25a34 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("25 a 29 anos", "30 a 34 anos"))	
faixa_etaria_25a34 <- faixa_etaria_25a34 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_25a34$FAIXA_ETARIA <- "25 a 34 anos"
faixa_etaria_25a34 <- as.data.frame(faixa_etaria_25a34)

faixa_etaria_35a44 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("35 a 39 anos", "40 a 44 anos"))	
faixa_etaria_35a44 <- faixa_etaria_35a44 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_35a44$FAIXA_ETARIA <- "35 a 44 anos"
faixa_etaria_35a44 <- as.data.frame(faixa_etaria_35a44)

faixa_etaria_45a54 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("45 a 49 anos", "50 a 54 anos"))	
faixa_etaria_45a54 <- faixa_etaria_45a54 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_45a54$FAIXA_ETARIA <- "45 a 54 anos"
faixa_etaria_45a54 <- as.data.frame(faixa_etaria_45a54)

faixa_etaria_55a64 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("55 a 59 anos", "60 a 64 anos"))	
faixa_etaria_55a64 <- faixa_etaria_55a64 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_55a64$FAIXA_ETARIA <- "55 a 64 anos"
faixa_etaria_55a64 <- as.data.frame(faixa_etaria_55a64)

faixa_etaria_65a74 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("65 a 69 anos", "70 a 74 anos"))	
faixa_etaria_65a74 <- faixa_etaria_65a74 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_65a74$FAIXA_ETARIA <- "65 a 74 anos"
faixa_etaria_65a74 <- as.data.frame(faixa_etaria_65a74)

faixa_etaria_mais75 <- subset(faixa_etaria, faixa_etaria$FAIXA_ETARIA %in% c("75 a 79 anos", "80 a 84 anos", "85 a 89 anos", 
   									     "90 a 94 anos", "95 a 99 anos", "100 anos ou mais"))	
faixa_etaria_mais75 <- faixa_etaria_mais75 %>%
	group_by(ESTADO, MUNICIPIO) %>%
	summarize(POP_FAIXA_ETARIA = sum(POP_FAIXA_ETARIA))
faixa_etaria_mais75$FAIXA_ETARIA <- "75 anos +"
faixa_etaria_mais75 <- as.data.frame(faixa_etaria_mais75)

faixa_etaria <- rbind(faixa_etaria_0a4, faixa_etaria_5a14, faixa_etaria_15a24,
		      faixa_etaria_25a34, faixa_etaria_35a44, faixa_etaria_45a54,
		      faixa_etaria_55a64, faixa_etaria_65a74, faixa_etaria_mais75) %>% as.data.frame()
 


sexo$PROP_MASC <- sexo$POP_MASC/(sexo$POP_MASC + sexo$POP_FEM)
sexo <- subset(sexo, sexo$FAIXA_ETARIA == "Total")
sexo$FAIXA_ETARIA <- NULL

faixa_etaria_sexo <- merge(faixa_etaria, sexo, by = c("ESTADO", "MUNICIPIO"))
faixa_etaria_sexo <- faixa_etaria_sexo[complete.cases(faixa_etaria_sexo),]
faixa_etaria_sexo <- unique(faixa_etaria_sexo)


#Base com código dos estados para fazer merge das faixas etárias com as demais bases, uma vez que há municípios com o mesmo nome
estados <- read_csv("dados/estados.csv")
faixa_etaria_sexo <- merge(estados, faixa_etaria_sexo, by.x = "SIGLA", by.y = "ESTADO", all = T)
faixa_etaria_sexo$POP_FEM <- NULL
faixa_etaria_sexo$POP_MASC <- NULL

##PIB por município 2010 a 2017 - https://www.ibge.gov.br/estatisticas/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?=&t=downloads
pib <- read_csv("dados/bases_ibge/pib_municipios_2010_2017.csv", 
    locale = locale())
pib <- pib[,c(7,8,1,39,40)]
pib <- subset(pib, pib$Ano %in% c("2016", "2017"))
names(pib) <- c("COD_MUNICIPIO","MUNICIPIO", "ANO", "PIB", "PIB_PER_CAPITA")
pib$COD_MUNICIPIO <- as.character(pib$COD_MUNICIPIO)
pib$COD_MUNICIPIO <- substr(pib$COD_MUNICIPIO, 0,6) #O código aqui tem um número a mais
pib <- as.data.frame(pib)

##SIM 2016-2017
cardio_neuro_2016 <- read_csv("dados/bases_ministerio_saude/cardio_neuro_municipio_2016.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(cardio_neuro_2016) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
cardio_neuro_2016 <- melt(cardio_neuro_2016)
names(cardio_neuro_2016) <- c("MUNICIPIO", "FAIXA_ETARIA", "CARDIO_NEURO")
cardio_neuro_2016$ANO <- 2016
cardio_neuro_2017 <- read_csv("dados/bases_ministerio_saude/cardio_neuro_municipio_2017.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(cardio_neuro_2017) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
cardio_neuro_2017 <- melt(cardio_neuro_2017)
names(cardio_neuro_2017) <- c("MUNICIPIO", "FAIXA_ETARIA", "CARDIO_NEURO")
cardio_neuro_2017$ANO <- 2017

cardio_neuro <- rbind(cardio_neuro_2016, cardio_neuro_2017) %>% as.data.frame()
cardio_neuro$COD_MUNICIPIO <- substr(cardio_neuro$MUNICIPIO, 0,6)
cardio_neuro$MUNICIPIO <- substr(cardio_neuro$MUNICIPIO, 8,100)
cardio_neuro$ANO <- as.numeric(as.character(cardio_neuro$ANO))


traumato_2016 <- read_csv("dados/bases_ministerio_saude/traumato_municipio_2016.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(traumato_2016) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
traumato_2016 <- melt(traumato_2016)
names(traumato_2016) <- c("MUNICIPIO", "FAIXA_ETARIA", "TRAUMATO")
traumato_2016$ANO <- 2016
traumato_2017 <- read_csv("dados/bases_ministerio_saude/traumato_municipio_2017.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(traumato_2017) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
traumato_2017 <- melt(traumato_2017)
names(traumato_2017) <- c("MUNICIPIO", "FAIXA_ETARIA", "TRAUMATO")
traumato_2017$ANO <- 2017

traumato <- rbind(traumato_2016, traumato_2017) %>% as.data.frame()
traumato$COD_MUNICIPIO <- substr(traumato$MUNICIPIO, 0,6)
traumato$MUNICIPIO <- substr(traumato$MUNICIPIO, 8,100)
traumato$ANO <- as.numeric(as.character(traumato$ANO))


neoplasia_2016 <- read_csv("dados/bases_ministerio_saude/neo_municipio_2016.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(neoplasia_2016) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
neoplasia_2016 <- melt(neoplasia_2016)
names(neoplasia_2016) <- c("MUNICIPIO", "FAIXA_ETARIA", "NEOPLASIA")
neoplasia_2016$ANO <- 2016
neoplasia_2017 <- read_csv("dados/bases_ministerio_saude/neo_municipio_2017.prn", 
    locale = locale(encoding = "WINDOWS-1252"))
names(neoplasia_2017) <- c("MUNICIPIO", "0 a 1 anos", "1 a 4 anos", "5 a 14 anos", "15 a 24 anos", 
			      "25 a 34 anos", "35 a 44 anos", "45 a 54 anos", "55 a 64 anos", 
			      "65 a 74 anos", "75 anos +","Ign", "Total")
neoplasia_2017 <- melt(neoplasia_2017)
names(neoplasia_2017) <- c("MUNICIPIO", "FAIXA_ETARIA", "NEOPLASIA")
neoplasia_2017$ANO <- 2017

neoplasia <- rbind(neoplasia_2016, neoplasia_2017) %>% as.data.frame()
neoplasia$COD_MUNICIPIO <- substr(neoplasia$MUNICIPIO, 0,6)
neoplasia$MUNICIPIO <- substr(neoplasia$MUNICIPIO, 8,100)
neoplasia$ANO <- as.numeric(as.character(neoplasia$ANO))


obito <- merge(cardio_neuro,traumato, by = c("COD_MUNICIPIO","MUNICIPIO", "ANO", "FAIXA_ETARIA"))
obito <- merge(obito,neoplasia, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO", "FAIXA_ETARIA"))
obito <- merge(obito,est_pop, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"))

obito <- obito[complete.cases(obito$ANO),]
obito <- obito[which(obito$COD_MUNICIPIO != "Total"),]

obito$COD_ESTADO <- substr(obito$COD_MUNICIPIO, 0, 2) 
obito$CARDIO_NEURO_TRAUMATO <- obito$CARDIO_NEURO + obito$TRAUMATO

obito_0a4 <- subset(obito, obito$FAIXA_ETARIA %in% c("0 a 1 anos", "1 a 4 anos"))	
obito_0a4 <- obito_0a4 %>%
	group_by(COD_ESTADO, COD_MUNICIPIO, MUNICIPIO, FAIXA_ETARIA, ANO, POPULACAO) %>%
	summarize(CARDIO_NEURO = sum(CARDIO_NEURO),
		  TRAUMATO = sum(TRAUMATO),
		  NEOPLASIA = sum(NEOPLASIA),
		  CARDIO_NEURO_TRAUMATO = sum(CARDIO_NEURO_TRAUMATO))
obito_0a4$FAIXA_ETARIA <- "0 a 4 anos"
obito_0a4 <- as.data.frame(obito_0a4)

obito <- rbind(obito, obito_0a4) %>% as.data.frame()
obito <- subset(obito, !(obito$FAIXA_ETARIA %in% c("0 a 1 anos", "1 a 4 anos", "Total", "Ign")))


obito <- merge(obito, faixa_etaria_sexo, by = c("COD_ESTADO", "MUNICIPIO", "FAIXA_ETARIA"), all = T)
obito <- obito[complete.cases(obito),]

##Taxas de mortalidade


pop_brasil <- subset(faixa_etaria, faixa_etaria$MUNICIPIO == "Brasil")
pop_brasil <- pop_brasil[,c(3,4)]
names(pop_brasil) <- c("FAIXA_ETARIA", "POP_FAIXA_ETARIA_BR")
pop_brasil$POP_TOT_BRASIL <- sum(pop_brasil$POP_FAIXA_ETARIA_BR)
obito <- merge(obito, pop_brasil, by = "FAIXA_ETARIA")


###TX de óbitos padronizados
obito$TX_CARDIO_NEURO_IDADE <- obito$CARDIO_NEURO/obito$POP_FAIXA_ETARIA 
obito$OBIT_ESP_CARDIO_NEURO_IDADE <- obito$TX_CARDIO_NEURO_IDADE * obito$POP_FAIXA_ETARIA_BR 
obito$TX_TRAUMATO_IDADE <- obito$TRAUMATO/obito$POP_FAIXA_ETARIA 
obito$OBIT_ESP_TRAUMATO_IDADE <- obito$TX_TRAUMATO_IDADE * obito$POP_FAIXA_ETARIA_BR
obito$TX_CARDIO_NEURO_TRAUMATO_IDADE <- obito$CARDIO_NEURO_TRAUMATO/obito$POP_FAIXA_ETARIA 
obito$OBIT_ESP_CARDIO_NEURO_TRAUMATO_IDADE <- obito$TX_CARDIO_NEURO_TRAUMATO_IDADE * obito$POP_FAIXA_ETARIA_BR  
obito$TX_NEOPLASIA_IDADE <- obito$NEOPLASIA/obito$POP_FAIXA_ETARIA 
obito$OBIT_ESP_NEOPLASIA_IDADE <- obito$TX_NEOPLASIA_IDADE * obito$POP_FAIXA_ETARIA_BR  


obito <- obito %>%
	group_by(COD_MUNICIPIO, MUNICIPIO, ANO, POPULACAO, 
		 PROP_MASC, POP_TOT_BRASIL) %>%
	summarize(CARDIO_NEURO = sum(CARDIO_NEURO, na.rm = T),
		  TRAUMATO = sum(TRAUMATO, na.rm = T),
		  CARDIO_NEURO_TRAUMATO = sum(CARDIO_NEURO_TRAUMATO, na.rm = T),
		  NEOPLASIA = sum(NEOPLASIA, na.rm = T),
		  OBIT_ESP_CARDIO_NEURO = sum(OBIT_ESP_CARDIO_NEURO_IDADE, na.rm = T), 
		  OBIT_ESP_TRAUMATO = sum(OBIT_ESP_TRAUMATO_IDADE, na.rm = T), 
		  OBIT_ESP_CARDIO_NEURO_TRAUMATO = sum(OBIT_ESP_CARDIO_NEURO_TRAUMATO_IDADE, na.rm = T), 
		  OBIT_ESP_NEOPLASIA = sum(OBIT_ESP_NEOPLASIA_IDADE, na.rm = T))

obito$TX_CARDIO_NEURO_PADRO <- obito$OBIT_ESP_CARDIO_NEURO/obito$POP_TOT_BRASIL * 100000
obito$TX_TRAUMATO_PADRO <- obito$OBIT_ESP_TRAUMATO/obito$POP_TOT_BRASIL * 100000
obito$TX_CARDIO_NEURO_TRAUMATO_PADRO <- obito$OBIT_ESP_CARDIO_NEURO_TRAUMATO/obito$POP_TOT_BRASIL * 100000
obito$TX_NEOPLASIA_PADRO <- obito$OBIT_ESP_NEOPLASIA/obito$POP_TOT_BRASIL * 100000

obito$TX_CARDIO_NEURO_BRUTA <- obito$CARDIO_NEURO/obito$POPULACAO * 100000
obito$TX_TRAUMATO_BRUTA <- obito$TRAUMATO/obito$POPULACAO * 100000
obito$TX_CARDIO_NEURO_TRAUMATO_BRUTA <- obito$CARDIO_NEURO_TRAUMATO/obito$POPULACAO * 100000
obito$TX_NEOPLASIA_BRUTA <- obito$NEOPLASIA/obito$POPULACAO * 100000

##Produção 2016-2017
cons_upa <- read_csv("dados/bases_ministerio_saude/consultas_upas_municipios_2016_2017.csv", 
    locale = locale(encoding = "WINDOWS-1252"))
cons_upa <- melt(cons_upa)
names(cons_upa) <- c("MUNICIPIO", "ANO", "CONS_UPA")
cons_upa$COD_MUNICIPIO <- substr(cons_upa$MUNICIPIO, 0,6)
cons_upa$MUNICIPIO <- substr(cons_upa$MUNICIPIO, 8,100)
cons_upa$ANO <- as.numeric(as.character(cons_upa$ANO))

cons_aten_primaria <- read_csv("dados/bases_ministerio_saude/consultas_atencao_primaria_municipios_2016_2017.csv", 
    locale = locale(encoding = "WINDOWS-1252"))
cons_aten_primaria <- melt(cons_aten_primaria)
names(cons_aten_primaria) <- c("MUNICIPIO", "ANO", "CONS_ATENC_PRIMARIA")
cons_aten_primaria$COD_MUNICIPIO <- substr(cons_aten_primaria$MUNICIPIO, 0,6)
cons_aten_primaria$MUNICIPIO <- substr(cons_aten_primaria$MUNICIPIO, 8,100)
cons_aten_primaria$ANO <- as.numeric(as.character(cons_aten_primaria$ANO))

cons_outr_emerg <- read_csv("dados/bases_ministerio_saude/consultas_outras_emergencias_municipios_2016_2017.csv", 
    locale = locale(encoding = "WINDOWS-1252"))
cons_outr_emerg <- melt(cons_outr_emerg)
names(cons_outr_emerg) <- c("MUNICIPIO", "ANO", "CONS_OUTR_EMERG")
cons_outr_emerg$COD_MUNICIPIO <- substr(cons_outr_emerg$MUNICIPIO, 0,6)
cons_outr_emerg$MUNICIPIO <- substr(cons_outr_emerg$MUNICIPIO, 8,100)
cons_outr_emerg$ANO <- as.numeric(as.character(cons_outr_emerg$ANO))

#Beneficiários de Plano de Saúde - será utilizado junho de 2016 http://www.ans.gov.br/anstabnet/cgi-bin/tabnet?dados/tabnet_02.def
plano_saude <- read_delim("dados/beneficiarios_plano_saude.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE, skip = 3)
plano_saude <- plano_saude[,c(1,4)]
names(plano_saude) <- c("MUNICIPIO", "PLANO_SAUDE")
plano_saude$COD_MUNICIPIO <- substr(plano_saude$MUNICIPIO, 0,6)
plano_saude$MUNICIPIO <- substr(plano_saude$MUNICIPIO, 8,100)


##Merge das bases
base <- merge(obito,cons_upa, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"))
base <- merge(base,cons_aten_primaria, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"))
base <- merge(base,cons_outr_emerg, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"))
base <- merge(base, pib, by = c("COD_MUNICIPIO", "MUNICIPIO", "ANO"))
base <- merge(base, plano_saude, by = c("COD_MUNICIPIO", "MUNICIPIO"))

base <- base[!(base$MUNICIPIO %like% c("Município ignorado%")),]
base <- base[complete.cases(base),]

base$TX_CONS_UPA <- base$CONS_UPA/base$POPULACAO * 1000
base$TX_CONS_ATENC_PRIMARIA <- base$CONS_ATENC_PRIMARIA/base$POPULACAO * 1000
base$TX_CONS_OUTR_EMERG <- base$CONS_OUTR_EMERG/base$POPULACAO * 1000
base$TX_BENEF_PLANO_SAUDE <- base$PLANO_SAUDE/base$POPULACAO 





municipios <- unique(base[,c(1,2)])

write.csv(base, "dados/base.csv", row.names = F)
