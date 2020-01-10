
# Ambiente ----------------------------------------------------------------
options(scipen=999)

# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(reshape2)

# Bases -------------------------------------------------------------------
##Importação das bases
abertura_upas <- read_delim("dados/abertura_upas_2010_2019.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #PRONTO ATENDIMENTO - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/estabbr.def

abertura_outras_emergencias <- read_delim("dados/abertura_outras emergencias_2010_2019.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #HOSPITAL ESPECIALIZADO, HOSPITAL GERAL, PRONTO SOCORRO ESPECIALIZADO, PRONTO SOCORRO GERAL, PRONTO SOCORRO DE HOSPITAL GERAL (ANTIGO), PRONTO SOCORRO TRAUMATO-ORTOPEDICO (ANTIGO) - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?cnes/cnv/estabbr.def

internacoes <- read_delim("dados/internacoes_municipio_residencia.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sih/cnv/nruf.def

obitos_2013 <- read_delim("dados/obitos_2013.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def

obitos_2014 <- read_delim("dados/obitos_2014.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def

obitos_2015 <- read_delim("dados/obitos_2015.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def

obitos_2016 <- read_delim("dados/obitos_2016.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def

obitos_2017 <- read_delim("dados/obitos_2017.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #MUNICÍPIO DE RESIDÊNCIA - http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/obt10br.def

residentes <- read_delim("dados/residentes.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE) #Estimativa TCU

estados <- read_csv("dados/estados.csv")


# Transformação -----------------------------------------------------------

#Ajuste de colunas
manter <- c("\"\"\"Município\"\"", 
		"\"\"2013/Jan\"\"",    "\"\"2013/Fev\"\"",    "\"\"2013/Mar\"\"",    "\"\"2013/Abr\"\"",    "\"\"2013/Mai\"\"",   
		"\"\"2013/Jun\"\"",    "\"\"2013/Jul\"\"",    "\"\"2013/Ago\"\"",    "\"\"2013/Set\"\"",    "\"\"2013/Out\"\"",   
		"\"\"2013/Nov\"\"",    "\"\"2013/Dez\"\"",    "\"\"2014/Jan\"\"",    "\"\"2014/Fev\"\"",    "\"\"2014/Mar\"\"",   
		"\"\"2014/Abr\"\"",    "\"\"2014/Mai\"\"",    "\"\"2014/Jun\"\"",    "\"\"2014/Jul\"\"",    "\"\"2014/Ago\"\"",   
		"\"\"2014/Set\"\"",    "\"\"2014/Out\"\"",    "\"\"2014/Nov\"\"",    "\"\"2014/Dez\"\"",    "\"\"2015/Jan\"\"",   
		"\"\"2015/Fev\"\"",    "\"\"2015/Mar\"\"",    "\"\"2015/Abr\"\"",    "\"\"2015/Mai\"\"",    "\"\"2015/Jun\"\"",   
		"\"\"2015/Jul\"\"",    "\"\"2015/Ago\"\"",    "\"\"2015/Set\"\"",    "\"\"2015/Out\"\"",    "\"\"2015/Nov\"\"",   
		"\"\"2015/Dez\"\"",    "\"\"2016/Jan\"\"",    "\"\"2016/Fev\"\"",    "\"\"2016/Mar\"\"",    "\"\"2016/Abr\"\"",   
		"\"\"2016/Mai\"\"",    "\"\"2016/Jun\"\"",    "\"\"2016/Jul\"\"",    "\"\"2016/Ago\"\"",    "\"\"2016/Set\"\"",   
		"\"\"2016/Out\"\"",    "\"\"2016/Nov\"\"",    "\"\"2016/Dez\"\"",    "\"\"2017/Jan\"\"",    "\"\"2017/Fev\"\"",   
		"\"\"2017/Mar\"\"",    "\"\"2017/Abr\"\"",    "\"\"2017/Mai\"\"",    "\"\"2017/Jun\"\"",    "\"\"2017/Jul\"\"",   
		"\"\"2017/Ago\"\"",    "\"\"2017/Set\"\"",    "\"\"2017/Out\"\"",    "\"\"2017/Nov\"\"",    "\"\"2017/Dez\"\"" )
abertura_upas <- abertura_upas[,which(names(abertura_upas) %in% (manter))]
names(abertura_upas) <- c("Município", 
			  "2013/Jan", "2013/Feb","2013/Mar","2013/Apr","2013/May","2013/Jun",
			  "2013/Jul","2013/Aug","2013/Sep","2013/Oct","2013/Nov","2013/Dec",
			  "2014/Jan", "2014/Feb","2014/Mar","2014/Apr","2014/May","2014/Jun",
			  "2014/Jul","2014/Aug","2014/Sep","2014/Oct","2014/Nov","2014/Dec",
			  "2015/Jan", "2015/Feb","2015/Mar","2015/Apr","2015/May","2015/Jun",
			  "2015/Jul","2015/Aug","2015/Sep","2015/Oct","2015/Nov","2015/Dec",
			  "2016/Jan", "2016/Feb","2016/Mar","2016/Apr","2016/May","2016/Jun",
			  "2016/Jul","2016/Aug","2016/Sep","2016/Oct","2016/Nov","2016/Dec",
			  "2017/Jan", "2017/Feb","2017/Mar","2017/Apr","2017/May","2017/Jun",
			  "2017/Jul","2017/Aug","2017/Sep","2017/Oct","2017/Nov","2017/Dec")
abertura_outras_emergencias <- abertura_outras_emergencias[,which(names(abertura_outras_emergencias) %in% (manter))]
names(abertura_outras_emergencias) <- c("Município", 
			  "2013/Jan", "2013/Feb","2013/Mar","2013/Apr","2013/May","2013/Jun",
			  "2013/Jul","2013/Aug","2013/Sep","2013/Oct","2013/Nov","2013/Dec",
			  "2014/Jan", "2014/Feb","2014/Mar","2014/Apr","2014/May","2014/Jun",
			  "2014/Jul","2014/Aug","2014/Sep","2014/Oct","2014/Nov","2014/Dec",
			  "2015/Jan", "2015/Feb","2015/Mar","2015/Apr","2015/May","2015/Jun",
			  "2015/Jul","2015/Aug","2015/Sep","2015/Oct","2015/Nov","2015/Dec",
			  "2016/Jan", "2016/Feb","2016/Mar","2016/Apr","2016/May","2016/Jun",
			  "2016/Jul","2016/Aug","2016/Sep","2016/Oct","2016/Nov","2016/Dec",
			  "2017/Jan", "2017/Feb","2017/Mar","2017/Apr","2017/May","2017/Jun",
			  "2017/Jul","2017/Aug","2017/Sep","2017/Oct","2017/Nov","2017/Dec")
internacoes <- internacoes[,which(names(internacoes) %in% (manter))]
names(internacoes) <- c("Município", 
			  "2013/Jan", "2013/Feb","2013/Mar","2013/Apr","2013/May","2013/Jun",
			  "2013/Jul","2013/Aug","2013/Sep","2013/Oct","2013/Nov","2013/Dec",
			  "2014/Jan", "2014/Feb","2014/Mar","2014/Apr","2014/May","2014/Jun",
			  "2014/Jul","2014/Aug","2014/Sep","2014/Oct","2014/Nov","2014/Dec",
			  "2015/Jan", "2015/Feb","2015/Mar","2015/Apr","2015/May","2015/Jun",
			  "2015/Jul","2015/Aug","2015/Sep","2015/Oct","2015/Nov","2015/Dec",
			  "2016/Jan", "2016/Feb","2016/Mar","2016/Apr","2016/May","2016/Jun",
			  "2016/Jul","2016/Aug","2016/Sep","2016/Oct","2016/Nov","2016/Dec",
			  "2017/Jan", "2017/Feb","2017/Mar","2017/Apr","2017/May","2017/Jun",
			  "2017/Jul","2017/Aug","2017/Sep","2017/Oct","2017/Nov","2017/Dec")

obitos_2013 <- obitos_2013[,-14]
obitos_2014 <- obitos_2014[,-14]
obitos_2015 <- obitos_2015[,-14]
obitos_2016 <- obitos_2016[,-14]
obitos_2017 <- obitos_2017[,-14]
names(obitos_2013) <- c("Município", "2013/Jan", "2013/Feb","2013/Mar","2013/Apr","2013/May","2013/Jun",
			"2013/Jul","2013/Aug","2013/Sep","2013/Oct","2013/Nov","2013/Dec") 
names(obitos_2014) <- c("Município", "2014/Jan", "2014/Feb","2014/Mar","2014/Apr","2014/May","2014/Jun",
			"2014/Jul","2014/Aug","2014/Sep","2014/Oct","2014/Nov","2014/Dec") 
names(obitos_2015) <- c("Município", "2015/Jan", "2015/Feb","2015/Mar","2015/Apr","2015/May","2015/Jun",
			"2015/Jul","2015/Aug","2015/Sep","2015/Oct","2015/Nov","2015/Dec") 
names(obitos_2016) <- c("Município", "2016/Jan", "2016/Feb","2016/Mar","2016/Apr","2016/May","2016/Jun",
			"2016/Jul","2016/Aug","2016/Sep","2016/Oct","2016/Nov","2016/Dec") 
names(obitos_2017) <- c("Município", "2017/Jan", "2017/Feb","2017/Mar","2017/Apr","2017/May","2017/Jun",
			"2017/Jul","2017/Aug","2017/Sep","2017/Oct","2017/Nov","2017/Dec") 
names(residentes) <- c("Município", 2010:2018)
residentes <- residentes[,which(names(residentes) %in% c("Município", 2012:2017))]

abertura_upas <- melt(abertura_upas, id.vars = "Município")
abertura_outras_emergencias <- melt(abertura_outras_emergencias, id.vars = "Município")
internacoes <- melt(internacoes, id.vars = "Município")
obitos_2013 <- melt(obitos_2013, id.vars = "Município")
obitos_2014 <- melt(obitos_2014, id.vars = "Município")
obitos_2015 <- melt(obitos_2015, id.vars = "Município")
obitos_2016 <- melt(obitos_2016, id.vars = "Município")
obitos_2017 <- melt(obitos_2017, id.vars = "Município")
residentes <- melt(residentes, id.vars = "Município")



###Interpolação da população por mês
names(residentes) <- c("MUNICÍPIO", "DATA", "RESIDENTES")
municipios <- unique(residentes$MUNICÍPIO)
residentes$DATA <- as.Date(residentes$DATA, "%Y/%m")
mes <- seq(as.Date("2013/1/1"), as.Date("2017/12/1"), "month") %>% as.data.frame()
resid_mes <- list()
for(i in seq_along(municipios)){
	sub_municip <- subset(residentes, residentes$MUNICÍPIO == municipios[i]) %>% as.data.frame()
	##sub_municip <- sub_municip[,c(2,3)]
	resid_mes[[i]] <- data.frame(Data = mes, MUNICÍPIO = municipios[i], RESIDENTES = spline(sub_municip$RESIDENTES, method="fmm", n = 60)$y)	
}
resid_input <- do.call(rbind,resid_mes)
names(resid_input) <- c("DATA", "MUNICÍPIO", "POPULACAO")
resid_input$DATA <- format(resid_input$DATA, "%Y/%b") %>% as.character()
##Construção de base única
obitos <- rbind(obitos_2013, obitos_2014, obitos_2015, obitos_2016, obitos_2017)
names(obitos) <- c("MUNICÍPIO", "DATA", "OBITOS")
names(abertura_upas) <- c("MUNICÍPIO", "DATA", "UPAs")
names(abertura_outras_emergencias) <- c("MUNICÍPIO", "DATA", "OUTRAS_EMERGENCIAS")
names(internacoes) <- c("MUNICÍPIO", "DATA", "INTERNACOES")
base <- list(resid_input, obitos, abertura_upas, abertura_outras_emergencias, internacoes) %>% 
	reduce(left_join, by = c("DATA", "MUNICÍPIO"))

base[which(base$POPULACAO == "-"), 3] <- NA 
base[which(base$OBITOS  == "-"), 4] <- NA 
base[which(base$UPAs  == "-"), 5] <- NA 
base[which(base$OUTRAS_EMERGENCIAS  == "-"), 6] <- NA 
base[which(base$INTERNACOES  == "-"), 7] <- NA 


base$POPULACAO <- as.numeric(base$POPULACAO)
base$OBITOS <- as.numeric(base$OBITOS)
base$UPAs <- as.numeric(base$UPAs)
base$OUTRAS_EMERGENCIAS <- as.numeric(base$OUTRAS_EMERGENCIAS)
base$INTERNACOES <- as.numeric(base$INTERNACOES)

base$DATA <- paste0(base$DATA, "/01")
base$DATA <- as.Date(base$DATA, "%Y/%b/%d")

##Ajuste dos nomes dos municípios
base$COD_MUNICIPIO <- substr(base$MUNICÍPIO, 0, 6)
base$MUNICIPIO <- substr(base$MUNICÍPIO, 8, 10000)
base$MUNICÍPIO <- NULL
base$COD_ESTADO <- substr(base$COD_MUNICIPIO, 0,2)
base <- merge(base, estados[,c(1,2)], by = "COD_ESTADO", all.x = T)
base <- base[,c(1,10,8,9,2,5,6,3,4,7)]

##Construção de dummys para mês, ano, mês e ano, estado e município
base$MES <- format(base$DATA, "%b") %>% 
	factor(levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
base$ANO <- format(base$DATA, "%Y") %>% 
	factor(levels = c(2013:2017))
base$MES_ANO <- format(base$DATA, "%Y/%b") %>% 
	factor(levels = c("2013/Jan", "2013/Feb","2013/Mar","2013/Apr","2013/May","2013/Jun",
			  "2013/Jul","2013/Aug","2013/Sep","2013/Oct","2013/Nov","2013/Dec",
			  "2014/Jan", "2014/Feb","2014/Mar","2014/Apr","2014/May","2014/Jun",
			  "2014/Jul","2014/Aug","2014/Sep","2014/Oct","2014/Nov","2014/Dec",
			  "2015/Jan", "2015/Feb","2015/Mar","2015/Apr","2015/May","2015/Jun",
			  "2015/Jul","2015/Aug","2015/Sep","2015/Oct","2015/Nov","2015/Dec",
			  "2016/Jan", "2016/Feb","2016/Mar","2016/Apr","2016/May","2016/Jun",
			  "2016/Jul","2016/Aug","2016/Sep","2016/Oct","2016/Nov","2016/Dec",
			  "2017/Jan", "2017/Feb","2017/Mar","2017/Apr","2017/May","2017/Jun",
			  "2017/Jul","2017/Aug","2017/Sep","2017/Oct","2017/Nov","2017/Dec"))
base$ESTADO <- as.factor(base$ESTADO)
base$MUNICIPIO <- as.factor(base$MUNICIPIO)

##Cálculo das taxas de mortalidade e internação
base$TX_OBITO <- base$OBITOS/base$POPULACAO * 100000
base$TX_INTERNACAO <- base$INTERNACOES/base$POPULACAO * 100000


# Descrição ---------------------------------------------------------------
##Série histórica das variáveis
###Brasil
group_brasil <- base %>% 
	group_by(MES_ANO) %>%
	summarize(OBITOS = sum(OBITOS, na.rm = T), 
		  INTERNACOES = sum(INTERNACOES, na.rm = T),
		  UPAs = sum(UPAs, na.rm = T),
		  OUTRAS_EMERGENCIAS = sum(OUTRAS_EMERGENCIAS, na.rm = T),
		  POPULACAO = sum(POPULACAO, na.rm = T))
group_brasil$TX_OBITOS <- group_brasil$OBITOS/group_brasil$POPULACAO * 100000
group_brasil$TX_INTERNACOES <- group_brasil$INTERNACOES/group_brasil$POPULACAO * 100000
group_brasil$TX_UPAs <- group_brasil$UPAs/group_brasil$POPULACAO * 100000
group_brasil$TX_OUTRAS_EMERGENCIAS <- group_brasil$OUTRAS_EMERGENCIAS/group_brasil$POPULACAO * 100000

####Óbitos 
ggplot(group_brasil, aes(MES_ANO, OBITOS, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Óbitos 
ggplot(group_brasil, aes(MES_ANO, TX_OBITOS, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Internações 
ggplot(group_brasil, aes(MES_ANO, INTERNACOES, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Internações 
ggplot(group_brasil, aes(MES_ANO, TX_INTERNACOES, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####UPAs
ggplot(group_brasil, aes(MES_ANO, UPAs, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx UPAs
ggplot(group_brasil, aes(MES_ANO, TX_UPAs, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####OUTRAS EMERGENCIAS
ggplot(group_brasil, aes(MES_ANO, OUTRAS_EMERGENCIAS, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####TX OUTRAS EMERGENCIAS
ggplot(group_brasil, aes(MES_ANO, TX_OUTRAS_EMERGENCIAS, group = 1))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))


###Estados
group_estado <- base %>% 
	group_by(ESTADO, MES_ANO) %>%
	summarize(OBITOS = sum(OBITOS, na.rm = T), 
		  INTERNACOES = sum(INTERNACOES, na.rm = T),
		  UPAs = sum(UPAs, na.rm = T),
		  OUTRAS_EMERGENCIAS = sum(OUTRAS_EMERGENCIAS, na.rm = T), 
		  POPULACAO = sum(POPULACAO, na.rm = T))
group_estado$TX_OBITOS <- group_estado$OBITOS/group_estado$POPULACAO * 100000
group_estado$TX_INTERNACOES <- group_estado$INTERNACOES/group_estado$POPULACAO * 100000
group_estado$TX_UPAs <- group_estado$UPAs/group_estado$POPULACAO * 100000
group_estado$TX_OUTRAS_EMERGENCIAS <- group_estado$OUTRAS_EMERGENCIAS/group_estado$POPULACAO * 100000


####Óbitos 
ggplot(group_estado, aes(MES_ANO, OBITOS, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Óbitos 
ggplot(group_estado, aes(MES_ANO, TX_OBITOS, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Internações 
ggplot(group_estado, aes(MES_ANO, INTERNACOES, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Internações 
ggplot(group_estado, aes(MES_ANO, TX_INTERNACOES, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####UPAs
ggplot(group_estado, aes(MES_ANO, UPAs, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx UPAs
ggplot(group_estado, aes(MES_ANO, TX_UPAs, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####OUTRAS EMERGENCIAS
ggplot(group_estado, aes(MES_ANO, OUTRAS_EMERGENCIAS, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####TX OUTRAS EMERGENCIAS
ggplot(group_estado, aes(MES_ANO, TX_OUTRAS_EMERGENCIAS, color = ESTADO, group = ESTADO))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))


###Municípios com e sem upas
base$TEM_UPAS <- NA
base[which(is.na(base$UPAs)),16] <- "não"
base[which(!is.na(base$UPAs)),16] <- "sim"


group_upa <- base %>% 
	group_by(TEM_UPAS, MES_ANO) %>%
	summarize(OBITOS = sum(OBITOS, na.rm = T), 
		  INTERNACOES = sum(INTERNACOES, na.rm = T), 
		  POPULACAO = sum(POPULACAO, na.rm = T))
group_upa$TX_OBITOS <- group_upa$OBITOS/group_upa$POPULACAO * 100000
group_upa$TX_INTERNACOES <- group_upa$INTERNACOES/group_upa$POPULACAO * 100000

####Óbitos
ggplot(group_upa, aes(MES_ANO, OBITOS, color = TEM_UPAS, group = TEM_UPAS))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Óbitos
ggplot(group_upa, aes(MES_ANO, TX_OBITOS, color = TEM_UPAS, group = TEM_UPAS))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Internações
ggplot(group_upa, aes(MES_ANO, INTERNACOES, color = TEM_UPAS, group = TEM_UPAS))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))
####Tx Internações
ggplot(group_upa, aes(MES_ANO, TX_INTERNACOES, color = TEM_UPAS, group = TEM_UPAS))+
	geom_line()+
	theme_bw()+
	theme(axis.text.x = element_text(angle = 90))




###Número de Municípios com, ao menos 1 UPA
###Taxa de internação por 100.000 hab no Brasil
###Taxa de mortalidade por 100.000 hab no Brasil

##Estatística descritiva das variáveis


# Análise -----------------------------------------------------------------


