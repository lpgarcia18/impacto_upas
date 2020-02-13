# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)
# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(causaldrf)
library(causaldrf)
library(MASS)
library(caret)
library(gbm)
library(WeightIt)
library(twang)

# Base --------------------------------------------------------------------
base <- read_csv("dados/base.csv", locale = locale(encoding = "WINDOWS-1252"))



base_2016 <- subset(base, base$ANO == 2016)
base_2017 <- subset(base, base$ANO == 2017)
base_2016 <- base_2016[,colnames(base_2016) %in% c("MUNICIPIO", "POPULACAO", "PIB_PER_CAPITA", "TX_BENEF_PLANO_SAUDE","PROP_MASC",
						   "TX_CONS_ATENC_PRIMARIA", "TX_CONS_OUTR_EMERG", "TX_CONS_UPA", 
						    "TX_CARDIO_NEURO_PADRO", "TX_TRAUMATO_PADRO", "TX_NEOPLASIA_PADRO")]
names(base_2016) <- paste0(names(base_2016),"_2016")
base_2017 <- base_2017[,colnames(base_2017) %in% c("POPULACAO","TX_CONS_ATENC_PRIMARIA", "TX_CONS_OUTR_EMERG", "TX_CONS_UPA", 
						    "TX_CARDIO_NEURO_PADRO", "TX_TRAUMATO_PADRO", "TX_NEOPLASIA_PADRO")]
names(base_2017) <- paste0(names(base_2017),"_2017")

dose_resp <-  cbind(base_2016,base_2017) %>% as.data.frame()

names(dose_resp)[1] <- "MUNICIPIO"

dose_resp <- subset(dose_resp, dose_resp$TX_CONS_UPA_2017 <= quantile(dose_resp$TX_CONS_UPA_2017, probs = 0.9)) #Excluíndo outliers
dose_resp <- subset(dose_resp, dose_resp$TX_CONS_ATENC_PRIMARIA_2017 <= quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = 0.9)) #Excluíndo outliers
dose_resp <- subset(dose_resp, dose_resp$TX_CONS_OUTR_EMERG_2017 <= quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = 0.9)) #Excluíndo outliers

dose_resp <- subset(dose_resp, dose_resp$TX_CONS_ATENC_PRIMARIA_2017 != 0 & dose_resp$TX_CONS_UPA_2017 != 0) #Resposta nos tratados
dose_resp <- subset(dose_resp, dose_resp$TX_CARDIO_NEURO_PADRO_2017 != 0 & 
    			       dose_resp$TX_TRAUMATO_PADRO_2017 != 0 &
		    	       dose_resp$TX_NEOPLASIA_PADRO_2017 != 0) #Excluindo municípios que não tiveram óbitos

# Descrição ---------------------------------------------------------------
ggplot(dose_resp, aes(TX_CONS_ATENC_PRIMARIA_2017))+
	geom_density()
	
ggplot(dose_resp, aes(TX_CONS_UPA_2017))+
	geom_density()

ggplot(dose_resp, aes(TX_CONS_OUTR_EMERG_2017))+
	geom_density()

# Análise -----------------------------------------------------------------
# Hospitais -----------------------------------------------------------------
##Cardio Neurovascular
##Cálculo do gps
outr_emerg_cardio_neuro_gsm <- ps.cont(TX_CONS_OUTR_EMERG_2017 ~
		 		TX_CONS_OUTR_EMERG_2016 +
		 		TX_CARDIO_NEURO_PADRO_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_UPA_2017 +
			  	TX_CONS_ATENC_PRIMARIA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(outr_emerg_cardio_neuro_gsm)
twang::bal.table(outr_emerg_cardio_neuro_gsm) #twang's bal.table
dose_resp$CONS_OUTR_EMERG_GPS <- unlist(outr_emerg_cardio_neuro_gsm$ps[1])


outr_emerg_cardio_neuro_bart <- bart_est(Y = TX_CARDIO_NEURO_PADRO_2017, 
		      treat = TX_CONS_OUTR_EMERG_2017, 
		      outcome_formula = TX_CARDIO_NEURO_PADRO_2017 ~
				 	TX_CONS_OUTR_EMERG_2017 +
				 		CONS_OUTR_EMERG_GPS +
				 		TX_CARDIO_NEURO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_UPA_2017 +
					  	TX_CONS_ATENC_PRIMARIA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))

#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
outr_emerg_cardio_neuro_quint_0 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)]
outr_emerg_cardio_neuro_quint_1 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
outr_emerg_cardio_neuro_quint_2 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
outr_emerg_cardio_neuro_quint_3 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
outr_emerg_cardio_neuro_quint_4 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
outr_emerg_cardio_neuro_quint_5 <- outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

outr_emerg_cardio_neuro_cis_0 <- ci.fun(outr_emerg_cardio_neuro_quint_0)
outr_emerg_cardio_neuro_cis_1 <- ci.fun(outr_emerg_cardio_neuro_quint_1)
outr_emerg_cardio_neuro_cis_2 <- ci.fun(outr_emerg_cardio_neuro_quint_2)
outr_emerg_cardio_neuro_cis_3 <- ci.fun(outr_emerg_cardio_neuro_quint_3)
outr_emerg_cardio_neuro_cis_4 <- ci.fun(outr_emerg_cardio_neuro_quint_4)
outr_emerg_cardio_neuro_cis_5 <- ci.fun(outr_emerg_cardio_neuro_quint_5)


outr_emerg_cardio_neuro_plot <- rbind(outr_emerg_cardio_neuro_cis_0, 
				      outr_emerg_cardio_neuro_cis_1, 
				      outr_emerg_cardio_neuro_cis_2, 
				      outr_emerg_cardio_neuro_cis_3, 
				      outr_emerg_cardio_neuro_cis_4, 
				      outr_emerg_cardio_neuro_cis_5) %>% as.data.frame()
outr_emerg_cardio_neuro_plot$ADRF <- summary(outr_emerg_cardio_neuro_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_OUTR_EMERG_2017, dose_resp$TX_CARDIO_NEURO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      outr_emerg_cardio_neuro_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = outr_emerg_cardio_neuro_plot$`2.5%`, ymax = outr_emerg_cardio_neuro_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Doenças Cardio Neurovasculares / 100.000hab",
	     x = "Taxa de Consultas Hospitalares / 1000hab")
	


outr_emerg_cardio_neuro_dif_quintis <- t.test(outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)],outr_emerg_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
outr_emerg_cardio_neuro_resultado <- data.frame(DIFERENÇA = outr_emerg_cardio_neuro_dif_quintis$estimate[[1]] - outr_emerg_cardio_neuro_dif_quintis$estimate[[2]],
			IC05 = outr_emerg_cardio_neuro_dif_quintis$conf.int[[1]],
			IC95 = outr_emerg_cardio_neuro_dif_quintis$conf.int[[2]])
outr_emerg_cardio_neuro_resultado

##Traumato 
##Cálculo do gps
outr_emerg_traumato_gsm <- ps.cont(TX_CONS_OUTR_EMERG_2017 ~
		 		TX_CONS_OUTR_EMERG_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
		 		TX_CARDIO_NEURO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_UPA_2017 +
			  	TX_CONS_ATENC_PRIMARIA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(outr_emerg_traumato_gsm)
twang::bal.table(outr_emerg_traumato_gsm) #twang's bal.table
dose_resp$CONS_OUTR_EMERG_GPS <- unlist(outr_emerg_traumato_gsm$ps[1])

outr_emerg_traumato_bart <- bart_est(Y = TX_TRAUMATO_PADRO_2017, 
		      treat = TX_CONS_OUTR_EMERG_2017, 
		      outcome_formula = TX_TRAUMATO_PADRO_2017 ~
				 	TX_CONS_OUTR_EMERG_2017 +
				 		CONS_OUTR_EMERG_GPS +
				 		TX_TRAUMATO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_UPA_2017 +
					  	TX_CONS_ATENC_PRIMARIA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))



#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
outr_emerg_traumato_quint_0 <- outr_emerg_traumato_bart[[2]]$yhat.test[,1:(nt)]
outr_emerg_traumato_quint_1 <- outr_emerg_traumato_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
outr_emerg_traumato_quint_2 <- outr_emerg_traumato_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
outr_emerg_traumato_quint_3 <- outr_emerg_traumato_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
outr_emerg_traumato_quint_4 <- outr_emerg_traumato_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
outr_emerg_traumato_quint_5 <- outr_emerg_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

outr_emerg_traumato_cis_0 <- ci.fun(outr_emerg_traumato_quint_0)
outr_emerg_traumato_cis_1 <- ci.fun(outr_emerg_traumato_quint_1)
outr_emerg_traumato_cis_2 <- ci.fun(outr_emerg_traumato_quint_2)
outr_emerg_traumato_cis_3 <- ci.fun(outr_emerg_traumato_quint_3)
outr_emerg_traumato_cis_4 <- ci.fun(outr_emerg_traumato_quint_4)
outr_emerg_traumato_cis_5 <- ci.fun(outr_emerg_traumato_quint_5)


outr_emerg_traumato_plot <- rbind(outr_emerg_traumato_cis_0, 
				      outr_emerg_traumato_cis_1, 
				      outr_emerg_traumato_cis_2, 
				      outr_emerg_traumato_cis_3, 
				      outr_emerg_traumato_cis_4, 
				      outr_emerg_traumato_cis_5) %>% as.data.frame()
outr_emerg_traumato_plot$ADRF <- summary(outr_emerg_traumato_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_OUTR_EMERG_2017, dose_resp$TX_TRAUMATO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      outr_emerg_traumato_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_OUTR_EMERG_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = outr_emerg_traumato_plot$`2.5%`, ymax = outr_emerg_traumato_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Trauma / 100.000hab",
	     x = "Taxa de Consultas Hospitalares / 1000hab")
	


outr_emerg_traumato_dif_quintis <- t.test(outr_emerg_traumato_bart[[2]]$yhat.test[,1:(nt)],outr_emerg_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
outr_emerg_traumato_resultado <- data.frame(DIFERENÇA = outr_emerg_traumato_dif_quintis$estimate[[1]] - outr_emerg_traumato_dif_quintis$estimate[[2]],
			IC05 = outr_emerg_traumato_dif_quintis$conf.int[[1]],
			IC95 = outr_emerg_traumato_dif_quintis$conf.int[[2]])
outr_emerg_traumato_resultado


# UPAs --------------------------------------------------------------------
##Cardio Neurovascular
##Cálculo do gps
upa_cardio_neuro_gsm <- ps.cont(TX_CONS_UPA_2017 ~
		 		TX_CONS_UPA_2016 +
		 		TX_CARDIO_NEURO_PADRO_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_OUTR_EMERG_2017 +
			  	TX_CONS_ATENC_PRIMARIA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(upa_cardio_neuro_gsm)
twang::bal.table(upa_cardio_neuro_gsm) #twang's bal.table
dose_resp$CONS_UPA_GPS <- unlist(upa_cardio_neuro_gsm$ps[1])

upa_cardio_neuro_bart <- bart_est(Y = TX_CARDIO_NEURO_PADRO_2017, 
		      treat = TX_CONS_UPA_2017, 
		      outcome_formula = TX_CARDIO_NEURO_PADRO_2017 ~
				 	TX_CONS_UPA_2017 +
				 		CONS_UPA_GPS +
				 		TX_CARDIO_NEURO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_OUTR_EMERG_2017 +
					  	TX_CONS_ATENC_PRIMARIA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))

#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
upa_cardio_neuro_quint_0 <- upa_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)]
upa_cardio_neuro_quint_1 <- upa_cardio_neuro_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
upa_cardio_neuro_quint_2 <- upa_cardio_neuro_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
upa_cardio_neuro_quint_3 <- upa_cardio_neuro_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
upa_cardio_neuro_quint_4 <- upa_cardio_neuro_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
upa_cardio_neuro_quint_5 <- upa_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

upa_cardio_neuro_cis_0 <- ci.fun(upa_cardio_neuro_quint_0)
upa_cardio_neuro_cis_1 <- ci.fun(upa_cardio_neuro_quint_1)
upa_cardio_neuro_cis_2 <- ci.fun(upa_cardio_neuro_quint_2)
upa_cardio_neuro_cis_3 <- ci.fun(upa_cardio_neuro_quint_3)
upa_cardio_neuro_cis_4 <- ci.fun(upa_cardio_neuro_quint_4)
upa_cardio_neuro_cis_5 <- ci.fun(upa_cardio_neuro_quint_5)


upa_cardio_neuro_plot <- rbind(upa_cardio_neuro_cis_0, 
				      upa_cardio_neuro_cis_1, 
				      upa_cardio_neuro_cis_2, 
				      upa_cardio_neuro_cis_3, 
				      upa_cardio_neuro_cis_4, 
				      upa_cardio_neuro_cis_5) %>% as.data.frame()
upa_cardio_neuro_plot$ADRF <- summary(upa_cardio_neuro_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_UPA_2017, dose_resp$TX_CARDIO_NEURO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      upa_cardio_neuro_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = upa_cardio_neuro_plot$`2.5%`, ymax = upa_cardio_neuro_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Doenças Cardio Neurovasculares / 100.000hab",
	     x = "Taxa de Consultas nas UPAs / 1000hab")
	


upa_cardio_neuro_dif_quintis <- t.test(upa_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)],upa_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
upa_cardio_neuro_resultado <- data.frame(DIFERENÇA = upa_cardio_neuro_dif_quintis$estimate[[1]] - upa_cardio_neuro_dif_quintis$estimate[[2]],
			IC05 = upa_cardio_neuro_dif_quintis$conf.int[[1]],
			IC95 = upa_cardio_neuro_dif_quintis$conf.int[[2]])
upa_cardio_neuro_resultado

##Traumato 
##Cálculo do gps
upa_traumato_gsm <- ps.cont(TX_CONS_UPA_2017 ~
		 		TX_CONS_UPA_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
		 		TX_CARDIO_NEURO_PADRO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_OUTR_EMERG_2017 +
			  	TX_CONS_ATENC_PRIMARIA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(upa_traumato_gsm)
twang::bal.table(upa_traumato_gsm) #twang's bal.table
dose_resp$CONS_UPA_GPS <- unlist(upa_traumato_gsm$ps[1])

upa_traumato_bart <- bart_est(Y = TX_TRAUMATO_PADRO_2017, 
		      treat = TX_CONS_UPA_2017, 
		      outcome_formula = TX_TRAUMATO_PADRO_2017 ~
				 	TX_CONS_UPA_2017 +
				 		CONS_UPA_GPS +
				 		TX_TRAUMATO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_OUTR_EMERG_2017 +
					  	TX_CONS_ATENC_PRIMARIA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))



#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
upa_traumato_quint_0 <- upa_traumato_bart[[2]]$yhat.test[,1:(nt)]
upa_traumato_quint_1 <- upa_traumato_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
upa_traumato_quint_2 <- upa_traumato_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
upa_traumato_quint_3 <- upa_traumato_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
upa_traumato_quint_4 <- upa_traumato_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
upa_traumato_quint_5 <- upa_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

upa_traumato_cis_0 <- ci.fun(upa_traumato_quint_0)
upa_traumato_cis_1 <- ci.fun(upa_traumato_quint_1)
upa_traumato_cis_2 <- ci.fun(upa_traumato_quint_2)
upa_traumato_cis_3 <- ci.fun(upa_traumato_quint_3)
upa_traumato_cis_4 <- ci.fun(upa_traumato_quint_4)
upa_traumato_cis_5 <- ci.fun(upa_traumato_quint_5)


upa_traumato_plot <- rbind(upa_traumato_cis_0, 
				      upa_traumato_cis_1, 
				      upa_traumato_cis_2, 
				      upa_traumato_cis_3, 
				      upa_traumato_cis_4, 
				      upa_traumato_cis_5) %>% as.data.frame()
upa_traumato_plot$ADRF <- summary(upa_traumato_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_UPA_2017, dose_resp$TX_TRAUMATO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      upa_traumato_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_UPA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = upa_traumato_plot$`2.5%`, ymax = upa_traumato_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Trauma / 100.000hab",
	     x = "Taxa de Consultas nas UPAs / 1000hab")
	


upa_traumato_dif_quintis <- t.test(upa_traumato_bart[[2]]$yhat.test[,1:(nt)],upa_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
upa_traumato_resultado <- data.frame(DIFERENÇA = upa_traumato_dif_quintis$estimate[[1]] - upa_traumato_dif_quintis$estimate[[2]],
			IC05 = upa_traumato_dif_quintis$conf.int[[1]],
			IC95 = upa_traumato_dif_quintis$conf.int[[2]])
upa_traumato_resultado



# Atenção Primária --------------------------------------------------------
##Cardio Neurovascular
##Cálculo do gps
atenc_primaria_cardio_neuro_gsm <- ps.cont(TX_CONS_ATENC_PRIMARIA_2017 ~
		 		TX_CONS_ATENC_PRIMARIA_2016 +
		 		TX_CARDIO_NEURO_PADRO_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_OUTR_EMERG_2017 +
			  	TX_CONS_UPA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(atenc_primaria_cardio_neuro_gsm)
twang::bal.table(atenc_primaria_cardio_neuro_gsm) #twang's bal.table
dose_resp$CONS_ATENC_PRIMARIA_GPS <- unlist(atenc_primaria_cardio_neuro_gsm$ps[1])

atenc_primaria_cardio_neuro_bart <- bart_est(Y = TX_CARDIO_NEURO_PADRO_2017, 
		      treat = TX_CONS_ATENC_PRIMARIA_2017, 
		      outcome_formula = TX_CARDIO_NEURO_PADRO_2017 ~
				 	TX_CONS_ATENC_PRIMARIA_2017 +
				 		CONS_ATENC_PRIMARIA_GPS +
				 		TX_CARDIO_NEURO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_OUTR_EMERG_2017 +
					  	TX_CONS_UPA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))

#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
atenc_primaria_cardio_neuro_quint_0 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)]
atenc_primaria_cardio_neuro_quint_1 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
atenc_primaria_cardio_neuro_quint_2 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
atenc_primaria_cardio_neuro_quint_3 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
atenc_primaria_cardio_neuro_quint_4 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
atenc_primaria_cardio_neuro_quint_5 <- atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

atenc_primaria_cardio_neuro_cis_0 <- ci.fun(atenc_primaria_cardio_neuro_quint_0)
atenc_primaria_cardio_neuro_cis_1 <- ci.fun(atenc_primaria_cardio_neuro_quint_1)
atenc_primaria_cardio_neuro_cis_2 <- ci.fun(atenc_primaria_cardio_neuro_quint_2)
atenc_primaria_cardio_neuro_cis_3 <- ci.fun(atenc_primaria_cardio_neuro_quint_3)
atenc_primaria_cardio_neuro_cis_4 <- ci.fun(atenc_primaria_cardio_neuro_quint_4)
atenc_primaria_cardio_neuro_cis_5 <- ci.fun(atenc_primaria_cardio_neuro_quint_5)


atenc_primaria_cardio_neuro_plot <- rbind(atenc_primaria_cardio_neuro_cis_0, 
				      atenc_primaria_cardio_neuro_cis_1, 
				      atenc_primaria_cardio_neuro_cis_2, 
				      atenc_primaria_cardio_neuro_cis_3, 
				      atenc_primaria_cardio_neuro_cis_4, 
				      atenc_primaria_cardio_neuro_cis_5) %>% as.data.frame()
atenc_primaria_cardio_neuro_plot$ADRF <- summary(atenc_primaria_cardio_neuro_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, dose_resp$TX_CARDIO_NEURO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      atenc_primaria_cardio_neuro_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = atenc_primaria_cardio_neuro_plot$`2.5%`, ymax = atenc_primaria_cardio_neuro_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Doenças Cardio Neurovasculares / 100.000hab",
	     x = "Taxa de Consultas nos Centros de Saúde / 1000hab")
	


atenc_primaria_cardio_neuro_dif_quintis <- t.test(atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,1:(nt)],atenc_primaria_cardio_neuro_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
atenc_primaria_cardio_neuro_resultado <- data.frame(DIFERENÇA = atenc_primaria_cardio_neuro_dif_quintis$estimate[[1]] - atenc_primaria_cardio_neuro_dif_quintis$estimate[[2]],
			IC05 = atenc_primaria_cardio_neuro_dif_quintis$conf.int[[1]],
			IC95 = atenc_primaria_cardio_neuro_dif_quintis$conf.int[[2]])
atenc_primaria_cardio_neuro_resultado

##Traumato 
##Cálculo do gps
atenc_primaria_traumato_gsm <- ps.cont(TX_CONS_ATENC_PRIMARIA_2017 ~
		 		TX_CONS_ATENC_PRIMARIA_2016 +
		 		TX_TRAUMATO_PADRO_2016 +
		 		TX_CARDIO_NEURO_PADRO_2016 +
	      			POPULACAO_2016 +
	      			PIB_PER_CAPITA_2016 +
	      			TX_BENEF_PLANO_SAUDE_2016 +
			  	TX_CONS_OUTR_EMERG_2017 +
			  	TX_CONS_UPA_2017+
			 	PROP_MASC_2016, 
		data = dose_resp,
                stop.method = c("p.mean"),
                use.optimize = 2)
summary(atenc_primaria_traumato_gsm)
twang::bal.table(atenc_primaria_traumato_gsm) #twang's bal.table
dose_resp$CONS_ATENC_PRIMARIA_GPS <- unlist(atenc_primaria_traumato_gsm$ps[1])

atenc_primaria_traumato_bart <- bart_est(Y = TX_TRAUMATO_PADRO_2017, 
		      treat = TX_CONS_ATENC_PRIMARIA_2017, 
		      outcome_formula = TX_TRAUMATO_PADRO_2017 ~
				 	TX_CONS_ATENC_PRIMARIA_2017 +
				 		CONS_ATENC_PRIMARIA_GPS +
				 		TX_TRAUMATO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_OUTR_EMERG_2017 +
					  	TX_CONS_UPA_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))



#plot out response surface as a function of outcome
nt <- nrow(dose_resp)
atenc_primaria_traumato_quint_0 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,1:(nt)]
atenc_primaria_traumato_quint_1 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,(nt+1):(2*nt)]
atenc_primaria_traumato_quint_2 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,(2*nt+1):(3*nt)]
atenc_primaria_traumato_quint_3 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,(3*nt+1):(4*nt)]
atenc_primaria_traumato_quint_4 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,(4*nt+1):(5*nt)]
atenc_primaria_traumato_quint_5 <- atenc_primaria_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)]


ci.fun <- function(a){
c(quantile(a,.025),quantile(a,.975))
}

atenc_primaria_traumato_cis_0 <- ci.fun(atenc_primaria_traumato_quint_0)
atenc_primaria_traumato_cis_1 <- ci.fun(atenc_primaria_traumato_quint_1)
atenc_primaria_traumato_cis_2 <- ci.fun(atenc_primaria_traumato_quint_2)
atenc_primaria_traumato_cis_3 <- ci.fun(atenc_primaria_traumato_quint_3)
atenc_primaria_traumato_cis_4 <- ci.fun(atenc_primaria_traumato_quint_4)
atenc_primaria_traumato_cis_5 <- ci.fun(atenc_primaria_traumato_quint_5)


atenc_primaria_traumato_plot <- rbind(atenc_primaria_traumato_cis_0, 
				      atenc_primaria_traumato_cis_1, 
				      atenc_primaria_traumato_cis_2, 
				      atenc_primaria_traumato_cis_3, 
				      atenc_primaria_traumato_cis_4, 
				      atenc_primaria_traumato_cis_5) %>% as.data.frame()
atenc_primaria_traumato_plot$ADRF <- summary(atenc_primaria_traumato_bart)

ggplot()+
	geom_jitter(aes(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, dose_resp$TX_TRAUMATO_PADRO_2017), alpha = 0.1)+
	geom_line(aes(quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
		      atenc_primaria_traumato_plot$ADRF, group = 1), size = 1.5, linetype = "dashed")+
	geom_ribbon(aes(x= quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)), 
			ymin = atenc_primaria_traumato_plot$`2.5%`, ymax = atenc_primaria_traumato_plot$`97.5%`), alpha = 0.5)+
	theme_bw()+
	labs(y = "Taxa de Óbitos por Trauma / 100.000hab",
	     x = "Taxa de Consultas nos Centros de Saúde / 1000hab")
	


atenc_primaria_traumato_dif_quintis <- t.test(atenc_primaria_traumato_bart[[2]]$yhat.test[,1:(nt)],atenc_primaria_traumato_bart[[2]]$yhat.test[,(5*nt+1):(6*nt)])
atenc_primaria_traumato_resultado <- data.frame(DIFERENÇA = atenc_primaria_traumato_dif_quintis$estimate[[1]] - atenc_primaria_traumato_dif_quintis$estimate[[2]],
			IC05 = atenc_primaria_traumato_dif_quintis$conf.int[[1]],
			IC95 = atenc_primaria_traumato_dif_quintis$conf.int[[2]])
atenc_primaria_traumato_resultado



























bart_traumato_ap <- bart_est(Y = TX_TRAUMATO_PADRO_2017, 
		      treat = TX_CONS_ATENC_PRIMARIA_2017, 
		      outcome_formula = TX_TRAUMATO_PADRO_2017 ~
			     		TX_CONS_ATENC_PRIMARIA_2017 +
			     			TX_CONS_ATENC_PRIMARIA_2016 +
			     			TX_TRAUMATO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_UPA_2017 +
					  	TX_CONS_OUTR_EMERG_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = seq(0.0, 1, by = 0.2)))


summary(bart_traumato_ap)

plot(dose_resp$TX_CONS_ATENC_PRIMARIA_2017,
      dose_resp$TX_CARDIO_NEURO_PADRO_2017,
      xlab = "",
      ylab = "Y",
      main = "hi estimate")

lines(quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = seq(0.0, 1, by = 0.2)), 
      bart_traumato_ap$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
        "hi estimate",
        lty=2,
        lwd = 2,
        col = "blue",
        bty='Y',
        cex=1)

##Neoplasia - Padronizada
bart_neoplasia_ap <- bart_est(Y = TX_NEOPLASIA_PADRO_2017, 
		      treat = TX_CONS_ATENC_PRIMARIA_2017, 
		      outcome_formula = TX_NEOPLASIA_PADRO_2017 ~
		      			TX_CONS_ATENC_PRIMARIA_2017 + 
		      				TX_CONS_ATENC_PRIMARIA_2016 +
		      				TX_NEOPLASIA_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_UPA_2017 +
					  	TX_CONS_OUTR_EMERG_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = seq(0.0, 1, by = 0.2)))

summary(bart_neoplasia_ap)


plot(dose_resp$TX_CONS_ATENC_PRIMARIA_2017,
      dose_resp$TX_NEOPLASIA_PADRO_2017,
      xlab = "T",
      ylab = "Y",
      main = "hi estimate")

lines(quantile(dose_resp$TX_CONS_ATENC_PRIMARIA_2017, probs = seq(0.0, 1, by = 0.2)),
      bart_neoplasia_ap$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
        "hi estimate",
        lty=2,
        lwd = 2,
        col = "blue",
        bty='Y',
        cex=1)




# UPA ---------------------------------------------------------------------
##Cárdio e Neurovacular - Padronizada
bart_cardio_neuro_upa <- bart_est(Y = TX_CARDIO_NEURO_PADRO_2017, 
		      treat = TX_CONS_UPA_2017, 
		      outcome_formula = TX_CARDIO_NEURO_PADRO_2017 ~
				  	TX_CONS_UPA_2017 + 
				  		TX_CONS_UPA_2016 +
				  		TX_CARDIO_NEURO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_ATENC_PRIMARIA_2017 +
					  	TX_CONS_OUTR_EMERG_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.)))

summary(bart_cardio_neuro_upa)


plot(dose_resp$TX_CONS_UPA_2017,
      dose_resp$TX_CARDIO_NEURO_PADRO_2017,
      xlab = "T",
      ylab = "Y",
      main = "hi estimate")

lines(quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.2)),
      bart_cardio_neuro_upa$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
        "hi estimate",
        lty=2,
        lwd = 2,
        col = "blue",
        bty='Y',
        cex=1)


##Traumato - Padronizada # Retirado atenção primária
bart_traumato_upa <- bart_est(Y = TX_TRAUMATO_PADRO_2017, 
		      treat = TX_CONS_UPA_2017, 
		      outcome_formula = TX_TRAUMATO_PADRO_2017 ~
		      			TX_CONS_UPA_2017 + 
				  		TX_CONS_UPA_2016 +
				  		TX_TRAUMATO_PADRO_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_OUTR_EMERG_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.2)))


summary(bart_traumato_upa)

plot(dose_resp$TX_CONS_UPA_2017,
      dose_resp$TX_CARDIO_NEURO_PADRO_2017,
      xlab = "",
      ylab = "Y",
      main = "hi estimate")

lines(quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.2)), 
      bart_traumato_upa$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
        "hi estimate",
        lty=2,
        lwd = 2,
        col = "blue",
        bty='Y',
        cex=1)


##Neoplasia - Padronizada
bart_neoplasia_upa <- bart_est(Y = TX_NEOPLASIA_PADRO_2017, 
		      treat = TX_CONS_UPA_2017, 
		      outcome_formula = TX_NEOPLASIA_PADRO_2017 ~
		      			TX_CONS_UPA_2017 +
				  		TX_CONS_UPA_2016 +
				  		TX_NEOPLASIA_2016 +
			      			POPULACAO_2016 +
			      			PIB_PER_CAPITA_2016 +
			      			TX_BENEF_PLANO_SAUDE_2016 +
					  	TX_CONS_ATENC_PRIMARIA_2017 +
					  	TX_CONS_OUTR_EMERG_2017+
					 	PROP_MASC_2016,
		      data = dose_resp, 
		      grid_val = quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.2)))

summary(bart_neoplasia_upa)


plot(dose_resp$TX_CONS_UPA_2017,
      dose_resp$TX_NEOPLASIA_PADRO_2017,
      xlab = "T",
      ylab = "Y",
      main = "hi estimate")

lines(quantile(dose_resp$TX_CONS_UPA_2017, probs = seq(0.0, 1, by = 0.2)),
      bart_neoplasia_upa$param,
      lty = 2,
      lwd = 2,
      col = "blue")

legend('bottomright',
        "hi estimate",
        lty=2,
        lwd = 2,
        col = "blue",
        bty='Y',
        cex=1)

bart_cardio_neuro_ap[[2]]$yhat.train.mean


plot(bart_cardio_neuro_ap)

