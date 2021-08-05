#----------------------------------PACOTES--------------------------------------
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(zoo)
library(DT)
library(png)
library(googlesheets4)
library(reshape2)
library(readr)
library(shinybusy)

#----------------------------IMPORTANDO OS DADOS--------------------------------
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")

# sheets reauth with specified token and email address
gs4_auth(
  cache = ".secrets",
  email = "planilhasfreela@gmail.com"
)

bd <- gs4_get("https://docs.google.com/spreadsheets/d/1jV6EtQX4152zwwbdLnyU60xzSNgFCnjp2z40TrzUgLI/edit#gid=1607439528")
jogos <- googlesheets4::range_speedread(bd,"jogos")
jogos$DATA <- as.Date(jogos$DATA)
jogos$JOGO_ID_API <- as.numeric(jogos$JOGO_ID_API)
bdligas <- gs4_get('https://docs.google.com/spreadsheets/d/1MpSMp8fxDD4mWWtbwl-W67F5u0WTh5VKq28WTnqSG5E/edit#gid=1693049006')
ligas <- googlesheets4::range_speedread(bdligas,"campeonato")

#----------------------------------FUNCOES--------------------------------------
matchodds <- function(jogos,input1,input2) {  
  DF1 <- jogos[jogos$jogo_id_campeonato == ligas$league_api_id[ligas$league_name==input1],]
  DF1 <- DF1[DF1$ODDS.CASA != 0,]
  DF1 <- DF1 %>% select(DATA,'TIME.CASA',	'TIME.VISITANTE',	'GOLS.CASA',
                        'GOLS.VISITANTE',	'RESULTADO',	'ODDS.CASA',
                        'ODDS.EMPATE', 'ODDS.VISITANTE') %>%
    arrange(DATA) %>%  mutate('CASA' = ifelse(RESULTADO == 'C',100*ODDS.CASA-100,-100),
                              'EMPATE' = ifelse(RESULTADO == 'E',100*ODDS.EMPATE-100,-100),
                              'FORA' = ifelse(RESULTADO == 'F',100*ODDS.VISITANTE-100,-100))
  if (length(DF1$DATA)>input2 && input2 >150) {
    DF1 <- DF1[(length(DF1$TIME.CASA)-input2):length(DF1$TIME.CASA),]
  }
  
  DF2 <- DF1 %>% group_by(DATA) %>% summarise('CASA'=sum(CASA),
                                              'EMPATE'=sum(EMPATE),
                                              'FORA'=sum(FORA))
  DF2 <- DF2 %>% mutate(Peso = 1:nrow(DF2))
  DF2 <- DF2 %>% select(DATA,CASA,EMPATE,FORA,Peso)
  DF2$OUTRO.CASA <- c(0,0,cumsum(DF2$CASA)[3:length(DF2$CASA)])
  DF2$OUTRO.EMPATE <- c(0,0,cumsum(DF2$EMPATE)[3:length(DF2$EMPATE)])
  DF2$OUTRO.FORA <- c(0,0,cumsum(DF2$FORA)[3:length(DF2$FORA)])
  
  ##Hull H
  DF2$'16.CASA' <- 0
  DF2$'8.CASA' <- 0
  DF2$'Hull.CASA' <- 0
  DF2$'16.CASA'[16:length(DF2$CASA)] <- rollapply(DF2$OUTRO.CASA*DF2$Peso, 16, sum)/rollapply(DF2$Peso, 16, sum)
  DF2$'8.CASA'[8:length(DF2$CASA)] <- rollapply(DF2$OUTRO.CASA*DF2$Peso, 8, sum)/rollapply(DF2$Peso, 8, sum)
  DF2 <- DF2 %>% mutate(C.CASA=ifelse(`16.CASA` == 0, 0, 2*`8.CASA`-`16.CASA`))
  DF2$'Hull.CASA'[19:length(DF2$Hull.CASA)] <- rollapply(DF2$C.CASA[16:length(DF2$Hull.CASA)]*DF2$Peso[16:length(DF2$Hull.CASA)], 4, sum)/rollapply(DF2$Peso[16:length(DF2$Hull.CASA)], 4, sum)
  
  ##Hull D
  DF2$'16.EMPATE' <- 0
  DF2$'8.EMPATE' <- 0
  DF2$'Hull.EMPATE' <- 0
  DF2$'16.EMPATE'[16:length(DF2$EMPATE)] <- rollapply(DF2$OUTRO.EMPATE*DF2$Peso, 16, sum)/rollapply(DF2$Peso, 16, sum)
  DF2$'8.EMPATE'[8:length(DF2$EMPATE)] <- rollapply(DF2$OUTRO.EMPATE*DF2$Peso, 8, sum)/rollapply(DF2$Peso, 8, sum)
  DF2 <- DF2 %>% mutate(C.EMPATE=ifelse(`16.EMPATE` == 0, 0, 2*`8.EMPATE`-`16.EMPATE`))
  DF2$'Hull.EMPATE'[19:length(DF2$Hull.EMPATE)] <- rollapply(DF2$C.EMPATE[16:length(DF2$Hull.EMPATE)]*DF2$Peso[16:length(DF2$Hull.EMPATE)], 4, sum)/rollapply(DF2$Peso[16:length(DF2$Hull.EMPATE)], 4, sum)
  
  ##Hull A
  DF2$'16.FORA' <- 0
  DF2$'8.FORA' <- 0
  DF2$'Hull.FORA' <- 0
  DF2$'16.FORA'[16:length(DF2$FORA)] <- rollapply(DF2$OUTRO.FORA*DF2$Peso, 16, sum)/rollapply(DF2$Peso, 16, sum)
  DF2$'8.FORA'[8:length(DF2$FORA)] <- rollapply(DF2$OUTRO.FORA*DF2$Peso, 8, sum)/rollapply(DF2$Peso, 8, sum)
  DF2 <- DF2 %>% mutate(C.FORA=ifelse(`16.FORA` == 0, 0, 2*`8.FORA`-`16.FORA`))
  DF2$'Hull.FORA'[19:length(DF2$Hull.FORA)] <- rollapply(DF2$C.FORA[16:length(DF2$Hull.FORA)]*DF2$Peso[16:length(DF2$Hull.FORA)], 4, sum)/rollapply(DF2$Peso[16:length(DF2$Hull.FORA)], 4, sum)
  
  ##Inclinacao
  DF2 <- DF2[!is.na(DF2$OUTRO.CASA),]
  CASA.inclinacao <- c()
  EMPATE.inclinacao <- c()
  FORA.inclinacao <- c()
  
  for(i in 10:(length(DF2$Hull.CASA-1))) {
    CASA.inclinacao[(i-9)] <- lm(DF2$Hull.CASA[(i-9):i]~c(1:10))$coefficients[2]
    EMPATE.inclinacao[(i-9)] <- lm(DF2$Hull.EMPATE[(i-9):i]~DF2$Peso[(i-9):i])$coefficients[2]
    FORA.inclinacao[(i-9)] <- lm(DF2$Hull.FORA[(i-9):i]~DF2$Peso[(i-9):i])$coefficients[2]
  }
  
  CASA.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),CASA.inclinacao)
  EMPATE.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),EMPATE.inclinacao)
  FORA.inclinacao <-c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),FORA.inclinacao)
  
  CASA.inclinacao[1:27] <- NA
  EMPATE.inclinacao[1:27] <- NA
  FORA.inclinacao[1:27] <- NA
  
  Hulls <- DF2 %>% select(Hull.CASA,Hull.EMPATE,Hull.FORA)
  CASA.dif <- rollapply(Hulls$Hull.CASA, 10, mean) - Hulls$Hull.CASA[10:length(Hulls$Hull.CASA)]
  CASA.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),CASA.dif)
  Ddif <- rollapply(Hulls$Hull.EMPATE, 10, mean) - Hulls$Hull.EMPATE[10:length(Hulls$Hull.EMPATE)]
  EMPATE.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),Ddif)
  Adif <- rollapply(Hulls$Hull.FORA, 10, mean) - Hulls$Hull.FORA[10:length(Hulls$Hull.FORA)]
  FORA.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),Adif)
  CASA.dif[1:27] <- NA
  EMPATE.dif[1:27] <- NA
  FORA.dif[1:27] <- NA
  
  CASA.desv_pad <- rollapply(Hulls$Hull.CASA, 10, sd)
  EMPATE.desv_pad <- rollapply(Hulls$Hull.EMPATE, 10, sd)
  FORA.desv_pad <- rollapply(Hulls$Hull.FORA, 10, sd)
  
  
  
  CASA.bb <- rollapply(Hulls$Hull.CASA, 10, mean)+2*CASA.desv_pad-(rollapply(Hulls$Hull.CASA, 10, mean)-2*CASA.desv_pad)
  EMPATE.bb <- rollapply(Hulls$Hull.EMPATE, 10, mean)+2*EMPATE.desv_pad-(rollapply(Hulls$Hull.EMPATE, 10, mean)-2*EMPATE.desv_pad)
  FORA.bb <- rollapply(Hulls$Hull.FORA, 10, mean)+2*FORA.desv_pad-(rollapply(Hulls$Hull.FORA, 10, mean)-2*FORA.desv_pad)
  
  
  CASA.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),CASA.desv_pad)
  EMPATE.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),EMPATE.desv_pad)
  FORA.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),FORA.desv_pad)
  CASA.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),CASA.bb)
  EMPATE.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),EMPATE.bb)
  FORA.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),FORA.bb)
  
  
  CASA.bb[1:27] <- NA
  EMPATE.bb[1:27] <- NA
  FORA.bb[1:27] <- NA
  CASA.desv_pad[1:27] <- NA
  EMPATE.desv_pad[1:27] <- NA
  FORA.desv_pad[1:27] <- NA
  
  
  DF2 <- cbind(DF2,CASA.inclinacao,EMPATE.inclinacao,FORA.inclinacao,CASA.dif,EMPATE.dif,FORA.dif,CASA.desv_pad,
               EMPATE.desv_pad,FORA.desv_pad,CASA.bb,EMPATE.bb,FORA.bb)
  
  DF2 <- DF2 %>% mutate(RESULTADO = ifelse(is.na(CASA.bb),NA,ifelse(CASA>FORA & CASA>EMPATE,'H',
                                                                    ifelse(FORA>CASA & FORA>EMPATE,
                                                                           'A','D'))))
  RESU <- DF2$RESULTADO[2:length(DF2$RESULTADO)]
  RESU[(length(RESU)+1)] <- NA
  RESU[27] <- NA
  
  DF2 <- DF2 %>% mutate(RESULTADO=RESU)
  
  
  DF21 <- DF2[1:27,]
  DF22 <- DF2[!is.na(DF2$EMPATE.inclinacao),]
  DF2 <- rbind(DF21,DF22)
  
  Distancia_Euclidiana <- sqrt((DF2$CASA.inclinacao-DF2$CASA.inclinacao[length(DF2$CASA.inclinacao)])^2+
                                 (DF2$EMPATE.inclinacao-DF2$EMPATE.inclinacao[length(DF2$EMPATE.inclinacao)])^2+
                                 (DF2$FORA.inclinacao-DF2$FORA.inclinacao[length(DF2$FORA.inclinacao)])^2+
                                 (DF2$CASA.dif-DF2$CASA.dif[length(DF2$CASA.dif)])^2)
  
  
  DF2 <- cbind(DF2,Distancia_Euclidiana)
  
  
  RANK <- DF2$Distancia_Euclidiana[!is.na(DF2$Distancia_Euclidiana)]
  RANKING <- rank(RANK)
  temp <- c()
  temp[1:27] <-NA
  RANK <- append(temp,RANKING)
  DF2$RANK <- RANK
  
  Res <- DF2 %>% select(RESULTADO,RANK)
  Res <- Res[!is.na(Res$RANK),]
  Res <- Res[order(Res$RANK),] 
  Res <- Res[2:4,]
  
  return(list(DF1,DF2,Res))
}
BTTS <- function(jogos,input1,input2) {  
  DF1BTTS <- jogos[jogos$jogo_id_campeonato == ligas$league_api_id[ligas$league_name==input1],]
  DF1BTTS <- DF1BTTS[DF1BTTS$ODDS.BTTS.SIM != 0,]
  DF1BTTS <- DF1BTTS %>% select(DATA,'TIME.CASA',	'TIME.VISITANTE',	'GOLS.CASA',
                                'GOLS.VISITANTE',	'RESULTADO',	'ODDS.BTTS.SIM',
                                'ODDS.BTTS.NAO') %>%
    arrange(DATA) %>% mutate(BTTS = ifelse(GOLS.CASA>0 & GOLS.VISITANTE>0,10,-1)) %>%
    mutate('BTTS.SIM' = ifelse(BTTS == 10,100*ODDS.BTTS.SIM-100,-100),
           'BTTS.NAO' = ifelse(BTTS == -1,100*ODDS.BTTS.NAO-100,-100))
  
  if (length(DF1BTTS$DATA)>input2 && input2 >150) {
    DF1BTTS <- DF1BTTS[(length(DF1BTTS$TIME.CASA)-input2):length(DF1BTTS$TIME.CASA),]
  }
  
  DF2BTTS <- DF1BTTS %>% group_by(DATA) %>% summarise('BTTS.SIM'=sum(BTTS.SIM),
                                                      'BTTS.NAO'=sum(BTTS.NAO))
  DF2BTTS <-DF2BTTS %>% mutate(PesoBTTS = 1:nrow(DF2BTTS))
  DF2BTTS <- DF2BTTS %>% select(DATA,BTTS.SIM,BTTS.NAO,PesoBTTS)
  DF2BTTS$SOMA.SIM <- c(0,0,cumsum(DF2BTTS$BTTS.SIM)[3:length(DF2BTTS$BTTS.SIM)])
  DF2BTTS$SOMA.NAO <- c(0,0,cumsum(DF2BTTS$BTTS.NAO)[3:length(DF2BTTS$BTTS.NAO)])
  
  ##Hull H
  DF2BTTS$'16.SIM' <- 0
  DF2BTTS$'8.SIM' <- 0
  DF2BTTS$'Hull.SIM' <- 0
  DF2BTTS$'16.SIM'[16:length(DF2BTTS$BTTS.SIM)] <- rollapply(DF2BTTS$SOMA.SIM*DF2BTTS$PesoBTTS, 16, sum)/rollapply(DF2BTTS$PesoBTTS, 16, sum)
  DF2BTTS$'8.SIM'[8:length(DF2BTTS$BTTS.SIM)] <- rollapply(DF2BTTS$SOMA.SIM*DF2BTTS$PesoBTTS, 8, sum)/rollapply(DF2BTTS$PesoBTTS, 8, sum)
  DF2BTTS <- DF2BTTS %>% mutate(C.SIM=ifelse(`16.SIM` == 0, 0, 2*`8.SIM`-`16.SIM`))
  DF2BTTS$'Hull.SIM'[19:length(DF2BTTS$Hull.SIM)] <- rollapply(DF2BTTS$C.SIM[16:length(DF2BTTS$Hull.SIM)]*DF2BTTS$PesoBTTS[16:length(DF2BTTS$Hull.SIM)], 4, sum)/rollapply(DF2BTTS$PesoBTTS[16:length(DF2BTTS$Hull.SIM)], 4, sum)
  
  ##Hull D
  DF2BTTS$'16.NAO' <- 0
  DF2BTTS$'8.NAO' <- 0
  DF2BTTS$'Hull.NAO' <- 0
  DF2BTTS$'16.NAO'[16:length(DF2BTTS$BTTS.NAO)] <- rollapply(DF2BTTS$SOMA.NAO*DF2BTTS$PesoBTTS, 16, sum)/rollapply(DF2BTTS$PesoBTTS, 16, sum)
  DF2BTTS$'8.NAO'[8:length(DF2BTTS$BTTS.NAO)] <- rollapply(DF2BTTS$SOMA.NAO*DF2BTTS$PesoBTTS, 8, sum)/rollapply(DF2BTTS$PesoBTTS, 8, sum)
  DF2BTTS <- DF2BTTS %>% mutate(C.NAO=ifelse(`16.NAO` == 0, 0, 2*`8.NAO`-`16.NAO`))
  DF2BTTS$'Hull.NAO'[19:length(DF2BTTS$Hull.NAO)] <- rollapply(DF2BTTS$C.NAO[16:length(DF2BTTS$Hull.NAO)]*DF2BTTS$PesoBTTS[16:length(DF2BTTS$Hull.NAO)], 4, sum)/rollapply(DF2BTTS$PesoBTTS[16:length(DF2BTTS$Hull.NAO)], 4, sum)
  
  ##Inclinacao
  DF2BTTS <- DF2BTTS[!is.na(DF2BTTS$SOMA.SIM),]
  SIM.inclinacao <- c()
  NAO.inclinacao <- c()
  
  
  for(i in 10:(length(DF2BTTS$Hull.SIM-1))) {
    SIM.inclinacao[(i-9)] <- lm(DF2BTTS$Hull.SIM[(i-9):i]~c(1:10))$coefficients[2]
    NAO.inclinacao[(i-9)] <- lm(DF2BTTS$Hull.NAO[(i-9):i]~DF2BTTS$PesoBTTS[(i-9):i])$coefficients[2]
  }
  
  SIM.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),SIM.inclinacao)
  NAO.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),NAO.inclinacao)
  
  
  SIM.inclinacao[1:27] <- NA
  NAO.inclinacao[1:27] <- NA
  
  
  Hulls <- DF2BTTS %>% select(Hull.SIM,Hull.NAO)
  SIM.dif <- rollapply(Hulls$Hull.SIM, 10, mean) - Hulls$Hull.SIM[10:length(Hulls$Hull.SIM)]
  SIM.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),SIM.dif)
  Ddif <- rollapply(Hulls$Hull.NAO, 10, mean) - Hulls$Hull.NAO[10:length(Hulls$Hull.NAO)]
  NAO.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),Ddif)
  SIM.dif[1:27] <- NA
  NAO.dif[1:27] <- NA
  
  SIM.desv_pad <- rollapply(Hulls$Hull.SIM, 10, sd)
  NAO.desv_pad <- rollapply(Hulls$Hull.NAO, 10, sd)
  
  
  SIM.bb <- rollapply(Hulls$Hull.SIM, 10, mean)+2*SIM.desv_pad-(rollapply(Hulls$Hull.SIM, 10, mean)-2*SIM.desv_pad)
  NAO.bb <- rollapply(Hulls$Hull.NAO, 10, mean)+2*NAO.desv_pad-(rollapply(Hulls$Hull.NAO, 10, mean)-2*NAO.desv_pad)
  
  
  SIM.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),SIM.desv_pad)
  NAO.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),NAO.desv_pad)
  SIM.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),SIM.bb)
  NAO.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),NAO.bb)
  
  
  SIM.bb[1:27] <- NA
  NAO.bb[1:27] <- NA
  SIM.desv_pad[1:27] <- NA
  NAO.desv_pad[1:27] <- NA
  
  BTSIM <- DF1BTTS$BTTS.SIM[1:length(DF2BTTS$BTTS.SIM)]
  BTNAO <- DF1BTTS$BTTS.NAO[1:length(DF2BTTS$BTTS.SIM)]
  
  DF2BTTS <- cbind(DF2BTTS,SIM.inclinacao,NAO.inclinacao,SIM.dif,NAO.dif,SIM.desv_pad,
                   NAO.desv_pad,SIM.bb,NAO.bb,BTSIM,BTNAO)
  
  DF2BTTS <- DF2BTTS %>% mutate(RESULTADO = ifelse(is.na(SIM.bb),NA,ifelse(BTSIM>BTNAO,'SIM',
                                                                           'NAO')))
  
  RESU <- DF2BTTS$RESULTADO[2:length(DF2BTTS$RESULTADO)]
  RESU[(length(RESU)+1)] <- NA
  RESU[27] <- NA
  
  DF2BTTS <- DF2BTTS %>% mutate(RESULTADO=RESU)
  
  
  DF2BTTS1 <- DF2BTTS[1:27,]
  DF2BTTS2 <- DF2BTTS[!is.na(DF2BTTS$NAO.inclinacao),]
  DF2BTTS <- rbind(DF2BTTS1,DF2BTTS2)
  
  Distancia_Euclidiana <- sqrt((DF2BTTS$SIM.inclinacao-DF2BTTS$SIM.inclinacao[length(DF2BTTS$SIM.inclinacao)])^2+
                                 (DF2BTTS$NAO.inclinacao-DF2BTTS$NAO.inclinacao[length(DF2BTTS$NAO.inclinacao)])^2+
                                 (DF2BTTS$SIM.dif-DF2BTTS$SIM.dif[length(DF2BTTS$SIM.dif)])^2)
  
  
  DF2BTTS <- cbind(DF2BTTS,Distancia_Euclidiana)
  
  
  RANK <- DF2BTTS$Distancia_Euclidiana[!is.na(DF2BTTS$Distancia_Euclidiana)]
  RANKING <- rank(RANK)
  temp <- c()
  temp[1:27] <-NA
  RANK <- append(temp,RANKING)
  DF2BTTS$RANK <- RANK
  
  Res <- DF2BTTS %>% select(RESULTADO,RANK)
  Res <- Res[!is.na(Res$RANK),]
  Res <- Res[order(Res$RANK),] 
  Res <- Res[2:4,]
  
  return(list(DF1BTTS,DF2BTTS,Res))
}
OVER_05 <- function(jogos,input1,input2) {  
  DF1OVER.05 <- jogos[jogos$jogo_id_campeonato == ligas$league_api_id[ligas$league_name==input1],]
  DF1OVER.05 <- DF1OVER.05[DF1OVER.05$ODDS.OVER.05.HT != 0,]
  DF1OVER.05 <- DF1OVER.05 %>% select(DATA,'TIME.CASA',	'TIME.VISITANTE',	'GOLS.CASA.HT',
                                      'GOLS.VISITANTE.HT',	'RESULTADO',	'ODDS.OVER.05.HT',
                                      'ODDS.UNDER.05.HT') %>%
    arrange(DATA) %>% mutate(OVER.05 = ifelse(GOLS.CASA.HT>0 & GOLS.VISITANTE.HT>0,10,-1)) %>%
    mutate('OVER.05.HT' = ifelse(OVER.05 == 10,100*ODDS.OVER.05.HT-100,-100),
           'UNDER.05.HT' = ifelse(OVER.05 == -1,100*ODDS.UNDER.05.HT-100,-100))
  
  if (length(DF1OVER.05$DATA)>input2 && input2 >150) {
    DF1OVER.05 <- DF1OVER.05[(length(DF1OVER.05$TIME.CASA)-input2):length(DF1OVER.05$TIME.CASA),]
  }
  
  DF2OVER.05 <- DF1OVER.05 %>% group_by(DATA) %>% summarise('OVER.05.HT'=sum(OVER.05.HT),
                                                            'UNDER.05.HT'=sum(UNDER.05.HT))
  DF2OVER.05 <-DF2OVER.05 %>% mutate(PesoOVER.05 = 1:nrow(DF2OVER.05))
  DF2OVER.05 <- DF2OVER.05 %>% select(DATA,OVER.05.HT,UNDER.05.HT,PesoOVER.05)
  DF2OVER.05$SOMA.OVER <- c(0,0,cumsum(DF2OVER.05$OVER.05.HT)[3:length(DF2OVER.05$OVER.05.HT)])
  DF2OVER.05$SOMA.UNDER <- c(0,0,cumsum(DF2OVER.05$UNDER.05.HT)[3:length(DF2OVER.05$UNDER.05.HT)])
  
  ##Hull OVER
  DF2OVER.05$'16.OVER' <- 0
  DF2OVER.05$'8.OVER' <- 0
  DF2OVER.05$'Hull.OVER' <- 0
  DF2OVER.05$'16.OVER'[16:length(DF2OVER.05$OVER.05.HT)] <- rollapply(DF2OVER.05$SOMA.OVER*DF2OVER.05$PesoOVER.05, 16, sum)/rollapply(DF2OVER.05$PesoOVER.05, 16, sum)
  DF2OVER.05$'8.OVER'[8:length(DF2OVER.05$OVER.05.HT)] <- rollapply(DF2OVER.05$SOMA.OVER*DF2OVER.05$PesoOVER.05, 8, sum)/rollapply(DF2OVER.05$PesoOVER.05, 8, sum)
  DF2OVER.05 <- DF2OVER.05 %>% mutate(C.OVER=ifelse(`16.OVER` == 0, 0, 2*`8.OVER`-`16.OVER`))
  DF2OVER.05$'Hull.OVER'[19:length(DF2OVER.05$Hull.OVER)] <- rollapply(DF2OVER.05$C.OVER[16:length(DF2OVER.05$Hull.OVER)]*DF2OVER.05$PesoOVER.05[16:length(DF2OVER.05$Hull.OVER)], 4, sum)/rollapply(DF2OVER.05$PesoOVER.05[16:length(DF2OVER.05$Hull.OVER)], 4, sum)
  
  ##Hull UNDER
  DF2OVER.05$'16.UNDER' <- 0
  DF2OVER.05$'8.UNDER' <- 0
  DF2OVER.05$'Hull.UNDER' <- 0
  DF2OVER.05$'16.UNDER'[16:length(DF2OVER.05$UNDER.05.HT)] <- rollapply(DF2OVER.05$SOMA.UNDER*DF2OVER.05$PesoOVER.05, 16, sum)/rollapply(DF2OVER.05$PesoOVER.05, 16, sum)
  DF2OVER.05$'8.UNDER'[8:length(DF2OVER.05$UNDER.05.HT)] <- rollapply(DF2OVER.05$SOMA.UNDER*DF2OVER.05$PesoOVER.05, 8, sum)/rollapply(DF2OVER.05$PesoOVER.05, 8, sum)
  DF2OVER.05 <- DF2OVER.05 %>% mutate(C.UNDER=ifelse(`16.UNDER` == 0, 0, 2*`8.UNDER`-`16.UNDER`))
  DF2OVER.05$'Hull.UNDER'[19:length(DF2OVER.05$Hull.UNDER)] <- rollapply(DF2OVER.05$C.UNDER[16:length(DF2OVER.05$Hull.UNDER)]*DF2OVER.05$PesoOVER.05[16:length(DF2OVER.05$Hull.UNDER)], 4, sum)/rollapply(DF2OVER.05$PesoOVER.05[16:length(DF2OVER.05$Hull.UNDER)], 4, sum)
  
  ##Inclinacao
  DF2OVER.05 <- DF2OVER.05[!is.na(DF2OVER.05$SOMA.OVER),]
  OVER.inclinacao <- c()
  UNDER.inclinacao <- c()
  
  
  for(i in 10:(length(DF2OVER.05$Hull.OVER-1))) {
    OVER.inclinacao[(i-9)] <- lm(DF2OVER.05$Hull.OVER[(i-9):i]~c(1:10))$coefficients[2]
    UNDER.inclinacao[(i-9)] <- lm(DF2OVER.05$Hull.UNDER[(i-9):i]~DF2OVER.05$PesoOVER.05[(i-9):i])$coefficients[2]
  }
  
  OVER.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.inclinacao)
  UNDER.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.inclinacao)
  
  
  OVER.inclinacao[1:27] <- NA
  UNDER.inclinacao[1:27] <- NA
  
  
  Hulls <- DF2OVER.05 %>% select(Hull.OVER,Hull.UNDER)
  OVER.dif <- rollapply(Hulls$Hull.OVER, 10, mean) - Hulls$Hull.OVER[10:length(Hulls$Hull.OVER)]
  OVER.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.dif)
  UNDER.dif <- rollapply(Hulls$Hull.UNDER, 10, mean) - Hulls$Hull.UNDER[10:length(Hulls$Hull.UNDER)]
  UNDER.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.dif)
  OVER.dif[1:27] <- NA
  UNDER.dif[1:27] <- NA
  
  OVER.desv_pad <- rollapply(Hulls$Hull.OVER, 10, sd)
  UNDER.desv_pad <- rollapply(Hulls$Hull.UNDER, 10, sd)
  
  
  OVER.bb <- rollapply(Hulls$Hull.OVER, 10, mean)+2*OVER.desv_pad-(rollapply(Hulls$Hull.OVER, 10, mean)-2*OVER.desv_pad)
  UNDER.bb <- rollapply(Hulls$Hull.UNDER, 10, mean)+2*UNDER.desv_pad-(rollapply(Hulls$Hull.UNDER, 10, mean)-2*UNDER.desv_pad)
  
  
  OVER.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.desv_pad)
  UNDER.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.desv_pad)
  OVER.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.bb)
  UNDER.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.bb)
  
  
  OVER.bb[1:27] <- NA
  UNDER.bb[1:27] <- NA
  OVER.desv_pad[1:27] <- NA
  UNDER.desv_pad[1:27] <- NA
  
  OVER <- DF1OVER.05$OVER.05.HT[1:length(DF2OVER.05$OVER.05.HT)]
  UNDER <- DF1OVER.05$UNDER.05.HT[1:length(DF2OVER.05$UNDER.05.HT)]
  
  DF2OVER.05 <- cbind(DF2OVER.05,OVER.inclinacao,UNDER.inclinacao,OVER.dif,UNDER.dif,OVER.desv_pad,
                      UNDER.desv_pad,OVER.bb,UNDER.bb,OVER,UNDER)
  
  DF2OVER.05 <- DF2OVER.05 %>% mutate(RESULTADO = ifelse(is.na(OVER.bb),NA,ifelse(OVER>UNDER,'OVER',
                                                                                  'UNDER')))
  
  RESU <- DF2OVER.05$RESULTADO[2:length(DF2OVER.05$RESULTADO)]
  RESU[(length(RESU)+1)] <- NA
  RESU[27] <- NA
  
  DF2OVER.05 <- DF2OVER.05 %>% mutate(RESULTADO=RESU)
  
  
  DF2OVER.051 <- DF2OVER.05[1:27,]
  DF2OVER.052 <- DF2OVER.05[!is.na(DF2OVER.05$UNDER.inclinacao),]
  DF2OVER.05 <- rbind(DF2OVER.051,DF2OVER.052)
  
  Distancia_Euclidiana <- sqrt((DF2OVER.05$OVER.inclinacao-DF2OVER.05$OVER.inclinacao[length(DF2OVER.05$OVER.inclinacao)])^2+
                                 (DF2OVER.05$UNDER.inclinacao-DF2OVER.05$UNDER.inclinacao[length(DF2OVER.05$UNDER.inclinacao)])^2+
                                 (DF2OVER.05$OVER.dif-DF2OVER.05$OVER.dif[length(DF2OVER.05$OVER.dif)])^2)
  
  
  DF2OVER.05 <- cbind(DF2OVER.05,Distancia_Euclidiana)
  
  
  RANK <- DF2OVER.05$Distancia_Euclidiana[!is.na(DF2OVER.05$Distancia_Euclidiana)]
  RANKING <- rank(RANK)
  temp <- c()
  temp[1:27] <-NA
  RANK <- append(temp,RANKING)
  DF2OVER.05$RANK <- RANK
  
  Res <- DF2OVER.05 %>% select(RESULTADO,RANK)
  Res <- Res[!is.na(Res$RANK),]
  Res <- Res[order(Res$RANK),] 
  Res <- Res[2:4,]
  
  return(list(DF1OVER.05,DF2OVER.05,Res))
}
OVER_15 <- function(jogos,input1,input2) {  
  DF1OVER.15 <- jogos[jogos$jogo_id_campeonato == ligas$league_api_id[ligas$league_name==input1],]
  DF1OVER.15 <- DF1OVER.15[DF1OVER.15$ODDS.FT.OVER15 != 0,]
  DF1OVER.15 <- DF1OVER.15 %>% select(DATA,'TIME.CASA',	'TIME.VISITANTE',	'GOLS.CASA',
                                      'GOLS.VISITANTE',	'RESULTADO',	'ODDS.FT.OVER15',
                                      'ODDS.FT.UNDER15') %>%
    arrange(DATA) %>% mutate(OVER.15 = ifelse(GOLS.CASA>0 & GOLS.VISITANTE>0,10,-1)) %>%
    mutate('OVER.15.FT' = ifelse(OVER.15 == 10,100*ODDS.FT.OVER15-100,-100),
           'UNDER.15.FT' = ifelse(OVER.15 == -1,100*ODDS.FT.UNDER15-100,-100))
  
  if (length(DF1OVER.15$DATA)>input2 && input2 >150) {
    DF1OVER.15 <- DF1OVER.15[(length(DF1OVER.15$TIME.CASA)-input2):length(DF1OVER.15$TIME.CASA),]
  }
  
  DF2OVER.15 <- DF1OVER.15 %>% group_by(DATA) %>% summarise('OVER.15.FT'=sum(OVER.15.FT),
                                                            'UNDER.15.FT'=sum(UNDER.15.FT))
  DF2OVER.15 <-DF2OVER.15 %>% mutate(PesoOVER.15 = 1:nrow(DF2OVER.15))
  DF2OVER.15 <- DF2OVER.15 %>% select(DATA,OVER.15.FT,UNDER.15.FT,PesoOVER.15)
  DF2OVER.15$SOMA.OVER <- c(0,0,cumsum(DF2OVER.15$OVER.15.FT)[3:length(DF2OVER.15$OVER.15.FT)])
  DF2OVER.15$SOMA.UNDER <- c(0,0,cumsum(DF2OVER.15$UNDER.15.FT)[3:length(DF2OVER.15$UNDER.15.FT)])
  
  ##Hull OVER
  DF2OVER.15$'16.OVER' <- 0
  DF2OVER.15$'8.OVER' <- 0
  DF2OVER.15$'Hull.OVER' <- 0
  DF2OVER.15$'16.OVER'[16:length(DF2OVER.15$OVER.15.FT)] <- rollapply(DF2OVER.15$SOMA.OVER*DF2OVER.15$PesoOVER.15, 16, sum)/rollapply(DF2OVER.15$PesoOVER.15, 16, sum)
  DF2OVER.15$'8.OVER'[8:length(DF2OVER.15$OVER.15.FT)] <- rollapply(DF2OVER.15$SOMA.OVER*DF2OVER.15$PesoOVER.15, 8, sum)/rollapply(DF2OVER.15$PesoOVER.15, 8, sum)
  DF2OVER.15 <- DF2OVER.15 %>% mutate(C.OVER=ifelse(`16.OVER` == 0, 0, 2*`8.OVER`-`16.OVER`))
  DF2OVER.15$'Hull.OVER'[19:length(DF2OVER.15$Hull.OVER)] <- rollapply(DF2OVER.15$C.OVER[16:length(DF2OVER.15$Hull.OVER)]*DF2OVER.15$PesoOVER.15[16:length(DF2OVER.15$Hull.OVER)], 4, sum)/rollapply(DF2OVER.15$PesoOVER.15[16:length(DF2OVER.15$Hull.OVER)], 4, sum)
  
  ##Hull UNDER
  DF2OVER.15$'16.UNDER' <- 0
  DF2OVER.15$'8.UNDER' <- 0
  DF2OVER.15$'Hull.UNDER' <- 0
  DF2OVER.15$'16.UNDER'[16:length(DF2OVER.15$UNDER.15.FT)] <- rollapply(DF2OVER.15$SOMA.UNDER*DF2OVER.15$PesoOVER.15, 16, sum)/rollapply(DF2OVER.15$PesoOVER.15, 16, sum)
  DF2OVER.15$'8.UNDER'[8:length(DF2OVER.15$UNDER.15.FT)] <- rollapply(DF2OVER.15$SOMA.UNDER*DF2OVER.15$PesoOVER.15, 8, sum)/rollapply(DF2OVER.15$PesoOVER.15, 8, sum)
  DF2OVER.15 <- DF2OVER.15 %>% mutate(C.UNDER=ifelse(`16.UNDER` == 0, 0, 2*`8.UNDER`-`16.UNDER`))
  DF2OVER.15$'Hull.UNDER'[19:length(DF2OVER.15$Hull.UNDER)] <- rollapply(DF2OVER.15$C.UNDER[16:length(DF2OVER.15$Hull.UNDER)]*DF2OVER.15$PesoOVER.15[16:length(DF2OVER.15$Hull.UNDER)], 4, sum)/rollapply(DF2OVER.15$PesoOVER.15[16:length(DF2OVER.15$Hull.UNDER)], 4, sum)
  
  ##Inclinacao
  DF2OVER.15 <- DF2OVER.15[!is.na(DF2OVER.15$SOMA.OVER),]
  OVER.inclinacao <- c()
  UNDER.inclinacao <- c()
  
  
  for(i in 10:(length(DF2OVER.15$Hull.OVER-1))) {
    OVER.inclinacao[(i-9)] <- lm(DF2OVER.15$Hull.OVER[(i-9):i]~c(1:10))$coefficients[2]
    UNDER.inclinacao[(i-9)] <- lm(DF2OVER.15$Hull.UNDER[(i-9):i]~DF2OVER.15$PesoOVER.15[(i-9):i])$coefficients[2]
  }
  
  OVER.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.inclinacao)
  UNDER.inclinacao <- c(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.inclinacao)
  
  
  OVER.inclinacao[1:27] <- NA
  UNDER.inclinacao[1:27] <- NA
  
  
  Hulls <- DF2OVER.15 %>% select(Hull.OVER,Hull.UNDER)
  OVER.dif <- rollapply(Hulls$Hull.OVER, 10, mean) - Hulls$Hull.OVER[10:length(Hulls$Hull.OVER)]
  OVER.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.dif)
  UNDER.dif <- rollapply(Hulls$Hull.UNDER, 10, mean) - Hulls$Hull.UNDER[10:length(Hulls$Hull.UNDER)]
  UNDER.dif <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.dif)
  OVER.dif[1:27] <- NA
  UNDER.dif[1:27] <- NA
  
  OVER.desv_pad <- rollapply(Hulls$Hull.OVER, 10, sd)
  UNDER.desv_pad <- rollapply(Hulls$Hull.UNDER, 10, sd)
  
  
  OVER.bb <- rollapply(Hulls$Hull.OVER, 10, mean)+2*OVER.desv_pad-(rollapply(Hulls$Hull.OVER, 10, mean)-2*OVER.desv_pad)
  UNDER.bb <- rollapply(Hulls$Hull.UNDER, 10, mean)+2*UNDER.desv_pad-(rollapply(Hulls$Hull.UNDER, 10, mean)-2*UNDER.desv_pad)
  
  
  OVER.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.desv_pad)
  UNDER.desv_pad <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.desv_pad)
  OVER.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),OVER.bb)
  UNDER.bb <- append(c(NA,NA,NA,NA,NA,NA,NA,NA,NA),UNDER.bb)
  
  
  OVER.bb[1:27] <- NA
  UNDER.bb[1:27] <- NA
  OVER.desv_pad[1:27] <- NA
  UNDER.desv_pad[1:27] <- NA
  
  OVER <- DF1OVER.15$OVER.15.FT[1:length(DF2OVER.15$OVER.15.FT)]
  UNDER <- DF1OVER.15$UNDER.15.FT[1:length(DF2OVER.15$UNDER.15.FT)]
  
  DF2OVER.15 <- cbind(DF2OVER.15,OVER.inclinacao,UNDER.inclinacao,OVER.dif,UNDER.dif,OVER.desv_pad,
                      UNDER.desv_pad,OVER.bb,UNDER.bb,OVER,UNDER)
  
  DF2OVER.15 <- DF2OVER.15 %>% mutate(RESULTADO = ifelse(is.na(OVER.bb),NA,ifelse(OVER>UNDER,'OVER',
                                                                                  'UNDER')))
  
  RESU <- DF2OVER.15$RESULTADO[2:length(DF2OVER.15$RESULTADO)]
  RESU[(length(RESU)+1)] <- NA
  RESU[27] <- NA
  
  DF2OVER.15 <- DF2OVER.15 %>% mutate(RESULTADO=RESU)
  
  
  DF2OVER.151 <- DF2OVER.15[1:27,]
  DF2OVER.152 <- DF2OVER.15[!is.na(DF2OVER.15$UNDER.inclinacao),]
  DF2OVER.15 <- rbind(DF2OVER.151,DF2OVER.152)
  
  Distancia_Euclidiana <- sqrt((DF2OVER.15$OVER.inclinacao-DF2OVER.15$OVER.inclinacao[length(DF2OVER.15$OVER.inclinacao)])^2+
                                 (DF2OVER.15$UNDER.inclinacao-DF2OVER.15$UNDER.inclinacao[length(DF2OVER.15$UNDER.inclinacao)])^2+
                                 (DF2OVER.15$OVER.dif-DF2OVER.15$OVER.dif[length(DF2OVER.15$OVER.dif)])^2)
  
  
  DF2OVER.15 <- cbind(DF2OVER.15,Distancia_Euclidiana)
  
  
  RANK <- DF2OVER.15$Distancia_Euclidiana[!is.na(DF2OVER.15$Distancia_Euclidiana)]
  RANKING <- rank(RANK)
  temp <- c()
  temp[1:27] <-NA
  RANK <- append(temp,RANKING)
  DF2OVER.15$RANK <- RANK
  
  Res <- DF2OVER.15 %>% select(RESULTADO,RANK)
  Res <- Res[!is.na(Res$RANK),]
  Res <- Res[order(Res$RANK),] 
  Res <- Res[2:4,]
  
  return(list(DF1OVER.15,DF2OVER.15,Res))
}
LAY_OVER <- function(jogos,input1,input2) {  
  DF1LAYOVER <- jogos[jogos$jogo_id_campeonato == ligas$league_api_id[ligas$league_name==input1],]
  DF1LAYOVER <- DF1LAYOVER %>% select(DATA,'TIME.CASA',	'TIME.VISITANTE',	'GOLS.CASA',
                                      'GOLS.VISITANTE',	'RESULTADO') %>%
    arrange(DATA) %>% mutate(UNDER.05 = ifelse(GOLS.CASA+GOLS.VISITANTE>0,-1,10))
  
  if (length(DF1LAYOVER$DATA)>input2 && input2 >150) {
    DF1LAYOVER <- DF1LAYOVER[(length(DF1LAYOVER$TIME.CASA)-input2):length(DF1LAYOVER$TIME.CASA),]
  }
  
  DF2LAYOVER <- DF1LAYOVER %>% group_by(DATA) %>% summarise('DIARIO'=sum(UNDER.05))
  
  DF2LAYOVER <-DF2LAYOVER %>% mutate(PesoLAYOVER = 1:nrow(DF2LAYOVER))
  DF2LAYOVER <- DF2LAYOVER %>% select(DATA,DIARIO,PesoLAYOVER)
  DF2LAYOVER$ACUMULADO <- cumsum(DF2LAYOVER$DIARIO)
  
  ##Hull H
  DF2LAYOVER$'16.05' <- 0
  DF2LAYOVER$'8.05' <- 0
  DF2LAYOVER$'Hull.05' <- 0
  DF2LAYOVER$'16.05'[16:length(DF2LAYOVER$ACUMULADO)] <- rollapply(DF2LAYOVER$ACUMULADO*DF2LAYOVER$PesoLAYOVER, 16, sum)/rollapply(DF2LAYOVER$PesoLAYOVER, 16, sum)
  DF2LAYOVER$'8.05'[8:length(DF2LAYOVER$ACUMULADO)] <- rollapply(DF2LAYOVER$ACUMULADO*DF2LAYOVER$PesoLAYOVER, 8, sum)/rollapply(DF2LAYOVER$PesoLAYOVER, 8, sum)
  DF2LAYOVER <- DF2LAYOVER %>% mutate(C.05=ifelse(`16.05` == 0, 0, 2*`8.05`-`16.05`))
  DF2LAYOVER$'Hull.05'[19:length(DF2LAYOVER$Hull.05)] <- rollapply(DF2LAYOVER$C.05[16:length(DF2LAYOVER$Hull.05)]*DF2LAYOVER$PesoLAYOVER[16:length(DF2LAYOVER$Hull.05)], 4, sum)/rollapply(DF2LAYOVER$PesoLAYOVER[16:length(DF2LAYOVER$Hull.05)], 4, sum)
  
  ##plot
  
  LODF <- DF2LAYOVER[19:length(DF2LAYOVER$Hull.05),] %>% select(Hull.05,ACUMULADO)
  LODF <- LODF %>% mutate(" " = 1:nrow(LODF))
  LODF <- melt(LODF,id=' ')
  
  LVPLOT <- ggplot(data = LODF,aes(x=` `,y = value,group=variable,color=variable)) + 
    geom_line(size=1)+
    ggtitle("HULL 05 E ACUMULADO") +
    ylab("")
  
  return(list(DF1LAYOVER,DF2LAYOVER,LVPLOT))
}
##------------------------------------UI----------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "BET Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estatisticas", icon = icon("futbol"),
               menuSubItem('Matchodds', tabName = 'Matchodds'),
               menuSubItem('BTTS', tabName = 'BTTS'),
               menuSubItem('OVER 05 HT', tabName = 'OVER_05'),
               menuSubItem('OVER 15 FT', tabName = 'OVER_15'),
               menuSubItem('LAY OVER - GRAFICO', tabName = 'LAY_OVER')
               ),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      actionButton (inputId = "atualizar",label = "ATUALIZAR BANCO DE DADOS")
    )),
  dashboardBody(
##------------------------------UI MATCHODDS------------------------------------
    tabItems(
      tabItem("Matchodds",fluidRow(
        column(
          htmlOutput("league_logo"), width = 2
        ),
        column(
          selectInput(inputId = "league",label = "LIGA",choices = ligas$league_name,width = 500),style ="font-size:100%; font-family:Arial;padding:10px",width = 3,offset = 0
        ),
        column(
          numericInput(inputId = "registros",label = "REGISTROS", value = 1000),style ="font-size:100%; font-family:Arial;padding:10px" ,width = 1
        ),
        column(
          actionButton (inputId = "buscar",label = "BUSCAR"),style ="font-size:130%; font-family:Arial;padding-top:40px" ,width = 1
        )),
        tags$hr(style="border-color: grey;"),
        fluidRow(
          column(valueBoxOutput('num1',width = 12),width = 4),
          column(valueBoxOutput('num2',width = 12),width = 4),
          column(valueBoxOutput('num3',width = 12),width = 4)
        ),
        fluidRow(
          box(
            dataTableOutput("table1"),style = "overflow-y: scroll;overflow-x: scroll;",
            width = 12)),
        fluidRow(
          box(
            dataTableOutput("table2"),style = "overflow-y: scroll;overflow-x: scroll;",
            width = 12))),
##---------------------------------UI BTTS--------------------------------------
      tabItem("BTTS",fluidRow(
        column(
          htmlOutput("league_logoBTTS"), width = 2
        ),
        column(
          selectInput(inputId = "leagueBTTS",label = "LIGA",choices = ligas$league_name,width = 500),style ="font-size:100%; font-family:Arial;padding:10px",width = 3,offset = 0
        ),
        column(
          numericInput(inputId = "registrosBTTS",label = "REGISTROS", value = 1000),style ="font-size:100%; font-family:Arial;padding:10px" ,width = 1
        ),
        column(
          actionButton (inputId = "buscarBTTS",label = "BUSCAR"),style ="font-size:130%; font-family:Arial;padding-top:40px" ,width = 1
        )
      ),
      tags$hr(style="border-color: grey;"),
      fluidRow(
        column(valueBoxOutput('num1BTTS',width = 12),width = 4),
        column(valueBoxOutput('num2BTTS',width = 12),width = 4),
        column(valueBoxOutput('num3BTTS',width = 12),width = 4)),
      fluidRow(
        box(
          dataTableOutput("table1BTTS"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12)),
      fluidRow(
        box(
          dataTableOutput("table2BTTS"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12))),
#--------------------------------UI OVER 05 HT----------------------------------
      tabItem("OVER_05",fluidRow(
        column(
          htmlOutput("league_logoOVER_05"), width = 2
        ),
        column(
          selectInput(inputId = "leagueOVER_05",label = "LIGA",choices = ligas$league_name,width = 500),style ="font-size:100%; font-family:Arial;padding:10px",width = 3,offset = 0
        ),
        column(
          numericInput(inputId = "registrosOVER_05",label = "REGISTROS", value = 1000),style ="font-size:100%; font-family:Arial;padding:10px" ,width = 1
        ),
        column(
          actionButton (inputId = "buscarOVER_05",label = "BUSCAR"),style ="font-size:130%; font-family:Arial;padding-top:40px" ,width = 1
        )
      ),
      tags$hr(style="border-color: grey;"),
      fluidRow(
        column(valueBoxOutput('num1OVER_05',width = 12),width = 4),
        column(valueBoxOutput('num2OVER_05',width = 12),width = 4),
        column(valueBoxOutput('num3OVER_05',width = 12),width = 4)),
      fluidRow(
        box(
          dataTableOutput("table1OVER_05"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12)),
      fluidRow(
        box(
          dataTableOutput("table2OVER_05"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12))),
#---------------------------------UI OVER 15------------------------------------
      tabItem("OVER_15",fluidRow(
        column(
          htmlOutput("league_logoOVER_15"), width = 2
        ),
        column(
          selectInput(inputId = "leagueOVER_15",label = "LIGA",choices = ligas$league_name,width = 500),style ="font-size:100%; font-family:Arial;padding:10px",width = 3,offset = 0
        ),
        column(
          numericInput(inputId = "registrosOVER_15",label = "REGISTROS", value = 1000),style ="font-size:100%; font-family:Arial;padding:10px" ,width = 1
        ),
        column(
          actionButton (inputId = "buscarOVER_15",label = "BUSCAR"),style ="font-size:130%; font-family:Arial;padding-top:40px" ,width = 1
        )
      ),
      tags$hr(style="border-color: grey;"),
      fluidRow(
        column(valueBoxOutput('num1OVER_15',width = 12),width = 4),
        column(valueBoxOutput('num2OVER_15',width = 12),width = 4),
        column(valueBoxOutput('num3OVER_15',width = 12),width = 4)),
      fluidRow(
        box(
          dataTableOutput("table1OVER_15"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12)),
      fluidRow(
        box(
          dataTableOutput("table2OVER_15"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12))),

#---------------------------------UI LAY OVER-----------------------------------
      tabItem("LAY_OVER",fluidRow(
        column(
          htmlOutput("league_logoLAY_OVER"), width = 2
        ),
        column(
          selectInput(inputId = "leagueLAY_OVER",label = "LIGA",choices = ligas$league_name,width = 500),style ="font-size:100%; font-family:Arial;padding:10px",width = 3,offset = 0
        ),
        column(
          numericInput(inputId = "registrosLAY_OVER",label = "REGISTROS", value = 1000),style ="font-size:100%; font-family:Arial;padding:10px" ,width = 1
        ),
        column(
          actionButton (inputId = "buscarLAY_OVER",label = "BUSCAR"),style ="font-size:130%; font-family:Arial;padding-top:40px" ,width = 1
        )
      ),
      tags$hr(style="border-color: grey;"),
      fluidRow(
        box(
          plotOutput("plotLAY_OVER"), width = 12
        )
        ),
      fluidRow(
        box(
          dataTableOutput("table1LAY_OVER"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12)),
      fluidRow(
        box(
          dataTableOutput("table2LAY_OVER"),style = "overflow-y: scroll;overflow-x: scroll;",
          width = 12)))
    )
  )
)
#-----------------------------------SERVER--------------------------------------
server <- function(input,output) {
  
#-----------------------------SERVER MATCH ODDS---------------------------------
  currentjogos <- reactiveVal()
  currentcalc <- reactiveVal()
  currentresultado <- reactiveValues()
  
  buscar<-observeEvent(input$buscar,{
    DF <- matchodds(jogos,input$league,input$registros)
    currentjogos(DF[[1]])
    currentcalc(DF[[2]])
    currentresultado$a<-DF[[3]][1,1]
    currentresultado$b<-DF[[3]][2,1]
    currentresultado$c<-DF[[3]][3,1]
  })
  df1 <- eventReactive(input$buscar,{datatable(currentjogos(), options = list(paging = FALSE,dom='t'))})
  df2 <- eventReactive(input$buscar,{datatable(currentcalc(),options = list(paging = FALSE,dom='t'))})
  if1 <- eventReactive(input$buscar,valueBox(currentresultado$a,"1#",icon("trophy"),color="yellow"))
  if2 <- eventReactive(input$buscar,valueBox(currentresultado$b,"2#",icon("medal"),color="light-blue"))
  if3 <- eventReactive(input$buscar,valueBox(currentresultado$c,"3#",icon("award"),color="orange"))
  output$table1 <-renderDT({df1()})
  output$table2 <-renderDT({df2()})
  output$num1 <- renderValueBox(if1())
  output$num2 <- renderValueBox(if2())
  output$num3 <- renderValueBox(if3())
  output$league_logo <-renderText({c('<img src="',ligas$league.logo[ligas$league_api_id==ligas$league_api_id[ligas$league_name==input$league]],'">')})
  
  
#--------------------------------SERVER BTTS------------------------------------
  currentjogosBTTS <- reactiveVal()
  currentcalcBTTS <- reactiveVal()
  currentresultadoBTTS <- reactiveValues()
  buscarBTTS<-observeEvent(input$buscarBTTS,{
    DFBTTS <- BTTS(jogos,input$leagueBTTS,input$registrosBTTS)
    currentjogosBTTS(DFBTTS[[1]])
    currentcalcBTTS(DFBTTS[[2]])
    currentresultadoBTTS$a<-DFBTTS[[3]][1,1]
    currentresultadoBTTS$b<-DFBTTS[[3]][2,1]
    currentresultadoBTTS$c<-DFBTTS[[3]][3,1]
  })
  df1BTTS <- eventReactive(input$buscarBTTS,{datatable(currentjogosBTTS(), options = list(paging = FALSE,dom='t'))})
  df2BTTS <- eventReactive(input$buscarBTTS,{datatable(currentcalcBTTS(),options = list(paging = FALSE,dom='t'))})
  if1BTTS <- eventReactive(input$buscarBTTS,valueBox(isolate(currentresultadoBTTS$a),"1#",icon("trophy"),color="yellow"))
  if2BTTS <- eventReactive(input$buscarBTTS,valueBox(isolate(currentresultadoBTTS$b),"2#",icon("medal"),color="light-blue"))
  if3BTTS <- eventReactive(input$buscarBTTS,valueBox(isolate(currentresultadoBTTS$c),"3#",icon("award"),color="orange"))
  output$table1BTTS <-renderDT({df1BTTS()})
  output$table2BTTS <-renderDT({df2BTTS()})
  output$num1BTTS <- renderValueBox(if1BTTS())
  output$num2BTTS <- renderValueBox(if2BTTS())
  output$num3BTTS <- renderValueBox(if3BTTS())
  output$league_logoBTTS <-renderText({c('<img src="',ligas$league.logo[ligas$league_api_id==ligas$league_api_id[ligas$league_name==input$leagueBTTS]],'">')})
#-------------------------------SERVER OVER 05----------------------------------
  currentjogosOVER_05 <- reactiveVal()
  currentcalcOVER_05 <- reactiveVal()
  currentresultadoOVER_05 <- reactiveValues()
  buscarOVER_05<-observeEvent(input$buscarOVER_05,{
    DFOVER_05 <- OVER_05(jogos,input$leagueOVER_05,input$registrosOVER_05)
    currentjogosOVER_05(DFOVER_05[[1]])
    currentcalcOVER_05(DFOVER_05[[2]])
    currentresultadoOVER_05$a<-DFOVER_05[[3]][1,1]
    currentresultadoOVER_05$b<-DFOVER_05[[3]][2,1]
    currentresultadoOVER_05$c<-DFOVER_05[[3]][3,1]
  })
  df1OVER_05 <- eventReactive(input$buscarOVER_05,{datatable(currentjogosOVER_05(), options = list(paging = FALSE,dom='t'))})
  df2OVER_05 <- eventReactive(input$buscarOVER_05,{datatable(currentcalcOVER_05(),options = list(paging = FALSE,dom='t'))})
  if1OVER_05 <- eventReactive(input$buscarOVER_05,valueBox(isolate(currentresultadoOVER_05$a),"1#",icon("trophy"),color="yellow"))
  if2OVER_05 <- eventReactive(input$buscarOVER_05,valueBox(isolate(currentresultadoOVER_05$b),"2#",icon("medal"),color="light-blue"))
  if3OVER_05 <- eventReactive(input$buscarOVER_05,valueBox(isolate(currentresultadoOVER_05$c),"3#",icon("award"),color="orange"))
  output$table1OVER_05 <-renderDT({df1OVER_05()})
  output$table2OVER_05 <-renderDT({df2OVER_05()})
  output$num1OVER_05 <- renderValueBox(if1OVER_05())
  output$num2OVER_05 <- renderValueBox(if2OVER_05())
  output$num3OVER_05 <- renderValueBox(if3OVER_05())
  output$league_logoOVER_05 <-renderText({c('<img src="',ligas$league.logo[ligas$league_api_id==ligas$league_api_id[ligas$league_name==input$leagueOVER_05]],'">')})
#-------------------------------SERVER OVER 15----------------------------------
  currentjogosOVER_15 <- reactiveVal()
  currentcalcOVER_15 <- reactiveVal()
  currentresultadoOVER_15 <- reactiveValues()
  buscarOVER_15<-observeEvent(input$buscarOVER_15,{
    DFOVER_15 <- OVER_15(jogos,input$leagueOVER_15,input$registrosOVER_15)
    currentjogosOVER_15(DFOVER_15[[1]])
    currentcalcOVER_15(DFOVER_15[[2]])
    currentresultadoOVER_15$a<-DFOVER_15[[3]][1,1]
    currentresultadoOVER_15$b<-DFOVER_15[[3]][2,1]
    currentresultadoOVER_15$c<-DFOVER_15[[3]][3,1]
  })
  df1OVER_15 <- eventReactive(input$buscarOVER_15,{datatable(currentjogosOVER_15(), options = list(paging = FALSE,dom='t'))})
  df2OVER_15 <- eventReactive(input$buscarOVER_15,{datatable(currentcalcOVER_15(),options = list(paging = FALSE,dom='t'))})
  if1OVER_15 <- eventReactive(input$buscarOVER_15,valueBox(isolate(currentresultadoOVER_15$a),"1#",icon("trophy"),color="yellow"))
  if2OVER_15 <- eventReactive(input$buscarOVER_15,valueBox(isolate(currentresultadoOVER_15$b),"2#",icon("medal"),color="light-blue"))
  if3OVER_15 <- eventReactive(input$buscarOVER_15,valueBox(isolate(currentresultadoOVER_15$c),"3#",icon("award"),color="orange"))
  output$table1OVER_15 <-renderDT({df1OVER_15()})
  output$table2OVER_15 <-renderDT({df2OVER_15()})
  output$num1OVER_15 <- renderValueBox(if1OVER_15())
  output$num2OVER_15 <- renderValueBox(if2OVER_15())
  output$num3OVER_15 <- renderValueBox(if3OVER_15())
  output$league_logoOVER_15 <-renderText({c('<img src="',ligas$league.logo[ligas$league_api_id==ligas$league_api_id[ligas$league_name==input$leagueOVER_15]],'">')})
#-------------------------------SERVER LAY OVER---------------------------------
  currentjogosLAY_OVER <- reactiveVal()
  currentcalcLAY_OVER <- reactiveVal()
  currentresultadoLAY_OVER <- reactiveVal()
  buscarLAY_OVER<-observeEvent(input$buscarLAY_OVER,{
    DFLAY_OVER <- LAY_OVER(jogos,input$leagueLAY_OVER,input$registrosLAY_OVER)
    currentjogosLAY_OVER(DFLAY_OVER[[1]])
    currentcalcLAY_OVER(DFLAY_OVER[[2]])
    currentresultadoLAY_OVER(DFLAY_OVER[[3]])
  })
  df1LAY_OVER <- eventReactive(input$buscarLAY_OVER,{datatable(currentjogosLAY_OVER(), options = list(paging = FALSE,dom='t'))})
  df2LAY_OVER <- eventReactive(input$buscarLAY_OVER,{datatable(currentcalcLAY_OVER(),options = list(paging = FALSE,dom='t'))})
  plotLAY_OVER <- eventReactive(input$buscarLAY_OVER,plot(currentresultadoLAY_OVER()))
  output$table1LAY_OVER <-renderDT({df1LAY_OVER()})
  output$table2LAY_OVER <-renderDT({df2LAY_OVER()})
  output$plotLAY_OVER <- renderPlot(plotLAY_OVER())
  output$league_logoLAY_OVER <-renderText({c('<img src="',ligas$league.logo[ligas$league_api_id==ligas$league_api_id[ligas$league_name==input$leagueLAY_OVER]],'">')})
#----------------------------------SERVER API-----------------------------------
  atualizar<-observeEvent(input$atualizar,{
    shinybusy::show_modal_gif('https://www.cbf7.com.br/Images/carregando.gif',
                              text = "ATUALIZANDO",height = 300,width = 400,
                              modal_size = 'l')
    Sys.sleep(5)
    shinybusy::remove_modal_gif()
  })
  }
shinyApp(ui, server)
