
#### -------- Simulering gytebestand ------- ####

###### OPPDATERINGER ########

# Der det er merka med stjerne i figur skal det ikke beregnes noenting! 


# Få skriptet til å fungere med beskatningsrate og fangstandel lik 0 
# Beskatningsrate og fangstrate = 0 når det det ikke er registrert avlivet aller gjenutsatt fangst og det ikke er tellinger
# Beskatningsrate og fangstandel skal stå som NA når fangsten er så liten at det ikke er nok grunnlag for vurdering

rm(list = ls())  # Jeg pleier å kjøre denne kommandoen først for å starte med et reint miljø

# Biblioteker
#install.packages("dplyr")  # Eksempel på hvordan laste ned pakke
#install.packages("tidyr")    # pakke for datasettmanipulering
#install.packages("xlsx")     # pakke for å laste in excel-filer og lage excel-filer
#install.packages("ggplot2")  # pakke for plotting
#install.packages("cowplot")  # pakke for plotting
#install.packages("ggpubr")   # pakke for å sette sammen flere plott
#install.packages("triangle")


library(dplyr)    # pakke for datasettmanipulering
library(tidyr)    # pakke for datasettmanipulering
#library(xlsx)     # pakke for å laste in excel-filer og lage excel-filer
library(openxlsx)
library(ggplot2)  # pakke for plotting
library(cowplot)  # pakke for plotting
library(ggpubr)   # pakke for å sette sammen flere plott
library(triangle)



#### ---------- Data ----------- #### 

# Gjøre tekstfil om til excel-fil 
# d <- read.table("Surna.txt",header=T, encoding = "UTF-8")
# write.xlsx(d, "Surna.xlsx", col.names = T, row.names = F)  # gjøre om textfil til excelfil

#d <- read.xlsx("LakselvPorsanger.xlsx",1, encoding = "UTF-8", header = T) 
setwd("C:/Users/morten.falkegard/OneDrive - NINA/Vitenskapsråd/2020/Gytebestandsimulering (R skript, Astrid)")
d <- read.xlsx("LakselvPorsanger.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
########----- TESTDATA -----------############## 

# Sette beskatningsrate og fangstandel til NA når det mangler fangst og gjennutsatte og ikke er telling
# 
d$ExpStorMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpStorMin)
d$ExpStorMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpStorMed)
d$ExpStorMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpStorMax)

d$ExpMellomMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpMellomMin)
d$ExpMellomMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpMellomMed)
d$ExpMellomMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpMellomMax)

d$ExpSmallMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpSmallMin)
d$ExpSmallMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpSmallMed)
d$ExpSmallMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$ExpSmallMax)

d$FangstAndStorMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndStorMin)
d$FangstAndStorMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndStorMed)
d$FangstAndStorMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndStorMax)

d$FangstAndMellomMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndMellomMin)
d$FangstAndMellomMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndMellomMed)
d$FangstAndMellomMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndMellomMax)

d$FangstAndSmallMin <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndSmallMin)
d$FangstAndSmallmMed <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndSmallMed)
d$FangstAndSmallMax <- ifelse(d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant), NA, d$FangstAndSmallMax)

# Sette beskatningsrate og fangstandel til 0 der det mangler fangst for en vektklasse men det er fangst for andre vektklasser

d$ExpStorMin <- ifelse(d$Laks_ant_o7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o3u7kg > 0), 0, d$ExpStorMin)
d$ExpStorMed <- ifelse(d$Laks_ant_o7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o3u7kg > 0), 0, d$ExpStorMed)
d$ExpStorMax <- ifelse(d$Laks_ant_o7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o3u7kg > 0), 0, d$ExpStorMax)

d$FangstAndStorMin <- ifelse(!is.na(d$ExpStorMin) & d$ExpStorMin == 0 & (d$Gjen_ant_o7kg == 0 | is.na(d$Gjen_ant_o7kg)), 0, d$FangstAndStorMin)
d$FangstAndStorMed <- ifelse(!is.na(d$ExpStorMin) & d$ExpStorMin == 0 & (d$Gjen_ant_o7kg == 0 | is.na(d$Gjen_ant_o7kg)), 0, d$FangstAndStorMed)
d$FangstAndStorMax <- ifelse(!is.na(d$ExpStorMin) & d$ExpStorMin == 0 & (d$Gjen_ant_o7kg == 0 | is.na(d$Gjen_ant_o7kg)), 0, d$FangstAndStorMax)

d$ExpMellomMin <- ifelse(d$Laks_ant_o3u7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o7kg > 0), 0, d$ExpMellomMin)
d$ExpMellomMed <- ifelse(d$Laks_ant_o3u7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o7kg > 0), 0, d$ExpMellomMed)
d$ExpMellomMax <- ifelse(d$Laks_ant_o3u7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o7kg > 0), 0, d$ExpMellomMax)

d$FangstAndMellomMin <- ifelse(!is.na(d$ExpMellomMin) & d$ExpMellomMin == 0 & (d$Gjen_ant_o3u7kg == 0 | is.na(d$Gjen_ant_o3u7kg)), 0, d$FangstAndMellomMin)
d$FangstAndMellomMed <- ifelse(!is.na(d$ExpMellomMin) & d$ExpMellomMin == 0 & (d$Gjen_ant_o3u7kg == 0 | is.na(d$Gjen_ant_o3u7kg)), 0, d$FangstAndMellomMed)
d$FangstAndMellomMax <- ifelse(!is.na(d$ExpMellomMin) & d$ExpMellomMin == 0 & (d$Gjen_ant_o3u7kg == 0 | is.na(d$Gjen_ant_o3u7kg)), 0, d$FangstAndMellomMax)


#### ---------- Variabler ---------- ####

# Feil navn på Gjen_vekt_o7kg i inputfil, ta vekk linja under når dette er fikset i inputfiler
d <- d %>% rename(Gjen_vekt_o7kg = Gjen_vekto7kg) 

# setter vekt og antall til 0 for å forenkle skriptet 

d$StamAntSmaHo[is.na(d$StamAntSmaHo)]<-0
d$StamAntMelHo[is.na(d$StamAntMelHo)]<-0
d$StamAntStorHo[is.na(d$StamAntStorHo)]<-0

d$Laks_vekt_u3kg[is.na(d$Laks_vekt_u3kg)]<-0
d$Laks_vekt_o3u7kg[is.na(d$Laks_vekt_o3u7kg)]<-0
d$Laks_vekt_o7kg[is.na(d$Laks_vekt_o7kg)]<-0

d$Laks_ant_u3kg[is.na(d$Laks_ant_u3kg)]<-0
d$Laks_ant_o3u7kg[is.na(d$Laks_ant_o3u7kg)]<-0
d$Laks_ant_o7kg[is.na(d$Laks_ant_o7kg)]<-0

d$Gjen_vekt_u3kg[is.na(d$Gjen_vekt_u3kg)]<-0
d$Gjen_vekt_o3u7kg[is.na(d$Gjen_vekt_o3u7kg)]<-0
d$Gjen_vekt_o7kg[is.na(d$Gjen_vekt_o7kg)]<-0

d$Gjen_ant_u3kg[is.na(d$Gjen_ant_u3kg)]<-0
d$Gjen_ant_o3u7kg[is.na(d$Gjen_ant_o3u7kg)]<-0
d$Gjen_ant_o7kg[is.na(d$Gjen_ant_o7kg)]<-0

d$Laks_ant[is.na(d$Laks_ant)] <- 0
d$Gjen_ant[is.na(d$Gjen_ant)] <- 0

d$telling <- with(d, ifelse(is.na(Obs_laks_ant), 0, 1))


#### --------- Vekt i år med telling --------- ####

# For år som har fangst og/eller gjenutsatte regner vi ut gjennomsnittsvekt: gjen_vekt+laks_vekt/gjen_ant+Laks_ant 

# År uten fangst eller gjenutsatte : gjennomsnittet av fangst og gjenutsatt 5 nærmeste år

if(sum(d$telling) > 0){                                   # Denne chuncken kjøres bare dersom det er telling 
  
  d$Laks_vekt_u3kg_sum <- with(d, Laks_vekt_u3kg + Gjen_vekt_u3kg)
  d$Laks_ant_u3kg_sum <- with(d, Laks_ant_u3kg + Gjen_ant_u3kg)
  
  d$Laks_vekt_o3u7kg_sum <- with(d, Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg)
  d$Laks_ant_o3u7kg_sum <- with(d, Laks_ant_o3u7kg + Gjen_ant_o3u7kg)
  
  d$Laks_vekt_o7kg_sum <- with(d, Laks_vekt_o7kg + Gjen_vekt_o7kg)
  d$Laks_ant_o7kg_sum <- with(d, Laks_ant_o7kg + Gjen_ant_o7kg)
  
  
  ### SMÅLAKS 
  
  # Mangler avliva fangst i år med telling
  uten_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & d$Laks_vekt_u3kg_sum == 0, 1, 0)
  
  # Færre enn 5 avliva fangst i år med telling  
  lite_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & (d$Laks_ant_u3kg < 5 & d$Laks_ant_u3kg > 0), 1, 0)
  
  
  if(sum(lite_fangst_smaa, na.rm = T) > 0 | sum(uten_fangst_smaa, na.rm = T) > 0){
    
    lite_eller_uten_fangst_smaa <- which(lite_fangst_smaa | uten_fangst_smaa)  # Rader med fangst færre enn 5
    
    
    # Færre enn totalt 5 avliva storlaks i tidsserien ---> default snittvekt   
    if(sum(d$Laks_ant_u3kg_sum, na.rm = T) < 5){
      
      d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- 2 
      d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- 1
      
    } 
    
    else {
      
      fangst <- which(d$Laks_vekt_u3kg_sum > 0)       # Finne rader som har vekt
      
      # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse årene   
      if(length(fangst) < 5) {
        
        d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- sum(d$Laks_vekt_u3kg_sum)
        d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- sum(d$Laks_ant_u3kg_sum)
        
      } else {
        
        # Hvis det er flere enn fem år med fangst, bruker vi vekt fra fem nærmeste år som har vekt: 
        # Funksjon for å sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
        
        n.min <- 5 
        f <- function(rw) {                
          O <- order(rw)[1:n.min]
          rbind(O)
        }
        
        abs_diff <- sapply(lite_eller_uten_fangst_smaa, function(x) abs(fangst-x)) 
        min_dist <- t(apply(abs_diff, 2, f)) 
        
        
        d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]]<- d$Laks_vekt_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 1]]]+ d$Laks_vekt_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 2]]]+ d$Laks_vekt_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 3]]]+ d$Laks_vekt_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 4]]]+d$Laks_vekt_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 5]]]
        
        d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]]<- d$Laks_ant_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 1]]]+ d$Laks_ant_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 2]]]+ d$Laks_ant_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 3]]]+ d$Laks_ant_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 4]]]+d$Laks_ant_u3kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_smaa), 5]]]
        
        
        
      }
      
    }
    
  }
  
  
  
  ### MELLOMLAKS 
  
  # Mangler avliva fangst for år med telling
  uten_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & d$Laks_vekt_o3u7kg_sum == 0, 1, 0)
  
  # Færre enn 5 avliva fangst for år med telling  
  lite_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & (d$Laks_ant_o3u7kg < 5 & d$Laks_ant_o3u7kg > 0), 1, 0)
  
  
  if(sum(lite_fangst_mellom, na.rm = T) > 0 | sum(uten_fangst_mellom, na.rm = T) > 0){
    
    lite_eller_uten_fangst_mellom <- which(lite_fangst_mellom | uten_fangst_mellom)  # Rader med fangst færre enn 5
    
    
    # Færre enn totalt 5 avliva storlaks i tidsserien ---> standard snittvekt   
    if(sum(d$Laks_ant_o3u7kg_sum, na.rm = T) < 5){
      
      d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- 4 
      d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- 1
      
    } 
    
    else {
      
      fangst <- which(d$Laks_vekt_o3u7kg_sum > 0)       # Finne rader som har vekt
      
      # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse    
      if(length(fangst) < 5) {
        
        d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- sum(d$Laks_vekt_o3u7kg_sum)
        d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- sum(d$Laks_ant_o3u7kg_sum)
        
      } else {
        
        # Hvis det er flere enn fem år med fangst, bruker vi vekt fra fem nærmeste år som har vekt: 
        # Funksjon for å sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
        
        n.min <- 5 
        f <- function(rw) {                
          O <- order(rw)[1:n.min]
          rbind(O)
        }
        
        abs_diff <- sapply(lite_eller_uten_fangst_mellom, function(x) abs(fangst-x)) 
        min_dist <- t(apply(abs_diff, 2, f)) 
        
        
        d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]]<- d$Laks_vekt_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 1]]]+ d$Laks_vekt_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 2]]]+ d$Laks_vekt_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 3]]]+ d$Laks_vekt_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 4]]]+d$Laks_vekt_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 5]]]
        
        d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]]<- d$Laks_ant_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 1]]]+ d$Laks_ant_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 2]]]+ d$Laks_ant_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 3]]]+ d$Laks_ant_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 4]]]+d$Laks_ant_o3u7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_mellom), 5]]]
        
        
        
      }
      
    }
    
  }
  
  
  ### STORLAKS
  
  # Mangler avliva fangst for år med telling
  uten_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & d$Laks_vekt_o7kg_sum == 0, 1, 0)
  
  # Færre enn 5 avliva fangst for år med telling  
  lite_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & (d$Laks_ant_o7kg < 5 & d$Laks_ant_o7kg > 0), 1, 0)
  
  
  if(sum(lite_fangst_stor, na.rm = T) > 0 | sum(uten_fangst_stor, na.rm = T) > 0){
    
    lite_eller_uten_fangst_stor <- which(lite_fangst_stor | uten_fangst_stor)  # Rader med fangst færre enn 5
    
    
    # Færre enn totalt 5 avliva storlaks i tidsserien ---> standard snittvekt   
    if(sum(d$Laks_ant_o7kg_sum, na.rm = T) < 5){
      
      d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- 8 
      d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- 1
      
    } 
    
    else {
      
      fangst <- which(d$Laks_vekt_o7kg_sum > 0)       # Finne rader som har vekt
      
      # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse    
      if(length(fangst) < 5) {
        
        d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- sum(d$Laks_vekt_o7kg_sum)
        d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- sum(d$Laks_ant_o7kg_sum)
        
      } else {
        
        # Hvis det er flere enn fem år, bruker vi vekt fra fem nærmeste år som har vekt: 
        # Funksjon for å sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
        
        n.min <- 5 
        f <- function(rw) {                
          O <- order(rw)[1:n.min]
          rbind(O)
        }
        
        abs_diff <- sapply(lite_eller_uten_fangst_stor, function(x) abs(fangst-x)) 
        min_dist <- t(apply(abs_diff, 2, f)) 
        
        
        d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]]<- d$Laks_vekt_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 1]]]+ d$Laks_vekt_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 2]]]+ d$Laks_vekt_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 3]]]+ d$Laks_vekt_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 4]]]+d$Laks_vekt_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 5]]]
        
        d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]]<- d$Laks_ant_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 1]]]+ d$Laks_ant_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 2]]]+ d$Laks_ant_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 3]]]+ d$Laks_ant_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 4]]]+d$Laks_ant_o7kg_sum[fangst[min_dist[1:length(lite_eller_uten_fangst_stor), 5]]]
        
        
      }
      
    }
    
  }
  
  
  # Vekt til å bruke ved telling - gjennomsnittsvekt fra fangst og/eller gjenutsatte * antall fra tellinger 
  d$vekt_smaa_obs <- with(d, ifelse(Obs_laks_ant_u3kg > 0, Laks_vekt_u3kg_sum/Laks_ant_u3kg_sum * Obs_laks_ant_u3kg, NA))
  d$vekt_mellom_obs <- with(d, ifelse(Obs_laks_ant_o3u7kg > 0, Laks_vekt_o3u7kg_sum/Laks_ant_o3u7kg_sum * Obs_laks_ant_o3u7kg, NA))
  d$vekt_stor_obs <- with(d, ifelse(Obs_laks_ant_o7kg > 0, Laks_vekt_o7kg_sum/Laks_ant_o7kg_sum * Obs_laks_ant_o7kg, NA))
  
} 

# Sjekke om det ser riktig ut
#d %>% select(Laks_vekt_u3kg_sum, Laks_ant_u3kg_sum, Obs_laks_ant_u3kg, vekt_smaa_obs, telling)

#### --------- Simulering --------- ####

n.sim <- 5000   
n.years <- length(d$Aar)

empty.matrix <- matrix(NA, nrow=n.years, ncol=n.sim)
empty.matrix2<-matrix(NA,nrow=n.years)


# Andel hoer i bestanden

prhusmall<-empty.matrix
prhumellom<-empty.matrix
prhustor<-empty.matrix


for(i in 1:n.years){
  prhusmall[i,] <- rtriangle(n = n.sim, a = d$PrHuSmall[i]-0.05, b = d$PrHuSmall[i]+0.05, c = d$PrHuSmall[i])
  prhumellom[i,] <- rtriangle(n = n.sim, a = d$PrHuMellom[i]-0.05, b = d$PrHuMellom[i]+0.05, c = d$PrHuMellom[i])
  prhustor[i,] <- rtriangle(n = n.sim, a = d$PrHuStor[i]-0.05, b = d$PrHuStor[i]+0.05, c = d$PrHuStor[i])
}


# Andel hoer gjenutsatte

prhusmall_gjen <-empty.matrix
prhumellom_gjen <-empty.matrix
prhustor_gjen <-empty.matrix

for(i in 1:n.years){  
  prhusmall_gjen[i,] <- if(!is.na(d$PrHuSmall_Gjenut[i])) (rtriangle(n = n.sim, a = d$PrHuSmall_Gjenut[i]-0.05, b = d$PrHuSmall[i]+0.05, c = d$PrHuSmall_Gjenut[i])) else NA
  prhumellom_gjen[i,] <-if(!is.na(d$PrHuMellom_Gjenut[i])) (rtriangle(n = n.sim, a = d$PrHuMellom_Gjenut[i]-0.05, b = d$PrHuMellom_Gjenut[i]+0.05, c = d$PrHuMellom_Gjenut[i])) else NA
  prhustor_gjen[i,] <- if(!is.na(d$PrHuStor_Gjenut[i])) (rtriangle(n = n.sim, a = d$PrHuStor_Gjenut[i]-0.05, b = d$PrHuStor_Gjenut[i]+0.05, c = d$PrHuStor_Gjenut[i])) else NA
}




# Simulere prosent antatt observert dersom det er  år med telling, og regne ut beskatningsrate fra dette   
################## ENDRING: if(d$telling[i]) til if(!is.na(d$Obs_laks_ant_u3kg[i]) & d$Obs_laks_ant_u3kg[i]> 0)

if(sum(d$telling) > 0){
  probsmall <- empty.matrix
  probmellom <- empty.matrix
  probstor <- empty.matrix
  
  for(i in 1:n.years){
    probsmall[i,] <- if(!is.na(d$Obs_laks_ant_u3kg[i]) & d$Obs_laks_ant_u3kg[i] > 0) (rtriangle(n = n.sim, a = d$Probs_small_min[i], b = d$Probs_small_max[i], c = d$Probs_small_med[i])) else NA
    probmellom[i,] <- if(!is.na(d$Obs_laks_ant_o3u7kg[i]) & d$Obs_laks_ant_o3u7kg[i] > 0) (rtriangle(n = n.sim, a = d$Probs_mellom_min[i], b = d$Probs_mellom_max[i], c = d$Probs_mellom_med[i])) else NA
    probstor[i,] <- if(!is.na(d$Obs_laks_ant_o7kg[i]) & d$Obs_laks_ant_o7kg[i] > 0) (rtriangle(n = n.sim, a = d$Probs_stor_min[i], b = d$Probs_stor_max[i], c = d$Probs_stor_max[i])) else NA
  }
  
  

  # Oppdrettantall (trenger dette for å regne ut antall ville)
  
  Oppdrettantall <- empty.matrix
  Oppdrettsmaa <- empty.matrix
  Oppdrettmel <- empty.matrix
  Oppdrettstor <- empty.matrix
  
  
  # Antall oppdrett 
  for(i in 1:n.years){
    Oppdrettantall[i,]<-rbinom(n = n.sim, size = d$Laks_ant[i], prob = d$ProsOpp[i]/100)
    Oppdrettsmaa[i,] <-rbinom(n = n.sim, size = Oppdrettantall[i,], prob = 0.31)                     
    Oppdrettmel[i,] <-rbinom(n = n.sim, size = Oppdrettantall[i,]-Oppdrettsmaa[i,], prob = 0.81)
    Oppdrettstor[i,]<-Oppdrettantall[i,]-Oppdrettmel[i,]-Oppdrettsmaa[i,]
  }
  
  
  # Antall villaks
  
  AntVillSmaa<-empty.matrix
  AntVillMel<-empty.matrix
  AntVillStor<-empty.matrix
  
  # Smålaks
  
  for(s in 1:n.sim){
    for(i in 1:n.years){
      AntVillSmaa[i,s]<- if(!is.na(d$Obs_laks_ant_u3kg[i]))((d$Obs_laks_ant_u3kg[i]/probsmall[i,s]) + d$Laks_ant_u3kg[i] - Oppdrettsmaa[i,s]) else NA  
    } }
  
  AntVillSmaa[AntVillSmaa < 0] <- 0     # Setter negative verdier til 0
  
  # Mellomlaks
  
  for(s in 1:n.sim){
    for(i in 1:n.years){
      AntVillMel[i,s]<- if(!is.na(d$Obs_laks_ant_o3u7kg[i]))((d$Obs_laks_ant_o3u7kg[i]/probmellom[i,s]) + d$Laks_ant_o3u7kg[i] - Oppdrettmel[i,s])else NA
    } 
  }
  
  AntVillMel[AntVillMel < 0] <- 0
  
  
  # Storlaks
  
  for(s in 1:n.sim){
    for(i in 1:n.years){
      AntVillStor[i,s]<- if(!is.na(d$Obs_laks_ant_o7kg[i]))((d$Obs_laks_ant_o7kg[i]/probstor[i,s]) + d$Laks_ant_o7kg[i] - Oppdrettstor[i,s]) else NA 
    }
  }
  
  
  AntVillStor[AntVillStor<0] <- 0
  
  # Oppdatering: regner ut beskatningsrate fra tellinger, der vi har tellinger for gitt aldersklasse og beskatningsrate mangler. Hvis ikke blir beskatningsrate stående som den er.  
  
  d$ExpSmallMed <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMed), Laks_ant_u3kg/median(AntVillSmaa, na.rm = T), ExpSmallMed))
  d$ExpSmallMin <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMin), Laks_ant_u3kg/max(AntVillSmaa, na.rm = T), ExpSmallMin))
  d$ExpSmallMax <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMax), Laks_ant_u3kg/min(AntVillSmaa, na.rm = T), ExpSmallMax))
  
  d$ExpMellomMed <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMed), Laks_ant_o3u7kg/median(AntVillMel, na.rm = T), ExpMellomMed))
  d$ExpMellomMin <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMin), Laks_ant_o3u7kg/max(AntVillMel, na.rm = T), ExpMellomMin))
  d$ExpMellomMax <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMax), Laks_ant_o3u7kg/min(AntVillMel, na.rm = T), ExpMellomMax))
  
  d$ExpStorMed <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMed), Laks_ant_o7kg/median(AntVillStor, na.rm = T), ExpStorMed)) # Funker med NA her, viss det ikke er grunnlag for å regne ut antall ville (NA), blir dette NA 
  d$ExpStorMin <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMin), Laks_ant_o7kg/max(AntVillStor, na.rm = T), ExpStorMin))
  d$ExpStorMax <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMax), Laks_ant_o7kg/min(AntVillStor, na.rm = T), ExpStorMax))
  
}


# Regne ut beskatningsrate for år der det ikke er oppgitt 
# Regne ut fangstandel for år der det ikke er oppgitt

# Beskatningsrate = avliva/((avliva + gjenutsatte)/fangstandel)
# Fangstandel = (avliva + gjenutsatte)/(avliva/beskatningsrate)



### Oppdatering: 
## dersom beskatning er oppgitt beholder vi denne.
## dersom beskatning OG fangstandel er satt til NA beholder vi dette (for da er det ikke grunnlag for vurdering)
## dersom beskatning ikke er oppgitt og fangstandel er oppgitt, regner vi beskatning fra fangstandel, og motsatt.

Beskatning <- function(Laks_ant, Gjen_ant, FangstAnd, Exp){
  Exp <- ifelse(!is.na(Exp) | (is.na(Exp) & is.na(FangstAnd)), Exp,
                       Laks_ant/((Laks_ant + Gjen_ant)/FangstAnd))
  return(Exp)
}


d$ExpSmallMed  <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMed, ExpSmallMed))
d$ExpSmallMin <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMin, ExpSmallMin))
d$ExpSmallMax <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMax, ExpSmallMax))

d$ExpMellomMed <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMed, ExpMellomMed))
d$ExpMellomMin <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMin, ExpMellomMin))
d$ExpMellomMax <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMax, ExpMellomMax))

d$ExpStorMed <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMed, ExpStorMed))
d$ExpStorMin <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMin, ExpStorMin))
d$ExpStorMax <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMax, ExpStorMax))



FangstAndel <- function(Laks_ant, Gjen_ant, FangstAnd, Exp){
  FangstAnd <- ifelse(!is.na(FangstAnd) | (is.na(FangstAnd) & (is.na(Exp)| Exp == 0)), FangstAnd,
                             (Laks_ant + Gjen_ant)/(Laks_ant/Exp))
}


d$FangstAndSmallMed  <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMed, ExpSmallMed))
d$FangstAndSmallMin <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMin, ExpSmallMin))
d$FangstAndSmallMax <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMax, ExpSmallMax))

d$FangstAndMellomMed <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMed, ExpMellomMed))
d$FangstAndMellomMin <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMin, ExpMellomMin))
d$FangstAndMellomMax <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMax, ExpMellomMax))

d$FangstAndStorMed <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMed, ExpStorMed))
d$FangstAndStorMin <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMin, ExpStorMin))
d$FangstAndStorMax <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMax, ExpStorMax))

# Fil med beskatningsrate og fangstandel 

Beskatning_og_fangstandel <-  d %>% select(Vassdrag, Vdrnr, Aar, 
                                           ExpSmallMin, FangstAndSmallMin, 
                                           ExpSmallMed, FangstAndSmallMed,
                                           ExpSmallMax, FangstAndSmallMax,
                                           ExpMellomMin, FangstAndMellomMin,
                                           ExpMellomMed, FangstAndMellomMed,
                                           ExpMellomMax, FangstAndMellomMax,
                                           ExpStorMin, FangstAndStorMin,
                                           ExpStorMed, FangstAndStorMed,
                                           ExpStorMax, FangstAndStorMax
)

Beskatning_og_fangstandel %>% filter(Aar == 1993)

# Dersom fila allerede finnes, så legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil. 

if(paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files()) {
  write.table(Beskatning_og_fangstandel, paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
              col.names=FALSE, append=TRUE)
} else {
  write.table(Beskatning_og_fangstandel, paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
}

# Fjerne duplikater
Beskatning_og_fangstandel <- read.table(paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
Beskatning_og_fangstandel <- Beskatning_og_fangstandel[ !duplicated(Beskatning_og_fangstandel[, c("Vassdrag", "Aar")], fromLast=T),]

write.table(Beskatning_og_fangstandel, paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)




# Simulere beskatningsrate, fangstandel og prosent antatt observert

## Beholder NA der det er NA, beholder 0 der det er 0

Expsmall <- empty.matrix
Expmellom <- empty.matrix
Expstor <- empty.matrix

for(i in 1:n.years){
  Expsmall[i,] <- if(!is.na(d$ExpSmallMed[i])) (rtriangle(n = n.sim, a = d$ExpSmallMin[i], b = d$ExpSmallMax[i], c = d$ExpSmallMed[i])) else NA
  Expmellom[i,] <- if(!is.na(d$ExpMellomMed[i])) (rtriangle(n = n.sim, a = d$ExpMellomMin[i], b = d$ExpMellomMax[i], c = d$ExpMellomMed[i])) else NA
  Expstor[i,] <- if(!is.na(d$ExpStorMed[i])) (rtriangle(n = n.sim, a = d$ExpStorMin[i], b = d$ExpStorMax[i], c = d$ExpStorMed[i])) else NA
} 


# Fangstandel

FangstAndSmall <- empty.matrix
FangstAndMellom <- empty.matrix
FangstAndStor <- empty.matrix


for(i in 1:n.years){
  FangstAndSmall[i,] <- if(!is.na(d$FangstAndSmallMed[i])) (rtriangle(n = n.sim, a = d$FangstAndSmallMin[i], b = d$FangstAndSmallMax[i], c = d$FangstAndSmallMed[i])) else NA
  FangstAndMellom[i,] <- if(!is.na(d$FangstAndMellomMed[i])) (rtriangle(n = n.sim, a = d$FangstAndMellomMin[i], b = d$FangstAndMellomMax[i], c = d$FangstAndMellomMed[i])) else NA
  FangstAndStor[i,] <- if(!is.na(d$FangstAndStorMed[i])) (rtriangle(n = n.sim, a = d$FangstAndStorMin[i], b = d$FangstAndStorMax[i], c = d$FangstAndStorMed[i])) else NA
} 




# Oppdrettantall og  kg oppdrett

Oppdrettantall <- empty.matrix
Oppdrettsmaa <- empty.matrix
Oppdrettmel <- empty.matrix
Oppdrettstor <- empty.matrix

KgOppdrSmaa <- empty.matrix
KgOppdrMel <- empty.matrix
KgOppdrStor <- empty.matrix
KgOppdrTot <- empty.matrix


# Antall oppdrett 
for(i in 1:n.years){
  Oppdrettantall[i,]<- rbinom(n = n.sim, size = d$Laks_ant[i], prob = d$ProsOpp[i]/100)
  Oppdrettsmaa[i,] <- rbinom(n = n.sim, size = Oppdrettantall[i,], prob = 0.31)                    
  Oppdrettmel[i,] <- rbinom(n = n.sim, size = Oppdrettantall[i,]-Oppdrettsmaa[i,], prob = 0.81)
  Oppdrettstor[i,]<- Oppdrettantall[i,]-Oppdrettmel[i,]-Oppdrettsmaa[i,]
}



# Kg oppdrett 
for(i in 1:n.years){
  KgOppdrSmaa[i,] <- Oppdrettsmaa[i,]*((d$Laks_vekt_u3kg[i]+2)/(d$Laks_ant_u3kg[i]+1))
  KgOppdrMel[i,]<- Oppdrettmel[i,]*((d$Laks_vekt_o3u7kg[i]+5)/(d$Laks_ant_o3u7kg[i]+1)) 
  KgOppdrStor[i,]<- Oppdrettstor[i,]*((d$Laks_vekt_o7kg[i]+9)/(d$Laks_ant_o7kg[i]+1))
  KgOppdrTot[i,]<-KgOppdrSmaa[i,]+KgOppdrMel[i,]+KgOppdrStor[i,]
}



# Antall oppdrettlaks 
# Oppdatering: regne hvis antall oppdrett og beskatning er større enn 0, eller sette til 0.

AntOppdr<-empty.matrix
AntOppdrSmaa<-empty.matrix
AntOppdrMel<-empty.matrix
AntOppdrStor<-empty.matrix

for(s in 1:n.sim){
  for (i in 1:n.years){
    AntOppdrSmaa[i,s]<- if(Oppdrettsmaa[i,s] > 0 & Expsmall[i,s] > 0) (Oppdrettsmaa[i,s]/Expsmall[i,s]) else 0
    AntOppdrMel[i,s]<-  if(Oppdrettmel[i,s] > 0 & Expmellom[i,s] > 0) (Oppdrettmel[i,s]/Expmellom[i,s]) else 0
    AntOppdrStor[i,s]<- if(Oppdrettstor[i,s] > 0 & Expstor[i,s] > 0) (Oppdrettstor[i,s]/Expstor[i,s]) else 0
    AntOppdr[i,s]<-AntOppdrSmaa[i,s]+AntOppdrMel[i,s]+AntOppdrStor[i,s]
  }
}

# ENDRINGER: 
# bruker tellinger om det er tellinger for den gitte aldersklassen
# Antall villaks bli kun regnet ut fra fangstandel dersom fangstandel er oppgitt og større enn 0. Ellers satt til 0. 

AntVillSmaa<-empty.matrix
AntVillMel<-empty.matrix
AntVillStor<-empty.matrix
AntVillTot<-empty.matrix


# Smålaks

for(s in 1:n.sim){
  for(i in 1:n.years){
    AntVillSmaa[i,s]<- if(is.na(d$Obs_laks_ant_u3kg[i]) | d$Obs_laks_ant_u3kg[i] == 0){
        if(!is.na(FangstAndSmall[i,s]) & FangstAndSmall[i,s] > 0)
          ((d$Laks_ant_u3kg[i] - Oppdrettsmaa[i,s] + (d$Gjen_ant_u3kg[i] - (d$Gjen_ant_u3kg[i] * FangstAndSmall[i,s] * 0.2)))/FangstAndSmall[i,s]) else 0}
    else {
      (d$Obs_laks_ant_u3kg[i]/probsmall[i,s]) + d$Laks_ant_u3kg[i] - Oppdrettsmaa[i,s]}  
  } 
}

AntVillSmaa[AntVillSmaa < 0] <- 0     # Setter negative verdier til 0


# Mellomlaks


for(s in 1:n.sim){
  for(i in 1:n.years){
    AntVillMel[i,s]<- if(is.na(d$Obs_laks_ant_o3u7kg[i]) | d$Obs_laks_ant_o3u7kg[i] == 0){
    if(!is.na(FangstAndMellom[i,s]) & FangstAndMellom[i,s] > 0)
          ((d$Laks_ant_o3u7kg[i] - Oppdrettmel[i,s] + (d$Gjen_ant_o3u7kg[i] - (d$Gjen_ant_o3u7kg[i] * FangstAndMellom[i,s] * 0.2)))/FangstAndMellom[i,s]) else 0}
    else {
      (d$Obs_laks_ant_o3u7kg[i]/probmellom[i,s]) + d$Laks_ant_o3u7kg[i] - Oppdrettmel[i,s]} 
  }
}

AntVillMel[AntVillMel < 0] <- 0     # Setter negative verdier til 0

# Storlaks

for(s in 1:n.sim){
  for(i in 1:n.years){
    AntVillStor[i,s]<- if(is.na(d$Obs_laks_ant_o7kg[i]) | d$Obs_laks_ant_o7kg[i] == 0){
      if(!is.na(FangstAndStor[i,s]) & FangstAndStor[i,s] > 0)
     ((d$Laks_ant_o7kg[i] - Oppdrettstor[i,s] + (d$Gjen_ant_o7kg[i] - (d$Gjen_ant_o7kg[i] * FangstAndStor[i,s] * 0.2)))/FangstAndStor[i,s]) else 0}
    else {
      (d$Obs_laks_ant_o7kg[i]/probstor[i,s]) + d$Laks_ant_o7kg[i] - Oppdrettstor[i,s]} 
  }
}

AntVillStor[AntVillStor < 0] <- 0     # Setter negative verdier til 0

# Totalt antall ville 
for (s in 1:n.sim){
  for (i in 1:n.years){
    AntVillTot[i,s]<-AntVillSmaa[i,s]+AntVillMel[i,s]+AntVillStor[i,s]
  }
}



# Oppdrett percentiler 

OppdrettAntall025<-empty.matrix2
OppdrettAntall25<-empty.matrix2
OppdrettAntall50<-empty.matrix2
OppdrettAntall75<-empty.matrix2
OppdrettAntall975<-empty.matrix2

AntOppdr025<-empty.matrix2
AntOppdr25<-empty.matrix2
AntOppdr50<-empty.matrix2
AntOppdr75<-empty.matrix2
AntOppdr975<-empty.matrix2

KgOppdr025<-empty.matrix2
KgOppdr25<-empty.matrix2
KgOppdr50<-empty.matrix2
KgOppdr75<-empty.matrix2
KgOppdr975<-empty.matrix2



# OppdrettAntall 0.025, 0.25, 0.50, 0.75, 0.975
for(i in 1:n.years){
  OppdrettAntall025[i]<-quantile(Oppdrettantall[i, ], probs=0.025)
  OppdrettAntall25[i]<-quantile(Oppdrettantall[i, ], probs = 0.25)
  OppdrettAntall50[i]<-quantile(Oppdrettantall[i, ], probs = 0.50)
  OppdrettAntall75[i]<-quantile(Oppdrettantall[i, ], probs = 0.75)
  OppdrettAntall975[i]<-quantile(Oppdrettantall[i, ], probs = 0.975)
}


# AntOppdr 0.025, 0.25, 0.50, 0.75, 0.975
for(i in 1:n.years){
  AntOppdr025[i]<-quantile(AntOppdr[i, ], probs=0.025)
  AntOppdr25[i]<-quantile(AntOppdr[i, ], probs = 0.25)
  AntOppdr50[i]<-quantile(AntOppdr[i, ], probs = 0.50)
  AntOppdr75[i]<-quantile(AntOppdr[i, ], probs = 0.75)
  AntOppdr975[i]<-quantile(AntOppdr[i, ], probs = 0.975)
}


# Kg oppdrett 0.025, 0.25, 0.50, 0.75, 0.975
for(i in 1:n.years){
  KgOppdr025[i]<-quantile(KgOppdrTot[i, ], probs=0.025)
  KgOppdr25[i]<-quantile(KgOppdrTot[i, ], probs=0.25)
  KgOppdr50[i]<-quantile(KgOppdrTot[i, ], probs=0.50)
  KgOppdr75[i]<-quantile(KgOppdrTot[i, ], probs=0.75)
  KgOppdr975[i]<-quantile(KgOppdrTot[i, ], probs=0.975)
}



# Elvebestand villfisk 0.025, 0.25, 0.50, 0.75, 0.975

ElvebestVillSma025<-empty.matrix2
ElvebestVillSma25<-empty.matrix2
ElvebestVillSma50<-empty.matrix2
ElvebestVillSma75<-empty.matrix2
ElvebestVillSma975<-empty.matrix2

ElvebestVillMel025<-empty.matrix2
ElvebestVillMel25<-empty.matrix2
ElvebestVillMel50<-empty.matrix2
ElvebestVillMel75<-empty.matrix2
ElvebestVillMel975<-empty.matrix2

ElvebestVillStor025<-empty.matrix2
ElvebestVillStor25<-empty.matrix2
ElvebestVillStor50<-empty.matrix2
ElvebestVillStor75<-empty.matrix2
ElvebestVillStor975<-empty.matrix2

ElvebestVillTot025<-empty.matrix2
ElvebestVillTot25<-empty.matrix2
ElvebestVillTot50<-empty.matrix2
ElvebestVillTot75<-empty.matrix2
ElvebestVillTot975<-empty.matrix2


# Vill bestand smaalaks
for(i in 1:n.years){
  ElvebestVillSma025[i]<-quantile(AntVillSmaa[i, ], probs=0.025)
  ElvebestVillSma25[i]<-quantile(AntVillSmaa[i, ], probs=0.25)
  ElvebestVillSma50[i]<-quantile(AntVillSmaa[i, ], probs=0.50)
  ElvebestVillSma75[i]<-quantile(AntVillSmaa[i, ], probs=0.75)
  ElvebestVillSma975[i]<-quantile(AntVillSmaa[i, ], probs=0.975)
}

# Vill mellomlaks
for(i in 1:n.years){
  ElvebestVillMel025[i]<-quantile(AntVillMel[i, ], probs=0.025)
  ElvebestVillMel25[i]<-quantile(AntVillMel[i, ], probs=0.25)
  ElvebestVillMel50[i]<-quantile(AntVillMel[i, ], probs=0.50)
  ElvebestVillMel75[i]<-quantile(AntVillMel[i, ], probs=0.75)
  ElvebestVillMel975[i]<-quantile(AntVillMel[i, ], probs=0.975)
}


# Vill storlaks
for(i in 1:n.years){
  ElvebestVillStor025[i]<-quantile(AntVillStor[i, ], probs=0.025)
  ElvebestVillStor25[i]<-quantile(AntVillStor[i, ], probs=0.25)
  ElvebestVillStor50[i]<-quantile(AntVillStor[i, ], probs=0.50)
  ElvebestVillStor75[i]<-quantile(AntVillStor[i, ], probs=0.75)
  ElvebestVillStor975[i]<-quantile(AntVillStor[i, ], probs=0.975)
}

# Vill totalt
for(i in 1:n.years){
  ElvebestVillTot025[i]<-quantile(AntVillTot[i, ], probs=0.025)
  ElvebestVillTot25[i]<-quantile(AntVillTot[i, ], probs=0.25)
  ElvebestVillTot50[i]<-quantile(AntVillTot[i, ], probs=0.50)
  ElvebestVillTot75[i]<-quantile(AntVillTot[i, ], probs=0.75)
  ElvebestVillTot975[i]<-quantile(AntVillTot[i, ], probs=0.975)
}


# Fil med antall oppdrett og villfisk 
# SØrge for at det blir NA der det skal være NA 

TilfilAntallVillogOppdrett <- data_frame(ElvebestVillSma025, ElvebestVillSma25, ElvebestVillSma50, ElvebestVillSma75, ElvebestVillSma975,
                                         ElvebestVillMel025, ElvebestVillMel25, ElvebestVillMel50, ElvebestVillMel75, ElvebestVillMel975,
                                         ElvebestVillStor025, ElvebestVillStor25, ElvebestVillStor50, ElvebestVillStor75, ElvebestVillStor975,
                                         ElvebestVillTot025, ElvebestVillTot25, ElvebestVillTot50, ElvebestVillTot75, ElvebestVillTot975,
                                         OppdrettAntall025, OppdrettAntall25, OppdrettAntall50, OppdrettAntall75, OppdrettAntall975,
                                         AntOppdr025, AntOppdr25, AntOppdr50, AntOppdr75, AntOppdr975) %>%
  mutate(Vassdrag = d$Vassdrag) %>% 
  mutate(Vdrnr = d$Vdrnr) %>%
  mutate(Kommune = d$Kommune) %>% 
  mutate(Aar = d$Aar) %>% 
  mutate(GBMkghunner = d$GBMkghunner)%>%
  select(Vassdrag:GBMkghunner, everything())   # Flytte kolonner Vassdrag til GBMkghunner fremst i datasettet 

# Sette år uten grunnlag for vurdering til NA 

# Vill elvebestand smålaks, mellomlaks og storlaks til NA i år der beskatningsrate er NA
TilfilAntallVillogOppdrett[which(is.na(d$ExpSmallMin)), 6:10] <- NA
TilfilAntallVillogOppdrett[which(is.na(d$ExpMellomMin)), 11:15] <- NA
TilfilAntallVillogOppdrett[which(is.na(d$ExpStorMin)), 16:20] <- NA

# Total elvebestand satt til NA dersom der ikke er avliva eller gjenutsatt fangst og ikke telling
TilfilAntallVillogOppdrett[which(d$Laks_ant == 0 & d$Gjen_ant == 0 & d$telling == 0), 21:25] <- NA

# Oppdrettantall og AntOppdr satt til NA dersom det ikke er avliva fangst
TilfilAntallVillogOppdrett[which(d$Laks_ant == 0), 26:35] <- NA


# Dersom fila allerede finnes, så legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil. 

if(paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files()) {
  write.table(TilfilAntallVillogOppdrett, paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
              col.names=FALSE, append=TRUE)
} else {
  write.table(TilfilAntallVillogOppdrett, paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
}


# Fjerner duplikater:  beholder bare det som ble kjørt sist 

AntVillogOpp <- read.table(paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
AntVillogOpp <- AntVillogOpp[ !duplicated(AntVillogOpp[, c("Vassdrag", "Aar")], fromLast=T),]


write.table(AntVillogOpp, paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)





#### ------- Simulering gytebestand -------#### 


# Triangulærfordeling andel døde
AndelDode <- rtriangle(n = n.sim, a = 1-0.96 , b = 1-0.85, c = 1-0.93)



# Gjenutsatte justert for gjenfangst og andel hoer
# Avliva fangst justert for oppdrett og andel hoer 

Gjen_vekt_ho_u3kg_just <- empty.matrix 
Gjen_vekt_ho_o3u7kg_just <- empty.matrix
Gjen_vekt_ho_o7kg_just <- empty.matrix
Laks_vekt_ho_u3kg_just <- empty.matrix
Laks_vekt_ho_o3u7kg_just <- empty.matrix
Laks_vekt_ho_o7kg_just <- empty.matrix

for(s in 1:n.sim){
  for(i in 1:n.years){
    Gjen_vekt_ho_u3kg_just[i,s] <- if(d$Gjen_vekt_u3kg[i] > 0) ((d$Gjen_vekt_u3kg[i] - (d$Gjen_vekt_u3kg[i] * FangstAndSmall[i, s] * 0.2)) * prhusmall_gjen[i,s]) else 0
    Gjen_vekt_ho_o3u7kg_just[i,s] <- if(d$Gjen_vekt_o3u7kg[i] > 0)((d$Gjen_vekt_o3u7kg[i] - (d$Gjen_vekt_o3u7kg[i] * FangstAndMellom[i, s] * 0.2)) * prhumellom_gjen[i,s]) else 0
    Gjen_vekt_ho_o7kg_just[i,s] <- if(d$Gjen_vekt_o7kg[i] > 0) ((d$Gjen_vekt_o7kg[i] - (d$Gjen_vekt_o7kg[i] * FangstAndStor[i, s] * 0.2)) * prhumellom_gjen[i,s]) else 0
    Laks_vekt_ho_u3kg_just[i,s] <- if(d$Laks_vekt_u3kg[i] > 0) ((d$Laks_vekt_u3kg[i] - KgOppdrSmaa[i,s]) * prhusmall[i,s]) else 0 
    Laks_vekt_ho_o3u7kg_just[i,s] <- if(d$Laks_vekt_o3u7kg[i] > 0) ((d$Laks_vekt_o3u7kg[i] - KgOppdrMel[i,s]) * prhumellom[i,s]) else 0 
    Laks_vekt_ho_o7kg_just[i,s] <- if(d$Laks_vekt_o7kg[i] > 0) ((d$Laks_vekt_o7kg[i] - KgOppdrStor[i,s]) * prhustor[i,s]) else 0 
  }
}


# Simulering gytebestand 

# To alternative prosedyrer: telling og prosent antatt observert, eller total fangst og fangstandel


kghunsmall<-empty.matrix
kghunmellom<-empty.matrix
kghunstor<-empty.matrix
kghuntot<-empty.matrix

# Smålaks


for(s in 1:n.sim){
  for(i in 1:n.years){
    kghunsmall[i,s]<- if(is.na(d$Obs_laks_ant_u3kg[i]) | d$Obs_laks_ant_u3kg[i] == 0){
      if((KgOppdrSmaa[i,s] < d$Laks_vekt_u3kg[i] + d$Gjen_vekt_u3kg[i]) & !is.na(FangstAndSmall[i,s]))
        ((Gjen_vekt_ho_u3kg_just[i, s] + Laks_vekt_ho_u3kg_just[i,s])/FangstAndSmall[i,s] - d$StamAntSmaHo[i]*2 - Gjen_vekt_ho_u3kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_u3kg_just[i,s]) else 0}
    else {
      if(KgOppdrSmaa[i,s]<d$vekt_smaa_obs[i])
        ((d$vekt_smaa_obs[i]*prhusmall[i,s])/probsmall[i,s]-d$StamAntSmaHo[i]*2) else 0} 
  }}



# Mellomlaks

for(s in 1:n.sim){
  for(i in 1:n.years){
    kghunmellom[i,s]<- if(is.na(d$Obs_laks_ant_o3u7kg[i]) | d$Obs_laks_ant_o3u7kg[i] == 0){
      if((KgOppdrMel[i,s] < d$Laks_vekt_o3u7kg[i] + d$Gjen_vekt_o3u7kg[i]) & !is.na(Expmellom[i,s]))
        ((Gjen_vekt_ho_o3u7kg_just[i, s] + Laks_vekt_ho_o3u7kg_just[i,s])/FangstAndMellom[i,s] - d$StamAntMelHo[i]*2 - Gjen_vekt_ho_o3u7kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_o3u7kg_just[i,s]) else 0}
    else {
      if(KgOppdrMel[i,s]<d$vekt_mellom_obs[i])
        (((d$vekt_mellom_obs[i]-KgOppdrMel[i,s])*prhumellom[i,s])/probmellom[i,s]-d$StamAntMelHo[i]*4) else 0} 
  }}



# Storlaks 

for(s in 1:n.sim){
  for(i in 1:n.years){
    kghunstor[i,s]<- if(is.na(d$Obs_laks_ant_o7kg[i]) | d$Obs_laks_ant_o7kg[i] == 0){
      if((KgOppdrStor[i,s] < d$Laks_vekt_o7kg[i] + d$Gjen_vekt_o7kg[i]) & !is.na(Expstor[i,s]))
        ((Gjen_vekt_ho_o7kg_just[i, s] + Laks_vekt_ho_o7kg_just[i,s])/FangstAndStor[i,s] - d$StamAntStorHo[i]*2 - Gjen_vekt_ho_o7kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_o7kg_just[i,s]) else 0}
    else {
      if(KgOppdrStor[i,s]<d$vekt_stor_obs[i])
        (((d$vekt_stor_obs[i]-KgOppdrStor[i,s])*prhustor[i,s])/probstor[i,s]-d$StamAntStorHo[i]*8) else 0} 
  }}

# Totalt kg hoer 
# Legger sammen totalt kg hoer, og setter NA i år der det ikke er oppgitt beskatningsrate 

for(s in 1:n.sim){
  for(i in 1:n.years){
    kghuntot[i,s]<-if(!is.na(Expsmall[i,s]) & !is.na(Expmellom[i,s]) & !is.na(Expstor[i,s]))(kghunsmall[i,s]+kghunmellom[i,s]+kghunstor[i,s]) else NA
     }}


# Triangulærfordeling gytebestandsmål

gbm<-empty.matrix

for(i in 1:n.years){
  gbm[i,]<-rtriangle(n = n.sim,a=d$GBMmin[i],b=d$GBMmax[i],c=d$GBMkghunner[i])
}



# Oppnåelse gytebestandsmål
# 1 = nådd, 0 = ikke nådd

naadgbm<-empty.matrix

for(s in 1:n.sim){
  for(i in 1:n.years){
    naadgbm[i,s]<- if (!is.na(kghuntot[i,s])) {if(kghuntot[i,s]>gbm[i,s]) 1 else 0} else NA}}


Prosnaad<-empty.matrix2

for(i in 1:n.years){
  Prosnaad[i]<-(sum(naadgbm[i,])*100)/n.sim}


Prosnaadsistefire <- empty.matrix2
for(i in 4:n.years){
  Prosnaadsistefire[i] <- (Prosnaad[i]+ Prosnaad[i-1] + Prosnaad[i-2] + Prosnaad[i-3])/4
}

# Prosnaad siste fire
(Prosnaad[n.years]+Prosnaad[n.years-1]+Prosnaad[n.years-2]+Prosnaad[n.years-3])/4



# Prosent oppnådd gytebestandsmål

prosavgbm<-empty.matrix

for(s in 1:n.sim){
  for(i in 1:n.years){
    prosavgbm[i,s]<- if(!is.na(kghuntot[i,s])){
      if((100*kghuntot[i,s]/gbm[i,s])>100) 100 else (100*kghuntot[i,s]/gbm[i,s])} else NA}
  }

prosmaaloppnaaelse<-empty.matrix2

for(i in 1:n.years){prosmaaloppnaaelse[i]<-(sum(prosavgbm[i,]))/n.sim}


prosmaaloppnaaelsesistefire <- empty.matrix2
for(i in 4:n.years){
  prosmaaloppnaaelsesistefire[i]<- (prosmaaloppnaaelse[i] + prosmaaloppnaaelse[i-1] + prosmaaloppnaaelse[i-2] + prosmaaloppnaaelse[i-3])/4
}

# prosmaaloppnaaelse siste fire:
(prosmaaloppnaaelse[n.years] + prosmaaloppnaaelse[n.years-1] + prosmaaloppnaaelse[n.years-2] + prosmaaloppnaaelse[n.years-3])/4



# Prosent oppnådd gytebestandsmål utrunkert 

prosavgbmutrunk<-empty.matrix

for(s in 1:n.sim){
  for(i in 1:n.years){
    prosavgbmutrunk[i,s]<-100*kghuntot[i,s]/gbm[i,s]}}

prosmaaloppnaaelseutrunk<-empty.matrix2
for(i in 1:n.years){
  prosmaaloppnaaelseutrunk[i]<-(sum(prosavgbmutrunk[i,]))/n.sim}


Utrunksistefire <- empty.matrix2
for(i in 4:n.years){
  Utrunksistefire[i] <- (prosmaaloppnaaelseutrunk[i]+prosmaaloppnaaelseutrunk[i-1]+prosmaaloppnaaelseutrunk[i-2]+prosmaaloppnaaelseutrunk[i-3])/4
}

# Utrunk siste fire:
(prosmaaloppnaaelseutrunk[n.years]+prosmaaloppnaaelseutrunk[n.years-1]+prosmaaloppnaaelseutrunk[n.years-2]+prosmaaloppnaaelseutrunk[n.years-3])/4



# Fil med måloppnåelse 

Tilfilmaaloppnaelse <- data_frame(Prosnaad, Prosnaadsistefire, prosmaaloppnaaelse, prosmaaloppnaaelsesistefire, prosmaaloppnaaelseutrunk, Utrunksistefire) %>% 
  mutate(Vassdrag = d$Vassdrag) %>% 
  mutate(Vdrnr = d$Vdrnr) %>% 
  mutate(Aar = d$Aar) %>% 
  select(Vassdrag:Aar, everything())


if(paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files()) {
  write.table(Tilfilmaaloppnaelse, paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
              col.names=FALSE, append=TRUE)
} else {
  write.table(Tilfilmaaloppnaelse, paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
}


# Fjerne duplikater 
Maaloppnaaelse <- read.table(paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), header = T)

Maaloppnaaelse <- Maaloppnaaelse[ !duplicated(Maaloppnaaelse[, c("Vassdrag", "Aar")], fromLast=T),]

write.table(Maaloppnaaelse, paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)

# Dersom du vil skrive ut fila i excelformat:
# write.xlsx(Maaloppnaaelse, paste("Maaloppnaaelse", d$Aar[max(n.years)], ".xlsx", sep = ""), append = FALSE, row.names = FALSE)



##### ----- FIGURER -----####


# Dataramme for å bruke til figur 1; kg hoer 

kghuntot_df <- as_tibble(kghuntot) %>%  
  mutate(Aar = c(d$Aar)) %>% 
  pivot_longer(cols = c(1:5000)) %>%  # Gjøre om til langt format, sånn at vi får 5000 rader per år 
  rename(kghuntot = value)            # Kolonne med kghuntot

# Mangler avliva og gjenutsatt fangst og uten telling ---> NA 
mangler_data <- with(d, ifelse(Laks_ant == 0 & Gjen_ant == 0 & telling == 0,  1, 0))
Aar <- as.vector(d$Aar)
datagrunnlag <- cbind(Aar, mangler_data)
 
kghuntot_df <- merge(kghuntot_df, datagrunnlag, by = "Aar")  

kghuntot_df <- kghuntot_df %>% 
   mutate(kghuntot=replace(kghuntot, mangler_data == 1, NA)) # Legge inn NA i år som mangler datagrunnlag


# Dataramme for å plotte figur 1 og 2; prosent måloppnåelse og sannsynlighet for måloppnåelse

Tilplottmaaloppnaaelse <- merge(Tilfilmaaloppnaelse, datagrunnlag, by = "Aar", all.x = TRUE)

Tilplottmaaloppnaaelse <- Tilplottmaaloppnaaelse %>% 
   mutate(Prosnaad = replace(Prosnaad, mangler_data == 1, NA)) %>% 
   mutate(prosmaaloppnaaelse = replace(prosmaaloppnaaelse, mangler_data == 1, NA))       # Erstatte Prosnaad med NA 
 



## Lage Figurer

p <- kghuntot_df %>% ggplot(aes(x = factor(Aar), y = kghuntot))+
  theme_classic()+
  geom_boxplot(outlier.shape = NA, col = "black", fatten = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(axis.title = element_text(size = 10))+
  scale_x_discrete(breaks = seq(1993, max(d$Aar), by = 2))+ # "by = " intervall på hvor mange tall som skal vises på x-akse
  ylab("Gytebestand kg hunner")+
  xlab("År")+
  geom_hline(data = d, aes(yintercept = GBMkghunner[1]), size = 1)+
  geom_hline(data = d, aes(yintercept = GBMmin[1]))+
  geom_hline(data = d, aes(yintercept = GBMmax[1]))+
  ylim(0, NA)



p2 <- Tilplottmaaloppnaaelse %>% ggplot(aes(x = factor(Aar), y = Prosnaad))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1))+
  theme(axis.title = element_text(size = 10))+
  theme(axis.title.y = element_text(margin = margin(r = -20)))+
  scale_x_discrete(breaks = seq(1993, max(d$Aar), by = 2))+
  ylab("Prosent sannsynlighet for oppnådd GBM")+
  xlab("År")+
  ylim(0,100)


# Legge inn stjerne på år som mangler datagrunnlag, dersom det er noen år som mangler datagrunnlag
if (sum(is.na(Tilplottmaaloppnaaelse$Prosnaad)) > 0) {
  p2 <- p2 + geom_text(data = subset(Tilplottmaaloppnaaelse, is.na(Prosnaad)), aes(y = 0, label = "*"))
} else {
  p2 <- p2 
}



p3 <- Tilplottmaaloppnaaelse %>% ggplot(aes(x = factor(Aar), y = prosmaaloppnaaelse, group = Aar))+
  geom_col()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  theme(axis.title = element_text(size = 10))+
  scale_x_discrete(breaks = seq(1993, max(d$Aar), by = 2))+
  ylab("Prosent oppnåelse av GBM")+
  xlab("År")+
  #ggtitle(d$Vassdrag[1])+
  ylim(c(0,100))


if (sum(is.na(Tilplottmaaloppnaaelse$prosmaaloppnaaelse)) > 0) {
  p3 <- p3 + geom_text(data = subset(Tilplottmaaloppnaaelse, is.na(prosmaaloppnaaelse)), aes(y = 0, label = "*"))
} else {
  p3 <- p3 
}



fig <-  cowplot::plot_grid(             # Kombinere figurene i en figur
  p, p2, p3, 
  nrow = 2,
  ncol = 2,
  align = "vh")


# Legge til elvenavn 
fig <- annotate_figure(fig,                 
                       top = text_grob(d$Vassdrag[1], face = "bold", size = 14),
)

# Legge til tekstforklaring for stjerne, dersom det er år som mangler datagrunnlag 
if (sum(is.na(Tilplottmaaloppnaaelse$prosmaaloppnaaelse)) > 0){
  fig <- annotate_figure(fig, bottom = text_grob("* Mangler datagrunnlag for vurdering", vjust = -7, hjust = -0.5, size = 10))
} else {fig <- fig}


fig

# Lagre figur
ggsave(fig, filename=paste("Figur",d$Vassdrag[1], d$Aar[max(n.years)],".jpeg",sep=""), width = 7, height = 7)   



########################################################

Prosnaadsistefire[n.years]
prosmaaloppnaaelsesistefire[n.years]
Utrunksistefire[n.years]

