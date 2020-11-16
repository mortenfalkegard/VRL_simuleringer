#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar ferdige vassdragsdata og kjører en full gytebestandssimulering (basert på skriptet til Astrid)
# Output fra skriptet er data som kan importeres rett inn i sjøfangstfordelingen.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(dplyr)    # pakke for datasettmanipulering
library(tidyr)    # pakke for datasettmanipulering
library(openxlsx)
library(ggplot2)  # pakke for plotting
library(cowplot)  # pakke for plotting
library(ggpubr)   # pakke for å sette sammen flere plott
library(triangle)
library(stringr)

elveliste <- read.csv("data/elveliste.csv", header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")
antall_elver <- nrow(elveliste)

for (m in 1:antall_elver) {
  if (elveliste$GytingSim[m]) { # test om vassdraget skal inkluderes i simulering, loop til neste vassdrag dersom ikke
    
    ElvFilNavn <- paste("data/vassdrag/", elveliste[m, "Filnavn"], ".csv", sep="")
    d <- read.csv(ElvFilNavn, header = TRUE, sep = ";", fileEncoding = "UTF-8")
    
    #---------------------------------------------------------------------------------------------------------
    # her starter det originale skriptet til Astrid
    #---------------------------------------------------------------------------------------------------------
    
    # Sette beskatningsrate og fangstandel til NA n?r det mangler fangst og gjennutsatte og ikke er telling
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
    
    # Feil navn p? Gjen_vekt_o7kg i inputfil, ta vekk linja under n?r dette er fikset i inputfiler
    d <- d %>% rename(Gjen_vekt_o7kg = Gjen_vekto7kg) 
    
    # setter vekt og antall til 0 for ? forenkle skriptet 
    
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
    
    
    #### --------- Vekt i ?r med telling --------- ####
    
    # For ?r som har fangst og/eller gjenutsatte regner vi ut gjennomsnittsvekt: gjen_vekt+laks_vekt/gjen_ant+Laks_ant 
    
    # ?r uten fangst eller gjenutsatte : gjennomsnittet av fangst og gjenutsatt 5 n?rmeste ?r
    
    if(sum(d$telling) > 0){                                   # Denne chuncken kj?res bare dersom det er telling 
      
      d$Laks_vekt_u3kg_sum <- with(d, Laks_vekt_u3kg + Gjen_vekt_u3kg)
      d$Laks_ant_u3kg_sum <- with(d, Laks_ant_u3kg + Gjen_ant_u3kg)
      
      d$Laks_vekt_o3u7kg_sum <- with(d, Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg)
      d$Laks_ant_o3u7kg_sum <- with(d, Laks_ant_o3u7kg + Gjen_ant_o3u7kg)
      
      d$Laks_vekt_o7kg_sum <- with(d, Laks_vekt_o7kg + Gjen_vekt_o7kg)
      d$Laks_ant_o7kg_sum <- with(d, Laks_ant_o7kg + Gjen_ant_o7kg)
      
      
      ### SM?LAKS 
      
      # Mangler avliva fangst i ?r med telling
      uten_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & d$Laks_vekt_u3kg_sum == 0, 1, 0)
      
      # F?rre enn 5 avliva fangst i ?r med telling  
      lite_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & (d$Laks_ant_u3kg < 5 & d$Laks_ant_u3kg > 0), 1, 0)
      
      
      if(sum(lite_fangst_smaa, na.rm = T) > 0 | sum(uten_fangst_smaa, na.rm = T) > 0){
        
        lite_eller_uten_fangst_smaa <- which(lite_fangst_smaa | uten_fangst_smaa)  # Rader med fangst f?rre enn 5
        
        
        # F?rre enn totalt 5 avliva storlaks i tidsserien ---> default snittvekt   
        if(sum(d$Laks_ant_u3kg_sum, na.rm = T) < 5){
          
          d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- 2 
          d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- 1
          
        } 
        
        else {
          
          fangst <- which(d$Laks_vekt_u3kg_sum > 0)       # Finne rader som har vekt
          
          # Dersom det er f?rre enn fem ?r med avliva laks, bruk snitt av alle disse ?rene   
          if(length(fangst) < 5) {
            
            d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- sum(d$Laks_vekt_u3kg_sum)
            d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa[1:length(lite_eller_uten_fangst_smaa)]] <- sum(d$Laks_ant_u3kg_sum)
            
          } else {
            
            # Hvis det er flere enn fem ?r med fangst, bruker vi vekt fra fem n?rmeste ?r som har vekt: 
            # Funksjon for ? sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
            
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
      
      # Mangler avliva fangst for ?r med telling
      uten_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & d$Laks_vekt_o3u7kg_sum == 0, 1, 0)
      
      # F?rre enn 5 avliva fangst for ?r med telling  
      lite_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & (d$Laks_ant_o3u7kg < 5 & d$Laks_ant_o3u7kg > 0), 1, 0)
      
      
      if(sum(lite_fangst_mellom, na.rm = T) > 0 | sum(uten_fangst_mellom, na.rm = T) > 0){
        
        lite_eller_uten_fangst_mellom <- which(lite_fangst_mellom | uten_fangst_mellom)  # Rader med fangst f?rre enn 5
        
        
        # F?rre enn totalt 5 avliva storlaks i tidsserien ---> standard snittvekt   
        if(sum(d$Laks_ant_o3u7kg_sum, na.rm = T) < 5){
          
          d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- 4 
          d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- 1
          
        } 
        
        else {
          
          fangst <- which(d$Laks_vekt_o3u7kg_sum > 0)       # Finne rader som har vekt
          
          # Dersom det er f?rre enn fem ?r med avliva laks, bruk snitt av alle disse    
          if(length(fangst) < 5) {
            
            d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- sum(d$Laks_vekt_o3u7kg_sum)
            d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom[1:length(lite_eller_uten_fangst_mellom)]] <- sum(d$Laks_ant_o3u7kg_sum)
            
          } else {
            
            # Hvis det er flere enn fem ?r med fangst, bruker vi vekt fra fem n?rmeste ?r som har vekt: 
            # Funksjon for ? sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
            
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
      
      # Mangler avliva fangst for ?r med telling
      uten_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & d$Laks_vekt_o7kg_sum == 0, 1, 0)
      
      # F?rre enn 5 avliva fangst for ?r med telling  
      lite_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & (d$Laks_ant_o7kg < 5 & d$Laks_ant_o7kg > 0), 1, 0)
      
      
      if(sum(lite_fangst_stor, na.rm = T) > 0 | sum(uten_fangst_stor, na.rm = T) > 0){
        
        lite_eller_uten_fangst_stor <- which(lite_fangst_stor | uten_fangst_stor)  # Rader med fangst f?rre enn 5
        
        
        # F?rre enn totalt 5 avliva storlaks i tidsserien ---> standard snittvekt   
        if(sum(d$Laks_ant_o7kg_sum, na.rm = T) < 5){
          
          d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- 8 
          d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- 1
          
        } 
        
        else {
          
          fangst <- which(d$Laks_vekt_o7kg_sum > 0)       # Finne rader som har vekt
          
          # Dersom det er f?rre enn fem ?r med avliva laks, bruk snitt av alle disse    
          if(length(fangst) < 5) {
            
            d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- sum(d$Laks_vekt_o7kg_sum)
            d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor[1:length(lite_eller_uten_fangst_stor)]] <- sum(d$Laks_ant_o7kg_sum)
            
          } else {
            
            # Hvis det er flere enn fem ?r, bruker vi vekt fra fem n?rmeste ?r som har vekt: 
            # Funksjon for ? sortere de 1-5 minste absoluttavstandene til rader med fangst for hver rad uten eller med lite fangst      
            
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
      
      
      # Vekt til ? bruke ved telling - gjennomsnittsvekt fra fangst og/eller gjenutsatte * antall fra tellinger 
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
    
    
    
    
    # Simulere prosent antatt observert dersom det er  ?r med telling, og regne ut beskatningsrate fra dette   
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
      
      
      
      # Oppdrettantall (trenger dette for ? regne ut antall ville)
      
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
      
      # Sm?laks
      
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
      
      # Oppdatering: regner ut beskatningsrate fra tellinger, der vi har tellinger for gitt aldersklasse og beskatningsrate mangler. Hvis ikke blir beskatningsrate st?ende som den er.  
      
      d$ExpSmallMed <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMed), Laks_ant_u3kg/median(AntVillSmaa, na.rm = T), ExpSmallMed))
      d$ExpSmallMin <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMin), Laks_ant_u3kg/max(AntVillSmaa, na.rm = T), ExpSmallMin))
      d$ExpSmallMax <- with(d, ifelse(!is.na(Obs_laks_ant_u3kg) & is.na(ExpSmallMax), Laks_ant_u3kg/min(AntVillSmaa, na.rm = T), ExpSmallMax))
      
      d$ExpMellomMed <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMed), Laks_ant_o3u7kg/median(AntVillMel, na.rm = T), ExpMellomMed))
      d$ExpMellomMin <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMin), Laks_ant_o3u7kg/max(AntVillMel, na.rm = T), ExpMellomMin))
      d$ExpMellomMax <- with(d, ifelse(!is.na(Obs_laks_ant_o3u7kg) & is.na(ExpMellomMax), Laks_ant_o3u7kg/min(AntVillMel, na.rm = T), ExpMellomMax))
      
      d$ExpStorMed <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMed), Laks_ant_o7kg/median(AntVillStor, na.rm = T), ExpStorMed)) # Funker med NA her, viss det ikke er grunnlag for ? regne ut antall ville (NA), blir dette NA 
      d$ExpStorMin <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMin), Laks_ant_o7kg/max(AntVillStor, na.rm = T), ExpStorMin))
      d$ExpStorMax <- with(d, ifelse(!is.na(Obs_laks_ant_o7kg) & is.na(ExpStorMax), Laks_ant_o7kg/min(AntVillStor, na.rm = T), ExpStorMax))
      
    }
    
    
    # Regne ut beskatningsrate for ?r der det ikke er oppgitt 
    # Regne ut fangstandel for ?r der det ikke er oppgitt
    
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
    
    # Dersom fila allerede finnes, s? legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil. 
    
    if(paste("Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files("results")) {
      write.table(Beskatning_og_fangstandel, paste("results/Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
                  col.names=FALSE, append=TRUE)
    } else {
      write.table(Beskatning_og_fangstandel, paste("results/Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
    }
    
    # Fjerne duplikater
    Beskatning_og_fangstandel <- read.table(paste("results/Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
    Beskatning_og_fangstandel <- Beskatning_og_fangstandel[ !duplicated(Beskatning_og_fangstandel[, c("Vassdrag", "Aar")], fromLast=T),]
    
    write.table(Beskatning_og_fangstandel, paste("results/Beskatning_FangstAndel", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)
    
    
    
    
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
    # Oppdatering: regne hvis antall oppdrett og beskatning er st?rre enn 0, eller sette til 0.
    
    AntOppdr<-empty.matrix
    AntOppdrSmaa<-empty.matrix
    AntOppdrMel<-empty.matrix
    AntOppdrStor<-empty.matrix
    
    for(s in 1:n.sim){
      for (i in 1:n.years){
        AntOppdrSmaa[i,s]<- if(Oppdrettsmaa[i,s] > 0 & Expsmall[i,s] > 0 & !is.na(Expsmall[i,s])) (Oppdrettsmaa[i,s]/Expsmall[i,s]) else 0
        AntOppdrMel[i,s]<-  if(Oppdrettmel[i,s] > 0 & Expmellom[i,s] > 0 & !is.na(Expmellom[i,s])) (Oppdrettmel[i,s]/Expmellom[i,s]) else 0
        AntOppdrStor[i,s]<- if(Oppdrettstor[i,s] > 0 & Expstor[i,s] > 0 & !is.na(Expstor[i,s])) (Oppdrettstor[i,s]/Expstor[i,s]) else 0
        AntOppdr[i,s]<-AntOppdrSmaa[i,s]+AntOppdrMel[i,s]+AntOppdrStor[i,s]
      }
    }
    
    # ENDRINGER: 
    # bruker tellinger om det er tellinger for den gitte aldersklassen
    # Antall villaks bli kun regnet ut fra fangstandel dersom fangstandel er oppgitt og st?rre enn 0. Ellers satt til 0. 
    
    AntVillSmaa<-empty.matrix
    AntVillMel<-empty.matrix
    AntVillStor<-empty.matrix
    AntVillTot<-empty.matrix
    
    
    # Sm?laks
    
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
    # S?rge for at det blir NA der det skal v?re NA 
    
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
    
    # Sette ?r uten grunnlag for vurdering til NA 
    
    # Vill elvebestand sm?laks, mellomlaks og storlaks til NA i ?r der beskatningsrate er NA
    TilfilAntallVillogOppdrett[which(is.na(d$ExpSmallMin)), 6:10] <- NA
    TilfilAntallVillogOppdrett[which(is.na(d$ExpMellomMin)), 11:15] <- NA
    TilfilAntallVillogOppdrett[which(is.na(d$ExpStorMin)), 16:20] <- NA
    
    # Total elvebestand satt til NA dersom der ikke er avliva eller gjenutsatt fangst og ikke telling
    TilfilAntallVillogOppdrett[which(d$Laks_ant == 0 & d$Gjen_ant == 0 & d$telling == 0), 21:25] <- NA
    
    # Oppdrettantall og AntOppdr satt til NA dersom det ikke er avliva fangst
    TilfilAntallVillogOppdrett[which(d$Laks_ant == 0), 26:35] <- NA
    
    
    # Dersom fila allerede finnes, s? legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil. 
    
    if(paste("AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files("results")) {
      write.table(TilfilAntallVillogOppdrett, paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
                  col.names=FALSE, append=TRUE)
    } else {
      write.table(TilfilAntallVillogOppdrett, paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
    }
    
    
    # Fjerner duplikater:  beholder bare det som ble kj?rt sist 
    
    AntVillogOpp <- read.table(paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
    AntVillogOpp <- AntVillogOpp[ !duplicated(AntVillogOpp[, c("Vassdrag", "Aar")], fromLast=T),]
    
    
    write.table(AntVillogOpp, paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)
    
    
    
    
    
    #### ------- Simulering gytebestand -------#### 
    
    
    # Triangul?rfordeling andel d?de
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
    
    # Sm?laks
    
    
    for(s in 1:n.sim) {
      
      for(i in 1:n.years) {
        
        kghunsmall[i,s] <- if(is.na(d$Obs_laks_ant_u3kg[i]) | d$Obs_laks_ant_u3kg[i] == 0) {
          if((KgOppdrSmaa[i,s] < d$Laks_vekt_u3kg[i] + d$Gjen_vekt_u3kg[i]) & !is.na(FangstAndSmall[i,s]))
            ((Gjen_vekt_ho_u3kg_just[i, s] + Laks_vekt_ho_u3kg_just[i,s])/FangstAndSmall[i,s] - d$StamAntSmaHo[i]*2 - Gjen_vekt_ho_u3kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_u3kg_just[i,s])
          else 
            0
        }
        else {
          if(KgOppdrSmaa[i,s]<d$vekt_smaa_obs[i])
            ((d$vekt_smaa_obs[i]*prhusmall[i,s])/probsmall[i,s]-d$StamAntSmaHo[i]*2) 
          else 
            0
        }
      }
    }

    # Mellomlaks
    
    for(s in 1:n.sim) {
      
      for(i in 1:n.years) {
        
        kghunmellom[i,s] <- if(is.na(d$Obs_laks_ant_o3u7kg[i]) | d$Obs_laks_ant_o3u7kg[i] == 0) {
          if((KgOppdrMel[i,s] < d$Laks_vekt_o3u7kg[i] + d$Gjen_vekt_o3u7kg[i]) & !is.na(Expmellom[i,s]))
            ((Gjen_vekt_ho_o3u7kg_just[i, s] + Laks_vekt_ho_o3u7kg_just[i,s])/FangstAndMellom[i,s] - d$StamAntMelHo[i]*2 - Gjen_vekt_ho_o3u7kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_o3u7kg_just[i,s]) 
          else 
            0
        }
        else {
          if(KgOppdrMel[i,s]<d$vekt_mellom_obs[i])
            (((d$vekt_mellom_obs[i]-KgOppdrMel[i,s])*prhumellom[i,s])/probmellom[i,s]-d$StamAntMelHo[i]*4) 
          else 
            0
        }
      }
    }

    # Storlaks 
    
    for(s in 1:n.sim) {
      
      for(i in 1:n.years) {
        
        kghunstor[i,s] <- if(is.na(d$Obs_laks_ant_o7kg[i]) | d$Obs_laks_ant_o7kg[i] == 0) {
          if((KgOppdrStor[i,s] < d$Laks_vekt_o7kg[i] + d$Gjen_vekt_o7kg[i]) & !is.na(Expstor[i,s]))
            ((Gjen_vekt_ho_o7kg_just[i, s] + Laks_vekt_ho_o7kg_just[i,s]) / FangstAndStor[i,s] - d$StamAntStorHo[i] * 2 - Gjen_vekt_ho_o7kg_just[i,s] * AndelDode[s] - Laks_vekt_ho_o7kg_just[i,s])
          else 
            0
        }
        else {
          if(KgOppdrStor[i,s] < d$vekt_stor_obs[i])
            (((d$vekt_stor_obs[i] - KgOppdrStor[i,s]) * prhustor[i,s]) / probstor[i,s] - d$StamAntStorHo[i] * 8) 
          else 
            0
        }
      }
    }

    # Totalt kg hoer 
    # Legger sammen totalt kg hoer, og setter NA i ?r der det ikke er oppgitt beskatningsrate 
    
    for(s in 1:n.sim) {
      for(i in 1:n.years) {
        kghuntot[i,s] <- if(!is.na(Expsmall[i,s]) & !is.na(Expmellom[i,s]) & !is.na(Expstor[i,s]))
          (kghunsmall[i,s]+kghunmellom[i,s]+kghunstor[i,s]) 
        else 
          NA
      }
    }

    #--------------------------------------------------------------------------------------------------------------
    # NY SEKSJON: Lag en rapporteringsfil med år for år median justert hunnfangst og kg hunnlaks i ulike størrelser
    #--------------------------------------------------------------------------------------------------------------
    
    FangstHunnU3_Justert <- empty.matrix2
    FangstHunn37_Justert <- empty.matrix2
    FangstHunnO7_Justert <- empty.matrix2
    
    GytingHunnU3 <- empty.matrix2
    GytingHunn37 <- empty.matrix2
    GytingHunnO7 <- empty.matrix2
    
    AndelHunnU3 <- empty.matrix2
    AndelHunn37 <- empty.matrix2
    AndelHunnO7 <- empty.matrix2
    
    for(i in 1:n.years) {
      FangstHunnU3_Justert[i] <- median(Laks_vekt_ho_u3kg_just[,i])
      FangstHunn37_Justert[i] <- median(Laks_vekt_ho_o3u7kg_just[,i])
      FangstHunnO7_Justert[i] <- median(Laks_vekt_ho_o7kg_just[,i])
      GytingHunnU3[i] <- median(kghunsmall[,i])
      GytingHunn37[i] <- median(kghunmellom[,i])
      GytingHunnO7[i] <- median(kghunstor[,i])
      AndelHunnU3[i] <- median(prhusmall[,i])
      AndelHunn37[i] <- median(prhumellom[,i])
      AndelHunnO7[i] <- median(prhustor[,i])
    }
    
    Tilfilkggyting <- data_frame(FangstHunnU3_Justert, FangstHunn37_Justert, FangstHunnO7_Justert, 
                                 GytingHunnU3, GytingHunn37, GytingHunnO7, AndelHunnU3, AndelHunn37, AndelHunnO7) %>% 
      mutate(Vassdrag = d$Vassdrag) %>% 
      mutate(Vdrnr = d$Vdrnr) %>% 
      mutate(Aar = d$Aar) %>% 
      select(Vassdrag:Aar, everything())
    
    
    if(paste("KgHunnlaks", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files("results")) {
      write.table(Tilfilkggyting, paste("results/KgHunnlaks", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
                  col.names=FALSE, append=TRUE)
    } else {
      write.table(Tilfilkggyting, paste("results/KgHunnlaks", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
    }
    
    # Fjerne duplikater 
    KgHunnlaksFil <- read.table(paste("results/KgHunnlaks", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
  
    KgHunnlaksFil <- KgHunnlaksFil[ !duplicated(KgHunnlaksFil[, c("Vassdrag", "Aar")], fromLast=T),]
    
    write.table(KgHunnlaksFil, paste("results/KgHunnlaks", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)
    
    #--------------------------------------------------------------------------------------------------------------
    # SLUTT NY SEKSJON. Nedenfor fortsetter originalskriptet fra Astrid
    #--------------------------------------------------------------------------------------------------------------
    
    
    # Triangul?rfordeling gytebestandsm?l
    
    gbm<-empty.matrix
    
    for(i in 1:n.years) {
      gbm[i,] <- rtriangle(n = n.sim, a = d$GBMmin[i], b = d$GBMmax[i], c = d$GBMkghunner[i])
    }
    
    
    
    # Oppn?else gytebestandsm?l
    # 1 = n?dd, 0 = ikke n?dd
    
    naadgbm<-empty.matrix
    
    for(s in 1:n.sim) {
      for(i in 1:n.years) {
        naadgbm[i,s] <- if (!is.na(kghuntot[i,s])) {
          if(kghuntot[i,s] > gbm[i,s]) 
            1 
          else 
            0
        } 
        else 
          NA
      }
    }
    
    
    Prosnaad <- empty.matrix2
    
    for(i in 1:n.years) {
      Prosnaad[i] <- (sum(naadgbm[i,]) * 100) / n.sim
    }

    Prosnaadsistefire <- empty.matrix2
    for(i in 4:n.years){
      Prosnaadsistefire[i] <- (Prosnaad[i] + Prosnaad[i-1] + Prosnaad[i-2] + Prosnaad[i-3]) / 4
    }
    
    # Prosnaad siste fire
    (Prosnaad[n.years] + Prosnaad[n.years - 1] + Prosnaad[n.years - 2] + Prosnaad[n.years - 3]) / 4
    
    
    
    # Prosent oppn?dd gytebestandsm?l
    
    prosavgbm <- empty.matrix
    
    for(s in 1:n.sim) {
      for(i in 1:n.years) {
        prosavgbm[i,s] <- if(!is.na(kghuntot[i,s])) {
          if((100 * kghuntot[i,s] / gbm[i,s]) > 100) 
            100 
          else 
            (100 * kghuntot[i,s] / gbm[i,s])
        } 
        else 
          NA
      }
    }

    prosmaaloppnaaelse<-empty.matrix2
    
    for(i in 1:n.years) { 
      prosmaaloppnaaelse[i] <- (sum(prosavgbm[i,])) / n.sim
    }

    prosmaaloppnaaelsesistefire <- empty.matrix2
    for(i in 4:n.years) {
      prosmaaloppnaaelsesistefire[i] <- (prosmaaloppnaaelse[i] + prosmaaloppnaaelse[i-1] + prosmaaloppnaaelse[i-2] + prosmaaloppnaaelse[i-3]) / 4
    }
    
    # prosmaaloppnaaelse siste fire:
    (prosmaaloppnaaelse[n.years] + prosmaaloppnaaelse[n.years-1] + prosmaaloppnaaelse[n.years-2] + prosmaaloppnaaelse[n.years-3])/4
    
    
    
    # Prosent oppn?dd gytebestandsm?l utrunkert 
    
    prosavgbmutrunk<-empty.matrix
    
    for(s in 1:n.sim) {
      for(i in 1:n.years) {
        prosavgbmutrunk[i,s] <- 100 * kghuntot[i,s] / gbm[i,s]
      }
    }
    
    prosmaaloppnaaelseutrunk <- empty.matrix2
    for(i in 1:n.years) {
      prosmaaloppnaaelseutrunk[i] <- (sum(prosavgbmutrunk[i,])) / n.sim
    }

    Utrunksistefire <- empty.matrix2
    for(i in 4:n.years) {
      Utrunksistefire[i] <- (prosmaaloppnaaelseutrunk[i] + prosmaaloppnaaelseutrunk[i-1] + prosmaaloppnaaelseutrunk[i-2] + prosmaaloppnaaelseutrunk[i-3]) / 4
    }
    
    # Utrunk siste fire:
    (prosmaaloppnaaelseutrunk[n.years]+prosmaaloppnaaelseutrunk[n.years-1]+prosmaaloppnaaelseutrunk[n.years-2]+prosmaaloppnaaelseutrunk[n.years-3])/4
    
    
    
    # Fil med m?loppn?else 
    
    Tilfilmaaloppnaelse <- data_frame(Prosnaad, Prosnaadsistefire, prosmaaloppnaaelse, prosmaaloppnaaelsesistefire, prosmaaloppnaaelseutrunk, Utrunksistefire) %>% 
      mutate(Vassdrag = d$Vassdrag) %>% 
      mutate(Vdrnr = d$Vdrnr) %>% 
      mutate(Aar = d$Aar) %>% 
      select(Vassdrag:Aar, everything())
    
    
    if(paste("Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = "") %in% list.files("results")) {
      write.table(Tilfilmaaloppnaelse, paste("results/Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE,
                  col.names=FALSE, append=TRUE)
    } else {
      write.table(Tilfilmaaloppnaelse, paste("results/Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), row.names=FALSE)
    }
    
    
    # Fjerne duplikater 
    Maaloppnaaelse <- read.table(paste("results/Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), header = T)
    
    Maaloppnaaelse <- Maaloppnaaelse[ !duplicated(Maaloppnaaelse[, c("Vassdrag", "Aar")], fromLast=T),]
    
    write.table(Maaloppnaaelse, paste("results/Maaloppnaaelse", d$Aar[max(n.years)], ".txt", sep = ""), append = FALSE, row.names = FALSE)


    
    
  }
}






#------------------------------------------------------------------------------------------------------------
# subrutine for å gjøre om excelfiler lokalt til csv på Git
#------------------------------------------------------------------------------------------------------------

for (i in 1:antall_elver) {
  if (elveliste$GytingSim[i]) {
    ElvFilNavn <- paste("datatemp/", elveliste[i, "Filnavn"], ".xlsx", sep="")
    ValgtElv <- read.xlsx(ElvFilNavn, sheet = 1, startRow = 1, colNames = TRUE)
    TilFilNavn <- paste("data/v/", elveliste[i, "Filnavn"], ".csv", sep = "")
    write.table(ValgtElv, TilFilNavn, sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
  }
}

#------------------------------------------------------------------------------------------------------------
# subrutine for å teste csv-filer (noen får dessverre feil fra write.table)
#------------------------------------------------------------------------------------------------------------

for (i in 1:antall_elver) {
  if (elveliste$GytingSim[i]) {
    ElvFilNavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep="")
    d <- read.table(ElvFilNavn, header = TRUE, sep = ";", fileEncoding = "UTF-8")
  }
}

#------------------------------------------------------------------------------------------------------------
# subrutine for å konvertere SSB fangstfil
#------------------------------------------------------------------------------------------------------------
ssb_fangst <- read.xlsx("ssb elv 2010-2019.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# hent ut vassdrag og dropp fylkes- og landstall
ssb_fangst$VdrNr <- str_replace(ssb_fangst$VdrNr, "X1", "X1Z")
ssb_fangst$VdrNr <- str_replace(ssb_fangst$VdrNr, "246.1A", "246.1AZ")
ssb_filtrert <- filter(ssb_fangst, str_detect(VdrNr, "Z"))

# fjern fylkeskoder fra vassdragsnr
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "02-", "")
ssb_filtrert$VdrNr <- str_sub(ssb_filtrert$VdrNr, 3)

# manuelle endringer i vassdragsnr for å matche vassdragsnr i lakseregisteret
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "018.3Z", "018.2Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "041.32Z", "041.34Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "041.31Z", "041.32Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "051.1Z", "051.2AZ")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "065.4X1Z", "065.4Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "094.41Z", "094.42Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "110.33Z", "110.3Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "142.3ZX1Z", "142.3Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "170.510X1Z", "170.3Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "179.32Z", "179.5Z")
ssb_filtrert$VdrNr <- str_replace(ssb_filtrert$VdrNr, "185.442X1Z", "185.441Z")

write.table(ssb_filtrert, "data/ssb_elv_2010-2019.csv", sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
