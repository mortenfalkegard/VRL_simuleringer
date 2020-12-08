#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar utgangspunkt i gytebestand, beskatning, elvefangst og sjøfangst. Sjøfangst fordeles på
# regionnivå og så videre på vassdragsnivå. Skriptet beregner så innsig i forskjellige områder. Estimatene
# gjøres på størrelsesgruppe og både biomasse og antall.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(openxlsx)
library(tidyverse)
library(rio)

# Variablene nedenfor definerer startår og antall år som skal estimeres. Skriptet er fleksibelt organisert rundt disse for å gi mulighet
# til å gjennomføre historiske sammenligninger, for eksempel før og etter viktige reguleringsendringer
start_aar <- 2012
antall_aar <- 8

# Variable som gir navn på output-filer fra siste gytebestandsimulering
sim_beskat_filnavn <- "results/Beskatning_FangstAndel2019.txt"
sim_gyt_filnavn <- "results/Maaloppnaaelse2019.txt"
sim_villOppdr_filnavn <- "results/AntVillogOppdrettElvEstimater2019.txt"
sim_kghunnlaks_filnavn <- "results/KgHunnlaks2019.txt"

# initialiser en liste over vassdrag som skal være med i innsigsfordelingen med noen bakgrunnstall
elveliste <- import("data/elveliste.csv", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

# fordelingsnøkkel for fordeling av fangsten i kystregionene 
kyst_fordeling <- import("data/fordelingsnokkel_sjofangst.csv", encoding = "UTF-8")

# fordelingsnøkkel for å fordele kommunefangst til regioner
region_fordeling <- import("data/fordelingsnokkel_kommune_region.csv", encoding = "UTF-8")
antall_regioner <- ncol(region_fordeling) - 1
antall_kommuner <- nrow(region_fordeling)

# les inn fangststatistikk fra sjølaksefisket
sjofangst <- read.xlsx("data/fangst_sjo_1993-2019.xlsx", sheet = 2, startRow = 1, colNames = TRUE, cols = c(1:6, 9:11), na.strings = c(":", ".."))

# initier noen variable
region_fangst <- array(0, c(antall_aar, antall_regioner, 6))
fangst <- matrix(nrow = antall_kommuner, ncol = antall_regioner)
aar_liste <- vector()

elv_fangst <- array(0, c(antall_aar, antall_elver, 6)) # 6 =  3 størrelsesklasser for vekg og antall
elv_gyting <- array(0, c(antall_aar, antall_elver, 6))
elv_innsig <- array(0, c(antall_aar, antall_elver, 6))
elv_andelhunn <- matrix(nrow = antall_aar, ncol = antall_elver)
elv_hunnproporsjon <- array(0, c(antall_aar, antall_elver, 3)) # 3 størrelseskategorier
elv_beskatningsrate <- array(0, c(antall_aar, antall_elver, 3))
snittvekt <- vector()

#-----------------------------------------------------------------------------------------------------------------
# lag regional fangstmatrise for valgte år
#-----------------------------------------------------------------------------------------------------------------
for (j in 1:antall_aar) {
  aar_indeks <- start_aar + j - 1
  aar_liste[j] <- aar_indeks
  df <- filter(sjofangst, aar == aar_indeks)
  for (i in 1:6) {
    fangst <- df[,i+3] * region_fordeling[,2:(antall_regioner+1)]
    region_fangst[j, ,i] <- colSums(fangst, na.rm = TRUE)
  }
}

#-----------------------------------------------------------------------------------------------------------------
# les inn tabell med tall fra gytebestandsimulering samt data for andre vassdrag 
#-----------------------------------------------------------------------------------------------------------------

#simul_beskatning <- read.table(sim_beskat_filnavn, stringsAsFactors = FALSE, header = TRUE)
#simul_gyting <- read.table(sim_gyt_filnavn, stringsAsFactors = FALSE, header = TRUE)
#simul_villoppdrett <- read.table(sim_villOppdr_filnavn, stringsAsFactors = FALSE, header = TRUE)
simul_kghunnlaks <- import(sim_kghunnlaks_filnavn, encoding = "UTF-8")
simul_kghunnlaks[is.na(simul_kghunnlaks)] <- 0

# elvedatamatrise <- read.table("data/elvedata_2019.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8-BOM")
elvefangst_ssb <- import("data/ssb_elv_2010-2019.csv", encoding = "UTF-8")

#-----------------------------------------------------------------------------------------------------------------
# gå gjennom alle vassdrag, hent inn data for hver enkelt elv og beregn innsig 
#-----------------------------------------------------------------------------------------------------------------

for (i in 1:antall_elver) {

  elv_vdrnr <- elveliste$VdrNr[i]
  
  if(elveliste$GytingSim[i]) { # vassdrag med i gytebestandsimulering, hent data fra simulering og vassdragsfil
    
    elv_filnavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep="")
    elv_grunnlag <- read.table(elv_filnavn, header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
#    elv_grunnlag <- import(elv_filnavn, encoding = "UTF-8")
    elv_grunnlag <- elv_grunnlag %>% rename(Gjen_vekt_o7kg = Gjen_vekto7kg)

    for (j in 1:antall_aar) {
      df <- filter(simul_kghunnlaks, Aar == aar_liste[j], Vdrnr == elveliste$VdrNr[i])
      df2 <- filter(elv_grunnlag, Aar == aar_liste[j])
      df2[is.na(df2)] <- 0
      df3 <- filter(elvefangst_ssb, Aar == aar_liste[j])
      fangst_elv_ssb <- filter(elvefangst_ssb, Aar == aar_liste[j], VdrNr == elveliste$VdrNr[i])
      
      elv_hunnproporsjon[j, i, 1] <- df$AndelHunnU3
      elv_hunnproporsjon[j, i, 2] <- df$AndelHunn37
      elv_hunnproporsjon[j, i, 3] <- df$AndelHunnO7

      # kolonne 1-3 er data for vekt
      elv_fangst[j, i, 1] <- df$FangstHunnU3_Justert / df$AndelHunnU3
      elv_fangst[j, i, 2] <- df$FangstHunn37_Justert / df$AndelHunn37
      elv_fangst[j, i, 3] <- df$FangstHunnO7_Justert / df$AndelHunnO7
      
      # bruk simulert gyting hunn og andel hunn til å beregne gyting hann+hunn 
      elv_gyting[j, i, 1] <- df$GytingHunnU3 / df$AndelHunnU3
      elv_gyting[j, i, 2] <- df$GytingHunn37 / df$AndelHunn37
      elv_gyting[j, i, 3] <- df$GytingHunnO7 / df$AndelHunnO7
      
      # bruk snittvekt til å estimere fangst og gyting i antall, gitt i kolonne 4-6
      if(df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg > 4) {
        snittvekt_u3 <- (df2$Laks_vekt_u3kg + df2$Gjen_vekt_u3kg) / (df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg)
      } else {
        snittvekt_u3 <- sum(df3$Laks_vekt_u3kg) / sum(df3$Laks_ant_u3kg) # nasjonalt snitt hvis færre enn 5 laks
      }
      if(df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg > 4) {
        snittvekt_37 <- (df2$Laks_vekt_o3u7kg + df2$Gjen_vekt_o3u7kg) / (df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg)
      } else {
        snittvekt_37 <- sum(df3$Laks_vekt_o3u7kg) / sum(df3$Laks_ant_o3u7kg) # nasjonalt snitt hvis færre enn 5 laks
      }
      if(df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg > 4) {
        snittvekt_o7 <- (df2$Laks_vekt_o7kg + df2$Gjen_vekt_o7kg) / (df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg)
      } else {
        snittvekt_o7 <- sum(df3$Laks_vekt_o7kg) / sum(df3$Laks_ant_o7kg) # nasjonalt snitt hvis færre enn 5 laks
      }
      
      elv_fangst[j, i, 4] <- if(snittvekt_u3 > 0) (elv_fangst[j, i, 1] / snittvekt_u3) else 0
      elv_fangst[j, i, 5] <- if(snittvekt_37 > 0) (elv_fangst[j, i, 2] / snittvekt_37) else 0
      elv_fangst[j, i, 6] <- if(snittvekt_o7 > 0) (elv_fangst[j, i, 3] / snittvekt_o7) else 0
      
      elv_gyting[j, i, 4] <- if(snittvekt_u3 > 0) (elv_gyting[j, i, 1] / snittvekt_u3) else 0
      elv_gyting[j, i, 5] <- if(snittvekt_37 > 0) (elv_gyting[j, i, 2] / snittvekt_37) else 0
      elv_gyting[j, i, 6] <- if(snittvekt_o7 > 0) (elv_gyting[j, i, 3] / snittvekt_o7) else 0
      
      elv_innsig[j, i, ] <- elv_fangst[j, i, ] + elv_gyting[j, i, ]
    }
    
  } else { # elv uten simulering, hent data fra elvedatafiler
    
    elv_filnavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep="")
    elv_grunnlag <- import(elv_filnavn, encoding = "UTF-8")

    for (j in 1:antall_aar) {
#      df <- filter(elvedatamatrise, VdrNr == elv_vdrnr)
      df <- filter(elv_grunnlag, Aar == aar_liste[j])
      
      elv_andelhunn[j, i] <- df$Andel_hunn
      
      df2 <- filter(elvefangst_ssb, VdrNr == elv_vdrnr, Aar == aar_liste[j])
      df3 <- filter(elvefangst_ssb, Aar == aar_liste[j])
      
      snittvekt_u3 <- sum(df3$Laks_vekt_u3kg) / sum(df3$Laks_ant_u3kg)
      snittvekt_37 <- sum(df3$Laks_vekt_o3u7kg) / sum(df3$Laks_ant_o3u7kg)
      snittvekt_o7 <- sum(df3$Laks_vekt_o7kg) / sum(df3$Laks_ant_o7kg)
      
      andel_ant_u3 <- sum(df3$Laks_ant_u3kg) / sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg)
      andel_ant_37 <- sum(df3$Laks_ant_o3u7kg) / sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg)
      andel_ant_o7 <- sum(df3$Laks_ant_o7kg) / sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg)
      
      andel_vekt_u3 <- sum(df3$Laks_vekt_u3kg) / sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg)
      andel_vekt_37 <- sum(df3$Laks_vekt_o3u7kg) / sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg)
      andel_vekt_o7 <- sum(df3$Laks_vekt_o7kg) / sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg)
      
      if(!is.na(df$Maloppnaelse)) { # vassdrag med GBM og estimert måloppnåelse
        
        # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen
        # inntil videre brukes snittverdier for størrelsesfordeling fra nasjonal fangst
        
        elv_gyting[j, i, 1] <- elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_u3
        elv_gyting[j, i, 2] <- elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_37
        elv_gyting[j, i, 3] <- elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_o7
        elv_gyting[j, i, 4] <- elv_gyting[j, i, 1] / snittvekt_u3
        elv_gyting[j, i, 5] <- elv_gyting[j, i, 2] / snittvekt_37
        elv_gyting[j, i, 6] <- elv_gyting[j, i, 3] / snittvekt_o7
        
        if((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) elv_fangst[j, i, ] <- 0
        else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }
        
      } else if(!is.na(df$Gytebestand)) { # vassdrag med estimat på gytebestand

        # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen
        # inntil videre brukes snittverdier for størrelsesfordeling fra nasjonal fangst
        
        elv_gyting[j, i, 1] <- df$Gytebestand * andel_ant_u3 * snittvekt_u3
        elv_gyting[j, i, 2] <- df$Gytebestand * andel_ant_37 * snittvekt_37
        elv_gyting[j, i, 3] <- df$Gytebestand * andel_ant_o7 * snittvekt_o7
        elv_gyting[j, i, 4] <- df$Gytebestand * andel_ant_u3
        elv_gyting[j, i, 5] <- df$Gytebestand * andel_ant_37
        elv_gyting[j, i, 6] <- df$Gytebestand * andel_ant_o7
        
        if((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) elv_fangst[j, i, ] <- 0
        else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }
        
      } else if(!is.na(df$Elvinnsig)) { # vassdrag med telling av oppvandrende laks, for eksempel video
        
        # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen
        # inntil videre brukes snittverdier for størrelsesfordeling fra nasjonal fangst
        
        elv_gyting[j, i, 1] <- df$Elvinnsig * andel_ant_u3 * snittvekt_u3
        elv_gyting[j, i, 2] <- df$Elvinnsig * andel_ant_37 * snittvekt_37
        elv_gyting[j, i, 3] <- df$Elvinnsig * andel_ant_o7 * snittvekt_o7
        elv_gyting[j, i, 4] <- df$Elvinnsig * andel_ant_u3
        elv_gyting[j, i, 5] <- df$Elvinnsig * andel_ant_37
        elv_gyting[j, i, 6] <- df$Elvinnsig * andel_ant_o7
        
        if((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) elv_fangst[j, i, ] <- 0
        else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }
        
      } else if((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) > 0) { # vassdrag med fangst
        
        elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
        elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
        elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
        elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
        elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
        elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        
        if(is.na(df$Fangstrate)) { # hvis estimat finnes, bruk dette, dersom ikke bruk fangstrate 0.5 foreløpig, inntil vi har bedre kontroll på alternative tilnærminger
          df$Fangstrate <- 0.5
        }
        
        elv_beskatningsrate[j, i, ] <- df$Fangstrate
        
        elv_gyting[j, i, 1] <- (df2$Laks_vekt_u3kg / df$Fangstrate) - df2$Laks_vekt_u3kg
        elv_gyting[j, i, 2] <- (df2$Laks_vekt_o3u7kg / df$Fangstrate) - df2$Laks_vekt_o3u7kg
        elv_gyting[j, i, 3] <- (df2$Laks_vekt_o7kg / df$Fangstrate) - df2$Laks_vekt_o7kg
        elv_gyting[j, i, 4] <- (df2$Laks_ant_u3kg / df$Fangstrate) - df2$Laks_ant_u3kg
        elv_gyting[j, i, 5] <- (df2$Laks_ant_o3u7kg / df$Fangstrate) - df2$Laks_ant_o3u7kg
        elv_gyting[j, i, 6] <- (df2$Laks_ant_o7kg / df$Fangstrate) - df2$Laks_ant_o7kg
        
      } else { # data mangler helt, sett gyting og fangst til 0
        
        elv_gyting[j, i, ] <- 0
        elv_fangst[j, i, ] <- 0
        
      }

      elv_innsig[j, i, ] <- elv_fangst[j, i, ] + elv_gyting[j, i, ]
      
    } # slutt for-loop over år 

  } # slutt if, ferdig beregning av elvinnsig
  
} # slutt for-loop over elver

#-----------------------------------------------------------------------------------------------------------------
# initier data.frame for samlede resultat og begynn å sette inn variable med resultat
#-----------------------------------------------------------------------------------------------------------------

resultat_fordeling <- data.frame(matrix(0, nrow = antall_elver * antall_aar, ncol = 44, 
                                        dimnames = list(NULL, c("VdrNr", "Vassdrag", "Aar", "RegionNr", "Region", "Gyt_vekt_u3", "Gyt_vekt_37",
                                                                "Gyt_vekt_o7", "Gyt_ant_u3", "Gyt_ant_37", "Gyt_ant_o7", "Fangst_elv_vekt_u3", 
                                                                "Fangst_elv_vekt_37", "Fangst_elv_vekt_o7", "Fangst_elv_ant_u3", "Fangst_elv_ant_37", 
                                                                "Fangst_elv_ant_o7", "Innsig_elv_vekt_u3", "Innsig_elv_vekt_37", "Innsig_elv_vekt_o7",
                                                                "Innsig_elv_ant_u3", "Innsig_elv_ant_37", "Innsig_elv_ant_o7", "Andel_region_vekt_u3",
                                                                "Andel_region_vekt_37", "Andel_region_vekt_o7", "Andel_region_ant_u3", 
                                                                "Andel_region_ant_37", "Andel_region_ant_o7", "Sjofangst_vekt_u3",
                                                                "Sjofangst_vekt_37", "Sjofangst_vekt_o7", "Sjofangst_ant_u3", "Sjofangst_ant_37",
                                                                "Sjofangst_ant_o7", "Innsig_sjo_vekt_u3", "Innsig_sjo_vekt_37", "Innsig_sjo_vekt_o7",
                                                                "Innsig_sjo_ant_u3", "Innsig_sjo_ant_37", "Innsig_sjo_ant_o7", "Innsig_total_hunn", 
                                                                "Overbeskatning", "MaxSustExp"
                                                                ))))
i <- 1
for (j in 1:antall_aar) {
  l <- antall_elver + i - 1
  resultat_fordeling[i:l, 1] <- elveliste$VdrNr
  resultat_fordeling[i:l, 2] <- elveliste$Vassdrag
  resultat_fordeling[i:l, 3] <- aar_liste[j]
  resultat_fordeling[i:l, 4] <- elveliste$RegionNr
  resultat_fordeling[i:l, 5] <- elveliste$RegionNavn
  
  resultat_fordeling[i:l, 6:11] <- elv_gyting[j, , 1:6] 
  resultat_fordeling[i:l, 12:17] <- elv_fangst[j, , 1:6] 
  resultat_fordeling[i:l, 18:23] <- elv_fangst[j, , 1:6] + elv_gyting[j, , 1:6]
  
  i <- i + antall_elver
}

#-----------------------------------------------------------------------------------------------------------------
# beregn innsig til elv og kyst for hvert vassdrag
#-----------------------------------------------------------------------------------------------------------------
ddf <- resultat_fordeling %>% 
  group_by(Aar, Region) %>%
  summarize(across(Innsig_elv_vekt_u3:Innsig_elv_ant_o7, sum)) # beregn samlet elveinnsig pr region, bruk det til å beregne hva slags fangstandel hvert vassdrag utgjør

l <- 1 # indeks for vassdrag i resultat_fordeling
for (j in 1:antall_aar) {
  for (i in 1:antall_elver) {
    k <- filter(ddf, Region == elveliste$RegionNavn[i] & Aar == aar_liste[j])
    resultat_fordeling[l, 24:29] <- resultat_fordeling[l, 18:23] / k[1, 3:8] # andel vassdrag utgjør i region
    resultat_fordeling[l, 30:35] <- resultat_fordeling[l, 24:29] * region_fangst[j, elveliste$RegionNr[i], 1:6] # sjøfangst
    resultat_fordeling[l, 36:41] <- resultat_fordeling[l, 30:35] + resultat_fordeling[l, 18:23] # innsig totalt
    
    # totalt innsig hunn, vekt
    if(elveliste$GytingSim[i]) {
      m <- filter(simul_kghunnlaks, Vdrnr == elveliste$VdrNr[i] & Aar == aar_liste[j])
      resultat_fordeling$Innsig_total_hunn[l] <- resultat_fordeling$Innsig_sjo_vekt_u3[l] * m$AndelHunnU3 +
        resultat_fordeling$Innsig_sjo_vekt_37[l] * m$AndelHunn37 + resultat_fordeling$Innsig_sjo_vekt_o7[l] * m$AndelHunnO7
      samlet_gytebestand_hunn <- m$GytingHunnU3 + m$GytingHunn37 + m$GytingHunnO7
    } else {
      resultat_fordeling$Innsig_total_hunn[l] <- sum(resultat_fordeling[l, 36:38]) * elv_andelhunn[j, i]
      samlet_gytebestand_hunn <- sum(resultat_fordeling[l, 6:8]) * elv_andelhunn[j, i]
    }

    # beregn overbeskatning
    if(!is.na(elveliste$GBM[i])) { 
      if(samlet_gytebestand_hunn > elveliste$GBM[i]) 
        resultat_fordeling$Overbeskatning[l] <- 0 # gytebestand nådd, ingen overbeskatning
      else if(resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i]) { # innsig større enn gytebestand vil si noe beskattbart overskudd
        resultat_fordeling$Overbeskatning[l] <- (elveliste$GBM[i] - samlet_gytebestand_hunn) / elveliste$GBM[i] # bare beskatning nedenfor GBM er overbeskatning
      } else { # intet beskattbart overskudd, alt fiske er overbeskatning
        if(elveliste$GytingSim[i])
          resultat_fordeling$Overbeskatning[l] <- (((resultat_fordeling$Fangst_elv_vekt_u3[l] + resultat_fordeling$Sjofangst_vekt_u3[l]) * m$AndelHunnU3) +
            ((resultat_fordeling$Fangst_elv_vekt_37[l] + resultat_fordeling$Sjofangst_vekt_37[l]) * m$AndelHunn37) + 
            ((resultat_fordeling$Fangst_elv_vekt_o7[l] + resultat_fordeling$Sjofangst_vekt_o7[l]) * m$AndelHunnO7)) /
            elveliste$GBM[i]
        else
          resultat_fordeling$Overbeskatning[l] <- ((resultat_fordeling$Fangst_elv_vekt_u3[l] + resultat_fordeling$Fangst_elv_vekt_37[l] +
                                                     resultat_fordeling$Fangst_elv_vekt_o7[l] + resultat_fordeling$Sjofangst_vekt_u3[l] +
                                                     resultat_fordeling$Sjofangst_vekt_37[l] + resultat_fordeling$Sjofangst_vekt_o7[l]) *
                                                     elv_andelhunn[j, i]) / elveliste$GBM[i]
      }
    }
    
    # beregn maksimal bærekraftig beskatning
    if(!is.na(elveliste$GBM[i])) {
      if(resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i])
        resultat_fordeling$MaxSustExp[l] <- (resultat_fordeling$Innsig_total_hunn[l] - elveliste$GBM[i]) / resultat_fordeling$Innsig_total_hunn[l]
      else
        resultat_fordeling$MaxSustExp[l] <- 0
    }

    l <- l + 1
  }
}

export(resultat_fordeling, "results/resultat_fordeling.csv", sep = ";", dec = ".", bom = TRUE)
