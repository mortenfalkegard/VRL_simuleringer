#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar utgangspunkt i gytebestand, beskatning, elvefangst og sjøfangst. Sjøfangst fordeles på
# regionnivå og så videre på vassdragsnivå. Skriptet beregner så innsig i forskjellige områder. Estimatene
# gjøres på størrelsesgruppe og både biomasse og antall.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(openxlsx)
library(tidyverse)

# Variablene nedenfor definerer startår og antall år som skal estimeres. Skriptet er fleksibelt organisert rundt disse for å gi mulighet
# til å gjennomføre historiske sammenligninger, for eksempel før og etter viktige reguleringsendringer
start_aar <- 2019
antall_aar <- 1

# Variable som gir navn på output-filer fra siste gytebestandsimulering
sim_beskat_filnavn <- "results/Beskatning_FangstAndel2019.txt"
sim_gyt_filnavn <- "results/Maaloppnaaelse2019.txt"
sim_villOppdr_filnavn <- "results/AntVillogOppdrettElvEstimater2019.txt"
sim_kghunnlaks_filnavn <- "results/KgHunnlaks2019.txt"

# initialiser en liste over vassdrag som skal være med i innsigsfordelingen med noen bakgrunnstall
elveliste <- read.csv("data/elveliste.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
antall_elver <- nrow(elveliste)

# fordelingsnøkkel for fordeling av fangsten i kystregionene 
kyst_fordeling <- read.csv("data/fordelingsnokkel_sjofangst.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

# fordelingsnøkkel for å fordele kommunefangst til regioner
region_fordeling <- read.csv("data/fordelingsnokkel_kommune_region.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
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
simul_kghunnlaks <- read.table(sim_kghunnlaks_filnavn, stringsAsFactors = FALSE, header = TRUE)

elvedatamatrise <- read.table("data/elvedata_2019.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8-BOM")
elvefangst_ssb <- read.table("data/ssb_elv_2010-2019.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, fileEncoding = "UTF-8")

#-----------------------------------------------------------------------------------------------------------------
# gå gjennom alle vassdrag, hent inn data for hver enkelt elv og beregn innsig 
#-----------------------------------------------------------------------------------------------------------------

for (i in 1:antall_elver) {

  elv_vdrnr <- elveliste$VdrNr[i]
  
  if(!is.na(elveliste[i, "Filnavn"])) { # vassdrag har filnavn, hent data fra simulering og vassdragsfil
    
    elv_filnavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep="")
    elv_grunnlag <- read.table(elv_filnavn, header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    elv_vdrnr <- elveliste$VdrNr[i]
    
    for (j in 1:antall_aar) {
      df <- filter(simul_kghunnlaks, Aar == aar_liste[j], Vdrnr == elveliste$VdrNr[i])
      df2 <- filter(elv_grunnlag, Aar == aar_liste[j])
      
      elv_hunnproporsjon[j, i, 1] <- df$AndelHunnU3
      elv_hunnproporsjon[j, i, 2] <- df$AndelHunn37
      elv_hunnproporsjon[j, i, 3] <- df$AndelHunnO7

      # kolonne 1-3 er data for vekt
      elv_fangst[j, i, 1] <- df$FangstHunnU3_Justert / df$AndelHunnU3
      elv_fangst[j, i, 2] <- df$FangstHunn37_Justert / df$AndelHunn37
      elv_fangst[j, i, 3] <- df$FangstHunnO7_Justert / df$AndelHunnO7
      
      elv_gyting[j, i, 1] <- df$GytingHunnU3 / df$AndelHunnU3
      elv_gyting[j, i, 2] <- df$GytingHunn37 / df$AndelHunn37
      elv_gyting[j, i, 3] <- df$GytingHunnO7 / df$AndelHunnO7
      
      # bruk snittvekt til å estimere fangst og gyting i antall, gitt i kolonne 4-6
      snittvekt[1] <- df2$Laks_vekt_u3kg / df2$Laks_ant_u3kg
      snittvekt[2] <- df2$Laks_vekt_o3u7kg / df2$Laks_ant_o3u7kg
      snittvekt[3] <- df2$Laks_vekt_o7kg / df2$Laks_ant_o7kg
      
      elv_fangst[j, i, 4] <- elv_fangst[j, i, 1] / snittvekt[1]
      elv_fangst[j, i, 5] <- elv_fangst[j, i, 2] / snittvekt[2]
      elv_fangst[j, i, 6] <- elv_fangst[j, i, 3] / snittvekt[3]
      
      elv_gyting[j, i, 4] <- elv_gyting[j, i, 1] / snittvekt[1]
      elv_gyting[j, i, 5] <- elv_gyting[j, i, 2] / snittvekt[2]
      elv_gyting[j, i, 6] <- elv_gyting[j, i, 3] / snittvekt[3]
      
      elv_innsig[j, i, ] <- elv_fangst[j, i, ] + elv_gyting[j, i, ]
    }
    
  } else { # mangler filnavn, det vil si elv uten simulering, hent data fra elvedatamatrise og SSB
    
    for (j in 1:antall_aar) {
      df <- filter(elvedatamatrise, VdrNr == elv_vdrnr)
      df2 <- filter(elvefangst_ssb, VdrNr == elv_vdrnr, Aar == aar_liste[j])
      
      if(is.na(df$Fangst) | (df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) { # elv uten fangst, bruk alternativ fra elvedatamatrise
        elv_fangst[j, i, 1] <- 0
        elv_fangst[j, i, 2] <- 0
        elv_fangst[j, i, 3] <- 0
        elv_fangst[j, i, 4] <- 0
        elv_fangst[j, i, 5] <- 0
        elv_fangst[j, i, 6] <- 0
        
        if(!is.na(df$Maloppnaelse)) { # vassdrag med GBM og estimert måloppnåelse
          
          # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen
          
        } else if(!is.na(df$Elvinnsig)) { # vassdrag med telling, for eksempel video
          
          # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen
          
        } else if(!is.na(df$Gytebestand)) { # vassdrag med gytefisktelling
        
          # her må det gjøres en jobb med å sette opp antatt størrelsesfordeling i vassdraget, for eksempel i elvedatamatrisen

        } else { # data mangler helt, sett gyting og fangst til 0
          
          elv_gyting[j, i, 1] <- 0
          elv_gyting[j, i, 2] <- 0
          elv_gyting[j, i, 3] <- 0
          elv_gyting[j, i, 4] <- 0
          elv_gyting[j, i, 5] <- 0
          elv_gyting[j, i, 6] <- 0
          
        }
        
      } else { # elv med fangst, sjekk elvedatamatrise for evt ekstra kunnskap
        
        elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
        elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
        elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
        elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
        elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
        elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg

        snittvekt[1] <- df2$Laks_vekt_u3kg / df2$Laks_ant_u3kg
        snittvekt[2] <- df2$Laks_vekt_o3u7kg / df2$Laks_ant_o3u7kg
        snittvekt[3] <- df2$Laks_vekt_o7kg / df2$Laks_ant_o7kg

        if(is.na(df$Fangstrate)) { # hvis estimat finnes, bruk dette, dersom ikke bruk fangstrate 0.5 foreløpig, inntil vi har bedre kontroll på alternative tilnærminger
          df$Fangstrate <- 0.5
        }
        
        elv_beskatningsrate[j, i, 1] <- df$Fangstrate
        elv_beskatningsrate[j, i, 2] <- df$Fangstrate
        elv_beskatningsrate[j, i, 3] <- df$Fangstrate

        elv_gyting[j, i, 1] <- (df2$Laks_vekt_u3kg / df$Fangstrate) - df2$Laks_vekt_u3kg
        elv_gyting[j, i, 2] <- (df2$Laks_vekt_o3u7kg / df$Fangstrate) - df2$Laks_vekt_o3u7kg
        elv_gyting[j, i, 3] <- (df2$Laks_vekt_o7kg / df$Fangstrate) - df2$Laks_vekt_o7kg
        elv_gyting[j, i, 4] <- (df2$Laks_ant_u3kg / df$Fangstrate) - df2$Laks_ant_u3kg
        elv_gyting[j, i, 5] <- (df2$Laks_ant_o3u7kg / df$Fangstrate) - df2$Laks_ant_o3u7kg
        elv_gyting[j, i, 6] <- (df2$Laks_ant_o7kg / df$Fangstrate) - df2$Laks_ant_o7kg
        
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
  resultat_fordeling[i:antall_elver+i-1, 1] <- elveliste$VdrNr
  resultat_fordeling[i:antall_elver+i-1, 2] <- elveliste$Vassdrag
  resultat_fordeling[i:antall_elver+i-1, 3] <- aar_liste[j]
  resultat_fordeling[i:antall_elver+i-1, 4] <- elveliste$RegionNr
  resultat_fordeling[i:antall_elver+i-1, 5] <- elveliste$RegionNavn
  
  resultat_fordeling[i:antall_elver+i-1, 6:11] <- elv_gyting[j, , 1:6] 
  resultat_fordeling[i:antall_elver+i-1, 12:17] <- elv_fangst[j, , 1:6] 
  resultat_fordeling[i:antall_elver+i-1, 18:23] <- elv_fangst[j, , 1:6] + elv_gyting[j, , 1:6]
  
  i <- i + antall_elver
}

#-----------------------------------------------------------------------------------------------------------------
# beregn innsig til elv og kyst for hvert vassdrag
#-----------------------------------------------------------------------------------------------------------------
ddf <- resultat_fordeling %>% 
  group_by(Aar, Region) %>%
  summarize(across(PFA_elv_vekt_u3:PFA_elv_ant_o7, sum)) # beregn samlet elveinnsig pr region, bruk det til å beregne hva slags fangstandel hvert vassdrag utgjør

l <- 1 # indeks for vassdrag i resultat_fordeling
for (j in 1:antall_aar) {
  for (i in 1:antall_elver) {
    k <- filter(ddf, Region == elveliste$RegionNavn[i] & Aar == aar_liste[j])
    resultat_fordeling[l, 24:29] <- resultat_fordeling[l, 18:23] / k[1, 3:8] # andel vassdrag utgjør i region
    resultat_fordeling[l, 30:35] <- resultat_fordeling[l, 24:29] * region_fangst[j, elveliste$RegionNr[i], 1:6] # sjøfangst
    resultat_fordeling[l, 36:41] <- resultat_fordeling[l, 30:35] + resultat_fordeling[l, 18:23]
    
    # totalt innsig hunn, vekt
    if(elveliste$GytingSim[i]) {
      m <- filter(simul_kghunnlaks, Vdrnr == elveliste$VdrNr[i] & Aar = aar_liste[j])
      resultat_fordeling$Innsig_total_hunn[l] <- resultat_fordeling$Innsig_sjo_vekt_u3[l] * m$AndelHunnU3 +
        resultat_fordeling$Innsig_sjo_vekt_37[l] * m$AndelHunn37 + resultat_fordeling$Innsig_sjo_vekt_o7[l] * m$AndelHunnO7
    } else {
      resultat_fordeling$Innsig_total_hunn[l] <- sum(resultat_fordeling[l, 36:38]) * elvedatamatrise$Andel_hunn[i]
    }

    # beregn overbeskatning
    if(!is.na(elveliste$GBM[i])) { 
      if(sum(resultat_fordeling[l, 30:32]) > elveliste$GBM[i]) 
        resultat_fordeling$Overbeskatning[l] <- 0 # gytebestand nådd, ingen overbeskatning
      else if(resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i]) {
        if(elveliste$GytingSim[i]) 
          resultat_fordeling$Overbeskatning[l] <- (elveliste$GBM[i] - sum(m[1, 7:9])) / elveliste$GBM[i]
        else
          resultat_fordeling$Overbeskatning[l] <- (elveliste$GBM[i] - (sum(resultat_fordeling[l, 6:8]) * elvedatamatrise$Andel_hunn[i])) / elveliste$GBM[i]
      } else {
        if(elveliste$GytingSim[i])
          resultat_fordeling$Overbeskatning[l] <- (((resultat_fordeling$Fangst_elv_vekt_u3[l] + resultat_fordeling$Sjofangst_vekt_u3[l]) * m$AndelHunnU3) +
            ((resultat_fordeling$Fangst_elv_vekt_37[l] + resultat_fordeling$Sjofangst_vekt_37[l]) * m$AndelHunn37) + 
            ((resultat_fordeling$Fangst_elv_vekt_o7[l] + resultat_fordeling$Sjofangst_vekt_o7[l]) * m$AndelHunnO7)) /
            elveliste$GBM[i]
        else
          resultat_fordeling$Overbeskatning[l] <- ((resultat_fordeling$Fangst_elv_vekt_u3[l] + resultat_fordeling$Fangst_elv_vekt_37[l] +
                                                     resultat_fordeling$Fangst_elv_vekt_o7[l] + resultat_fordeling$Sjofangst_vekt_u3[l] +
                                                     resultat_fordeling$Sjofangst_vekt_37[l] + resultat_fordeling$Sjofangst_vekt_o7[l]) *
                                                     elvedatamatrise$Andel_hunn[i]) / elveliste$GBM[i]
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

