# Skriptet tar resultat av gytebestandsimulering og sjøfangstfiler som input
# Sjøfangst fra ytre kyst fordeles til fjordregion basert på en fordelingsnøkkel
# Fangst i fjordregion samt fordelt fangst fra ytre kyst fordeles på vassdrag basert på gytebestandsimulering

library(openxlsx)
library(tidyverse)

setwd("C:/Users/morten.falkegard/OneDrive - NINA/Vitenskapsråd/2020/Fordeling av sjøfangst, skripting")

# Variables defining starting year and number of years used in estimation
# The script are based around these in order to provide a bit of flexibility in case investigations into historic changes are needed
start_aar <- 2019
antall_aar <- 1
sim_beskat_filnavn <- "Beskatning_FangstAndel2019.txt"
sim_gyt_filnavn <- "Maaloppnaaelse2019.txt"
sim_villOppdr_filnavn <- "AntVillogOppdrettElvEstimater2019.txt"

# obtain list of rivers with spawning targets, 1.5 kg proportions and region names
elveliste <- read.xlsx("fangstfordeling_elveliste.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
antall_elver <- nrow(elveliste)

# obtain coastal catch distribution numbers (proportion of salmon from fjord regions in different coastal regions)
kyst_fordeling <- read.xlsx("fordeling fangst kystregioner.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# obtain distribution key for regions and municipalities
region_fordeling <- read.xlsx("fordeling sjøfangst, kommune til region.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
antall_regioner <- ncol(region_fordeling) - 1
antall_kommuner <- nrow(region_fordeling)

# read coastal catch file
sjofangst <- read.xlsx("sjø kommune.xlsx", sheet = 2, startRow = 1, colNames = TRUE, cols = c(1:6, 9:11), na.strings = c(":", ".."))

# initialize empty variables
region_fangst <- array(0, c(antall_aar, antall_regioner, 6))
fangst <- matrix(nrow = antall_kommuner, ncol = antall_regioner)
aar_liste <- vector()

elv_fangst <- array(0, c(antall_aar, antall_elver, 6)) # 6 =  3 size categories for biomass and 3 for numbers
elv_hunnproporsjon <- array(0, c(antall_aar, antall_elver, 3)) # 3 size categories
elv_beskatningsrate <- elv_hunnproporsjon # 3 size categories

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
# les inn tabell med beskatningstall og måloppnåelse fra gytesimulering og 
# elvedatamatrise (tall for vassdrag utenfor simulering)
#-----------------------------------------------------------------------------------------------------------------
simul_beskatning <- read.table(sim_beskat_filnavn, header = TRUE)
simul_gyt <- read.table(sim_gyt_filnavn, header = TRUE)
simul_villoppdr <- read.table(sim_villOppdr_filnavn, header = TRUE)
elvedatamatrise <- read.xlsx("elvedata_matrise_2019.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

#-----------------------------------------------------------------------------------------------------------------
# gå gjennom elvelisten og hent inn info som skal brukes i fordelingen
#-----------------------------------------------------------------------------------------------------------------
for (i in 1:antall_elver) {
  i <- 3
  if(!is.na(elveliste[i, "Filnavn"])) { # vassdrag har filnavn, hent data fra simulering og vassdragsfil
    
    ElvFilNavn <- paste("Simuleringsdata/", elveliste[i, "Filnavn"], ".xlsx", sep="")
    ValgtElv <- read.xlsx(ElvFilNavn, sheet = 1, startRow = 1, colNames = TRUE)
    
  } else { # mangler filnavn, det vil si elv uten simulering, hent data fra elvedatamatrise
    
  }
  
  
  elv_filnavn <- paste(elveliste[i, "Filnavn"], ".xlsx", sep="")
  elv_grunnlag <- read.xlsx(elv_filnavn, sheet = 1, startRow = 1, colNames = TRUE)
  
  j <- which(elv_grunnlag$Aar == start_aar)
  
}








#-----------------------------------------------------------------------------------------------------------------
# sett sammen fangst og beskatningsdata fra laksereg og gamle Excel-fordelinger
#-----------------------------------------------------------------------------------------------------------------
elveliste <- read.xlsx("fangstfordeling_elveliste.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
elveliste_nokkel <- read.xlsx("fangstfordeling_elveliste.xlsx", sheet = 2, startRow = 1, colNames = TRUE)
antall_elver <- nrow(elveliste)
elvefangst <- read.xlsx("fangststatistikk 2019.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
fordelingstabell <- read.xlsx("tabell fordelingsdata 2019.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

elvedata_matrise <- data.frame(matrix(0, nrow = antall_elver, ncol = 19, dimnames=list(NULL, 
                                                                                      c("VdrNr", "Elv", "Region", "Fjord", "GBM", "Simulert", "Andel15",
                                                                                        "Andel_hunn", "Maloppnaelse", "Fangstrate", "Gytebestand",
                                                                                        "Elvinnsig", "Fangst_storstygg", "LaksU3_ant_avl", "Laks37_ant_avl",
                                                                                        "LaksO7_ant_avl", "LaksU3_kg_avl", "Laks37_kg_avl", "LaksO7_kg_avl"))))


for (i in 1:antall_elver) {
  j <- which(elvefangst$Vassdragnr == elveliste$VdrNr[i])
  elvedata_matrise[i, 1] <- elveliste$VdrNr[i]
  elvedata_matrise[i, 2] <- elveliste$Vassdrag[i]
  elvedata_matrise[i, 3] <- elveliste$RegionNavn[i]
  elvedata_matrise[i, 4] <- elveliste$Fjord[i]
  elvedata_matrise[i, 5] <- elveliste$GBM[i]
  elvedata_matrise[i, 6] <- elveliste$GytingSim[i]
  elvedata_matrise[i, 7] <- fordelingstabell$And15[i]
  elvedata_matrise[i, 8] <- fordelingstabell$AndelHunner[i]
  elvedata_matrise[i, 13] <- fordelingstabell$ElveFangst[i]
  
  if(length(j)) {
    elvedata_matrise[i, 14] <- elvefangst$LaksU3_ant_avl[j]
    elvedata_matrise[i, 15] <- elvefangst$Laks37_ant_avl[j]
    elvedata_matrise[i, 16] <- elvefangst$LaksO7_ant_avl[j]
    elvedata_matrise[i, 17] <- elvefangst$LaksU3_kg_avl[j]
    elvedata_matrise[i, 18] <- elvefangst$Laks37_kg_avl[j]
    elvedata_matrise[i, 19] <- elvefangst$LaksO7_kg_avl[j]
  }
  
  if(!elveliste$GytingSim[i]) {
    if(!is.na(elveliste_nokkel$Måloppnåelse[i])) elvedata_matrise[i, 9] <- fordelingstabell$ProsentOppn[i]
    if(!is.na(elveliste_nokkel$Fangstrate[i])) elvedata_matrise[i, 10] <- fordelingstabell$Fangstrate[i]
    if(!is.na(elveliste_nokkel$Gytebestand[i])) elvedata_matrise[i, 11] <- fordelingstabell$Gytebestand[i]
    if(!is.na(elveliste_nokkel$Elvinnsig[i])) elvedata_matrise[i, 12] <- fordelingstabell$ElvInnsig[i]
  }
}

write.xlsx(elvedata_matrise, "elvedata_matrise_2019.xlsx")


#--------------------------------------------------------
# sekvens for å hente inn og sammenligne gytebestandsmål
#--------------------------------------------------------
gbm <- read.xlsx("tabell gytebestandsmål Norge 2016.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
gbm_matrix <- data.frame(matrix(0, nrow = antall_elver, ncol = 6, dimnames=list(NULL, c("VdrNr", "Elv", "GBM", "GBM_med", "GBM_low", "GBM_upr"))))

for (i in 1:antall_elver) {
  j <- which(gbm$Vnr == elveliste$VdrNr[i])
  gbm_matrix[i, 1] <- elveliste$VdrNr[i]
  gbm_matrix[i, 2] <- elveliste$Vassdrag[i]
  gbm_matrix[i, 3] <- elveliste$GBM[i]
  if(!length(j)) {
    gbm_matrix[i, 4:6] <- NA
  } else {
    gbm_matrix[i, 4] <- gbm$GBM_med[j]
    gbm_matrix[i, 5] <- gbm$GBM_low[j]
    gbm_matrix[i, 6] <- gbm$GBM_upr[j]
  }
}
write.xlsx(gbm_matrix, "gbm_matrix.xlsx")
