#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet inneholder forskjellige subrutiner for å tilpasse datafiler til simulering og innsigsfordeling.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(openxlsx)
library(tidyverse)
library(rio)
library(readr)
library(stringr)

#------------------------------------------------------------------------------------------------------------
# subrutine for å gjøre om excelfiler lokalt til csv på Git
#------------------------------------------------------------------------------------------------------------
i <- 74
for (i in 206:antall_elver) {
  if (elveliste$GytingSim[i]) {
    ElvFilNavn <- paste("datatemp/", elveliste[i, "Filnavn"], ".xlsx", sep="")
    ValgtElv <- read.xlsx(ElvFilNavn, sheet = 1, startRow = 1, colNames = TRUE)
    TilFilNavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep = "")
    write.table(ValgtElv, TilFilNavn, sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
  }
}

konverter <- read.xlsx("data/Prosent1SWblantLaksMindreEnn3kgInnsigsregioner.xlsx")
write.table(konverter, "data/Prosent1SWblantLaksMindreEnn3kgInnsigsregioner.csv", sep = ";", row.names = FALSE, fileEncoding = "UTF-8")

#------------------------------------------------------------------------------------------------------------
# subrutine for å konvertere SSB fangstfil
#------------------------------------------------------------------------------------------------------------
ssb_fangst <- read.xlsx("data/ssb_elv_2023.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# hent ut vassdrag og dropp fylkes- og landstall
ssb_fangst$VdrNr <- str_replace(ssb_fangst$VdrNr, "X1", "X1Z")
ssb_fangst$VdrNr <- str_replace(ssb_fangst$VdrNr, "246.1A", "246.1AZ")
ssb_filtrert <- filter(ssb_fangst, str_detect(VdrNr, "Z"))

ssb_filtrert <- subset(ssb_filtrert, VdrNr != "12041.21Z")

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

write.table(ssb_filtrert, "data/ssb_elv_2023.csv", sep = ";", row.names = FALSE, fileEncoding = "UTF-8")

#------------------------------------------------------------------------------------------------------------
# Skriptet tar det samlede datasettet fra de gamle Excel-filene og lager vassdragsspesifikke
# datafiler for de vassdragene som ikke er inkludert i simuleringen.
#------------------------------------------------------------------------------------------------------------

elvedata <- import("datatemp/elvedata_2012-2019.csv", setclass = "tibble", encoding = "UTF-8")
elvefangst_ssb <- import("data/ssb_elv_2010-2019.csv", setclass = "tibble", encoding = "UTF-8")
elveliste <- import("data/elveliste.csv", setclass = "tibble", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

for (i in 1:antall_elver) {
  
  if(!elveliste$GytingSim[i]) {
    df <- elvedata %>% filter(VdrNr == elveliste$VdrNr[i])
    df_ssb <- elvefangst_ssb %>% filter(VdrNr == elveliste$VdrNr[i], Aar > 2011) %>% select(-Vassdrag)
    
    elveliste$Filnavn[i] <- str_c(elveliste$VdrNr[i], "_", str_sub(elveliste$Vassdrag[i], 1, 8))
    
    df_ferdig <- left_join(df, df_ssb)
    export(df_ferdig, str_c("data/vassdrag/", elveliste$Filnavn[i], ".csv"), ";")
  }
  
}

elveliste <- import("data/elveliste.csv", setclass = "tibble", dec = ",", encoding = "UTF-8")

export(elveliste, "data/elveliste.csv", ";", dec = ".", bom = TRUE)

for (i in 1:antall_elver) {
  if(elveliste$GytingSim[i]) {
    filnavn <- str_c("data/vassdrag/", elveliste$Filnavn[i], ".csv")
    df <- import(filnavn, encoding = "UTF-8")
    export(df, filnavn, ";", dec = ".", bom = TRUE)
    #df <- read.csv(filnavn, header = TRUE, sep = ";", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    #write.table(df, filnavn, sep = ";", row.names = FALSE, fileEncoding = "UTF-8")
  }
}

dd <- import("data/fordelingsnokkel_sjofangst.csv", encoding = "UTF-8")
export(dd, "data/fordelingsnokkel_sjofangst.csv", ";", dec = ".", bom = TRUE)

elveliste <- import("data/elveliste.csv", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

# Oppdater simuleringsfiler med siste års data
for (i in 1:antall_elver) {
  if(elveliste$GytingSim[i]) {
    cat(i, " ") 
    flush.console()
    filnavn <- str_c("data/sim/csv/", elveliste$Filnavn[i], ".csv")
    filnavn2 <- str_c("data/sim/", elveliste$Filnavn[i], ".xlsx")
    df2 <- import(filnavn2, na = "#N/A")
    export(df2, filnavn, ";", dec = ".", bom = TRUE, na = "NA")
  }
}



# oppdater ikke-simulerte vassdrag med tidligere år -----------------------

elvefangst_ssb <- import("data/ssb_elv_2002-2010.csv", setclass = "tibble", encoding = "UTF-8")
elveliste <- import("data/elveliste.csv", setclass = "tibble", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

new_df <- data.frame(matrix(NA, nrow = 19, ncol = 22, dimnames = list(NULL, c("Vassdrag", "VdrNr", "Aar", "Andel_hunn", "Fangst_storstygg", "Maloppnaelse",
                                                                                 "Fangstrate", "Gytebestand", "Elvinnsig", "GytingSim", "Laks_vekt_u3kg", "Laks_vekt_o3u7kg",
                                                                                 "Laks_vekt_o7kg", "Laks_ant_u3kg", "Laks_ant_o3u7kg", "Laks_ant_o7kg", "Gjen_vekt_u3kg",
                                                                                 "Gjen_vekt_o3u7kg", "Gjen_vekto7kg", "Gjen_ant_u3kg", "Gjen_ant_o3u7kg", "Gjen_ant_o7kg"))))


for (i in 1:antall_elver) {
  if(!elveliste$GytingSim[i]) {
    filnavn <- str_c("data/vassdrag/", elveliste$Filnavn[i], ".csv")
    df <- import(filnavn, encoding = "UTF-8")
    df_ssb <- elvefangst_ssb %>% filter(VdrNr == df$VdrNr[1])# %>% select(-Vassdrag)
    new_df$Vassdrag <- df$Vassdrag[1]
    new_df$VdrNr <- df$VdrNr[1]
    new_df$Aar <- 2002:2020
    new_df$Andel_hunn <- df$Andel_hunn[1]
    new_df$Fangst_storstygg[9:19] <- df$Fangst_storstygg
    new_df$GytingSim <- df$GytingSim[1]
    new_df[9:19, 6:9] <- df[1:11, 6:9]
    new_df[1:8, 6:9] <- df[1, 6:9]
    new_df[9:19, 11:22] <- df[1:11, 11:22]
    new_df[1:8, 11:22] <- df_ssb[1:8, 4:15]
    export(new_df, filnavn, ";", dec = ".", bom = TRUE)
  }
}



# importer SSB-tall fra siste år ------------------------------------------

elvefangst_ssb <- import("data/ssb_elv_siste.csv", setclass = "tibble", encoding = "UTF-8")
elveliste <- import("data/elveliste.csv", setclass = "tibble", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

for (i in 1:antall_elver) {
  if(!elveliste$GytingSim[i]) {
    filnavn <- str_c("data/v/", elveliste$Filnavn[i], ".csv")
    df <- read_delim(filnavn, delim = ";", show_col_types = FALSE)
    df_ssb <- elvefangst_ssb %>% filter(VdrNr == df$VdrNr[1])# %>% select(-Vassdrag)
    antall_aar <- nrow(df)
    df <- df %>% add_row()
    df$Vassdrag[antall_aar + 1] <- df$Vassdrag[1]
    df$VdrNr[antall_aar + 1] <- df$VdrNr[1]
    df$Aar[antall_aar + 1] <- df$Aar[antall_aar] + 1
    df$Andel_hunn[antall_aar + 1] <- df$Andel_hunn[antall_aar]
    df$GytingSim[antall_aar + 1] <- df$GytingSim[1]
    df[antall_aar + 1, 11:22] <- df_ssb[1, 4:15]
    write_excel_csv(df, filnavn, delim = ";")
  }
}


# les inn xlsx fra simulering og lag csv ----------------------------------

elveliste <- import("data/elveliste.csv", setclass = "tibble", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

for (i in 1:antall_elver) {
  if(elveliste$GytingSim[i]) {
    filnavn <- str_c("data/sim/", elveliste$Filnavn[i], ".xlsx")
    df <- import(filnavn)
    expfilnavn <- str_c("data/sim/csv/", elveliste$Filnavn[i], ".csv")
    export(df, expfilnavn, ";", dec = ".", bom = TRUE)
  }
}
filliste <- as.data.frame(list.files("data/sim"))
export(filliste, "filliste.csv", ";", bom = TRUE)
