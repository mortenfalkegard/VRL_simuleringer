#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar det samlede datasettet fra de gamle Excel-filene og lager vassdragsspesifikke
# datafiler for de vassdragene som ikke er inkludert i simuleringen.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(openxlsx)
library(tidyverse)
library(rio)

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
    export(df_ferdig, str_c("data/vassdrag/", elveliste$Filnavn[i], ".csv"))
  }
  
}

export(elveliste, "data/elveliste.csv")
