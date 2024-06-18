#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar ferdige vassdragsdata og kjører en full gytebestandssimulering (basert på skriptet til Astrid)
# Output fra skriptet er data som kan importeres rett inn i sjøfangstfordelingen.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(dplyr)       # pakke for datasettmanipulering
library(tidyr)       # pakke for datasettmanipulering
library(openxlsx)    # pakke for å lese og skrive Excel-filer
library(ggplot2)     # pakke for plotting
library(cowplot)     # pakke for plotting
library(ggpubr)      # pakke for å sette sammen flere plott
library(triangle)    # pakke for trekke tilfeldige tall fra trekantfordeling
library(stringr)     # pakke for tekstbehandling
library(rio)         # pakke for import og eksport av data
library(qlcMatrix)   # pakke for matriseregning
library(matrixStats) # pakke for matriseregning

# "n_sim" spesifiserer antall iterasjoner i simuleringene. Dette tallet bør være så høyt som mulig for å få
# mest mulig stabile resultater. Samtidig vil høye tall føre til at skriptet tar lengre tid å kjøre.
n_sim <- 5000

# "elveliste.csv" er en fil som inneholder informasjon om vassdragene som skal simuleres
elveliste <- import("data/elveliste.csv", encoding = "UTF-8")
indeks_simuler <- which(elveliste$GytingSim) # liste over vassdrag som skal simuleres

# importer datafilene for alle vassdragene i elveliste slik at alle er tilgjenlige i ett datasett
for (m in indeks_simuler) {
  elv_filnavn <- paste("data/vassdrag/", elveliste[m, "Filnavn"], ".csv", sep = "")
  d <- import(elv_filnavn, encoding = "UTF-8")
  d$Vdrnr <- elveliste$VdrNr[m] # erstatt vassdragsnummer slik at vi er sikker på at vi har med oss riktig nr videre
  d <- d %>%
    rename(Gjen_vekt_o7kg = Gjen_vekto7kg) %>% # feil navn på Gjen_vekt_o7kg i input, ta vekk når dette er fikset
    mutate(Region = elveliste$RegionNr[m]) # legg til region-id
  if (m == 1) {
    elvedata <- d
  } else {
    elvedata <- rbind(elvedata, d)
  }
}

# regn ut regionale gjennomsnittsstørrelser pr år for laks i de ulike vektklassene (vekt/antall),
# disse snittstørrelsene brukes i simuleringen for å beregne gytebestand i vassdrag med telling og
# manglende fangstdata
gjennomsnitt_str <- tibble(region = numeric(),
                           aar = numeric(),
                           snitt_u3kg = numeric(),
                           snitt_o3u7kg = numeric(),
                           snitt_o7kg = numeric())
indeks_regioner <- unique(elvedata$Region) # liste over regioner i elvedataene
indeks_aar <- unique(elvedata$Aar) # liste over år i elvedataene
for (r in indeks_regioner) {
  for (a in indeks_aar) {
    d <- elvedata %>% filter(Region == r & Aar == a)
    gjennomsnitt_str <- gjennomsnitt_str %>%
      add_row(region = r, aar = a,
              snitt_u3kg = sum(d$Laks_vekt_u3kg, d$Gjen_vekt_u3kg) / sum(d$Laks_ant_u3kg, d$Gjen_ant_u3kg),
              snitt_o3u7kg = sum(d$Laks_vekt_o3u7kg, d$Gjen_vekt_o3u7kg) / sum(d$Laks_ant_o3u7kg, d$Gjen_ant_o3u7kg),
              snitt_o7kg = sum(d$Laks_vekt_o7kg, d$Gjen_vekt_o7kg) / sum(d$Laks_ant_o7kg, d$Gjen_ant_o7kg))
  }
}
gjennomsnitt_str <- gjennomsnitt_str %>% # sett NA verdier i gjennomsnitt_str til regionale gjennomsnitt
  group_by(region) %>%
  mutate_at(vars(snitt_u3kg:snitt_o7kg), ~replace_na(., mean(., na.rm = TRUE)))

# ====================  START SIMULERING ==================== #
for (m in indeks_simuler) {

  # hent ut data for ett vassdrag
  d <- elvedata %>% filter(Vdrnr == elveliste$VdrNr[m])

  # sette beskatningsrate og fangstandel til NA i år der det mangler fangst og gjenutsatte og ikke er telling
  kolonner <- c("ExpStorMin", "ExpStorMed", "ExpStorMax", "ExpMellomMin", "ExpMellomMed", "ExpMellomMax",
                "ExpSmallMin", "ExpSmallMed", "ExpSmallMax")
  betingelse <- d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant)
  for (k in kolonner) {
    d[[k]][betingelse] <- NA
  }

  # flagg år som i simuleringen skal komme ut med asterisk (uten fangst og/eller problemer med telling)
  # "Asterisk" blir satt til 0 for år som ikke skal simuleres, og 1 for år som skal simuleres
  # dette flagget er den gamle måten å flagge år som ikke simuleres på, og er nå bare delvis i bruk
  # det er nå kommmet et nytt flagg i datafilene som heter "Simulering"
  # "Asterisk" skal derfor fases ut etterhvert
  d$Asterisk <- ifelse(is.na(d$ExpSmallMed) & is.na(d$FangstAndSmallMed) & is.na(d$Probs_small_med), 1, 0)

  # nedenfor settes fangstandel til NA i år der det mangler fangst og gjenutsatte og ikke er telling
  kolonner <- c("FangstAndStorMin", "FangstAndStorMed", "FangstAndStorMax", "FangstAndMellomMin",
                "FangstAndMellomMed", "FangstAndMellomMax", "FangstAndSmallMin", "FangstAndSmallMed",
                "FangstAndSmallMax")
  betingelse <- d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant)
  for (k in kolonner) {
    d[[k]][betingelse] <- NA
  }

  # sette beskatningsrate til 0 der det mangler fangst for en vektklasse men det er avliva fangst for
  # andre vektklasser, dersom da ikke året er flagget allerede
  kolonner <- c("ExpStorMin", "ExpStorMed", "ExpStorMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_o7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o3u7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  kolonner <- c("ExpMellomMin", "ExpMellomMed", "ExpMellomMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_o3u7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  kolonner <- c("ExpSmallMin", "ExpSmallMed", "ExpSmallMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_u3kg == 0 & (d$Laks_ant_o3u7kg > 0 | d$Laks_ant_o7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  # erstatt NA med 0 i utvalgte kolonner for å forenkle skriptet
  kolonner <- c("StamAntSmaHo", "StamAntMelHo", "StamAntStorHo",
                "Laks_vekt_u3kg", "Laks_vekt_o3u7kg", "Laks_vekt_o7kg",
                "Laks_ant_u3kg", "Laks_ant_o3u7kg", "Laks_ant_o7kg",
                "Gjen_vekt_u3kg", "Gjen_vekt_o3u7kg", "Gjen_vekt_o7kg",
                "Gjen_ant_u3kg", "Gjen_ant_o3u7kg", "Gjen_ant_o7kg",
                "Laks_ant", "Gjen_ant")
  for (k in kolonner) {
    d[[k]][is.na(d[[k]])] <- 0
  }

  # definer indeks for om det er telling eller ikke i et år, 1=telling, 0=ikke telling
  d$telling <- with(d, ifelse(is.na(Obs_laks_ant), 0, 1))

  #### --------- Vekt i år med telling --------- ####
  # For år som har fangst og/eller gjenutsatte regner vi ut gjennomsnittsvekt: gjen_vekt+laks_vekt/gjen_ant+Laks_ant
  # År uten fangst eller gjenutsatte : gjennomsnittet av fangst og gjenutsatt 5 nærmeste år

  if (sum(d$telling) > 0) { # denne delen kjøres bare dersom det har vært telling

    d$Laks_vekt_u3kg_sum <- with(d, Laks_vekt_u3kg + Gjen_vekt_u3kg)
    d$Laks_ant_u3kg_sum <- with(d, Laks_ant_u3kg + Gjen_ant_u3kg)
    d$Laks_vekt_o3u7kg_sum <- with(d, Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg)
    d$Laks_ant_o3u7kg_sum <- with(d, Laks_ant_o3u7kg + Gjen_ant_o3u7kg)
    d$Laks_vekt_o7kg_sum <- with(d, Laks_vekt_o7kg + Gjen_vekt_o7kg)
    d$Laks_ant_o7kg_sum <- with(d, Laks_ant_o7kg + Gjen_ant_o7kg)

    ### SMÅLAKS
    # Flagg hvis det mangler fangst i år med telling
    uten_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & d$Laks_vekt_u3kg_sum == 0, 1, 0)

    # Flagg hvis færre enn 5 laks fanget i år med telling
    lite_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & (d$Laks_ant_u3kg_sum < 5 & d$Laks_ant_u3kg_sum > 0), 1, 0)

    if (sum(lite_fangst_smaa, na.rm = TRUE) > 0 || sum(uten_fangst_smaa, na.rm = TRUE) > 0) {
      lite_eller_uten_fangst_smaa <- which(lite_fangst_smaa | uten_fangst_smaa)  # Rader med fangst færre enn 5

      # Færre enn totalt 5 avliva smålaks i tidsserien ---> regional snittvekt
      if (sum(d$Laks_ant_u3kg_sum, na.rm = TRUE) < 5) {
        d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa] <- gjennomsnitt_str$snitt_u3kg[gjennomsnitt_str$region %in% d$Region[1] & gjennomsnitt_str$aar %in% d$Aar[lite_eller_uten_fangst_smaa]]
        d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa] <- 1
      } else {
        fangst <- which(d$Laks_vekt_u3kg_sum > 0) # Finne rader som har vekt

        # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse årene
        if (length(fangst) < 5) {
          d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa] <- sum(d$Laks_vekt_u3kg_sum)
          d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa] <- sum(d$Laks_ant_u3kg_sum)
        } else {
          # Hvis det er fem år eller med med fangst, bruker vi vekt fra fem nærmeste år som har vekt:
          # Funksjonen f sorterer de 1-5 minste avstandene til rader med fangst for hver rad med lite/ingen fangst
          n_min <- 5
          f <- function(rw) {
            O <- order(rw)[1:n_min]
            rbind(O)
          }

          abs_diff <- sapply(lite_eller_uten_fangst_smaa, function(x) abs(fangst - x))
          min_dist <- t(apply(abs_diff, 2, f))

          d$Laks_vekt_u3kg_sum[lite_eller_uten_fangst_smaa] <-
            d$Laks_vekt_u3kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_vekt_u3kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_vekt_u3kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_vekt_u3kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_vekt_u3kg_sum[fangst[min_dist[, 5]]]
          d$Laks_ant_u3kg_sum[lite_eller_uten_fangst_smaa] <-
            d$Laks_ant_u3kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_ant_u3kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_ant_u3kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_ant_u3kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_ant_u3kg_sum[fangst[min_dist[, 5]]]
        }
      }
    }

    ### MELLOMLAKS
    # Flagg hvis det mangler fangst i år med telling
    uten_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & d$Laks_vekt_o3u7kg_sum == 0, 1, 0)

    # Flagg hvis færre enn 5 laks fanget i år med telling
    lite_fangst_mellom <- ifelse(d$Obs_laks_ant_o3u7kg > 0 & (d$Laks_ant_o3u7kg_sum < 5 & d$Laks_ant_o3u7kg_sum > 0), 1, 0)

    if (sum(lite_fangst_mellom, na.rm = TRUE) > 0 || sum(uten_fangst_mellom, na.rm = TRUE) > 0) {
      lite_eller_uten_fangst_mellom <- which(lite_fangst_mellom | uten_fangst_mellom)  # Rader med fangst færre enn 5

      # Færre enn totalt 5 avliva mellomlaks i tidsserien ---> regional snittvekt
      if (sum(d$Laks_ant_o3u7kg_sum, na.rm = TRUE) < 5) {
        d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_smaa] <- gjennomsnitt_str$snitt_o3u7kg[gjennomsnitt_str$region %in% d$Region[1] & gjennomsnitt_str$aar %in% d$Aar[lite_eller_uten_fangst_smaa]]
        d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom] <- 1
      } else {
        fangst <- which(d$Laks_vekt_o3u7kg_sum > 0) # Finne rader som har vekt

        # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse årene
        if (length(fangst) < 5) {
          d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom] <- sum(d$Laks_vekt_o3u7kg_sum)
          d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom] <- sum(d$Laks_ant_o3u7kg_sum)
        } else {
          # Hvis det er flere enn fem år med fangst, bruker vi vekt fra fem nærmeste år som har vekt:
          # Funksjonen f sorterer de 1-5 minste avstandene til rader med fangst for hver rad med lite/ingen fangst
          n_min <- 5
          f <- function(rw) {
            O <- order(rw)[1:n_min]
            rbind(O)
          }

          abs_diff <- sapply(lite_eller_uten_fangst_mellom, function(x) abs(fangst - x))
          min_dist <- t(apply(abs_diff, 2, f))

          d$Laks_vekt_o3u7kg_sum[lite_eller_uten_fangst_mellom] <-
            d$Laks_vekt_o3u7kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_vekt_o3u7kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_vekt_o3u7kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_vekt_o3u7kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_vekt_o3u7kg_sum[fangst[min_dist[, 5]]]
          d$Laks_ant_o3u7kg_sum[lite_eller_uten_fangst_mellom] <-
            d$Laks_ant_o3u7kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_ant_o3u7kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_ant_o3u7kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_ant_o3u7kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_ant_o3u7kg_sum[fangst[min_dist[, 5]]]
        }
      }
    }

    ### STORLAKS
    # Flagg hvis det mangler fangst for år med telling
    uten_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & d$Laks_vekt_o7kg_sum == 0, 1, 0)

    # Flagg hvis færre enn 5 laks fanget i år med telling
    lite_fangst_stor <- ifelse(d$Obs_laks_ant_o7kg > 0 & (d$Laks_ant_o7kg_sum < 5 & d$Laks_ant_o7kg_sum > 0), 1, 0)

    if (sum(lite_fangst_stor, na.rm = TRUE) > 0 || sum(uten_fangst_stor, na.rm = TRUE) > 0) {
      lite_eller_uten_fangst_stor <- which(lite_fangst_stor | uten_fangst_stor)  # Rader med fangst færre enn 5

      # Færre enn totalt 5 avliva storlaks i tidsserien ---> regional snittvekt
      if (sum(d$Laks_ant_o7kg_sum, na.rm = TRUE) < 5) {
        d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_smaa] <- gjennomsnitt_str$snitt_o7kg[gjennomsnitt_str$region %in% d$Region[1] & gjennomsnitt_str$aar %in% d$Aar[lite_eller_uten_fangst_smaa]]
        d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor] <- 1
      } else {
        fangst <- which(d$Laks_vekt_o7kg_sum > 0) # Finne rader som har vekt

        # Dersom det er færre enn fem år med avliva laks, bruk snitt av alle disse årene
        if (length(fangst) < 5) {
          d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor] <- sum(d$Laks_vekt_o7kg_sum)
          d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor] <- sum(d$Laks_ant_o7kg_sum)
        } else {
          # Hvis det er flere enn fem år, bruker vi vekt fra fem nærmeste år som har vekt:
          # Funksjonen f sorterer de 1-5 minste avstandene til rader med fangst for hver rad med lite/ingen fangst
          n_min <- 5
          f <- function(rw) {
            O <- order(rw)[1:n_min]
            rbind(O)
          }

          abs_diff <- sapply(lite_eller_uten_fangst_stor, function(x) abs(fangst - x))
          min_dist <- t(apply(abs_diff, 2, f))

          d$Laks_vekt_o7kg_sum[lite_eller_uten_fangst_stor] <-
            d$Laks_vekt_o7kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_vekt_o7kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_vekt_o7kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_vekt_o7kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_vekt_o7kg_sum[fangst[min_dist[, 5]]]
          d$Laks_ant_o7kg_sum[lite_eller_uten_fangst_stor] <-
            d$Laks_ant_o7kg_sum[fangst[min_dist[, 1]]] +
            d$Laks_ant_o7kg_sum[fangst[min_dist[, 2]]] +
            d$Laks_ant_o7kg_sum[fangst[min_dist[, 3]]] +
            d$Laks_ant_o7kg_sum[fangst[min_dist[, 4]]] +
            d$Laks_ant_o7kg_sum[fangst[min_dist[, 5]]]
        }
      }
    }

    # Vekt til å bruke ved telling - gjennomsnittsvekt fra fangst og/eller gjenutsatte * antall fra tellinger
    d$vekt_smaa_obs <- with(d,
                            ifelse(Obs_laks_ant_u3kg > 0,
                                   Laks_vekt_u3kg_sum / Laks_ant_u3kg_sum * Obs_laks_ant_u3kg,
                                   NA))
    d$vekt_mellom_obs <- with(d,
                              ifelse(Obs_laks_ant_o3u7kg > 0,
                                     Laks_vekt_o3u7kg_sum / Laks_ant_o3u7kg_sum * Obs_laks_ant_o3u7kg,
                                     NA))
    d$vekt_stor_obs <- with(d,
                            ifelse(Obs_laks_ant_o7kg > 0,
                                   Laks_vekt_o7kg_sum / Laks_ant_o7kg_sum * Obs_laks_ant_o7kg,
                                   NA))
  }

  #### --------- Simulering --------- ####
  n_years <- length(d$Aar) # antall år det skal beregnes gytebestand for

  # tomme matriser som brukes til å sette opp de ulike datamatrisene i simuleringen
  empty_matrix <- matrix(NA, nrow = n_years, ncol = n_sim)
  empty_matrix2 <- matrix(NA, nrow = n_years)

  # lag sannsynlighetsfordelinger for andel hunnlaks i ulike størrelsesgrupper
  dist_andel_hunn_u3kg <- empty_matrix
  dist_andel_hunn_37kg <- empty_matrix
  dist_andel_hunn_o7kg <- empty_matrix
  for (i in 1:n_years) {
    dist_andel_hunn_u3kg[i, ] <- rtriangle(n = n_sim,
                                           a = d$PrHuSmall[i] - 0.05,
                                           b = d$PrHuSmall[i] + 0.05,
                                           c = d$PrHuSmall[i])
    dist_andel_hunn_37kg[i, ] <- rtriangle(n = n_sim,
                                           a = d$PrHuMellom[i] - 0.05,
                                           b = d$PrHuMellom[i] + 0.05,
                                           c = d$PrHuMellom[i])
    dist_andel_hunn_o7kg[i, ] <- rtriangle(n = n_sim,
                                           a = d$PrHuStor[i] - 0.05,
                                           b = d$PrHuStor[i] + 0.05,
                                           c = d$PrHuStor[i])
  }

  # lag sannsynlighetsfordelinger for andel gjenutsatte hunnlaks i ulike størrelsesgrupper
  dist_andel_hunn_u3kg_gjen <- empty_matrix
  dist_andel_hunn_37kg_gjen <- empty_matrix
  dist_andel_hunn_o7kg_gjen <- empty_matrix
  for (i in 1:n_years) {
    dist_andel_hunn_u3kg_gjen[i, ] <- if (!is.na(d$PrHuSmall_Gjenut[i]))
      (rtriangle(n = n_sim,
                 a = d$PrHuSmall_Gjenut[i] - 0.05,
                 b = d$PrHuSmall_Gjenut[i] + 0.05,
                 c = d$PrHuSmall_Gjenut[i]))
    else
      NA
    dist_andel_hunn_37kg_gjen[i, ] <- if (!is.na(d$PrHuMellom_Gjenut[i]))
      (rtriangle(n = n_sim,
                 a = d$PrHuMellom_Gjenut[i] - 0.05,
                 b = d$PrHuMellom_Gjenut[i] + 0.05,
                 c = d$PrHuMellom_Gjenut[i]))
    else
      NA
    dist_andel_hunn_o7kg_gjen[i, ] <- if (!is.na(d$PrHuStor_Gjenut[i]))
      (rtriangle(n = n_sim,
                 a = d$PrHuStor_Gjenut[i] - 0.05,
                 b = d$PrHuStor_Gjenut[i] + 0.05,
                 c = d$PrHuStor_Gjenut[i]))
    else
      NA
  }

  # Simulere prosent antatt observert dersom det er  år med telling, og regne ut beskatningsrate fra dette
  ################## ENDRING: if(d$telling[i]) til if(!is.na(d$Obs_laks_ant_u3kg[i]) & d$Obs_laks_ant_u3kg[i]> 0)
  if (sum(d$telling) > 0) {
    dist_andel_obs_u3kg <- empty_matrix
    dist_andel_obs_37kg <- empty_matrix
    dist_andel_obs_o7kg <- empty_matrix

    for (i in 1:n_years) {
      dist_andel_obs_u3kg[i, ] <- if (!is.na(d$Obs_laks_ant_u3kg[i]) && d$Obs_laks_ant_u3kg[i] > 0)
        (rtriangle(n = n_sim, a = d$Probs_small_min[i], b = d$Probs_small_max[i], c = d$Probs_small_med[i]))
      else
        NA
      dist_andel_obs_37kg[i, ] <- if (!is.na(d$Obs_laks_ant_o3u7kg[i]) && d$Obs_laks_ant_o3u7kg[i] > 0)
        (rtriangle(n = n_sim, a = d$Probs_mellom_min[i], b = d$Probs_mellom_max[i], c = d$Probs_mellom_med[i]))
      else
        NA
      dist_andel_obs_o7kg[i, ] <- if (!is.na(d$Obs_laks_ant_o7kg[i]) && d$Obs_laks_ant_o7kg[i] > 0)
        (rtriangle(n = n_sim, a = d$Probs_stor_min[i], b = d$Probs_stor_max[i], c = d$Probs_stor_max[i]))
      else
        NA
    }

    # antall oppdrettslaks som årlige sannsynlighetsfordelinger basert på fangst og andel oppdrettslaks (ProsOpp)
    # (trenger dette for å regne ut antall ville)
    oppdrett_dist_totalt <- empty_matrix
    oppdrett_dist_u3kg <- empty_matrix
    oppdrett_dist_37kg <- empty_matrix
    oppdrett_dist_o7kg <- empty_matrix

    for (i in 1:n_years) {
      oppdrett_dist_totalt[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant[i]), prob = d$ProsOpp[i] / 100)
      oppdrett_dist_u3kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_u3kg[i]), prob = d$ProsOpp[i] / 100)
      oppdrett_dist_37kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_o3u7kg[i]), prob = d$ProsOpp[i] / 100)
      oppdrett_dist_o7kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_o7kg[i]), prob = d$ProsOpp[i] / 100)
    }

    # Antall villaks
    villaks_antall_u3kg <- empty_matrix
    villaks_antall_37kg <- empty_matrix
    villaks_antall_o7kg <- empty_matrix

    # Smålaks
    for (i in 1:n_years) {
      if (!is.na(d$Obs_laks_ant_u3kg[i]))
        villaks_antall_u3kg[i, ] <- (d$Obs_laks_ant_u3kg[i] / dist_andel_obs_u3kg[i, ]) + d$Laks_ant_u3kg[i] - oppdrett_dist_u3kg[i, ]
      else
        NA
    }
    villaks_antall_u3kg[villaks_antall_u3kg < 0] <- 0 # Setter negative verdier til 0

    # Mellomlaks
    for (i in 1:n_years) {
      if (!is.na(d$Obs_laks_ant_o3u7kg[i]))
        villaks_antall_37kg[i, ] <- (d$Obs_laks_ant_o3u7kg[i] / dist_andel_obs_37kg[i, ]) + d$Laks_ant_o3u7kg[i] - oppdrett_dist_37kg[i, ]
      else
        NA
    }
    villaks_antall_37kg[villaks_antall_37kg < 0] <- 0

    # Storlaks
    for (i in 1:n_years) {
      if (!is.na(d$Obs_laks_ant_o7kg[i]))
        villaks_antall_o7kg[i, ] <- (d$Obs_laks_ant_o7kg[i] / dist_andel_obs_o7kg[i, ]) + d$Laks_ant_o7kg[i] - oppdrett_dist_o7kg[i, ]
      else
        NA
    }
    villaks_antall_o7kg[villaks_antall_o7kg < 0] <- 0

    # Regner ut beskatningsrate fra tellinger, der vi har tellinger for gitt aldersklasse og
    # beskatningsrate mangler. Hvis ikke blir beskatningsrate stående som den er.
    ## NYTT 22.04.21: sette beskatningsrate til 0 dersom det var telling, men 0 telt i gitt vektklasse
    d$ExpSmallMed <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg > 0 & is.na(ExpSmallMed),
                                 Laks_ant_u3kg / rowMedians(villaks_antall_u3kg, na.rm = TRUE), ExpSmallMed))
    d$ExpSmallMed <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg == 0 & is.na(ExpSmallMed),
                                 0, ExpSmallMed))
    d$ExpSmallMin <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg > 0 & is.na(ExpSmallMin),
                                 Laks_ant_u3kg / as.vector(rowMax(villaks_antall_u3kg)), ExpSmallMin))
    d$ExpSmallMin <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg == 0 & is.na(ExpSmallMin),
                                 0, ExpSmallMin))
    d$ExpSmallMax <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg > 0 & is.na(ExpSmallMax),
                                 Laks_ant_u3kg / as.vector(rowMin(villaks_antall_u3kg)), ExpSmallMax))
    d$ExpSmallMax <- with(d,
                          ifelse(!is.na(Obs_laks_ant_u3kg) & Obs_laks_ant_u3kg == 0 & is.na(ExpSmallMax),
                                 0, ExpSmallMax))
    d$ExpMellomMed <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg > 0 & is.na(ExpMellomMed),
                                  Laks_ant_o3u7kg / rowMedians(villaks_antall_37kg, na.rm = TRUE), ExpMellomMed))
    d$ExpMellomMed <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg == 0 & is.na(ExpMellomMed),
                                  0, ExpMellomMed))
    d$ExpMellomMin <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg > 0 & is.na(ExpMellomMin),
                                  Laks_ant_o3u7kg / as.vector(rowMax(villaks_antall_37kg)), ExpMellomMin))
    d$ExpMellomMin <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg == 0 & is.na(ExpMellomMin),
                                  0, ExpMellomMin))
    d$ExpMellomMax <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg > 0 & is.na(ExpMellomMax),
                                  Laks_ant_o3u7kg / as.vector(rowMin(villaks_antall_37kg)), ExpMellomMax))
    d$ExpMellomMax <- with(d,
                           ifelse(!is.na(Obs_laks_ant_o3u7kg) & Obs_laks_ant_o3u7kg == 0 & is.na(ExpMellomMax),
                                  0, ExpMellomMax))
    d$ExpStorMed <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg > 0 & is.na(ExpStorMed),
                                Laks_ant_o7kg / rowMedians(villaks_antall_o7kg,  na.rm = TRUE), ExpStorMed))
    d$ExpStorMed <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg == 0 & is.na(ExpStorMed),
                                0, ExpStorMed))
    d$ExpStorMin <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg > 0 & is.na(ExpStorMin),
                                Laks_ant_o7kg / as.vector(rowMax(villaks_antall_o7kg)), ExpStorMin))
    d$ExpStorMin <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg == 0 & is.na(ExpStorMin),
                                0, ExpStorMin))
    d$ExpStorMax <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg > 0 & is.na(ExpStorMax),
                                Laks_ant_o7kg / as.vector(rowMin(villaks_antall_o7kg)), ExpStorMax))
    d$ExpStorMax <- with(d,
                         ifelse(!is.na(Obs_laks_ant_o7kg) & Obs_laks_ant_o7kg == 0 & is.na(ExpStorMax),
                                0, ExpStorMax))
  }

  # Regne ut beskatningsrate og fangstandel for år der det ikke er oppgitt
  # Beskatningsrate = avliva/((avliva + gjenutsatte)/fangstandel)
  # Fangstandel = (avliva + gjenutsatte)/(avliva/beskatningsrate)
  ## dersom beskatning er oppgitt beholder vi denne.
  ## dersom beskatning OG fangstandel er satt til NA beholder vi dette (for da er det ikke grunnlag for vurdering)
  ## dersom beskatning ikke er oppgitt og fangstandel er oppgitt, regner vi beskatning fra fangstandel, og motsatt.

  Beskatning <- function(Laks_ant, Gjen_ant, FangstAnd, Exp) {
    Exp <- ifelse(!is.na(Exp) | (is.na(Exp) & is.na(FangstAnd)), Exp,
                  Laks_ant / ((Laks_ant + Gjen_ant) / FangstAnd))
    return(Exp)
  }

  d$ExpSmallMed <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMed, ExpSmallMed))
  d$ExpSmallMin <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMin, ExpSmallMin))
  d$ExpSmallMax <- with(d, Beskatning(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMax, ExpSmallMax))
  d$ExpMellomMed <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMed, ExpMellomMed))
  d$ExpMellomMin <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMin, ExpMellomMin))
  d$ExpMellomMax <- with(d, Beskatning(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMax, ExpMellomMax))
  d$ExpStorMed <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMed, ExpStorMed))
  d$ExpStorMin <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMin, ExpStorMin))
  d$ExpStorMax <- with(d, Beskatning(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMax, ExpStorMax))

  # Regner ut fangstandel fra beskatningsrate dersom fangstandel ikke er oppgitt og
  # beskatningsandel er oppgitt og større enn 0

  FangstAndel <- function(Laks_ant, Gjen_ant, FangstAnd, Exp) {
    FangstAnd <- ifelse(!is.na(FangstAnd) | (is.na(FangstAnd) & (is.na(Exp) | Exp == 0)), FangstAnd,
                        (Laks_ant + Gjen_ant) / (Laks_ant / Exp))
  }

  ############ TEST 22.04 ##############
  ## NYTT 22.04.21: dersom fangstandel fortsatt er NA etter å ha regnet ut fangstandel fra
  ## beskatning, og beskatning er 0, så blir fangstandel også 0

  d$FangstAndSmallMed <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMed, ExpSmallMed))
  d$FangstAndSmallMed <- with(d, ifelse(is.na(FangstAndSmallMed) & ExpSmallMed == 0, 0, FangstAndSmallMed))
  d$FangstAndSmallMin <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMin, ExpSmallMin))
  d$FangstAndSmallMin  <- with(d, ifelse(is.na(FangstAndSmallMed) & ExpSmallMed == 0, 0, FangstAndSmallMin))
  d$FangstAndSmallMax <- with(d, FangstAndel(Laks_ant_u3kg, Gjen_ant_u3kg, FangstAndSmallMax, ExpSmallMax))
  d$FangstAndSmallMax  <- with(d, ifelse(is.na(FangstAndSmallMax) & ExpSmallMax == 0, 0, FangstAndSmallMax))

  d$FangstAndMellomMed <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMed, ExpMellomMed))
  d$FangstAndMellomMed <- with(d, ifelse(is.na(FangstAndMellomMed) & ExpMellomMed == 0, 0, FangstAndMellomMed))
  d$FangstAndMellomMin <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMin, ExpMellomMin))
  d$FangstAndMellomMin <- with(d, ifelse(is.na(FangstAndMellomMin) & ExpMellomMin == 0, 0, FangstAndMellomMin))
  d$FangstAndMellomMax <- with(d, FangstAndel(Laks_ant_o3u7kg, Gjen_ant_o3u7kg, FangstAndMellomMax, ExpMellomMax))
  d$FangstAndMellomMax <- with(d, ifelse(is.na(FangstAndMellomMax) & ExpMellomMax == 0, 0, FangstAndMellomMax))

  d$FangstAndStorMed <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMed, ExpStorMed))
  d$FangstAndStorMed <- with(d, ifelse(is.na(FangstAndStorMed) & ExpStorMed == 0, 0, FangstAndStorMed))
  d$FangstAndStorMin <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMin, ExpStorMin))
  d$FangstAndStorMin <- with(d, ifelse(is.na(FangstAndStorMin) & ExpStorMin == 0, 0, FangstAndStorMin))
  d$FangstAndStorMax <- with(d, FangstAndel(Laks_ant_o7kg, Gjen_ant_o7kg, FangstAndStorMax, ExpStorMax))
  d$FangstAndStorMax <- with(d, ifelse(is.na(FangstAndStorMax) & ExpStorMax == 0, 0, FangstAndStorMax))

  # Fil med beskatningsrate og fangstandel
  til_fil_beskatning_fangstandel <-  d %>%
    select(Vassdrag, Vdrnr, Aar, ExpSmallMin, FangstAndSmallMin, ExpSmallMed, FangstAndSmallMed,
           ExpSmallMax, FangstAndSmallMax, ExpMellomMin, FangstAndMellomMin, ExpMellomMed,
           FangstAndMellomMed, ExpMellomMax, FangstAndMellomMax, ExpStorMin, FangstAndStorMin,
           ExpStorMed, FangstAndStorMed, ExpStorMax, FangstAndStorMax)

  # Dersom fila allerede finnes, legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil.
  if (paste("Beskatning_FangstAndel", d$Aar[max(n_years)], ".csv", sep = "") %in% list.files("results")) {
    write.table(til_fil_beskatning_fangstandel,
                paste("results/Beskatning_FangstAndel", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
  } else {
    write.table(til_fil_beskatning_fangstandel,
                paste("results/Beskatning_FangstAndel", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE)
  }

  # Simulere beskatningsrate, fangstandel og prosent antatt observert
  ## Beholder NA der det er NA, beholder 0 der det er 0
  beskatningsrate_u3kg <- empty_matrix
  beskatningsrate_37kg <- empty_matrix
  beskatningsrate_o7kg <- empty_matrix
  for (i in 1:n_years) {
    beskatningsrate_u3kg[i, ] <- if (!is.na(d$ExpSmallMed[i]))
      (rtriangle(n = n_sim, a = d$ExpSmallMin[i], b = d$ExpSmallMax[i], c = d$ExpSmallMed[i])) else NA
    beskatningsrate_37kg[i, ] <- if (!is.na(d$ExpMellomMed[i]))
      (rtriangle(n = n_sim, a = d$ExpMellomMin[i], b = d$ExpMellomMax[i], c = d$ExpMellomMed[i])) else NA
    beskatningsrate_o7kg[i, ] <- if (!is.na(d$ExpStorMed[i]))
      (rtriangle(n = n_sim, a = d$ExpStorMin[i], b = d$ExpStorMax[i], c = d$ExpStorMed[i])) else NA
  }

  fangstandel_u3kg <- empty_matrix
  fangstandel_37kg <- empty_matrix
  fangstandel_o7kg <- empty_matrix
  for (i in 1:n_years) {
    fangstandel_u3kg[i, ] <- if (!is.na(d$FangstAndSmallMed[i]))
      (rtriangle(n = n_sim, a = d$FangstAndSmallMin[i], b = d$FangstAndSmallMax[i], c = d$FangstAndSmallMed[i]))
    else NA
    fangstandel_37kg[i, ] <- if (!is.na(d$FangstAndMellomMed[i]))
      (rtriangle(n = n_sim, a = d$FangstAndMellomMin[i], b = d$FangstAndMellomMax[i], c = d$FangstAndMellomMed[i]))
    else NA
    fangstandel_o7kg[i, ] <- if (!is.na(d$FangstAndStorMed[i]))
      (rtriangle(n = n_sim, a = d$FangstAndStorMin[i], b = d$FangstAndStorMax[i], c = d$FangstAndStorMed[i]))
    else NA
  }

  # antall oppdrettslaks som årlige sannsynlighetsfordelinger basert på fangst (antall laks) og
  # andel oppdrettslaks (ProsOpp)
  oppdrett_dist_totalt <- empty_matrix
  oppdrett_dist_u3kg <- empty_matrix
  oppdrett_dist_37kg <- empty_matrix
  oppdrett_dist_o7kg <- empty_matrix
  for (i in 1:n_years) {
    oppdrett_dist_totalt[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant[i]), prob = d$ProsOpp[i] / 100)
    oppdrett_dist_u3kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_u3kg[i]), prob = d$ProsOpp[i] / 100)
    oppdrett_dist_37kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_o3u7kg[i]), prob = d$ProsOpp[i] / 100)
    oppdrett_dist_o7kg[i, ] <- rbinom(n = n_sim, size = round(d$Laks_ant_o7kg[i]), prob = d$ProsOpp[i] / 100)
  }

  # kg oppdrettslaks, basert på estimert antall ovenfor og gjennomsnittsvekt for de ulike størrelsesklassene
  oppdrett_kg_u3kg <- oppdrett_dist_u3kg * ((d$Laks_vekt_u3kg + 2) / (d$Laks_ant_u3kg + 1))
  oppdrett_kg_37kg <- oppdrett_dist_37kg * ((d$Laks_vekt_o3u7kg + 5) / (d$Laks_ant_o3u7kg + 1))
  oppdrett_kg_o7kg <- oppdrett_dist_o7kg * ((d$Laks_vekt_o7kg + 9) / (d$Laks_ant_o7kg + 1))
  oppdrett_kg_totalt <- oppdrett_kg_u3kg + oppdrett_kg_37kg + oppdrett_kg_o7kg

  # Antall oppdrettslaks samlet i elva (estimert fra fangst og beskatningsrate)
  # Oppdatering: regne hvis antall oppdrett og beskatning er større enn 0, eller sette til 0.
  oppdrett_antall_u3kg <- ifelse(is.na(beskatningsrate_u3kg) | oppdrett_dist_u3kg == 0 | beskatningsrate_u3kg == 0,
                                 0, oppdrett_dist_u3kg / beskatningsrate_u3kg)
  oppdrett_antall_37kg <- ifelse(is.na(beskatningsrate_37kg) | oppdrett_dist_37kg == 0 | beskatningsrate_37kg == 0,
                                 0, oppdrett_dist_37kg / beskatningsrate_37kg)
  oppdrett_antall_o7kg <- ifelse(is.na(beskatningsrate_o7kg) | oppdrett_dist_o7kg == 0 | beskatningsrate_o7kg == 0,
                                 0, oppdrett_dist_o7kg / beskatningsrate_o7kg)
  oppdrett_antall_totalt <- oppdrett_antall_u3kg + oppdrett_antall_37kg + oppdrett_antall_o7kg

  # ENDRINGER:
  # bruker tellinger om det er tellinger for den gitte aldersklassen
  # Antall villaks bli kun regnet ut fra fangstandel dersom fangstandel er oppgitt og større enn 0.
  # Ellers satt til 0.

  villaks_antall_u3kg <- empty_matrix
  villaks_antall_37kg <- empty_matrix
  villaks_antall_o7kg <- empty_matrix
  villaks_antall_totalt <- empty_matrix

  # Smålaks
  for (s in 1:n_sim) {
    villaks_antall_u3kg[, s] <- ifelse(is.na(d$Obs_laks_ant_u3kg) | d$Obs_laks_ant_u3kg == 0,
                                       ifelse(!is.na(fangstandel_u3kg[, s]) & fangstandel_u3kg[, s] > 0,
                                              ((d$Laks_ant_u3kg - oppdrett_dist_u3kg[, s] + (d$Gjen_ant_u3kg - (d$Gjen_ant_u3kg * fangstandel_u3kg[, s] * 0.2))) / fangstandel_u3kg[, s]), 0),
                                       (d$Obs_laks_ant_u3kg / dist_andel_obs_u3kg[, s]) + d$Laks_ant_u3kg - oppdrett_dist_u3kg[, s])
  }
  villaks_antall_u3kg[villaks_antall_u3kg < 0] <- 0     # Setter negative verdier til 0

  # Mellomlaks
  for (s in 1:n_sim) {
    villaks_antall_37kg[, s] <- ifelse(is.na(d$Obs_laks_ant_o3u7kg) | d$Obs_laks_ant_o3u7kg == 0,
                                       ifelse(!is.na(fangstandel_37kg[, s]) & fangstandel_37kg[, s] > 0,
                                              ((d$Laks_ant_o3u7kg - oppdrett_dist_37kg[, s] + (d$Gjen_ant_o3u7kg - (d$Gjen_ant_o3u7kg * fangstandel_37kg[, s] * 0.2))) / fangstandel_37kg[, s]), 0),
                                       (d$Obs_laks_ant_o3u7kg / dist_andel_obs_37kg[, s]) + d$Laks_ant_o3u7kg - oppdrett_dist_37kg[, s])
  }
  villaks_antall_37kg[villaks_antall_37kg < 0] <- 0     # Setter negative verdier til 0

  # Storlaks
  for (s in 1:n_sim) {
    villaks_antall_o7kg[, s] <- ifelse(is.na(d$Obs_laks_ant_o7kg) | d$Obs_laks_ant_o7kg == 0,
                                       ifelse(!is.na(fangstandel_o7kg[, s]) & fangstandel_o7kg[, s] > 0,
                                              ((d$Laks_ant_o7kg - oppdrett_dist_o7kg[, s] + (d$Gjen_ant_o7kg - (d$Gjen_ant_o7kg * fangstandel_o7kg[, s] * 0.2))) / fangstandel_o7kg[, s]), 0),
                                       (d$Obs_laks_ant_o7kg / dist_andel_obs_o7kg[, s]) + d$Laks_ant_o7kg - oppdrett_dist_o7kg[, s])
  }
  villaks_antall_o7kg [villaks_antall_o7kg < 0] <- 0 # Setter negative verdier til 0

  # Totalt antall ville
  villaks_antall_totalt <- villaks_antall_u3kg + villaks_antall_37kg + villaks_antall_o7kg

  # Oppdrett percentiler
  oppdrett_dist_q02.5 <- apply(oppdrett_dist_totalt, 1, quantile, probs = 0.025)
  oppdrett_dist_q25 <- apply(oppdrett_dist_totalt, 1, quantile, probs = 0.25)
  oppdrett_dist_q50 <- apply(oppdrett_dist_totalt, 1, quantile, probs = 0.50)
  oppdrett_dist_q75 <- apply(oppdrett_dist_totalt, 1, quantile, probs = 0.75)
  oppdrett_dist_q97.5 <- apply(oppdrett_dist_totalt, 1, quantile, probs = 0.975)

  oppdrett_antall_q02.5 <- apply(oppdrett_antall_totalt, 1, quantile, probs = 0.025)
  oppdrett_antall_q25 <- apply(oppdrett_antall_totalt, 1, quantile, probs = 0.25)
  oppdrett_antall_q50 <- apply(oppdrett_antall_totalt, 1, quantile, probs = 0.50)
  oppdrett_antall_q75 <- apply(oppdrett_antall_totalt, 1, quantile, probs = 0.75)
  oppdrett_antall_q97.5 <- apply(oppdrett_antall_totalt, 1, quantile, probs = 0.975)

  oppdrett_kg_q02.5 <- apply(oppdrett_kg_totalt, 1, quantile, probs = 0.025)
  oppdrett_kg_q25 <- apply(oppdrett_kg_totalt, 1, quantile, probs = 0.25)
  oppdrett_kg_q50 <- apply(oppdrett_kg_totalt, 1, quantile, probs = 0.50)
  oppdrett_kg_q75 <- apply(oppdrett_kg_totalt, 1, quantile, probs = 0.75)
  oppdrett_kg_q97.5 <- apply(oppdrett_kg_totalt, 1, quantile, probs = 0.975)

  # elvebestand villfisk 0.025, 0.25, 0.50, 0.75, 0.975
  villaks_antall_u3kg_q02.5 <- apply(villaks_antall_u3kg, 1, quantile, probs = 0.025)
  villaks_antall_u3kg_q25 <- apply(villaks_antall_u3kg, 1, quantile, probs = 0.25)
  villaks_antall_u3kg_q50 <- apply(villaks_antall_u3kg, 1, quantile, probs = 0.50)
  villaks_antall_u3kg_q75 <- apply(villaks_antall_u3kg, 1, quantile, probs = 0.75)
  villaks_antall_u3kg_q97.5 <- apply(villaks_antall_u3kg, 1, quantile, probs = 0.975)

  villaks_antall_37kg_q02.5 <- apply(villaks_antall_37kg, 1, quantile, probs = 0.025)
  villaks_antall_37kg_q25 <- apply(villaks_antall_37kg, 1, quantile, probs = 0.25)
  villaks_antall_37kg_q50 <- apply(villaks_antall_37kg, 1, quantile, probs = 0.50)
  villaks_antall_37kg_q75 <- apply(villaks_antall_37kg, 1, quantile, probs = 0.75)
  villaks_antall_37kg_q97.5 <- apply(villaks_antall_37kg, 1, quantile, probs = 0.975)

  villaks_antall_o7kg_q02.5 <- apply(villaks_antall_o7kg, 1, quantile, probs = 0.025)
  villaks_antall_o7kg_q25 <- apply(villaks_antall_o7kg, 1, quantile, probs = 0.25)
  villaks_antall_o7kg_q50 <- apply(villaks_antall_o7kg, 1, quantile, probs = 0.50)
  villaks_antall_o7kg_q75 <- apply(villaks_antall_o7kg, 1, quantile, probs = 0.75)
  villaks_antall_o7kg_q97.5 <- apply(villaks_antall_o7kg, 1, quantile, probs = 0.975)

  villaks_antall_totalt_q02.5 <- apply(villaks_antall_totalt, 1, quantile, probs = 0.025)
  villaks_antall_totalt_q25 <- apply(villaks_antall_totalt, 1, quantile, probs = 0.25)
  villaks_antall_totalt_q50 <- apply(villaks_antall_totalt, 1, quantile, probs = 0.50)
  villaks_antall_totalt_q75 <- apply(villaks_antall_totalt, 1, quantile, probs = 0.75)
  villaks_antall_totalt_q97.5 <- apply(villaks_antall_totalt, 1, quantile, probs = 0.975)

  # Fil med antall oppdrett og villfisk
  til_fil_antall_vill_oppdrett <- tibble(villaks_antall_u3kg_q02.5, villaks_antall_u3kg_q25,
                                         villaks_antall_u3kg_q50, villaks_antall_u3kg_q75,
                                         villaks_antall_u3kg_q97.5, villaks_antall_37kg_q02.5,
                                         villaks_antall_37kg_q25, villaks_antall_37kg_q50,
                                         villaks_antall_37kg_q75, villaks_antall_37kg_q97.5,
                                         villaks_antall_o7kg_q02.5, villaks_antall_o7kg_q25,
                                         villaks_antall_o7kg_q50, villaks_antall_o7kg_q75,
                                         villaks_antall_o7kg_q97.5, villaks_antall_totalt_q02.5,
                                         villaks_antall_totalt_q25, villaks_antall_totalt_q50,
                                         villaks_antall_totalt_q75, villaks_antall_totalt_q97.5,
                                         oppdrett_dist_q02.5, oppdrett_dist_q25,
                                         oppdrett_dist_q50, oppdrett_dist_q75,
                                         oppdrett_dist_q97.5, oppdrett_antall_q02.5,
                                         oppdrett_antall_q25, oppdrett_antall_q50,
                                         oppdrett_antall_q75, oppdrett_antall_q97.5) %>%
    mutate(Vassdrag = d$Vassdrag) %>%
    mutate(Vdrnr = d$Vdrnr) %>%
    mutate(Kommune = d$Kommune) %>%
    mutate(Aar = d$Aar) %>%
    mutate(GBMkghunner = d$GBMkghunner) %>%
    select(Vassdrag:GBMkghunner, everything())   # Flytte kolonner Vassdrag til GBMkghunner fremst i datasettet

  # Sette år uten grunnlag for vurdering til NA
  # Vill elvebestand smålaks, mellomlaks og storlaks til NA i år der beskatningsrate er NA
  til_fil_antall_vill_oppdrett[which(is.na(d$ExpSmallMin)), 6:10] <- NA
  til_fil_antall_vill_oppdrett[which(is.na(d$ExpMellomMin)), 11:15] <- NA
  til_fil_antall_vill_oppdrett[which(is.na(d$ExpStorMin)), 16:20] <- NA

  # Total elvebestand satt til NA dersom der ikke er avliva eller gjenutsatt fangst og ikke telling
  til_fil_antall_vill_oppdrett[which(d$Laks_ant == 0 & d$Gjen_ant == 0 & d$telling == 0), 21:25] <- NA

  # oppdrett_antall_totalt satt til NA dersom det ikke er avliva fangst
  til_fil_antall_vill_oppdrett[which(d$Laks_ant == 0), 26:35] <- NA

  # Dersom fila allerede finnes, så legges talla inn i eksisterende fil, hvis ikke, opprettes det en ny fil
  if (paste("AntVillogOppdrettElvEstimater", d$Aar[max(n_years)], ".csv", sep = "") %in% list.files("results")) {
    write.table(til_fil_antall_vill_oppdrett,
                paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  } else {
    write.table(til_fil_antall_vill_oppdrett,
                paste("results/AntVillogOppdrettElvEstimater", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE)
  }

  #### ------- Simulering gytebestand -------####

  # Triangulærfordeling andel døde (gjenutsatt)
  andel_dode_gjenuts <- rtriangle(n = n_sim, a = 1 - 0.96, b = 1 - 0.85, c = 1 - 0.93)

  # Gjenutsatt laks justert for gjenfangst og andel hunner
  gjenutsatt_hunn_kg_u3kg_just <- empty_matrix
  gjenutsatt_hunn_kg_37kg_just <- empty_matrix
  gjenutsatt_hunn_kg_o7kg_just <- empty_matrix

  gjenutsatt_hunn_kg_u3kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Gjen_vekt_u3kg > 0,
           (d$Gjen_vekt_u3kg - (d$Gjen_vekt_u3kg * fangstandel_u3kg[, s] * 0.2)) * dist_andel_hunn_u3kg[, s], 0)
  })
  gjenutsatt_hunn_kg_37kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Gjen_vekt_o3u7kg > 0,
           (d$Gjen_vekt_o3u7kg - (d$Gjen_vekt_o3u7kg * fangstandel_37kg[, s] * 0.2)) * dist_andel_hunn_37kg[, s], 0)
  })
  gjenutsatt_hunn_kg_o7kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Gjen_vekt_o7kg > 0,
           (d$Gjen_vekt_o7kg - (d$Gjen_vekt_o7kg * fangstandel_o7kg[, s] * 0.2)) * dist_andel_hunn_o7kg[, s], 0)
  })

  # Avlivet fangst justert for oppdrett og andel hunnlaks
  avlivet_hunn_kg_u3kg_just <- empty_matrix
  avlivet_hunn_kg_37kg_just <- empty_matrix
  avlivet_hunn_kg_o7kg_just <- empty_matrix

  avlivet_hunn_kg_u3kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Laks_vekt_u3kg > 0,
           (d$Laks_vekt_u3kg - oppdrett_kg_u3kg[, s]) * dist_andel_hunn_u3kg[, s], 0)
  })
  avlivet_hunn_kg_37kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Laks_vekt_o3u7kg > 0,
           (d$Laks_vekt_o3u7kg - oppdrett_kg_37kg[, s]) * dist_andel_hunn_37kg[, s], 0)
  })
  avlivet_hunn_kg_o7kg_just <- sapply(1:n_sim, function(s) {
    ifelse(d$Laks_vekt_o7kg > 0,
           (d$Laks_vekt_o7kg - oppdrett_kg_o7kg[, s]) * dist_andel_hunn_o7kg[, s], 0)
  })

  # Simulering gytebestand
  # To alternative prosedyrer: telling og prosent antatt observert, eller total fangst og fangstandel
  gyting_hunn_kg_u3kg <- empty_matrix
  gyting_hunn_kg_37kg <- empty_matrix
  gyting_hunn_kg_o7kg <- empty_matrix
  gyting_hunn_kg_totalt <- empty_matrix

  # Smålaks
  gyting_hunn_kg_u3kg <- empty_matrix
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      if (d$Simulering[i]) {
        gyting_hunn_kg_u3kg[i, s] <- if (is.na(d$Obs_laks_ant_u3kg[i]) || d$Obs_laks_ant_u3kg[i] == 0) {
          if ((oppdrett_kg_u3kg[i, s] < d$Laks_vekt_u3kg[i] + d$Gjen_vekt_u3kg[i]) && !is.na(fangstandel_u3kg[i, s]) && fangstandel_u3kg[i, s] > 0)
            ((gjenutsatt_hunn_kg_u3kg_just[i, s] + avlivet_hunn_kg_u3kg_just[i, s]) / fangstandel_u3kg[i, s] - d$StamAntSmaHo[i] * 2 - gjenutsatt_hunn_kg_u3kg_just[i, s] * andel_dode_gjenuts[s] - avlivet_hunn_kg_u3kg_just[i, s])
          else
            0
        } else {
          if (oppdrett_kg_u3kg[i, s] < d$vekt_smaa_obs[i])
            ((d$vekt_smaa_obs[i] * dist_andel_hunn_u3kg[i, s]) / dist_andel_obs_u3kg[i, s] - d$StamAntSmaHo[i] * 2)
          else
            0
        }
      } else {
        gyting_hunn_kg_u3kg[i, s] <- 0
      }
    }
  }

  # Mellomlaks
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      if (d$Simulering[i]) {
        gyting_hunn_kg_37kg[i, s] <- if (is.na(d$Obs_laks_ant_o3u7kg[i]) || d$Obs_laks_ant_o3u7kg[i] == 0) {
          if ((oppdrett_kg_37kg[i, s] < d$Laks_vekt_o3u7kg[i] + d$Gjen_vekt_o3u7kg[i]) && !is.na(fangstandel_37kg[i, s]) && fangstandel_37kg[i, s] > 0)
            ((gjenutsatt_hunn_kg_37kg_just[i, s] + avlivet_hunn_kg_37kg_just[i, s]) / fangstandel_37kg[i, s] - d$StamAntMelHo[i] * 2 - gjenutsatt_hunn_kg_37kg_just[i, s] * andel_dode_gjenuts[s] - avlivet_hunn_kg_37kg_just[i, s])
          else
            0
        } else {
          if (oppdrett_kg_37kg[i, s] < d$vekt_mellom_obs[i])
            (((d$vekt_mellom_obs[i] - oppdrett_kg_37kg[i, s]) * dist_andel_hunn_37kg[i, s]) / dist_andel_obs_37kg[i, s] - d$StamAntMelHo[i] * 4)
          else
            0
        }
      } else {
        gyting_hunn_kg_37kg[i, s] <- 0
      }
    }
  }

  # Storlaks
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      if (d$Simulering[i]) {
        gyting_hunn_kg_o7kg[i, s] <- if (is.na(d$Obs_laks_ant_o7kg[i]) || d$Obs_laks_ant_o7kg[i] == 0) {
          if ((oppdrett_kg_o7kg[i, s] < d$Laks_vekt_o7kg[i] + d$Gjen_vekt_o7kg[i]) && !is.na(fangstandel_o7kg[i, s]) && fangstandel_o7kg[i, s] > 0)
            ((gjenutsatt_hunn_kg_o7kg_just[i, s] + avlivet_hunn_kg_o7kg_just[i, s]) / fangstandel_o7kg[i, s] - d$StamAntStorHo[i] * 2 - gjenutsatt_hunn_kg_o7kg_just[i, s] * andel_dode_gjenuts[s] - avlivet_hunn_kg_o7kg_just[i, s])
          else
            0
        } else {
          if (oppdrett_kg_o7kg[i, s] < d$vekt_stor_obs[i])
            (((d$vekt_stor_obs[i] - oppdrett_kg_o7kg[i, s]) * dist_andel_hunn_o7kg[i, s]) / dist_andel_obs_o7kg[i, s] - d$StamAntStorHo[i] * 8)
          else
            0
        }
      } else {
        gyting_hunn_kg_o7kg[i, s] <- 0
      }
    }
  }

  # Erstatt negative verdier med null
  gyting_hunn_kg_u3kg <- replace(gyting_hunn_kg_u3kg, which(gyting_hunn_kg_u3kg < 0), 0)
  gyting_hunn_kg_37kg <- replace(gyting_hunn_kg_37kg, which(gyting_hunn_kg_37kg < 0), 0)
  gyting_hunn_kg_o7kg <- replace(gyting_hunn_kg_o7kg, which(gyting_hunn_kg_o7kg < 0), 0)

  # Totalt kg hoer
  # Legger sammen totalt kg hoer, og setter NA i år der det ikke er oppgitt beskatningsrate
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      gyting_hunn_kg_totalt[i, s] <- if (is.na(beskatningsrate_u3kg[i, s]) &&
                                           is.na(beskatningsrate_37kg[i, s]) &&
                                           is.na(beskatningsrate_o7kg[i, s])) NA
      else
        (gyting_hunn_kg_u3kg[i, s] + gyting_hunn_kg_37kg[i, s] + gyting_hunn_kg_o7kg[i, s])
    }
  }

  #--------------------------------------------------------------------------------------------------------------
  # NY SEKSJON: Lag en rapporteringsfil med år for år median justert hunnfangst og kg hunnlaks i ulike størrelser
  #--------------------------------------------------------------------------------------------------------------
  fangst_hunn_u3_justert <- rowMedians(avlivet_hunn_kg_u3kg_just, na.rm = TRUE)
  fangst_hunn_37_justert <- rowMedians(avlivet_hunn_kg_37kg_just, na.rm = TRUE)
  fangst_hunn_o7_justert <- rowMedians(avlivet_hunn_kg_o7kg_just, na.rm = TRUE)
  andel_hunn_u3 <- rowMedians(dist_andel_hunn_u3kg, na.rm = TRUE)
  andel_hunn_37 <- rowMedians(dist_andel_hunn_37kg, na.rm = TRUE)
  andel_hunn_o7 <- rowMedians(dist_andel_hunn_o7kg, na.rm = TRUE)
  gyting_hunn_u3 <- rowMedians(gyting_hunn_kg_u3kg, na.rm = TRUE)
  gyting_hunn_37 <- rowMedians(gyting_hunn_kg_37kg, na.rm = TRUE)
  gyting_hunn_o7 <- rowMedians(gyting_hunn_kg_o7kg, na.rm = TRUE)

  #--------------------------------------------------------------------------------------------------------------
  # Simuler gytebestandsmål ferdig og lag rapportfiler
  #--------------------------------------------------------------------------------------------------------------

  # Triangulærfordeling gytebestandsmål
  # MERK: Opprinnelig skript hentet GBM verdier fra de elvespesifikke datafilene.
  # Endret til å hente fra vassdragslisten.
  # MERK 2: Opprinnelig skript laget individuelle sannsynlighetsfordelinger for de ulike årene.
  # Optimalisert til å bare lage én felles fordeling.

  gbm <- rtriangle(n = n_sim, a = elveliste$GBM_lav[m], b = elveliste$GBM_hoy[m], c = elveliste$GBM[m])

  # Oppnåelse gytebestandsmål. 1 = nådd, 0 = ikke nådd
  naadgbm <- empty_matrix
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      naadgbm[i, s] <- if (!is.na(gyting_hunn_kg_totalt[i, s])) {
        if (gyting_hunn_kg_totalt[i, s] > gbm[s])
          1
        else
          0
      } else {
        NA
      }
    }
  }

  # prosent sannsynlighet for nådd gytebestandsmål
  prosent_sannsynlighet_gbm <- (rowSums(naadgbm, na.rm = TRUE) * 100) / n_sim

  # Regn ut sannsynlighet for nok gytelaks siste 4 år, ekskluder år uten simulering
  prosent_sanns_siste_fire <- empty_matrix2
  for (i in 4:n_years) {
    prosent_sanns_siste_fire[i] <- sum(prosent_sannsynlighet_gbm[(i - 3):i], na.rm = TRUE) /
      sum(d$Simulering[(i - 3):i])
  }

  # Regn ut prosent oppnådd gytebestandsmål for alle simulerte verdier (trunkert til 100)
  prosent_av_gbm_simulert <- empty_matrix
  for (s in 1:n_sim) {
    for (i in 1:n_years) {
      prosent_av_gbm_simulert[i, s] <- if (!is.na(gyting_hunn_kg_totalt[i, s])) {
        if ((100 * gyting_hunn_kg_totalt[i, s] / gbm[s]) > 100)
          100
        else
          (100 * gyting_hunn_kg_totalt[i, s] / gbm[s])
      } else {
        NA
      }
    }
  }

  # Regn ut gjennomsnittlig prosent måloppnåelse for hvert år
  prosent_maaloppnaaelse <- rowMeans(prosent_av_gbm_simulert, na.rm = TRUE)

  # regn ut prosent måloppnåelse siste 4 år, men ikke inkluder år uten simulering
  prosent_maalopp_siste_fire <- empty_matrix2
  for (i in 4:n_years) {
    prosent_maalopp_siste_fire[i] <- sum(prosent_maaloppnaaelse[(i - 3):i], na.rm = TRUE) / sum(d$Simulering[(i - 3):i])
  }

  # Prosent oppnådd gytebestandsmål utrunkert
  prosent_gbm_utrunk <- gyting_hunn_kg_totalt / gbm * 100

  prosent_maaloppnaaelse_utrunk <- rowMeans(prosent_gbm_utrunk, na.rm = TRUE)

  pros_maalopp_siste_fire_utrunk <- empty_matrix2
  for (i in 4:n_years) {
    pros_maalopp_siste_fire_utrunk[i] <- sum(prosent_maaloppnaaelse_utrunk[(i - 3):i], na.rm = TRUE) / sum(d$Simulering[(i - 3):i])
  }

  # Fil med måloppnåelse
  til_fil_maaloppnaelse <- tibble(prosent_sannsynlighet_gbm, prosent_sanns_siste_fire, prosent_maaloppnaaelse,
                                  prosent_maalopp_siste_fire,
                                  prosent_maaloppnaaelse_utrunk, pros_maalopp_siste_fire_utrunk) %>%
    mutate(Vassdrag = d$Vassdrag) %>%
    mutate(Vdrnr = d$Vdrnr) %>%
    mutate(Aar = d$Aar) %>%
    select(Vassdrag:Aar, everything())

  if (paste("Maaloppnaaelse", d$Aar[max(n_years)], ".csv", sep = "") %in% list.files("results")) {
    write.table(til_fil_maaloppnaelse, paste("results/Maaloppnaaelse", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE,
                col.names = FALSE, append = TRUE)
  } else {
    write.table(til_fil_maaloppnaelse, paste("results/Maaloppnaaelse", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE)
  }

  simulert <- d$Simulering

  til_fil_gyting <- tibble(fangst_hunn_u3_justert, fangst_hunn_37_justert, fangst_hunn_o7_justert,
                           gyting_hunn_u3, gyting_hunn_37, gyting_hunn_o7,
                           andel_hunn_u3, andel_hunn_37, andel_hunn_o7,
                           prosent_sannsynlighet_gbm, prosent_sanns_siste_fire, prosent_maaloppnaaelse,
                           prosent_maalopp_siste_fire, prosent_maaloppnaaelse_utrunk,
                           pros_maalopp_siste_fire_utrunk, simulert) %>%
    mutate(Vassdrag = d$Vassdrag) %>%
    mutate(Vdrnr = d$Vdrnr) %>%
    mutate(Aar = d$Aar) %>%
    select(Vassdrag:Aar, everything())

  if (paste("KgHunnlaks", d$Aar[max(n_years)], ".csv", sep = "") %in% list.files("results")) {
    write.table(til_fil_gyting, paste("results/KgHunnlaks", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE, col.names = FALSE, append = TRUE)
  } else {
    write.table(til_fil_gyting, paste("results/KgHunnlaks", d$Aar[max(n_years)], ".csv", sep = ""),
                sep = ";", row.names = FALSE)
  }

  #lagre elvespesifikke filer med de simulerte gytebestandene
  gyting_hunn_kg_totalt_df <- as_tibble(gyting_hunn_kg_totalt) %>%
    mutate(Aar = c(d$Aar)) %>%
    pivot_longer(cols = c(1:5000)) %>%  # Gjøre om til langt format, sånn at vi får 5000 rader per år
    rename(gyting_hunn_kg_totalt = value)            # Kolonne med gyting_hunn_kg_totalt

  # mangler data som gjør at det ikke kan simuleres? ---> NA
  Aar <- as.vector(d$Aar)
  datagrunnlag <- cbind(Aar, simulert)
  gyting_hunn_kg_totalt_df <- merge(gyting_hunn_kg_totalt_df, datagrunnlag, by = "Aar")
  gyting_hunn_kg_totalt_df <- gyting_hunn_kg_totalt_df %>%
    mutate(gyting_hunn_kg_totalt = replace(gyting_hunn_kg_totalt, simulert == 0, NA)) %>% # Sett NA ved ikke simulert
    select(-simulert) # Fjerne kolonnene som ikke skal være med i filen
  gytebestand_filnavn <- paste("results/vassdrag/", elveliste[m, "Filnavn"], "-gyting_hunn_kg_totalt.csv", sep = "")
  export(gyting_hunn_kg_totalt_df, gytebestand_filnavn, sep = ";", dec = ".", bom = TRUE)

}


for (m in indeks_simuler) {
  d <- elvedata %>% filter(Vdrnr == elveliste$VdrNr[m])
  kolonner <- c("ExpStorMin", "ExpStorMed", "ExpStorMax", "ExpMellomMin", "ExpMellomMed", "ExpMellomMax",
                "ExpSmallMin", "ExpSmallMed", "ExpSmallMax")
  betingelse <- d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant)
  for (k in kolonner) {
    d[[k]][betingelse] <- NA
  }

  # flagg år som i simuleringen skal komme ut med asterisk (uten fangst og/eller problemer med telling)
  # "Asterisk" blir satt til 0 for år som ikke skal simuleres, og 1 for år som skal simuleres
  # dette flagget er den gamle måten å flagge år som ikke simuleres på, og er nå bare delvis i bruk
  # det er nå kommmet et nytt flagg i datafilene som heter "Simulering"
  # "Asterisk" skal derfor fases ut etterhvert
  d$Asterisk <- ifelse(is.na(d$ExpSmallMed) & is.na(d$FangstAndSmallMed) & is.na(d$Probs_small_med), 1, 0)

  # nedenfor settes fangstandel til NA i år der det mangler fangst og gjenutsatte og ikke er telling
  kolonner <- c("FangstAndStorMin", "FangstAndStorMed", "FangstAndStorMax", "FangstAndMellomMin",
                "FangstAndMellomMed", "FangstAndMellomMax", "FangstAndSmallMin", "FangstAndSmallMed",
                "FangstAndSmallMax")
  betingelse <- d$Laks_ant == 0 & (d$Gjen_ant == 0 | is.na(d$Gjen_ant)) & is.na(d$Obs_laks_ant)
  for (k in kolonner) {
    d[[k]][betingelse] <- NA
  }

  # sette beskatningsrate til 0 der det mangler fangst for en vektklasse men det er avliva fangst for
  # andre vektklasser, dersom da ikke året er flagget allerede
  kolonner <- c("ExpStorMin", "ExpStorMed", "ExpStorMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_o7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o3u7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  kolonner <- c("ExpMellomMin", "ExpMellomMed", "ExpMellomMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_o3u7kg == 0 & (d$Laks_ant_u3kg > 0 | d$Laks_ant_o7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  kolonner <- c("ExpSmallMin", "ExpSmallMed", "ExpSmallMax")
  betingelse <- d$Asterisk == 0 & d$Laks_ant_u3kg == 0 & (d$Laks_ant_o3u7kg > 0 | d$Laks_ant_o7kg > 0)
  for (k in kolonner) {
    d[[k]][betingelse] <- 0
  }

  # erstatt NA med 0 i utvalgte kolonner for å forenkle skriptet
  kolonner <- c("StamAntSmaHo", "StamAntMelHo", "StamAntStorHo",
                "Laks_vekt_u3kg", "Laks_vekt_o3u7kg", "Laks_vekt_o7kg",
                "Laks_ant_u3kg", "Laks_ant_o3u7kg", "Laks_ant_o7kg",
                "Gjen_vekt_u3kg", "Gjen_vekt_o3u7kg", "Gjen_vekt_o7kg",
                "Gjen_ant_u3kg", "Gjen_ant_o3u7kg", "Gjen_ant_o7kg",
                "Laks_ant", "Gjen_ant")
  for (k in kolonner) {
    d[[k]][is.na(d[[k]])] <- 0
  }

  # definer indeks for om det er telling eller ikke i et år, 1=telling, 0=ikke telling
  d$telling <- with(d, ifelse(is.na(Obs_laks_ant), 0, 1))

  #### --------- Vekt i år med telling --------- ####
  # For år som har fangst og/eller gjenutsatte regner vi ut gjennomsnittsvekt: gjen_vekt+laks_vekt/gjen_ant+Laks_ant
  # År uten fangst eller gjenutsatte : gjennomsnittet av fangst og gjenutsatt 5 nærmeste år

  if (sum(d$telling) > 0) { # denne delen kjøres bare dersom det har vært telling

    d$Laks_vekt_u3kg_sum <- with(d, Laks_vekt_u3kg + Gjen_vekt_u3kg)
    d$Laks_ant_u3kg_sum <- with(d, Laks_ant_u3kg + Gjen_ant_u3kg)
    d$Laks_vekt_o3u7kg_sum <- with(d, Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg)
    d$Laks_ant_o3u7kg_sum <- with(d, Laks_ant_o3u7kg + Gjen_ant_o3u7kg)
    d$Laks_vekt_o7kg_sum <- with(d, Laks_vekt_o7kg + Gjen_vekt_o7kg)
    d$Laks_ant_o7kg_sum <- with(d, Laks_ant_o7kg + Gjen_ant_o7kg)

    ### SMÅLAKS
    # Flagg hvis det mangler fangst i år med telling
    uten_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & d$Laks_vekt_u3kg_sum == 0, 1, 0)

    # Flagg hvis færre enn 5 laks fanget i år med telling
    lite_fangst_smaa <- ifelse(d$Obs_laks_ant_u3kg > 0 & (d$Laks_ant_u3kg_sum < 5 & d$Laks_ant_u3kg_sum > 0), 1, 0)

    if (sum(d$Laks_ant_u3kg_sum) < 5) break
  }
}
