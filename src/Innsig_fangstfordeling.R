#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar utgangspunkt i gytebestand, beskatning, elvefangst og sjøfangst. Sjøfangst fordeles på
# regionnivå og så videre på vassdragsnivå. Skriptet beregner så innsig i forskjellige områder. Estimatene
# gjøres på størrelsesgruppe og både biomasse og antall.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(rio)
library(purrr)

# navn på output-fil fra gytebestandsimulering
sim_kghunnlaks_filnavn <- "results/KgHunnlaks2023.csv"

# Variablene nedenfor definerer startår og antall år som skal estimeres. Skriptet er fleksibelt organisert
# rundt disse for å gi mulighet til å gjennomføre historiske sammenligninger, for eksempel før og etter
# viktige reguleringsendringer
start_aar <- 2002
antall_aar <- 22

# initialiser en liste over vassdrag som skal være med i innsigsfordelingen med noen bakgrunnstall
elveliste <- import("data/elveliste.csv", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

# liste over sjøregioner
regionliste <- import("data/regionliste.csv", encoding = "UTF-8")
antall_regioner <- nrow(regionliste)

# fordelingsnøkkel for fordeling av fangsten i kystregionene
kyst_fordeling <- import("data/fordelingsnokkel_sjofangst.csv", encoding = "UTF-8")

#-----------------------------------------------------------------------------------------------------------------
# nedenfor leses de årlige fordelingsnøklene inn som brukes til å fordele kommunefangst til regioner
#-----------------------------------------------------------------------------------------------------------------

# katalog og filnavn for de årlige fordelingsnøklene
region_filnavn <- "data/fordeling_kommune/fordelingsnokkel_kommune_fjord-%d.csv"

# lag en liste med årstall
liste_aar <- start_aar:(start_aar + antall_aar - 1)

# importer og slå sammen fordelingsnøklene for alle år
region_fordeling <- map_dfr(liste_aar, ~ {
  file_path <- sprintf(region_filnavn, .x)
  import(file_path, encoding = "UTF-8") %>% mutate(Year = .x)
})

# konverter fylke til tall og erstatt NA med 0
region_fordeling <- region_fordeling %>%
  mutate(Fylke = as.numeric(Fylke)) %>%
  replace_na(list(Fylke = 0))

antall_kommuner <- n_distinct(region_fordeling$Kommune)
antall_fylker <- n_distinct(region_fordeling$Fylke)

# les inn fangststatistikk fra sjølaksefisket
sjofangst <- import("data/ssb_sjo_1993-.csv", encoding = "UTF-8", na.strings = c(":", ".."))

# les inn fylkesfordelt restfangst, må fordeles på regioner
restfangst_sjo <- import("data/restfangst_fylker_sjo.csv", encoding = "UTF-8")

# les inn andel 1SW i vekgruppe under 3 kg
region_andel1SW <- import("data/Prosent1SWblantLaksMindreEnn3kgInnsigsregioner.csv", encoding = "UTF-8")

# initier noen variable
region_fangst <- array(0, c(antall_aar, antall_regioner, 6))
region_leggtil <- array(0, c(antall_aar, antall_regioner, 6))
region_indre <- array(0, c(antall_aar, antall_regioner, 6))

region_deb <- array(0, c(antall_aar, antall_regioner, 6))

fangst <- matrix(0, nrow = antall_kommuner, ncol = antall_regioner)

aar_liste <- vector()
elv_fangst <- array(0, c(antall_aar, antall_elver, 6)) # 6 =  3 størrelsesklasser for vekt og antall
elv_gyting <- array(0, c(antall_aar, antall_elver, 6))
elv_innsig <- array(0, c(antall_aar, antall_elver, 6))
elv_innsigvekt_over15kg <- array(0, c(antall_aar, antall_elver)) # vekt smålaks innsig over 1.5 kg
andel_over15kg <- array(0, c(antall_aar, antall_elver)) # andel smålaks over 1.5 kg i innsig
elv_inkluder_normal <- array(0, c(antall_aar, antall_elver))
elv_andelhunn <- matrix(nrow = antall_aar, ncol = antall_elver)
elv_hunnproporsjon <- array(0, c(antall_aar, antall_elver, 3)) # 3 størrelseskategorier
elv_beskatningsrate <- array(0, c(antall_aar, antall_elver, 3))
elv_snittvekt <- array(0, c(antall_aar, antall_elver, 3))
elv_simulert <- array(0, c(antall_aar, antall_elver))
snittvekt <- vector()

#-----------------------------------------------------------------------------------------------------------------
# lag regional fangstmatrise for valgte år
#-----------------------------------------------------------------------------------------------------------------
for (j in 1:antall_aar) {
  aar_indeks <- start_aar + j - 1
  aar_liste[j] <- aar_indeks

  df <- sjofangst %>% 
    filter(aar == aar_indeks) %>%
    arrange(komnr)

  reg_ford <- region_fordeling  %>%
    filter(Year == aar_indeks, Kommunenr %in% df$komnr) %>%
    arrange(Kommunenr)

  for (i in 1:6) {
    fangst <- df[, i + 4] * reg_ford[, 4:(antall_regioner + 3)]
    region_fangst[j, , i] <- colSums(fangst, na.rm = TRUE)
  }
}

#-----------------------------------------------------------------------------------------------------------------
# lag fordeling av rest
#-----------------------------------------------------------------------------------------------------------------
fylkesliste <- unique(region_fordeling$Fylke)

for (j in 1:antall_aar) {
  aar_indeks <- start_aar + j - 1

  # initier en matrise for fordeling av restfangsten hvert år
  restfordeling_fylke <- matrix(0, nrow = antall_fylker, ncol = antall_regioner)

  # filtrer ut fangstlinjer for det aktuelle året
  df <- filter(sjofangst, aar == aar_indeks)
  df$fylke <- as.numeric(df$fylke)

  # fordelingsnøkkel for regioner i det aktuelle året
  fordeling_aar <- filter(region_fordeling, Year == aar_indeks)

  # loop gjennom alle radene i fangststatistikken
  for (k in seq_len(nrow(df))) {
    # sjekk om fangst ikke er rapportert (NA)
    if (is.na(df[k, 5])) {
      # finn hvilket fylke kommunen hører til
      fylkesindeks <- which(fylkesliste == df[k, 1])

      # finn hvilken linje i fordelingsmatrisen som tilsvarer kommunen
      fordelingsindeks <- which(fordeling_aar$Kommunenr == df[k, 3])

      # legg til kommunens regionfordeling til fylket
      restfordeling_fylke[fylkesindeks, ] <- restfordeling_fylke[fylkesindeks, ] +
        as.numeric(fordeling_aar[fordelingsindeks, 4:(antall_regioner + 3)])
    }
  }

  # normaliser fordelingsmatrisen og erstatt NA verdier med 0
  restfordeling_fylke <- proportions(restfordeling_fylke, 1) %>% replace(is.na(.), 0)

  # fordel fylkene med restfangst på regioner
  rest_aar <- filter(restfangst_sjo, Aar == aar_indeks)
  antall_fylker_rest <- nrow(rest_aar)

  # loop gjennom alle fylkene med restfangst
  for (l in 1:antall_fylker_rest) {
    # finn linjen i fordelingsmatrisen som tilsvarer fylket
    n <- which(fylkesliste == rest_aar[l, 1])

    # loop gjennom alle regionene for hvert fylke
    for (m in 1:antall_regioner) {

      # loop gjennom de tre størrelsesklassene fordelt på vekt og antall
      for (i in 1:6) {
        region_fangst[j, m, i] <- region_fangst[j, m, i] + (rest_aar[l, i + 2] * restfordeling_fylke[n, m])
        region_deb[j, m, i] <- region_deb[j, m, i] + (rest_aar[l, i + 2] * restfordeling_fylke[n, m])
      }
    }
  }
}

# fordel fangst på ytre kyst inn i fjordene
antall_kyst <- sum(regionliste$Kontrast == 0) # antall kystregioner
for (j in 1:antall_aar) { # loop gjennom alle årene
  region_indre[j, , 1:6] <- region_fangst[j, , 1:6] * regionliste$Kontrast
  for (k in 1:antall_kyst) { # loop gjennom de ytre kyst regionene
    for (i in 1:6) { # loop gjennom de tre størrelsesklassene fordelt på vekt og antall
      region_leggtil[j, , i] <- region_leggtil[j, , i] +
        (region_fangst[j, k, i] * kyst_fordeling[1:antall_regioner, k + 1])
    }
  }
}

region_fangst <- region_leggtil + region_indre

sjofordeling <- matrix(0, nrow = antall_regioner * antall_aar, ncol = 8,
                       dimnames = list(NULL, c("Region", "Aar", "Vekt_u3", "Vekt_37",
                                               "Vekt_o7", "Ant_u3", "Ant_37", "Ant_o7")))  %>%
  as_tibble()
l <- 1
for (i in 1:antall_aar) {
  sjofordeling$Region[l:(l + antall_regioner - 1)] <- regionliste$Regioner
  sjofordeling$Aar[l:(l + antall_regioner - 1)] <- start_aar + i - 1
  sjofordeling[l:(l + antall_regioner - 1), 3:8] <- region_fangst[i, , 1:6]
  l <- l + antall_regioner
}

# lagre sjøfangstfordeling til fil
sjofordeling_filnavn  <- paste("results/sjofangstfordeling_", start_aar, "-", start_aar + antall_aar - 1, ".csv", sep = "")
export(sjofordeling, sjofordeling_filnavn, sep = ";", dec = ".", bom = TRUE)

#-----------------------------------------------------------------------------------------------------------------
# les inn tabell med tall fra gytebestandsimulering samt data for andre vassdrag
#-----------------------------------------------------------------------------------------------------------------

simul_kghunnlaks <- import(sim_kghunnlaks_filnavn, encoding = "UTF-8")
simul_kghunnlaks[is.na(simul_kghunnlaks)] <- 0
elvefangst_ssb <- import("data/ssb_elv_1993-.csv", encoding = "UTF-8")
elvefangst_ssb <- elvefangst_ssb %>% rename(Gjen_vekt_o7kg = Gjen_vekto7kg)

#-----------------------------------------------------------------------------------------------------------------
# gå gjennom alle vassdrag, hent inn data for hver enkelt elv og beregn innsig
#-----------------------------------------------------------------------------------------------------------------

for (i in 1:antall_elver) {

  elv_filnavn <- paste("data/vassdrag/", elveliste[i, "Filnavn"], ".csv", sep = "")
  elv_grunnlag <- import(elv_filnavn, encoding = "UTF-8")
  elv_grunnlag <- elv_grunnlag %>%
    rename(Gjen_vekt_o7kg = Gjen_vekto7kg)

  if (elveliste$GytingSim[i]) { # vassdrag med i gytebestandsimulering, hent data fra simulering og vassdragsfil

    for (j in 1:antall_aar) {
      df <- filter(simul_kghunnlaks, Aar == aar_liste[j], Vdrnr == elveliste$VdrNr[i])
      df2 <- filter(elv_grunnlag, Aar == aar_liste[j])
      df2[is.na(df2)] <- 0
      df3 <- filter(elvefangst_ssb, Aar == aar_liste[j])
      fangst_elv_ssb <- filter(elvefangst_ssb, Aar == aar_liste[j], VdrNr == elveliste$VdrNr[i])

      elv_inkluder_normal[j, i] <- df2$InklNormal
      elv_hunnproporsjon[j, i, 1] <- df$andel_hunn_u3
      elv_hunnproporsjon[j, i, 2] <- df$andel_hunn_37
      elv_hunnproporsjon[j, i, 3] <- df$andel_hunn_o7

      # kolonne 1-3 i matrisen er data for vekt, 4-6 antall, tall fra inputfil elv
      elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
      elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
      elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
      elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
      elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
      elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg

      # bruk simulert gyting hunn og andel hunn til å beregne gyting hann+hunn,
      # dersom simulering mangler, bruk langtidssnitt til å fylle inn manglende år
      if (df$simulert == 0) { # år med manglende simuleringsdata
        elvsimuldata <- filter(simul_kghunnlaks, Vdrnr == elveliste$VdrNr[i], simulert == 1)
        snitt_maaloppn <- mean(elvsimuldata$prosent_maaloppnaaelse) / 100
        sum_gyting <- sum(elvsimuldata$gyting_hunn_u3, elvsimuldata$gyting_hunn_37, elvsimuldata$gyting_hunn_o7)
        snitt_andel_u3 <- sum(elvsimuldata$gyting_hunn_u3) / sum_gyting
        snitt_andel_37 <- sum(elvsimuldata$gyting_hunn_37) / sum_gyting
        snitt_andel_o7 <- sum(elvsimuldata$gyting_hunn_o7) / sum_gyting
        snitt_gytebestand <- elveliste$GBM[i] * snitt_maaloppn
        df$gyting_hunn_u3 <- snitt_gytebestand * snitt_andel_u3
        df$gyting_hunn_37 <- snitt_gytebestand * snitt_andel_37
        df$gyting_hunn_o7 <- snitt_gytebestand * snitt_andel_o7
        elv_simulert[j, i] <- 0
      } else {
        elv_simulert[j, i] <- 1
      }

      elv_gyting[j, i, 1] <- df$gyting_hunn_u3 / df$andel_hunn_u3
      elv_gyting[j, i, 2] <- df$gyting_hunn_37 / df$andel_hunn_37
      elv_gyting[j, i, 3] <- df$gyting_hunn_o7 / df$andel_hunn_o7

      # beregn snittvekt for vassdraget, bruk fangst i år, fangst i vassdraget eller nasjonalt snitt
      if ((df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg >= 4) && (df2$Laks_vekt_u3kg + df2$Gjen_vekt_u3kg > 0)) {
        snittvekt_u3 <- (df2$Laks_vekt_u3kg + df2$Gjen_vekt_u3kg) /
          (df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_u3kg, na.rm = TRUE) >= 4) {
        snittvekt_u3 <- (sum(elv_grunnlag$Laks_vekt_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_u3kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_u3kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_u3 <- sum(df3$Laks_vekt_u3kg, na.rm = TRUE) /
          sum(df3$Laks_ant_u3kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 4 laks totalt
      }
      if ((df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg >= 4) && (df2$Laks_vekt_o3u7kg + df2$Gjen_vekt_o3u7kg > 0)) {
        snittvekt_37 <- (df2$Laks_vekt_o3u7kg + df2$Gjen_vekt_o3u7kg) /
          (df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o3u7kg, na.rm = TRUE) >= 4) {
        snittvekt_37 <- (sum(elv_grunnlag$Laks_vekt_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_o3u7kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o3u7kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_37 <- sum(df3$Laks_vekt_o3u7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_o3u7kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 4 laks totalt
      }
      if ((df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg >= 4) && (df2$Laks_vekt_o7kg + df2$Gjen_vekt_o7kg > 0)) {
        snittvekt_o7 <- (df2$Laks_vekt_o7kg + df2$Gjen_vekt_o7kg) /
          (df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o7kg, na.rm = TRUE) >= 4) {
        snittvekt_o7 <- (sum(elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_o7kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o7kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_o7 <- sum(df3$Laks_vekt_o7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_o7kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 4 laks
      }

      elv_gyting[j, i, 4] <- if (snittvekt_u3 > 0) (elv_gyting[j, i, 1] / snittvekt_u3) else 0
      elv_gyting[j, i, 5] <- if (snittvekt_37 > 0) (elv_gyting[j, i, 2] / snittvekt_37) else 0
      elv_gyting[j, i, 6] <- if (snittvekt_o7 > 0) (elv_gyting[j, i, 3] / snittvekt_o7) else 0

      elv_snittvekt[j, i, 1] <- snittvekt_u3
      elv_snittvekt[j, i, 2] <- snittvekt_37
      elv_snittvekt[j, i, 3] <- snittvekt_o7

      elv_innsig[j, i, ] <- elv_fangst[j, i, ] + elv_gyting[j, i, ]
      andel_over15kg[j, i] <- pmax(0, pmin(1, 1 - ((-0.843 * snittvekt_u3) + 1.794)))
      elv_innsigvekt_over15kg[j, i] <- elv_innsig[j, i, 1] * andel_over15kg[j, i]
    }
  } else { # elv uten simulering, hent data fra elvedatafiler
    for (j in 1:antall_aar) {
      df <- filter(elv_grunnlag, Aar == aar_liste[j])
      elv_andelhunn[j, i] <- df$Andel_hunn
      elv_inkluder_normal[j, i] <- 0 # vassdrag som ikke simuleres brukes ikke til normalbeskatning
      df2 <- filter(elvefangst_ssb, VdrNr == elveliste$VdrNr[i], Aar == aar_liste[j])
      df3 <- filter(elvefangst_ssb, Aar == aar_liste[j])

      elv_simulert[j, i] <- 0

      # beregn snittvekt for vassdraget, bruk fangst i år, fangst i vassdraget eller nasjonalt snitt
      if ((df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg >= 4) && (df2$Laks_vekt_u3kg + df2$Gjen_vekt_u3kg > 0)) {
        snittvekt_u3 <- (df2$Laks_vekt_u3kg + df2$Gjen_vekt_u3kg) /
          (df2$Laks_ant_u3kg + df2$Gjen_ant_u3kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_u3kg, na.rm = TRUE) >= 4) {
        snittvekt_u3 <- (sum(elv_grunnlag$Laks_vekt_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_u3kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_u3kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_u3kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_u3 <- sum(df3$Laks_vekt_u3kg, na.rm = TRUE) /
          sum(df3$Laks_ant_u3kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 5 laks totalt
      }
      if ((df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg >= 4) && (df2$Laks_vekt_o3u7kg + df2$Gjen_vekt_o3u7kg > 0)) {
        snittvekt_37 <- (df2$Laks_vekt_o3u7kg + df2$Gjen_vekt_o3u7kg) /
          (df2$Laks_ant_o3u7kg + df2$Gjen_ant_o3u7kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o3u7kg, na.rm = TRUE) >= 4) {
        snittvekt_37 <- (sum(elv_grunnlag$Laks_vekt_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_o3u7kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_o3u7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o3u7kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_37 <- sum(df3$Laks_vekt_o3u7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_o3u7kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 5 laks totalt
      }
      if ((df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg >= 4) && (df2$Laks_vekt_o7kg + df2$Gjen_vekt_o7kg > 0)) {
        snittvekt_o7 <- (df2$Laks_vekt_o7kg + df2$Gjen_vekt_o7kg) /
          (df2$Laks_ant_o7kg + df2$Gjen_ant_o7kg) # fangst i år, bruk denne
      } else if (sum(elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o7kg, na.rm = TRUE) >= 4) {
        snittvekt_o7 <- (sum(elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_vekt_o7kg, na.rm = TRUE)) /
          (sum(elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) + sum(elv_grunnlag$Gjen_ant_o7kg, na.rm = TRUE)) # fangst i vassdraget, bruk denne
      } else {
        snittvekt_o7 <- sum(df3$Laks_vekt_o7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_o7kg, na.rm = TRUE) # nasjonalt snitt hvis færre enn 5 laks
      }

      # sett andel av ulike størrelsesgrupper, basert på fangst i vassdraget hvis den finnes, ellers nasjonalt snitt
      if (sum(elv_grunnlag$Laks_ant_u3kg, elv_grunnlag$Laks_ant_o3u7kg, elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) > 15) {
        andel_ant_u3 <- sum(elv_grunnlag$Laks_ant_u3kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_ant_u3kg, elv_grunnlag$Laks_ant_o3u7kg, elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE)
        andel_ant_37 <- sum(elv_grunnlag$Laks_ant_o3u7kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_ant_u3kg, elv_grunnlag$Laks_ant_o3u7kg, elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE)
        andel_ant_o7 <- sum(elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_ant_u3kg, elv_grunnlag$Laks_ant_o3u7kg, elv_grunnlag$Laks_ant_o7kg, na.rm = TRUE)
        andel_vekt_u3 <- sum(elv_grunnlag$Laks_vekt_u3kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_vekt_u3kg, elv_grunnlag$Laks_vekt_o3u7kg, elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE)
        andel_vekt_37 <- sum(elv_grunnlag$Laks_vekt_o3u7kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_vekt_u3kg, elv_grunnlag$Laks_vekt_o3u7kg, elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE)
        andel_vekt_o7 <- sum(elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE) /
          sum(elv_grunnlag$Laks_vekt_u3kg, elv_grunnlag$Laks_vekt_o3u7kg, elv_grunnlag$Laks_vekt_o7kg, na.rm = TRUE)
      } else {
        andel_ant_u3 <- sum(df3$Laks_ant_u3kg, na.rm = TRUE) /
          sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg, na.rm = TRUE)
        andel_ant_37 <- sum(df3$Laks_ant_o3u7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg, na.rm = TRUE)
        andel_ant_o7 <- sum(df3$Laks_ant_o7kg, na.rm = TRUE) /
          sum(df3$Laks_ant_u3kg, df3$Laks_ant_o3u7kg, df3$Laks_ant_o7kg, na.rm = TRUE)
        andel_vekt_u3 <- sum(df3$Laks_vekt_u3kg, na.rm = TRUE) /
          sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg, na.rm = TRUE)
        andel_vekt_37 <- sum(df3$Laks_vekt_o3u7kg, na.rm = TRUE) /
          sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg, na.rm = TRUE)
        andel_vekt_o7 <- sum(df3$Laks_vekt_o7kg, na.rm = TRUE) /
          sum(df3$Laks_vekt_u3kg, df3$Laks_vekt_o3u7kg, df3$Laks_vekt_o7kg, na.rm = TRUE)
      }

      if (!is.na(df$Maloppnaelse)) { # vassdrag med GBM og estimert måloppnåelse

        elv_gyting[j, i, 1] <- (elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_u3) / elv_andelhunn[j, i]
        elv_gyting[j, i, 2] <- (elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_37) / elv_andelhunn[j, i]
        elv_gyting[j, i, 3] <- (elveliste$GBM[i] * df$Maloppnaelse * andel_vekt_o7) / elv_andelhunn[j, i]
        elv_gyting[j, i, 4] <- elv_gyting[j, i, 1] / snittvekt_u3
        elv_gyting[j, i, 5] <- elv_gyting[j, i, 2] / snittvekt_37
        elv_gyting[j, i, 6] <- elv_gyting[j, i, 3] / snittvekt_o7

        if ((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) {
          elv_fangst[j, i, ] <- 0
        } else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }

      } else if (!is.na(df$Gytebestand)) { # vassdrag med estimat på gytebestand

        # bruk snittverdier for størrelsesfordeling fra nasjonal fangst
        elv_gyting[j, i, 1] <- df$Gytebestand * andel_ant_u3 * snittvekt_u3
        elv_gyting[j, i, 2] <- df$Gytebestand * andel_ant_37 * snittvekt_37
        elv_gyting[j, i, 3] <- df$Gytebestand * andel_ant_o7 * snittvekt_o7
        elv_gyting[j, i, 4] <- df$Gytebestand * andel_ant_u3
        elv_gyting[j, i, 5] <- df$Gytebestand * andel_ant_37
        elv_gyting[j, i, 6] <- df$Gytebestand * andel_ant_o7

        if ((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) {
          elv_fangst[j, i, ] <- 0
        } else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }
      } else if (!is.na(df$Elvinnsig)) { # vassdrag med telling av oppvandrende laks, for eksempel video
        # telletall i antall laks, så beregn gyting basert på snittvekter
        # bruk snittverdier for størrelsesfordeling fra nasjonal fangst
        elv_gyting[j, i, 1] <- df$Elvinnsig * andel_ant_u3 * snittvekt_u3
        elv_gyting[j, i, 2] <- df$Elvinnsig * andel_ant_37 * snittvekt_37
        elv_gyting[j, i, 3] <- df$Elvinnsig * andel_ant_o7 * snittvekt_o7
        elv_gyting[j, i, 4] <- df$Elvinnsig * andel_ant_u3
        elv_gyting[j, i, 5] <- df$Elvinnsig * andel_ant_37
        elv_gyting[j, i, 6] <- df$Elvinnsig * andel_ant_o7

        if ((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) == 0) {
          elv_fangst[j, i, ] <- 0
        } else {
          elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
          elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
          elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
          elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
          elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
          elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg
        }
      } else if ((df2$Laks_vekt_u3kg + df2$Laks_vekt_o3u7kg + df2$Laks_vekt_o7kg) > 0) { # vassdrag med fangst
        elv_fangst[j, i, 1] <- df2$Laks_vekt_u3kg
        elv_fangst[j, i, 2] <- df2$Laks_vekt_o3u7kg
        elv_fangst[j, i, 3] <- df2$Laks_vekt_o7kg
        elv_fangst[j, i, 4] <- df2$Laks_ant_u3kg
        elv_fangst[j, i, 5] <- df2$Laks_ant_o3u7kg
        elv_fangst[j, i, 6] <- df2$Laks_ant_o7kg

        # bruk estimat på fangstrate fra filen hvis det finnes, dersom ikke bruk fangstrate 0.5
        if (is.na(df$Fangstrate)) {
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

      elv_snittvekt[j, i, 1] <- snittvekt_u3
      elv_snittvekt[j, i, 2] <- snittvekt_37
      elv_snittvekt[j, i, 3] <- snittvekt_o7

      elv_innsig[j, i, ] <- elv_fangst[j, i, ] + elv_gyting[j, i, ]
      andel_over15kg[j, i] <- pmax(0, pmin(1, 1 - ((-0.843 * snittvekt_u3) + 1.794)))
      elv_innsigvekt_over15kg[j, i] <- elv_innsig[j, i, 1] * andel_over15kg[j, i]
    } # slutt for-loop over år
  } # slutt if, ferdig beregning av elvinnsig
} # slutt for-loop over elver

#-----------------------------------------------------------------------------------------------------------------
# initier data.frame for samlede resultat og begynn å sette inn variable med resultat
#-----------------------------------------------------------------------------------------------------------------

resultat_fordeling <- matrix(0, nrow = antall_elver * antall_aar, ncol = 75,
                             dimnames = list(NULL, c("VdrNr", "Vassdrag", "Aar", "RegionNr", "Region", "Gyt_vekt_u3", #1-6
                               "Gyt_vekt_37", "Gyt_vekt_o7", "Gyt_ant_u3", "Gyt_ant_37", "Gyt_ant_o7", #7-11
                               "Fangst_elv_vekt_u3", "Fangst_elv_vekt_37", "Fangst_elv_vekt_o7", #12-14
                               "Fangst_elv_ant_u3", "Fangst_elv_ant_37", "Fangst_elv_ant_o7", #15-17
                               "Innsig_elv_vekt_u3", "Innsig_elv_vekt_37", "Innsig_elv_vekt_o7", #18-20
                               "Innsig_elv_ant_u3", "Innsig_elv_ant_37", "Innsig_elv_ant_o7", #21-23
                               "Andel_region_vekt_u3", "Andel_region_vekt_37", "Andel_region_vekt_o7", #24-26
                               "Andel_region_ant_u3", "Andel_region_ant_37", "Andel_region_ant_o7", #27-29
                               "Sjofangst_vekt_u3", "Sjofangst_vekt_37", "Sjofangst_vekt_o7", #30-32
                               "Sjofangst_ant_u3", "Sjofangst_ant_37", "Sjofangst_ant_o7", "Innsig_sjo_vekt_u3", #33-36
                               "Innsig_sjo_vekt_37", "Innsig_sjo_vekt_o7", "Innsig_sjo_ant_u3", #37-39
                               "Innsig_sjo_ant_37", "Innsig_sjo_ant_o7", "Innsig_total_hunn", #40-42
                               "Andel_hunn_u3", "Andel_hunn_37", "Andel_hunn_o7", "Overbeskatning", #43-46
                               "MaxSustExp", "Besk_vekt_hunn", "Besk_tot_ant", "GBM", "Gyting_hunn", #47-51
                               "Hostbart_overskudd", "Hostbart_prosent", "Sannsyn_GBM", "ProsOppnaad", #52-55
                               "ProsOppnaadTrunk", "SannsynligSiste4", "BeskatningsRegion", "GytingSimulert", #56-59
                               "NormalOverskudd", "NormaltHostbart_prosent", "Innsig_sjo_ant_1SW", #60-62
                               "Innsig_sjo_ant_2SW", "Innsig_sjo_ant_MSW", "Andel_1SW", "Smoltalder", #63-66
                               "Sjoalder", "ForeldreMaal", "Inkluder_til_Norm", "Delbestand", #67-70
                               "elveinnsig_u3kg_over1.5kg", "andel_over_1.5kg", "snittvekt_u3", #71-73
                               "snittvekt_37", "snittvekt_o7" #74-75
                             ))) %>% as_tibble()
i <- 1
for (j in 1:antall_aar) {
  l <- antall_elver + i - 1
  resultat_fordeling$VdrNr[i:l] <- elveliste$VdrNr
  resultat_fordeling$Vassdrag[i:l] <- elveliste$Vassdrag
  resultat_fordeling$Aar[i:l] <- aar_liste[j]
  resultat_fordeling$RegionNr[i:l] <- elveliste$RegionNr
  resultat_fordeling$Region[i:l] <- elveliste$RegionNavn
  resultat_fordeling$BeskatningsRegion[i:l] <- elveliste$Normalbeskatningsregion
  resultat_fordeling$GytingSimulert[i:l] <- elv_simulert[j, ]
  resultat_fordeling$Delbestand[i:l] <- elveliste$Delbestand
  resultat_fordeling$Inkluder_til_Norm[i:l] <- elv_inkluder_normal[j, ]

  resultat_fordeling[i:l, 6:11] <- elv_gyting[j, , 1:6] # Gyt_vekt_u3 - Gyt_ant_o7
  resultat_fordeling[i:l, 12:17] <- elv_fangst[j, , 1:6] # Fangst_elv_vekt_u3 - Fangst_elv_ant_o7
  resultat_fordeling[i:l, 18:23] <- elv_innsig[j, , 1:6] # Innsig_elv

  resultat_fordeling$snittvekt_u3[i:l] <- elv_snittvekt[j, , 1]
  resultat_fordeling$snittvekt_37[i:l] <- elv_snittvekt[j, , 2]
  resultat_fordeling$snittvekt_o7[i:l] <- elv_snittvekt[j, , 3]

  resultat_fordeling$elveinnsig_u3kg_over1.5kg[i:l] <- elv_innsigvekt_over15kg[j, ]
  resultat_fordeling$andel_over_1.5kg[i:l] <- andel_over15kg[j, ]

  i <- i + antall_elver
}

#-----------------------------------------------------------------------------------------------------------------
# beregn innsig til elv og kyst for hvert vassdrag
#-----------------------------------------------------------------------------------------------------------------

# beregn samlet elveinnsig pr region, bruk det til å beregne hva slags fangstandel hvert vassdrag utgjør
ddf <- resultat_fordeling %>%
  select(Delbestand, Aar, Region, elveinnsig_u3kg_over1.5kg, Innsig_elv_vekt_37, Innsig_elv_vekt_o7,
         Innsig_elv_ant_u3, Innsig_elv_ant_37, Innsig_elv_ant_o7)  %>%
  filter(Delbestand == 0) %>% # kun hovedvassdrag
  group_by(Aar, Region) %>%
  summarize(across(elveinnsig_u3kg_over1.5kg:Innsig_elv_ant_o7, sum))

l <- 1 # indeks for vassdrag i resultat_fordeling

for (j in 1:antall_aar) {
  for (i in 1:antall_elver) {
    k <- filter(ddf, Region == elveliste$RegionNavn[i] & Aar == aar_liste[j])

    # andel vassdrag utgjør i region (24:29 = Andel_region_vekt_u3:Andel_region_ant_o7)
    resultat_fordeling[l, 24:29] <- ifelse(k[3:8] == 0, 0, resultat_fordeling[l, 18:23] / k[3:8])
    # sett Andel_region_vekt_u3 til andel basert på vekt over 1.5 kg
    resultat_fordeling$Andel_region_vekt_u3[l] <- ifelse(k$elveinnsig_u3kg_over1.5kg == 0, 0,
                                                         resultat_fordeling$elveinnsig_u3kg_over1.5kg[l] /
                                                           k$elveinnsig_u3kg_over1.5kg)
    resultat_fordeling$Andel_region_ant_u3[l] <- resultat_fordeling$Andel_region_vekt_u3[l]
    # sjøfangst (30:35 = Sjofangst_vekt_u3:Sjofangst_ant_o7)
    resultat_fordeling[l, 30:35] <- resultat_fordeling[l, 24:29] *
      region_fangst[j, which(regionliste$RegionNr == elveliste$RegionNr[i]), 1:6]
    # sjøinnsig (36:41 = Innsig_sjo_vekt_u3:Innsig_sjo_ant_o7)
    resultat_fordeling[l, 36:41] <- resultat_fordeling[l, 30:35] + resultat_fordeling[l, 18:23]

    # totalt innsig fordelt på estimert sjøalder
    if (resultat_fordeling$Aar[l] < min(region_andel1SW$InnsigsAar)) {
      andel_1sw <- 1
    } else {
      andel_1sw_rad <- region_andel1SW %>%
        filter(InnsigsAar == resultat_fordeling$Aar[l], Region == resultat_fordeling$BeskatningsRegion[l])
      andel_1sw <- andel_1sw_rad$Andel1SW
    }

    resultat_fordeling$Innsig_sjo_ant_1SW[l] <- resultat_fordeling$Innsig_sjo_ant_u3[l] * andel_1sw
    resultat_fordeling$Innsig_sjo_ant_2SW[l] <- resultat_fordeling$Innsig_sjo_ant_u3[l] * (1 - andel_1sw) +
      resultat_fordeling$Innsig_sjo_ant_37[l]
    resultat_fordeling$Innsig_sjo_ant_MSW[l] <- resultat_fordeling$Innsig_sjo_ant_o7[l]
    resultat_fordeling$Andel_1SW[l] <- andel_1sw

    # totalt innsig hunn, vekt
    if (elveliste$GytingSim[i]) {
      m <- filter(simul_kghunnlaks, Vdrnr == elveliste$VdrNr[i] & Aar == aar_liste[j])
      resultat_fordeling$Andel_hunn_u3[l] <- m$andel_hunn_u3
      resultat_fordeling$Andel_hunn_37[l] <- m$andel_hunn_37
      resultat_fordeling$Andel_hunn_o7[l] <- m$andel_hunn_o7
      resultat_fordeling$Innsig_total_hunn[l] <- resultat_fordeling$Innsig_sjo_vekt_u3[l] * m$andel_hunn_u3 +
        resultat_fordeling$Innsig_sjo_vekt_37[l] * m$andel_hunn_37 +
        resultat_fordeling$Innsig_sjo_vekt_o7[l] * m$andel_hunn_o7
      samlet_gytebestand_hunn <- m$gyting_hunn_u3 + m$gyting_hunn_37 + m$gyting_hunn_o7

      # legg inn tall på GBM måloppnåelse for simulerte vassdrag
      resultat_fordeling$Sannsyn_GBM[l] <- m$prosent_sannsynlighet_gbm / 100
      resultat_fordeling$ProsOppnaad[l] <- m$prosent_maaloppnaaelse_utrunk / 100
      resultat_fordeling$ProsOppnaadTrunk[l] <- m$prosent_maaloppnaaelse / 100
      resultat_fordeling$SannsynligSiste4[l] <- m$prosent_sanns_siste_fire / 100
    } else {
      resultat_fordeling$Innsig_total_hunn[l] <- sum(resultat_fordeling[l, 36:38]) * elv_andelhunn[j, i]
      samlet_gytebestand_hunn <- sum(resultat_fordeling[l, 6:8]) * elv_andelhunn[j, i]
    }

    # beregn overbeskatning
    if (!is.na(elveliste$GBM[i])) {
      if (samlet_gytebestand_hunn > elveliste$GBM[i])
        resultat_fordeling$Overbeskatning[l] <- 0 # gytebestand nådd, ingen overbeskatning
      else if (resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i]) {
        # innsig større enn gytebestand vil si noe beskattbart overskudd
        # bare beskatning nedenfor GBM er overbeskatning
        resultat_fordeling$Overbeskatning[l] <- (elveliste$GBM[i] - samlet_gytebestand_hunn) / elveliste$GBM[i]
      } else { # intet beskattbart overskudd, alt fiske er overbeskatning
        if (elveliste$GytingSim[i])
          resultat_fordeling$Overbeskatning[l] <- (((resultat_fordeling$Fangst_elv_vekt_u3[l] +
                                                       resultat_fordeling$Sjofangst_vekt_u3[l]) * m$andel_hunn_u3) +
                                                     ((resultat_fordeling$Fangst_elv_vekt_37[l] +
                                                         resultat_fordeling$Sjofangst_vekt_37[l]) * m$andel_hunn_37) +
                                                     ((resultat_fordeling$Fangst_elv_vekt_o7[l] +
                                                         resultat_fordeling$Sjofangst_vekt_o7[l]) * m$andel_hunn_o7)) /
            elveliste$GBM[i]
        else
          resultat_fordeling$Overbeskatning[l] <- ((resultat_fordeling$Fangst_elv_vekt_u3[l] +
                                                      resultat_fordeling$Fangst_elv_vekt_37[l] +
                                                      resultat_fordeling$Fangst_elv_vekt_o7[l] +
                                                      resultat_fordeling$Sjofangst_vekt_u3[l] +
                                                      resultat_fordeling$Sjofangst_vekt_37[l] +
                                                      resultat_fordeling$Sjofangst_vekt_o7[l]) *
                                                     elv_andelhunn[j, i]) / elveliste$GBM[i]
      }
    }

    # beregn maksimal bærekraftig beskatning
    if (!is.na(elveliste$GBM[i])) {
      if (resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i])
        resultat_fordeling$MaxSustExp[l] <- (resultat_fordeling$Innsig_total_hunn[l] -
                                               elveliste$GBM[i]) / resultat_fordeling$Innsig_total_hunn[l]
      else
        resultat_fordeling$MaxSustExp[l] <- 0
    }

    # beregn beskatning vekt hunn
    resultat_fordeling$Besk_vekt_hunn[l] <- ((resultat_fordeling$Fangst_elv_vekt_u3[l] + resultat_fordeling$Sjofangst_vekt_u3[l]) * m$andel_hunn_u3 +
                                               (resultat_fordeling$Fangst_elv_vekt_37[l] + resultat_fordeling$Sjofangst_vekt_37[l]) * m$andel_hunn_37 +
                                               (resultat_fordeling$Fangst_elv_vekt_o7[l] + resultat_fordeling$Sjofangst_vekt_o7[l]) * m$andel_hunn_o7) /
      resultat_fordeling$Innsig_total_hunn[l]

    # beregn total beskatning (antall)
    resultat_fordeling$Besk_tot_ant[l] <- (resultat_fordeling$Fangst_elv_ant_u3[l] + resultat_fordeling$Fangst_elv_ant_37[l] + resultat_fordeling$Fangst_elv_ant_o7[l] +
                                             resultat_fordeling$Sjofangst_ant_u3[l] + resultat_fordeling$Sjofangst_ant_37[l] + resultat_fordeling$Sjofangst_ant_o7[l]) /
      (resultat_fordeling$Innsig_sjo_ant_u3[l] + resultat_fordeling$Innsig_sjo_ant_37[l] + resultat_fordeling$Innsig_sjo_ant_o7[l])
    # gyting hunn og høstbart overskudd
    resultat_fordeling$Gyting_hunn[l] <- samlet_gytebestand_hunn
    if (!is.na(elveliste$GBM[i])) {
      if (resultat_fordeling$Innsig_total_hunn[l] > elveliste$GBM[i]) {
        resultat_fordeling$Hostbart_overskudd[l] <- resultat_fordeling$Innsig_total_hunn[l] - elveliste$GBM[i]
        resultat_fordeling$Hostbart_prosent[l] <- resultat_fordeling$Hostbart_overskudd[l] /
          resultat_fordeling$Innsig_total_hunn[l]
      } else {
        resultat_fordeling$Hostbart_overskudd[l] <- 0
        resultat_fordeling$Hostbart_prosent[l] <- 0
      }
    } else {
      resultat_fordeling$Hostbart_overskudd[l] <- NA
      resultat_fordeling$Hostbart_prosent[l] <- NA
    }
    resultat_fordeling$GBM[l] <- elveliste$GBM[i]

    l <- l + 1
  }
}

normal_region <- distinct(elveliste, Normalbeskatningsregion)
antall_beskatreg <- length(normal_region$Normalbeskatningsregion)
normal_beskatning <- matrix(0, antall_beskatreg, antall_aar)

l <- 1 # indeks for vassdrag i resultat_fordeling

for (j in 1:antall_aar) { # lag liste over måloppnåelse til foreldrene til innsiget basert på smolt og sjøalder
  for (i in 1:antall_elver) {
    if (resultat_fordeling$GytingSimulert[l]) {
      resultat_fordeling$Smoltalder[l] <- elveliste$SmoltAge[i]
      resultat_fordeling$Sjoalder[l] <- elveliste$FemSeaAge[i]
      if (elveliste$FemSeaAge[i] == 1.5) {
        antall_aar_tilbake <- elveliste$SmoltAge[i] + 2
        if (j > antall_aar_tilbake && resultat_fordeling$GytingSimulert[l - (antall_aar_tilbake * antall_elver)] == 1 &&
              resultat_fordeling$GytingSimulert[l - ((antall_aar_tilbake - 1) * antall_elver)] == 1) {
          resultat_fordeling$ForeldreMaal[l] <-
            (resultat_fordeling$ProsOppnaadTrunk[l - (antall_aar_tilbake * antall_elver)] +
             resultat_fordeling$ProsOppnaadTrunk[l - ((antall_aar_tilbake - 1) * antall_elver)]) / 2
        } else {
          resultat_fordeling$ForeldreMaal[l] <- NA
          resultat_fordeling$Inkluder_til_Norm[l] <- 0
        }
      } else {
        antall_aar_tilbake <- elveliste$SmoltAge[i] + elveliste$FemSeaAge[i] + 1
        if (j > antall_aar_tilbake && resultat_fordeling$GytingSimulert[l - (antall_aar_tilbake * antall_elver)] == 1) {
          resultat_fordeling$ForeldreMaal[l] <- resultat_fordeling$ProsOppnaadTrunk[l - (antall_aar_tilbake * antall_elver)]
        } else {
          resultat_fordeling$ForeldreMaal[l] <- NA
          resultat_fordeling$Inkluder_til_Norm[l] <- 0
        }
      }
    }
    l <- l + 1
  }
}

vassdrag_normal <- matrix(0, nrow = antall_elver * antall_aar, ncol = 8,
                          dimnames = list(NULL, c("VdrNr", "Vassdrag", "Aar", "Omrade", "ForeldreMaal",
                                                  "Hostbar_prosent", "Normal_beskatning",
                                                  "Normal_hostbar_prosent")))  %>% as_tibble()

l <- 1 # indeks for vassdrag i resultat_fordeling
for (j in 1:antall_aar) { # beregn normal beskatning for vassdragene i de ulike beskatningsregionene
  for (i in 1:antall_beskatreg) {
    df <- filter(resultat_fordeling, Aar == aar_liste[j] & BeskatningsRegion == normal_region$Normalbeskatningsregion[i] & GytingSimulert == TRUE & ForeldreMaal >= 0.9 & Inkluder_til_Norm == 1)
    antall_inkludert <- nrow(df)
    k <- l + nrow(df) - 1
    if (antall_inkludert > 0) {
      vassdrag_normal$VdrNr[l:k] <- df$VdrNr
      vassdrag_normal$Vassdrag[l:k] <- df$Vassdrag
      vassdrag_normal$Aar[l:k] <- df$Aar
      vassdrag_normal$Omrade[l:k] <- df$BeskatningsRegion
      vassdrag_normal$ForeldreMaal[l:k] <- df$ForeldreMaal
      vassdrag_normal$Hostbar_prosent[l:k] <- df$Hostbart_prosent
      normal_beskatning[i, j] <- median(df$Hostbart_prosent)
      vassdrag_normal$Normal_beskatning[l:k] <- normal_beskatning[i, j]
      l <- l + antall_inkludert
    }
  }
}

vassdrag_normal$Normal_hostbar_prosent <- vassdrag_normal$Hostbar_prosent / vassdrag_normal$Normal_beskatning

vassdrag_normal <- vassdrag_normal[-c(l:nrow(vassdrag_normal)), ]
export(vassdrag_normal, "results/vassdrag_inkludert_normalbeskatning.csv", sep = ";", dec = ".", bom = TRUE, row.names = TRUE)
colnames(normal_beskatning) <- as.character(c(aar_liste))
rownames(normal_beskatning) <- c(normal_region$Normalbeskatningsregion)
write_excel_csv(data.frame(Omrade = row.names(normal_beskatning), normal_beskatning), "results/normal_beskatning_tabell.csv", delim = ";")

l <- 1 # indeks for vassdrag i resultat_fordeling
for (j in 1:antall_aar) {
  for (i in 1:antall_elver) {
    region_rad <- which(normal_region$Normalbeskatningsregion == resultat_fordeling$BeskatningsRegion[l])
    resultat_fordeling$NormalOverskudd[l] <- normal_beskatning[region_rad, j]
    if (!is.na(resultat_fordeling$Hostbart_prosent[l]) && normal_beskatning[region_rad, j] > 0) {
      resultat_fordeling$NormaltHostbart_prosent[l] <- resultat_fordeling$Hostbart_prosent[l] /
        normal_beskatning[region_rad, j]
    } else {
      resultat_fordeling$NormaltHostbart_prosent[l] <- NA
    }
    l <- l + 1
  }
}

resultat_filnavn <- paste("results/resultat_fordeling_", start_aar + antall_aar - 1, ".csv", sep = "")
export(resultat_fordeling, resultat_filnavn, sep = ";", dec = ".", bom = TRUE)
