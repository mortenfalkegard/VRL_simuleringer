#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet tar utgangspunkt i gytebestand, beskatning, elvefangst og sjøfangst. Sjøfangst fordeles på
# regionnivå og så videre på vassdragsnivå. Skriptet beregner så innsig i forskjellige områder. Estimatene
# gjøres på størrelsesgruppe og både biomasse og antall.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyr)
library(dplyr)
library(rio)
library(purrr)

options(scipen = 999)

#-----------------------------------------------------------------------------------------------------------------
# Variablene nedenfor definerer startår og antall år som skal estimeres. Skriptet er fleksibelt organisert
# rundt disse for å gi mulighet til å gjennomføre historiske sammenligninger, for eksempel før og etter
# viktige reguleringsendringer
#-----------------------------------------------------------------------------------------------------------------
start_aar <- 1993
slutt_aar <- 2024
antall_aar <- slutt_aar - start_aar + 1
aar_liste <- seq(start_aar, slutt_aar) # definerer en liste med årstall

#-----------------------------------------------------------------------------------------------------------------
# initialiser en liste over vassdrag som skal være med i innsigsfordelingen med noen bakgrunnstall
#-----------------------------------------------------------------------------------------------------------------
elveliste <- import("data/elveliste.csv", encoding = "UTF-8")
antall_elver <- nrow(elveliste)

# les inn andel 1SW i vektgruppe under 3 kg
# tallene brukes til å fordele innsiget på sjøaldersgrupper
region_andel_1sw <- import(
  "data/Prosent1SWblantLaksMindreEnn3kgInnsigsregioner.csv",
  encoding = "UTF-8"
)
region_andel_1sw <- region_andel_1sw %>%
  rename(Normalbeskatningsregion = Region)

#-----------------------------------------------------------------------------------------------------------------
# les inn tabell med tall fra gytebestandsimulering samt data for andre vassdrag
#-----------------------------------------------------------------------------------------------------------------
simul_kghunnlaks <- import("results/KgHunnlaks2024.csv", encoding = "UTF-8")
simul_kghunnlaks[is.na(simul_kghunnlaks)] <- 0
simul_kghunnlaks <- simul_kghunnlaks %>%
  filter(Aar >= start_aar & Aar <= slutt_aar) %>%
  rename(VdrNr = Vdrnr)

elvefangst_ssb <- import("data/ssb_elv_1993-.csv", encoding = "UTF-8")
elvefangst_ssb <- elvefangst_ssb %>%
  filter(Aar >= start_aar & Aar <= slutt_aar) %>%
  rename(Gjen_vekt_o7kg = Gjen_vekto7kg) %>%
  mutate(
    total_vekt = Laks_vekt_u3kg + Laks_vekt_o3u7kg + Laks_vekt_o7kg,
    total_ant = Laks_ant_u3kg + Laks_ant_o3u7kg + Laks_ant_o7kg
  ) %>%
  # sett regionnavn fra elveliste inn i elvefangst_ssb
  left_join(
    select(
      elveliste,
      VdrNr,
      Region = RegionNavn
    ),
    by = "VdrNr"
  )

#-----------------------------------------------------------------------------------------------------------------
# Samle relevante data og beregn årlige elveinnsig
#-----------------------------------------------------------------------------------------------------------------

# start med å lage lister over elver med og uten simulering
elver_med_simulering <- elveliste %>%
  filter(TypeVassdrag == 1) %>%
  pull(VdrNr)
elver_uten_simulering <- elveliste %>%
  filter(TypeVassdrag == 2) %>%
  pull(VdrNr)
elver_bare_ssb <- elveliste %>%
  filter(TypeVassdrag == 3) %>%
  pull(VdrNr)

# lag en regional matrise med størrelsesfordelinger og snittvekter basert på SSB-data
regionale_data_tot <- elvefangst_ssb %>%
  group_by(
    Region
  ) %>%
  summarise(
    # beregn total vekt og antall for hver region og hvert år
    total_vekt_tot = sum(Laks_vekt_u3kg + Laks_vekt_o3u7kg + Laks_vekt_o7kg, na.rm = TRUE),
    total_ant_tot = sum(Laks_ant_u3kg + Laks_ant_o3u7kg + Laks_ant_o7kg, na.rm = TRUE),

    # beregn vektprosent for hver størrelsesgruppe
    pros_vekt_u3_tot = sum(Laks_vekt_u3kg, na.rm = TRUE) / total_vekt_tot,
    pros_vekt_37_tot = sum(Laks_vekt_o3u7kg, na.rm = TRUE) / total_vekt_tot,
    pros_vekt_o7_tot = sum(Laks_vekt_o7kg, na.rm = TRUE) / total_vekt_tot,

    # beregn antallprosent for hver størrelsesgruppe
    pros_ant_u3_tot = sum(Laks_ant_u3kg, na.rm = TRUE) / total_ant_tot,
    pros_ant_37_tot = sum(Laks_ant_o3u7kg, na.rm = TRUE) / total_ant_tot,
    pros_ant_o7_tot = sum(Laks_ant_o7kg, na.rm = TRUE) / total_ant_tot,

    # inkluder gjennomsnittsvekt for hver størrelsesgruppe
    snitt_vekt_u3_tot = sum(Laks_vekt_u3kg, na.rm = TRUE) /
      sum(Laks_ant_u3kg, na.rm = TRUE),
    snitt_vekt_37_tot = sum(Laks_vekt_o3u7kg, na.rm = TRUE) /
      sum(Laks_ant_o3u7kg, na.rm = TRUE),
    snitt_vekt_o7_tot = sum(Laks_vekt_o7kg, na.rm = TRUE) /
      sum(Laks_ant_o7kg, na.rm = TRUE)
  ) %>%
  mutate(
    # mulig delt på null
    across(
      starts_with(c("pros_", "snitt_")),
      ~replace_na(., 0)
    )
  ) %>%
  ungroup()

# lag en regional matrise med årlige størrelsesfordelinger og snittvekter basert på SSB-data
regionale_data <- elvefangst_ssb %>%
  group_by(
    Region,
    Aar
  ) %>%
  summarize(
    # beregn total vekt og antall for hver region og hvert år
    total_vekt = sum(Laks_vekt_u3kg + Laks_vekt_o3u7kg + Laks_vekt_o7kg, na.rm = TRUE),
    total_ant = sum(Laks_ant_u3kg + Laks_ant_o3u7kg + Laks_ant_o7kg, na.rm = TRUE),
    vekt_u3 = sum(Laks_vekt_u3kg, na.rm = TRUE),
    vekt_37 = sum(Laks_vekt_o3u7kg, na.rm = TRUE),
    vekt_o7 = sum(Laks_vekt_o7kg, na.rm = TRUE),
    ant_u3 = sum(Laks_ant_u3kg, na.rm = TRUE),
    ant_37 = sum(Laks_ant_o3u7kg, na.rm = TRUE),
    ant_o7 = sum(Laks_ant_o7kg, na.rm = TRUE)
  ) %>%
  # koble til regionale_data_tot for å få data over hele tidsperioden som kan brukes til å beregne snittverdier
  # i regioner i år med lite fangst
  left_join(
    regionale_data_tot,
    by = "Region"
  ) %>%
  # beregn regiondata basert på en test om det er lite fangst i ett år eller ikke
  mutate(
    # vekt prosent
    pros_vekt_u3 = if_else(
      total_ant < 25,
      pros_vekt_u3_tot,
      vekt_u3 / total_vekt
    ),
    pros_vekt_37 = if_else(
      total_ant < 25,
      pros_vekt_37_tot,
      vekt_37 / total_vekt
    ),
    pros_vekt_o7 = if_else(
      total_ant < 25,
      pros_vekt_o7_tot,
      vekt_o7 / total_vekt
    ),

    # antall prosent
    pros_ant_u3 = if_else(
      total_ant < 25,
      pros_ant_u3_tot,
      ant_u3 / total_ant
    ),
    pros_ant_37 = if_else(
      total_ant < 25,
      pros_ant_37_tot,
      ant_37 / total_ant
    ),
    pros_ant_o7 = if_else(
      total_ant < 25,
      pros_ant_o7_tot,
      ant_o7 / total_ant
    ),

    # snittvekter
    snitt_vekt_u3 = if_else(
      ant_u3 < 5,
      snitt_vekt_u3_tot,
      vekt_u3 / ant_u3
    ),
    snitt_vekt_37 = if_else(
      ant_37 < 5,
      snitt_vekt_37_tot,
      vekt_37 / ant_37
    ),
    snitt_vekt_o7 = if_else(
      ant_o7 < 5,
      snitt_vekt_o7_tot,
      vekt_o7 / ant_o7
    )
  ) %>%
  # fjern dupliserte kolonner (etter join)
  select(-ends_with("_tot")) %>%
  # ta bort eventuelle NA-verdier
  mutate(
    across(
      starts_with(c("pros_", "snitt_")),
      ~replace_na(., 0)
    )
  ) %>%
  ungroup()

# lagre regionale størrelsesfordelinger og snittvekter til fil
export(
  regionale_data,
  sprintf("results/regionale_fordelinger_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

#-----------------------------------------------------------------------------------------------------------------
# Beregne elveinnsig i simulerte vassdrag
#-----------------------------------------------------------------------------------------------------------------
# MARK:Elveinnsig simulerte

# importer alle bakgrunnsdatafilene til de simulerte elvene til en felles variabel
sim_elv_data_imp <- elver_med_simulering %>%
  map_df(
    function(elv) {
      filbane  <- file.path(
        "data",
        "vassdrag",
        paste0(elveliste$Filnavn[elveliste$VdrNr == elv], ".csv")
      )
      tryCatch(
        {
          import(filbane, encoding = "UTF-8")
        },
        error = function(e) {
          warning(sprintf("Fant ikke vassdragsfil for elv %d: %s", elv, e$message))
        }
      )
    }
  )

# lagre regionale størrelsesfordelinger og snittvekter til fil
export(
  sim_elv_data_imp,
  sprintf("results/sim_elv_data_imp_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# gjør noen tilpasninger til grunnlagsdataene for hver enkelt elv. Ta bort år som er utenfor start- og sluttår
# og erstatt NA med 0. Lag også noen nye variabler for samlet elvefangst og villfiskandel, samt sett inn regionnavn
sim_elv_data <- sim_elv_data_imp %>%
  # ta bort rader med år utenfor start- og sluttår som ble definert i starten av skriptet
  filter(Aar >= start_aar & Aar <= slutt_aar) %>%
  # erstatt eventuelle NA med 0
  mutate(
    Vekt_MS_u3kg = replace_na(Vekt_MS_u3kg, 0),
    Vekt_MS_37kg = replace_na(Vekt_MS_37kg, 0),
    Vekt_MS_o7kg = replace_na(Vekt_MS_o7kg, 0),
    Ant_MS_u3kg = replace_na(Ant_MS_u3kg, 0),
    Ant_MS_37kg = replace_na(Ant_MS_37kg, 0),
    Ant_MS_o7kg = replace_na(Ant_MS_o7kg, 0),
    ProsOpp = replace_na(ProsOpp, 0)
  ) %>%
  # lag variabler for samlet elvefangst
  mutate(
    Vekt_u3kg_tot = Laks_vekt_u3kg + Vekt_MS_u3kg,
    Vekt_37kg_tot = Laks_vekt_o3u7kg + Vekt_MS_37kg,
    Vekt_o7kg_tot = Laks_vekt_o7kg + Vekt_MS_o7kg,
    Ant_u3kg_tot = Laks_ant_u3kg + Ant_MS_u3kg,
    Ant_37kg_tot = Laks_ant_o3u7kg + Ant_MS_37kg,
    Ant_o7kg_tot = Laks_ant_o7kg + Ant_MS_o7kg,
    Vekt_u3kg_vill = Vekt_u3kg_tot * (1 - ProsOpp / 100),
    Vekt_37kg_vill = Vekt_37kg_tot * (1 - ProsOpp / 100),
    Vekt_o7kg_vill = Vekt_o7kg_tot * (1 - ProsOpp / 100),
    Ant_u3kg_vill = Ant_u3kg_tot * (1 - ProsOpp / 100),
    Ant_37kg_vill = Ant_37kg_tot * (1 - ProsOpp / 100),
    Ant_o7kg_vill = Ant_o7kg_tot * (1 - ProsOpp / 100)
  ) %>%
  # endre variabelnavn fra vassdragsgrunnlagsfilene for å være konsistent med annen input
  rename(VdrNr = Vdrnr) %>%
  # feil variabelnavn i grunnlagsfilene, endre navn for å matche data fra simulering
  rename(Gjen_vekt_o7kg = Gjen_vekto7kg) %>%
  # koble til regionnavn
  left_join(
    select(
      elveliste,
      VdrNr,
      Region = RegionNavn
    ),
    by = "VdrNr"
  )

# gå gjennom alle vassdrag og år i sim_elv_data og lag estimat i år hvor det sannsynligvis har vært fisket
# men ikke levert fangststatistikk. For disse årene brukes langtidssnittet av simulerte år til å fylle inn manglende data
# for å gjøre dette, må vi først finne ut hvilke år som mangler simulering for hvert vassdrag
# og deretter beregne snittverdier for de simulerte årene
sim_elv_data <- sim_elv_data %>%
  group_by(VdrNr) %>%
  mutate(
    # regn ut langtids gjennomsnitt for hver elv i år hvor EstimerFangst ikke er 1
    avg_vekt_u3kg_vill = mean(Vekt_u3kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_vekt_37kg_vill = mean(Vekt_37kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_vekt_o7kg_vill = mean(Vekt_o7kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_ant_u3kg_vill = mean(Ant_u3kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_ant_37kg_vill = mean(Ant_37kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_ant_o7kg_vill = mean(Ant_o7kg_vill[EstimerFangst != 1], na.rm = TRUE),
    avg_total_vekt = avg_vekt_u3kg_vill + avg_vekt_37kg_vill + avg_vekt_o7kg_vill,
    avg_total_ant = avg_ant_u3kg_vill + avg_ant_37kg_vill + avg_ant_o7kg_vill,

    # gjennomsnittlig størrelsesfordeling i elva
    avg_andel_vekt_u3 = avg_vekt_u3kg_vill / avg_total_vekt,
    avg_andel_vekt_37 = avg_vekt_37kg_vill / avg_total_vekt,
    avg_andel_vekt_o7 = avg_vekt_o7kg_vill / avg_total_vekt,
    avg_andel_ant_u3 = avg_ant_u3kg_vill / avg_total_ant,
    avg_andel_ant_37 = avg_ant_37kg_vill / avg_total_ant,
    avg_andel_ant_o7 = avg_ant_o7kg_vill / avg_total_ant,

    # sett inn gjennomsnittene i årene hvor EstimerFangst er 1
    Vekt_u3kg_vill = if_else(
      EstimerFangst == 1,
      avg_vekt_u3kg_vill,
      Vekt_u3kg_vill
    ),
    Vekt_37kg_vill = if_else(
      EstimerFangst == 1,
      avg_vekt_37kg_vill,
      Vekt_37kg_vill
    ),
    Vekt_o7kg_vill = if_else(
      EstimerFangst == 1,
      avg_vekt_o7kg_vill,
      Vekt_o7kg_vill
    ),
    Vekt_total_vill = Vekt_u3kg_vill + Vekt_37kg_vill + Vekt_o7kg_vill,

    Ant_u3kg_vill = if_else(
      EstimerFangst == 1,
      avg_ant_u3kg_vill,
      Ant_u3kg_vill
    ),
    Ant_37kg_vill = if_else(
      EstimerFangst == 1,
      avg_ant_37kg_vill,
      Ant_37kg_vill
    ),
    Ant_o7kg_vill = if_else(
      EstimerFangst == 1,
      avg_ant_o7kg_vill,
      Ant_o7kg_vill
    ),
    Ant_total_vill = Ant_u3kg_vill + Ant_37kg_vill + Ant_o7kg_vill,

    # lag variabler for fangst av avlivet (inkl de med estimering) + gjenutsatt
    Vekt_u3_vill_uts = Vekt_u3kg_vill + (Gjen_vekt_u3kg * (1 - ProsOpp / 100)),
    Vekt_37_vill_uts = Vekt_37kg_vill + (Gjen_vekt_o3u7kg * (1 - ProsOpp / 100)),
    Vekt_o7_vill_uts = Vekt_o7kg_vill + (Gjen_vekt_o7kg * (1 - ProsOpp / 100)),
    Vekt_total_vill_uts = Vekt_u3_vill_uts + Vekt_37_vill_uts + Vekt_o7_vill_uts,
    Ant_u3_vill_uts = Ant_u3kg_vill + (Gjen_ant_u3kg * (1 - ProsOpp / 100)),
    Ant_37_vill_uts = Ant_37kg_vill + (Gjen_ant_o3u7kg * (1 - ProsOpp / 100)),
    Ant_o7_vill_uts = Ant_o7kg_vill + (Gjen_ant_o7kg * (1 - ProsOpp / 100)),
    Ant_total_vill_uts = Ant_u3_vill_uts + Ant_37_vill_uts + Ant_o7_vill_uts,

    # beregn total vekt og antall for hver region og hvert år
    total_vekt = Vekt_u3kg_vill + Vekt_37kg_vill + Vekt_o7kg_vill,
    total_ant = Ant_u3kg_vill + Ant_37kg_vill + Ant_o7kg_vill,
    total_vekt_vill_uts = Vekt_u3_vill_uts + Vekt_37_vill_uts + Vekt_o7_vill_uts,
    total_ant_vill_uts = Ant_u3_vill_uts + Ant_37_vill_uts + Ant_o7_vill_uts
  ) %>%
  ungroup()

# koble på regionale snittvekter fra regionale_data
# disse brukes blant annet til å estimere biomasse i elvene i de årene det er telling og
# til å estimere i andre vassdrag og år hvor det skal estimeres fangst
sim_elv_data <- sim_elv_data %>%
  # lag en kobling til regionale_data for å hente inn regionale snittvekter
  left_join(
    regionale_data %>%
      select(
        Region,
        Aar,
        snitt_vekt_u3,
        snitt_vekt_37,
        snitt_vekt_o7
      ),
    by = c(
      "Region",
      "Aar"
    )
  )

# gå gjennom alle vassdrag og år i sim_elv_data og beregn elvespesifikk gjennomsnittsvekt for hver av de tre størrelsesgruppene
# hvis fangsten i et år i en størrelsesgruppe er mindre enn eller lik 4, brukes regionale gjennomsnittsvekter
sim_elv_data <- sim_elv_data %>%
  mutate(
    # beregn elvespesifikk snittvekt når fangst >= 4
    elv_snittvekt_u3 = case_when(
      Ant_u3kg_vill >= 4 & Vekt_u3kg_vill > 0 ~ Vekt_u3kg_vill / Ant_u3kg_vill,
      TRUE ~ snitt_vekt_u3
    ),
    elv_snittvekt_37 = case_when(
      Ant_37kg_vill >= 4 & Vekt_37kg_vill > 0 ~ Vekt_37kg_vill / Ant_37kg_vill,
      TRUE ~ snitt_vekt_37
    ),
    elv_snittvekt_o7 = case_when(
      Ant_o7kg_vill >= 4 & Vekt_o7kg_vill > 0 ~ Vekt_o7kg_vill / Ant_o7kg_vill,
      TRUE ~ snitt_vekt_o7
    )
  )

# beregn andel av de ulike størrelsesgruppene i elven
# hvis total_ant_vill_uts < 20, bruk gjennomsnittsverdier i stedet
sim_elv_data <- sim_elv_data %>%
  mutate(
    pros_vekt_u3 = if_else(
      total_ant_vill_uts >= 20,
      Vekt_u3_vill_uts / total_vekt_vill_uts,
      avg_andel_vekt_u3
    ),
    pros_vekt_37 = if_else(
      total_ant_vill_uts >= 20,
      Vekt_37_vill_uts / total_vekt_vill_uts,
      avg_andel_vekt_37
    ),
    pros_vekt_o7 = if_else(
      total_ant_vill_uts >= 20,
      Vekt_o7_vill_uts / total_vekt_vill_uts,
      avg_andel_vekt_o7
    ),
    pros_ant_u3 = if_else(
      total_ant_vill_uts >= 20,
      Ant_u3_vill_uts / total_ant_vill_uts,
      avg_andel_ant_u3
    ),
    pros_ant_37 = if_else(
      total_ant_vill_uts >= 20,
      Ant_37_vill_uts / total_ant_vill_uts,
      avg_andel_ant_37
    ),
    pros_ant_o7 = if_else(
      total_ant_vill_uts >= 20,
      Ant_o7_vill_uts / total_ant_vill_uts,
      avg_andel_ant_o7
    ),

    # beregn andel smålaks over 1.5 kg i bestanden
    Andel_over_1.5kg = pmax(0, pmin(1, 1 - ((-0.843 * elv_snittvekt_u3) + 1.794)))
  )

# lagre simulerte elvefangstdata til fil
export(
  sim_elv_data,
  sprintf("results/sim_elv_data_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# gå gjennom alle vassdrag og år i simul_kghunnlaks og lag estimat i år som mangler simulering (simulert = 0)
# for disse årene brukes snittet av de 4 nærmeste simulerte årene til å fylle inn manglende data
# for å gjøre dette, må vi først finne ut hvilke år som mangler simulering for hvert vassdrag
# og deretter identifisere de 4 nærmeste årene og beregne snittverdier for disse

simul_kghunnlaks <- simul_kghunnlaks %>%
  left_join(
    select(
      sim_elv_data,
      VdrNr,
      Aar,
      Simulering
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  group_by(VdrNr) %>%
  mutate(
    # lag en oppslagstabell for alle år med simulering
    sim_aar = list(Aar[Simulering == 1])
  ) %>%
  # behandle hver rad i datasettet individuelt for å finne de 4 nærmeste årene med data
  rowwise() %>%
  mutate(
    # identifiser de 4 nærmeste årene med data for hvert år som må estimeres
    naermeste_4_aar = if (Simulering == 0) {
      sim_aar_vektor <- unlist(sim_aar)
      if (length(sim_aar_vektor) > 0) {
        # regn ut avstanden fra hvert år til de simulerte årene
        aar_avstander <- abs(Aar - sim_aar_vektor)
        # hent indeksen til de 4 nærmeste årene (eller færre dersom det bare eksisterer færre enn 4)
        naermeste_indekser <- order(aar_avstander)[1:min(4, length(sim_aar_vektor))]
        # returner de 4 nærmeste årene
        list(sim_aar_vektor[naermeste_indekser])
      } else {
        list(integer(0))  # tom liste dersom det ikke er simulerte år
      }
    } else {
      list(integer(0))  # liste behøves ikke for de simulerte årene
    }
  ) %>%
  ungroup() %>%
  group_by(VdrNr) %>%
  mutate(
    # beregn gjennomsnittlig måloppnåelse og sannsynlighet for de 4 nærmeste årene
    avg_maaloppn = map_dbl(naermeste_4_aar, function(years) {
      if (length(years) > 0) {
        mean(prosent_maaloppnaaelse_utrunk[Aar %in% years], na.rm = TRUE) / 100
      } else {
        NA_real_
      }
    }),
    avg_sannsynl = map_dbl(naermeste_4_aar, function(years) {
      if (length(years) > 0) {
        mean(prosent_sannsynlighet_gbm[Aar %in% years], na.rm = TRUE) / 100
      } else {
        NA_real_
      }
    }),
    # beregn total gytebestand og andeler av de ulike størrelsesgruppene for 4 nærmeste år
    total_gyting_4_aar = map_dbl(naermeste_4_aar, function(years) {
      if (length(years) > 0) {
        sum(gyting_hunn_u3[Aar %in% years],
            gyting_hunn_37[Aar %in% years],
            gyting_hunn_o7[Aar %in% years],
            na.rm = TRUE)
      } else {
        NA_real_
      }
    }),

    avg_andel_u3 = map2_dbl(naermeste_4_aar, total_gyting_4_aar, function(years, total) {
      if (length(years) > 0 && !is.na(total) && total > 0) {
        sum(gyting_hunn_u3[Aar %in% years], na.rm = TRUE) / total
      } else {
        NA_real_
      }
    }),

    avg_andel_37 = map2_dbl(naermeste_4_aar, total_gyting_4_aar, function(years, total) {
      if (length(years) > 0 && !is.na(total) && total > 0) {
        sum(gyting_hunn_37[Aar %in% years], na.rm = TRUE) / total
      } else {
        NA_real_
      }
    }),

    avg_andel_o7 = map2_dbl(naermeste_4_aar, total_gyting_4_aar, function(years, total) {
      if (length(years) > 0 && !is.na(total) && total > 0) {
        sum(gyting_hunn_o7[Aar %in% years], na.rm = TRUE) / total
      } else {
        NA_real_
      }
    })
  ) %>%
  # fyll inn manglende verdier for år som ikke er simulert
  # hent først GBM fra elveliste
  left_join(
    select(
      elveliste,
      VdrNr,
      GBM
    ),
    by = "VdrNr"
  ) %>%
  # fyll inn manglende verdier fra 4-år gjennomsnittene
  mutate(
    # dersom 4-år gjennomsnittene er NA, bruk snittet av alle simulerte år
    avg_maaloppn = if_else(
      is.na(avg_maaloppn),
      mean(prosent_maaloppnaaelse_utrunk[Simulering == 1], na.rm = TRUE) / 100,
      avg_maaloppn
    ),
    avg_sannsynl = if_else(
      is.na(avg_sannsynl),
      mean(prosent_sannsynlighet_gbm[Simulering == 1], na.rm = TRUE) / 100,
      avg_sannsynl
    ),
    avg_andel_u3 = if_else(
      is.na(avg_andel_u3),
      sum(gyting_hunn_u3[Simulering == 1], na.rm = TRUE) /
        sum(gyting_hunn_u3[Simulering == 1] + gyting_hunn_37[Simulering == 1] +
              gyting_hunn_o7[Simulering == 1], na.rm = TRUE),
      avg_andel_u3
    ),
    avg_andel_37 = if_else(
      is.na(avg_andel_37),
      sum(gyting_hunn_37[Simulering == 1], na.rm = TRUE) /
        sum(gyting_hunn_u3[Simulering == 1] + gyting_hunn_37[Simulering == 1] +
              gyting_hunn_o7[Simulering == 1], na.rm = TRUE),
      avg_andel_37
    ),
    avg_andel_o7 = if_else(
      is.na(avg_andel_o7),
      sum(gyting_hunn_o7[Simulering == 1], na.rm = TRUE) /
        sum(gyting_hunn_u3[Simulering == 1] + gyting_hunn_37[Simulering == 1] +
              gyting_hunn_o7[Simulering == 1], na.rm = TRUE),
      avg_andel_o7
    ),

    # beregn gjennomsnittlig gytebestand for de 4 nærmeste årene
    avg_gytebestand = GBM * avg_maaloppn,

    # fyll inn de manglende verdiene dersom Simulering == 0
    gyting_hunn_u3 = if_else(
      Simulering == 0,
      avg_gytebestand * avg_andel_u3,
      gyting_hunn_u3
    ),
    gyting_hunn_37 = if_else(
      Simulering == 0,
      avg_gytebestand * avg_andel_37,
      gyting_hunn_37
    ),
    gyting_hunn_o7 = if_else(
      Simulering == 0,
      avg_gytebestand * avg_andel_o7,
      gyting_hunn_o7
    ),
    prosent_maaloppnaaelse = if_else(
      Simulering == 0,
      avg_maaloppn * 100,
      prosent_maaloppnaaelse
    ),
    prosent_sannsynlighet_gbm = if_else(
      Simulering == 0,
      avg_sannsynl * 100,
      prosent_sannsynlighet_gbm
    )
  ) %>%
  ungroup()

export(
  simul_kghunnlaks,
  sprintf("results/KgHunnlaks2024_utfylt.csv"),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# koble data fra gytebestandsimuleringen til sim_elv_data og beregn hunnlaksfangst
sim_elv_data <- sim_elv_data %>%
  left_join(
    select(
      simul_kghunnlaks,
      VdrNr,
      Aar,
      Andel_hunn_u3 = andel_hunn_u3,
      Andel_hunn_37 = andel_hunn_37,
      Andel_hunn_o7 = andel_hunn_o7,
      Gyting_hunn_u3 = gyting_hunn_u3,
      Gyting_hunn_37 = gyting_hunn_37,
      Gyting_hunn_o7 = gyting_hunn_o7
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  mutate(
    # beregn hunnlaksfangst i elv
    Elvefangst_vekt_hunn_u3 = Vekt_u3kg_vill * Andel_hunn_u3,
    Elvefangst_vekt_hunn_37 = Vekt_37kg_vill * Andel_hunn_37,
    Elvefangst_vekt_hunn_o7 = Vekt_o7kg_vill * Andel_hunn_o7,
    Elvefangst_vekt_hunn_total = Elvefangst_vekt_hunn_u3 +
      Elvefangst_vekt_hunn_37 + Elvefangst_vekt_hunn_o7,
    Elvefangst_ant_hunn_u3 = Ant_u3kg_vill * Andel_hunn_u3,
    Elvefangst_ant_hunn_37 = Ant_37kg_vill * Andel_hunn_37,
    Elvefangst_ant_hunn_o7 = Ant_o7kg_vill * Andel_hunn_o7,
    Elvefangst_ant_hunn_total = Elvefangst_ant_hunn_u3 +
      Elvefangst_ant_hunn_37 + Elvefangst_ant_hunn_o7
  )

# etabler en matrise med alle mulige kombinasjoner av elver og år
# denne brukes som utgangspunkt for å legge inn data inn i en resultattabell
sim_elver_aar <- expand_grid(
  VdrNr = elver_med_simulering,
  Aar = aar_liste
)

# begynn å sette sammen en resultatmatrise for de simulerte vassdragene
rapport_sim_elv <- sim_elver_aar %>%
  # koble til elveliste for å få med vassdragsnavn, region og gytebestandsmål
  left_join(
    select(
      elveliste,
      VdrNr,
      Vassdrag,
      RegionNr,
      Region = RegionNavn,
      GBM,
      Delbestand
    ),
    by = "VdrNr"
  ) %>%
  # koble til data fra de vassdragsspesifikke grunnlagsfilene
  left_join(
    select(
      sim_elv_data,
      VdrNr,
      Aar,
      GytingSimulert = Simulering,
      Inkluder_til_Norm = InklNormal,
      VisSimulert = VisSimulering,
      Elvefangst_vekt_u3 = Vekt_u3kg_vill,
      Elvefangst_vekt_37 = Vekt_37kg_vill,
      Elvefangst_vekt_o7 = Vekt_o7kg_vill,
      Elvefangst_vekt_total = Vekt_total_vill,
      Elvefangst_ant_u3 = Ant_u3kg_vill,
      Elvefangst_ant_37 = Ant_37kg_vill,
      Elvefangst_ant_o7 = Ant_o7kg_vill,
      Elvefangst_ant_total = Ant_total_vill,
      Elvefangst_hunn_u3 = Elvefangst_vekt_hunn_u3,
      Elvefangst_hunn_37 = Elvefangst_vekt_hunn_37,
      Elvefangst_hunn_o7 = Elvefangst_vekt_hunn_o7,
      Elvefangst_hunn_total = Elvefangst_vekt_hunn_total,
      Snittvekt_u3 = elv_snittvekt_u3,
      Snittvekt_37 = elv_snittvekt_37,
      Snittvekt_o7 = elv_snittvekt_o7,
      Andel_vekt_u3 = pros_vekt_u3,
      Andel_vekt_37 = pros_vekt_37,
      Andel_vekt_o7 = pros_vekt_o7,
      Andel_ant_u3 = pros_ant_u3,
      Andel_ant_37 = pros_ant_37,
      Andel_ant_o7 = pros_ant_o7,
      Andel_over_1.5kg
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  # koble til data fra gytebestandsimuleringen
  left_join(
    select(
      simul_kghunnlaks,
      VdrNr,
      Aar,
      Andel_hunn_u3 = andel_hunn_u3,
      Andel_hunn_37 = andel_hunn_37,
      Andel_hunn_o7 = andel_hunn_o7,
      Gyting_hunn_u3 = gyting_hunn_u3,
      Gyting_hunn_37 = gyting_hunn_37,
      Gyting_hunn_o7 = gyting_hunn_o7
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  # sett opp variabler for gyting hunn totalt, gyting hann+hunn og innsig elv
  mutate(
    Gyting_hunn_total = Gyting_hunn_u3 + Gyting_hunn_37 + Gyting_hunn_o7,

    # gyting hann+hunn
    Gyting_vekt_u3 = if_else(
      Andel_hunn_u3 > 0,
      Gyting_hunn_u3 / Andel_hunn_u3,
      0
    ),
    Gyting_vekt_37 = if_else(
      Andel_hunn_37 > 0,
      Gyting_hunn_37 / Andel_hunn_37,
      0
    ),
    Gyting_vekt_o7 = if_else(
      Andel_hunn_o7 > 0,
      Gyting_hunn_o7 / Andel_hunn_o7,
      0
    ),
    Gyting_ant_u3 = if_else(
      Snittvekt_u3 > 0,
      Gyting_vekt_u3 / Snittvekt_u3,
      0
    ),
    Gyting_ant_37 = if_else(
      Snittvekt_37 > 0,
      Gyting_vekt_37 / Snittvekt_37,
      0
    ),
    Gyting_ant_o7 = if_else(
      Snittvekt_o7 > 0,
      Gyting_vekt_o7 / Snittvekt_o7,
      0
    ),

    # innsig elv
    Innsig_elv_vekt_u3 = Gyting_vekt_u3 + Elvefangst_vekt_u3,
    Innsig_elv_vekt_37 = Gyting_vekt_37 + Elvefangst_vekt_37,
    Innsig_elv_vekt_o7 = Gyting_vekt_o7 + Elvefangst_vekt_o7,
    Innsig_elv_vekt_total = Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7,
    Innsig_elv_ant_u3 = Gyting_ant_u3 + Elvefangst_ant_u3,
    Innsig_elv_ant_37 = Gyting_ant_37 + Elvefangst_ant_37,
    Innsig_elv_ant_o7 = Gyting_ant_o7 + Elvefangst_ant_o7,
    Innsig_elv_ant_total = Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7,
    Innsig_elv_hunn_u3 = Gyting_hunn_u3 + Elvefangst_hunn_u3,
    Innsig_elv_hunn_37 = Gyting_hunn_37 + Elvefangst_hunn_37,
    Innsig_elv_hunn_o7 = Gyting_hunn_o7 + Elvefangst_hunn_o7,
    Innsig_elv_hunn_total = Innsig_elv_hunn_u3 + Innsig_elv_hunn_37 + Innsig_elv_hunn_o7,

    # legg inn variable også for innsig u3 over 1.5 kg
    Innsig_elv_vekt_u3_over15 = Innsig_elv_vekt_u3 * Andel_over_1.5kg,
    Innsig_elv_ant_u3_over15 = Innsig_elv_ant_u3 * Andel_over_1.5kg,

    # lag variable for beskatningsrate av de tre størrelsesgruppene
    Beskatningsrate_elv_vekt_u3 = if_else(
      Innsig_elv_vekt_u3 > 0,
      Elvefangst_vekt_u3 / Innsig_elv_vekt_u3,
      0
    ),
    Beskatningsrate_elv_vekt_37 = if_else(
      Innsig_elv_vekt_37 > 0,
      Elvefangst_vekt_37 / Innsig_elv_vekt_37,
      0
    ),
    Beskatningsrate_elv_vekt_o7 = if_else(
      Innsig_elv_vekt_o7 > 0,
      Elvefangst_vekt_o7 / Innsig_elv_vekt_o7,
      0
    ),
    Beskatningsrate_elv_ant_u3 = if_else(
      Innsig_elv_ant_u3 > 0,
      Elvefangst_ant_u3 / Innsig_elv_ant_u3,
      0
    ),
    Beskatningsrate_elv_ant_37 = if_else(
      Innsig_elv_ant_37 > 0,
      Elvefangst_ant_37 / Innsig_elv_ant_37,
      0
    ),
    Beskatningsrate_elv_ant_o7 = if_else(
      Innsig_elv_ant_o7 > 0,
      Elvefangst_ant_o7 / Innsig_elv_ant_o7,
      0
    ),
    Beskatningsrate_elv_vekt_hunn = if_else(
      (Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7) > 0,
      (Elvefangst_vekt_u3 * Andel_hunn_u3 + Elvefangst_vekt_37 * Andel_hunn_37 +
         Elvefangst_vekt_o7 * Andel_hunn_o7) /
        (Innsig_elv_vekt_u3 * Andel_hunn_u3 + Innsig_elv_vekt_37 * Andel_hunn_37 +
           Innsig_elv_vekt_o7 * Andel_hunn_o7),
      0
    ),
    Beskatningsrate_elv_ant_hunn = if_else(
      (Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7) > 0,
      (Elvefangst_ant_u3 * Andel_hunn_u3 + Elvefangst_ant_37 * Andel_hunn_37 +
         Elvefangst_ant_o7 * Andel_hunn_o7) /
        (Innsig_elv_ant_u3 * Andel_hunn_u3 + Innsig_elv_ant_37 * Andel_hunn_37 +
           Innsig_elv_ant_o7 * Andel_hunn_o7),
      0
    )
  )

export(
  rapport_sim_elv,
  sprintf("results/rapport_sim_elv_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

#-----------------------------------------------------------------------------------------------------------------
# Beregne elveinnsig i de ikke-simulerte vassdragene som har egne datafiler
#-----------------------------------------------------------------------------------------------------------------
# MARK:Elveinnsig andre

# importer alle bakgrunnsdatafilene til de ikke-simulerte elvene til en felles variabel
andre_elv_data_imp <- elver_uten_simulering %>%
  map_df(
    function(elv) {
      filbane  <- file.path(
        "data",
        "vassdrag",
        paste0(elveliste$Filnavn[elveliste$VdrNr == elv], ".csv")
      )
      tryCatch(
        {
          import(filbane, encoding = "UTF-8")
        },
        error = function(e) {
          warning(sprintf("Fant ikke vassdragsfil for elv %d: %s", elv, e$message))
        }
      )
    }
  )

# gjør noen tilpasninger til grunnlagsdataene for hver enkelt elv. Ta bort år som er utenfor start- og sluttår
# samt sett inn regionnavn
andre_elv_data <- andre_elv_data_imp %>%
  # ta bort rader med år utenfor start- og sluttår som ble definert i starten av skriptet
  filter(Aar >= start_aar & Aar <= slutt_aar) %>%
  # sett inn variable for total mengde laks
  mutate(
    total_ant = Laks_ant_u3kg + Laks_ant_o3u7kg + Laks_ant_o7kg +
      Gjen_ant_u3kg + Gjen_ant_o3u7kg + Gjen_ant_o7kg,
    total_vekt = Laks_vekt_u3kg + Laks_vekt_o3u7kg + Laks_vekt_o7kg +
      Gjen_vekt_u3kg + Gjen_vekt_o3u7kg + Gjen_vekto7kg,
    total_ant_avlivet = Laks_ant_u3kg + Laks_ant_o3u7kg + Laks_ant_o7kg,
    total_vekt_avlivet = Laks_vekt_u3kg + Laks_vekt_o3u7kg + Laks_vekt_o7kg,
    GytingSimulert = 0,
    Inkluder_til_Norm = 0,
    VisSimulert = 0
  ) %>%
  # feil variabelnavn i grunnlagsfilene, endre navn for å matche data fra simulering
  rename(Gjen_vekt_o7kg = Gjen_vekto7kg) %>%
  # koble til regionnavn og GBM for de som har
  left_join(
    select(
      elveliste,
      VdrNr,
      Region = RegionNavn,
      GBM
    ),
    by = "VdrNr"
  )

# gå gjennom alle vassdrag og år i andre_elv_data og beregn elvespesifikk gjennomsnittsvekt for hver av de tre størrelsesgruppene
# hvis fangsten i et år i en størrelsesgruppe er mindre enn eller lik 4, brukes regionale gjennomsnittsvekter
# (beregnet ovenfor for de simulerte vassdragene)
andre_elv_data <- andre_elv_data %>%
  # start med å hente de regionale snittverdiene
  left_join(
    regionale_data %>%
      select(
        Region,
        Aar,
        snitt_vekt_u3,
        snitt_vekt_37,
        snitt_vekt_o7
      ),
    by = c(
      "Region",
      "Aar"
    )
  ) %>%
  mutate(
    # beregn elvespesifikk snittvekt når fangst >= 4, ellers bruk regionale snittvekter
    elv_snittvekt_u3 = case_when(
      (Laks_ant_u3kg + Gjen_ant_u3kg) >= 4 & (Laks_vekt_u3kg + Gjen_vekt_u3kg) > 0 ~ (Laks_vekt_u3kg + Gjen_vekt_u3kg) / (Laks_ant_u3kg + Gjen_ant_u3kg),
      TRUE ~ snitt_vekt_u3
    ),
    elv_snittvekt_37 = case_when(
      (Laks_ant_o3u7kg + Gjen_ant_o3u7kg) >= 4 & (Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg) > 0 ~ (Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg) / (Laks_ant_o3u7kg + Gjen_ant_o3u7kg),
      TRUE ~ snitt_vekt_37
    ),
    elv_snittvekt_o7 = case_when(
      (Laks_ant_o7kg + Gjen_ant_o7kg) >= 4 & (Laks_vekt_o7kg + Gjen_vekt_o7kg) > 0 ~ (Laks_vekt_o7kg + Gjen_vekt_o7kg) / (Laks_ant_o7kg + Gjen_ant_o7kg),
      TRUE ~ snitt_vekt_o7
    ),
    # beregn andel smålaks over 1.5 kg
    Andel_over_1.5kg = pmax(0, pmin(1, 1 - ((-0.843 * elv_snittvekt_u3) + 1.794)))
  ) %>%
  # ta bort midlertidige kolonner fra join
  select(
    -starts_with("snitt_")
  ) %>%
  ungroup()

# etabler størrelsesfordelinger for de ikke-simulerte elvene
# start med å beregne snittfordeling over alle år per elv dersom total_ant > 20
# og sett inn regionale verdier dersom total_ant < 20
# dette gjøres for å unngå at små fangster gir store utslag i andelene
snitt_strklasse_andel_alle_aar <- andre_elv_data %>%
  group_by(
    VdrNr,
    Region
  ) %>%
  summarize(
    # beregn total vekt og antall for hver region og hvert år
    antall_totalt_alle_aar = sum(total_ant, na.rm = TRUE),
    vekt_u3 = sum(Laks_vekt_u3kg + Gjen_vekt_u3kg, na.rm = TRUE),
    vekt_37 = sum(Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg, na.rm = TRUE),
    vekt_o7 = sum(Laks_vekt_o7kg + Gjen_vekt_o7kg, na.rm = TRUE),
    ant_u3 = sum(Laks_ant_u3kg + Gjen_ant_u3kg, na.rm = TRUE),
    ant_37 = sum(Laks_ant_o3u7kg + Gjen_ant_o3u7kg, na.rm = TRUE),
    ant_o7 = sum(Laks_ant_o7kg + Gjen_ant_o7kg, na.rm = TRUE)
  ) %>%
  # koble på regionale data som brukes dersom det er lite rapportert fangst
  left_join(
    regionale_data_tot %>%
      select(Region,
             pros_vekt_u3_tot, pros_vekt_37_tot, pros_vekt_o7_tot,
             pros_ant_u3_tot, pros_ant_37_tot, pros_ant_o7_tot),
    by = "Region"
  ) %>%
  # beregn andel vekt og antall av de ulike størrelsesgruppene, bruk regionale verdier dersom
  # det er lite fangst (total_ant < 20)
  mutate(
    # vekt prosent
    pros_vekt_u3_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_vekt_u3_tot,
      vekt_u3 / (vekt_u3 + vekt_37 + vekt_o7)
    ),
    pros_vekt_37_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_vekt_37_tot,
      vekt_37 / (vekt_u3 + vekt_37 + vekt_o7)
    ),
    pros_vekt_o7_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_vekt_o7_tot,
      vekt_o7 / (vekt_u3 + vekt_37 + vekt_o7)
    ),

    # antall prosent
    pros_ant_u3_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_ant_u3_tot,
      ant_u3 / (ant_u3 + ant_37 + ant_o7)
    ),
    pros_ant_37_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_ant_37_tot,
      ant_37 / (ant_u3 + ant_37 + ant_o7)
    ),
    pros_ant_o7_alle_aar = if_else(
      antall_totalt_alle_aar < 20,
      pros_ant_o7_tot,
      ant_o7 / (ant_u3 + ant_37 + ant_o7)
    )
  ) %>%
  ungroup() %>%
  # fjern midlertidige kolonner (etter join)
  select(
    -ends_with("_tot")
  ) %>%
  # fjern sum av vekt og antall
  select(
    -starts_with("vekt_"),
    -starts_with("ant_")
  ) %>%
  # fjern antall totalt
  select(
    -antall_totalt_alle_aar
  ) %>%
  # fjern regionnavn
  select(
    -Region
  ) %>%
  # ta bort eventuelle NA-verdier
  mutate(
    across(
      starts_with(c("pros_", "snitt_")),
      ~replace_na(., 0)
    )
  )

# beregn elvefangst hunnlaks
andre_elv_data <- andre_elv_data %>%
  mutate(
    Elvefangst_hunn_u3 = Laks_vekt_u3kg * Andel_hunn,
    Elvefangst_hunn_37 = Laks_vekt_o3u7kg * Andel_hunn,
    Elvefangst_hunn_o7 = Laks_vekt_o7kg * Andel_hunn,
    Elvefangst_hunn_total = Elvefangst_hunn_u3 + Elvefangst_hunn_37 + Elvefangst_hunn_o7
  )

# fortsett med å gå gjennom alle vassdragene og årene i andre_elv_data og beregn elvespesifikke
# størrelsesfordelinger per år, enten basert på lokal fangst eller på snitt over alle år dersom antall
# er mindre enn 20
andre_elv_data <- andre_elv_data %>%
  left_join(
    snitt_strklasse_andel_alle_aar,
    by = "VdrNr"
  ) %>%
  mutate(
    # vekt prosent
    pros_vekt_u3 = if_else(
      total_ant < 20,
      pros_vekt_u3_alle_aar,
      (Laks_vekt_u3kg + Gjen_vekt_u3kg) / total_vekt
    ),
    pros_vekt_37 = if_else(
      total_ant < 20,
      pros_vekt_37_alle_aar,
      (Laks_vekt_o3u7kg + Gjen_vekt_o3u7kg) / total_vekt
    ),
    pros_vekt_o7 = if_else(
      total_ant < 20,
      pros_vekt_o7_alle_aar,
      (Laks_vekt_o7kg + Gjen_vekt_o7kg) / total_vekt
    ),

    # antall prosent
    pros_ant_u3 = if_else(
      total_ant < 20,
      pros_ant_u3_alle_aar,
      (Laks_ant_u3kg + Gjen_ant_u3kg) / total_ant
    ),
    pros_ant_37 = if_else(
      total_ant < 20,
      pros_ant_37_alle_aar,
      (Laks_ant_o3u7kg + Gjen_ant_o3u7kg) / total_ant
    ),
    pros_ant_o7 = if_else(
      total_ant < 20,
      pros_ant_o7_alle_aar,
      (Laks_ant_o7kg + Gjen_ant_o7kg) / total_ant
    )
  ) %>%
  # fjern midlertidige kolonner (etter join)
  select(
    -ends_with("_alle_aar")
  ) %>%
  # ta bort eventuelle NA-verdier
  mutate(
    across(
      starts_with(c("pros_", "snitt_")),
      ~replace_na(., 0)
    )
  ) %>%
  ungroup()

# vassdragsfilene er satt opp med fire metoder for å beregne gytebestand, nedenfor er det
# laget separate sekvenser for hver av disse

# 1) velg ut vassdragene som har oppgitt måloppnåelse ("Maloppnaelse") og beregn mengden gytere
# gjennom å multiplisere "Maloppnaelse" og "GBM"
andre_elv_data <- andre_elv_data %>%
  mutate(
    Gyting_hunn_u3 = if_else(
      !is.na(Maloppnaelse),
      GBM * Maloppnaelse * pros_vekt_u3,
      0
    ),
    Gyting_hunn_37 = if_else(
      !is.na(Maloppnaelse),
      GBM * Maloppnaelse * pros_vekt_37,
      0
    ),
    Gyting_hunn_o7 = if_else(
      !is.na(Maloppnaelse),
      GBM * Maloppnaelse * pros_vekt_o7,
      0
    ),
    Gyting_vekt_u3 = if_else(
      !is.na(Gyting_hunn_u3),
      Gyting_hunn_u3 / Andel_hunn,
      0
    ),
    Gyting_vekt_37 = if_else(
      !is.na(Gyting_hunn_37),
      Gyting_hunn_37 / Andel_hunn,
      0
    ),
    Gyting_vekt_o7 = if_else(
      !is.na(Gyting_hunn_o7),
      Gyting_hunn_o7 / Andel_hunn,
      0
    ),
    Gyting_ant_u3 = if_else(
      !is.na(Gyting_vekt_u3),
      Gyting_vekt_u3 / elv_snittvekt_u3,
      0
    ),
    Gyting_ant_37 = if_else(
      !is.na(Gyting_vekt_37),
      Gyting_vekt_37 / elv_snittvekt_37,
      0
    ),
    Gyting_ant_o7 = if_else(
      !is.na(Gyting_vekt_o7),
      Gyting_vekt_o7 / elv_snittvekt_o7,
      0
    )
  )

# 2) beregn gytebestand for de vassdragene som har oppgitt gytebestand i "Gytebestand"
andre_elv_data <- andre_elv_data %>%
  mutate(
    Gyting_ant_u3 = if_else(
      !is.na(Gytebestand),
      Gytebestand * pros_ant_u3,
      Gyting_ant_u3
    ),
    Gyting_ant_37 = if_else(
      !is.na(Gytebestand),
      Gytebestand * pros_ant_37,
      Gyting_ant_37
    ),
    Gyting_ant_o7 = if_else(
      !is.na(Gytebestand),
      Gytebestand * pros_ant_o7,
      Gyting_ant_o7
    ),
    Gyting_vekt_u3 = if_else(
      !is.na(Gytebestand),
      Gyting_ant_u3 * elv_snittvekt_u3,
      Gyting_vekt_u3
    ),
    Gyting_vekt_37 = if_else(
      !is.na(Gytebestand),
      Gyting_ant_37 * elv_snittvekt_37,
      Gyting_vekt_37
    ),
    Gyting_vekt_o7 = if_else(
      !is.na(Gytebestand),
      Gyting_ant_o7 * elv_snittvekt_o7,
      Gyting_vekt_o7
    ),
    Gyting_hunn_u3 = if_else(
      !is.na(Gytebestand),
      Gyting_vekt_u3 * Andel_hunn,
      Gyting_hunn_u3
    ),
    Gyting_hunn_37 = if_else(
      !is.na(Gytebestand),
      Gyting_vekt_37 * Andel_hunn,
      Gyting_hunn_37
    ),
    Gyting_hunn_o7 = if_else(
      !is.na(Gytebestand),
      Gyting_vekt_o7 * Andel_hunn,
      Gyting_hunn_o7
    )
  )

# 3) beregn gytebestand for de vassdragene som har oppgitt et elveinnsig
andre_elv_data <- andre_elv_data %>%
  mutate(
    Gyting_ant_u3 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_u3 - Laks_ant_u3kg,
      Gyting_ant_u3
    ),
    Gyting_ant_37 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_37 - Laks_ant_o3u7kg,
      Gyting_ant_37
    ),
    Gyting_ant_o7 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_o7 - Laks_ant_o7kg,
      Gyting_ant_o7
    ),
    Gyting_vekt_u3 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_u3 * elv_snittvekt_u3 - Laks_vekt_u3kg,
      Gyting_vekt_u3
    ),
    Gyting_vekt_37 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_37 * elv_snittvekt_37 - Laks_vekt_o3u7kg,
      Gyting_vekt_37
    ),
    Gyting_vekt_o7 = if_else(
      !is.na(Elvinnsig),
      Elvinnsig * pros_ant_o7 * elv_snittvekt_o7 - Laks_vekt_o7kg,
      Gyting_vekt_o7
    ),
    Gyting_hunn_u3 = if_else(
      !is.na(Elvinnsig),
      Gyting_vekt_u3 * Andel_hunn,
      Gyting_hunn_u3
    ),
    Gyting_hunn_37 = if_else(
      !is.na(Elvinnsig),
      Gyting_vekt_37 * Andel_hunn,
      Gyting_hunn_37
    ),
    Gyting_hunn_o7 = if_else(
      !is.na(Elvinnsig),
      Gyting_vekt_o7 * Andel_hunn,
      Gyting_hunn_o7
    )
  )

# 4) beregn gytebestand for de vassdragene som har oppgitt fangstrate
andre_elv_data <- andre_elv_data %>%
  mutate(
    Gyting_ant_u3 = if_else(
      !is.na(Fangstrate),
      Laks_ant_u3kg / Fangstrate - Laks_ant_u3kg,
      Gyting_ant_u3
    ),
    Gyting_ant_37 = if_else(
      !is.na(Fangstrate),
      Laks_ant_o3u7kg / Fangstrate - Laks_ant_o3u7kg,
      Gyting_ant_37
    ),
    Gyting_ant_o7 = if_else(
      !is.na(Fangstrate),
      Laks_ant_o7kg / Fangstrate - Laks_ant_o7kg,
      Gyting_ant_o7
    ),
    Gyting_vekt_u3 = if_else(
      !is.na(Fangstrate),
      Laks_vekt_u3kg / Fangstrate - Laks_vekt_u3kg,
      Gyting_vekt_u3
    ),
    Gyting_vekt_37 = if_else(
      !is.na(Fangstrate),
      Laks_vekt_o3u7kg / Fangstrate - Laks_vekt_o3u7kg,
      Gyting_vekt_37
    ),
    Gyting_vekt_o7 = if_else(
      !is.na(Fangstrate),
      Laks_vekt_o7kg / Fangstrate - Laks_vekt_o7kg,
      Gyting_vekt_o7
    ),
    Gyting_hunn_u3 = if_else(
      !is.na(Fangstrate),
      Gyting_vekt_u3 * Andel_hunn,
      Gyting_hunn_u3
    ),
    Gyting_hunn_37 = if_else(
      !is.na(Fangstrate),
      Gyting_vekt_37 * Andel_hunn,
      Gyting_hunn_37
    ),
    Gyting_hunn_o7 = if_else(
      !is.na(Fangstrate),
      Gyting_vekt_o7 * Andel_hunn,
      Gyting_hunn_o7
    ),
    Gyting_hunn_total = Gyting_hunn_u3 + Gyting_hunn_37 + Gyting_hunn_o7
  )

# etabler en matrise med alle mulige kombinasjoner av elver og år, denne brukes som utgangspunkt for å legge inn data i resultat_fordeling
andre_elver_aar <- expand_grid(
  VdrNr = elver_uten_simulering,
  Aar = aar_liste
)

# begynn å sette sammen en resultatmatrise for de ikke-simulerte vassdragene med egne datafiler
rapport_andre_elv <- andre_elver_aar %>%
  # koble til elveliste for å få med vassdragsnavn, region og gytebestandsmål
  left_join(
    select(
      elveliste,
      VdrNr,
      Vassdrag,
      RegionNr,
      Region = RegionNavn,
      GBM,
      Delbestand
    ),
    by = "VdrNr"
  ) %>%
  # koble til data fra de vassdragsspesifikke grunnlagsfilene
  left_join(
    select(
      andre_elv_data,
      VdrNr,
      Aar,
      GytingSimulert,
      Inkluder_til_Norm,
      VisSimulert,
      Elvefangst_vekt_u3 = Laks_vekt_u3kg,
      Elvefangst_vekt_37 = Laks_vekt_o3u7kg,
      Elvefangst_vekt_o7 = Laks_vekt_o7kg,
      Elvefangst_vekt_total = total_vekt_avlivet,
      Elvefangst_ant_u3 = Laks_ant_u3kg,
      Elvefangst_ant_37 = Laks_ant_o3u7kg,
      Elvefangst_ant_o7 = Laks_ant_o7kg,
      Elvefangst_ant_total = total_ant_avlivet,
      Elvefangst_hunn_u3,
      Elvefangst_hunn_37,
      Elvefangst_hunn_o7,
      Elvefangst_hunn_total,
      Snittvekt_u3 = elv_snittvekt_u3,
      Snittvekt_37 = elv_snittvekt_37,
      Snittvekt_o7 = elv_snittvekt_o7,
      Andel_vekt_u3 = pros_vekt_u3,
      Andel_vekt_37 = pros_vekt_37,
      Andel_vekt_o7 = pros_vekt_o7,
      Andel_ant_u3 = pros_ant_u3,
      Andel_ant_37 = pros_ant_37,
      Andel_ant_o7 = pros_ant_o7,
      Andel_over_1.5kg,
      Andel_hunn_u3 = Andel_hunn,
      Andel_hunn_37 = Andel_hunn,
      Andel_hunn_o7 = Andel_hunn,
      Gyting_hunn_u3,
      Gyting_hunn_37,
      Gyting_hunn_o7,
      Gyting_hunn_total,
      Gyting_vekt_u3,
      Gyting_vekt_37,
      Gyting_vekt_o7,
      Gyting_ant_u3,
      Gyting_ant_37,
      Gyting_ant_o7
    ),
    by = c("VdrNr", "Aar")
  ) %>%
  # sett opp variabler for innsig elv
  mutate(
    # innsig elv
    Innsig_elv_vekt_u3 = Gyting_vekt_u3 + Elvefangst_vekt_u3,
    Innsig_elv_vekt_37 = Gyting_vekt_37 + Elvefangst_vekt_37,
    Innsig_elv_vekt_o7 = Gyting_vekt_o7 + Elvefangst_vekt_o7,
    Innsig_elv_vekt_total = Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7,
    Innsig_elv_ant_u3 = Gyting_ant_u3 + Elvefangst_ant_u3,
    Innsig_elv_ant_37 = Gyting_ant_37 + Elvefangst_ant_37,
    Innsig_elv_ant_o7 = Gyting_ant_o7 + Elvefangst_ant_o7,
    Innsig_elv_ant_total = Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7,
    Innsig_elv_hunn_u3 = Gyting_hunn_u3 + Elvefangst_hunn_u3,
    Innsig_elv_hunn_37 = Gyting_hunn_37 + Elvefangst_hunn_37,
    Innsig_elv_hunn_o7 = Gyting_hunn_o7 + Elvefangst_hunn_o7,
    Innsig_elv_hunn_total = Innsig_elv_hunn_u3 + Innsig_elv_hunn_37 + Innsig_elv_hunn_o7,

    # legg inn variable også for innsig u3 over 1.5 kg
    Innsig_elv_vekt_u3_over15 = Innsig_elv_vekt_u3 * Andel_over_1.5kg,
    Innsig_elv_ant_u3_over15 = Innsig_elv_ant_u3 * Andel_over_1.5kg,

    # lag variable for beskatningsrate av de tre størrelsesgruppene
    Beskatningsrate_elv_vekt_u3 = if_else(
      Innsig_elv_vekt_u3 > 0,
      Elvefangst_vekt_u3 / Innsig_elv_vekt_u3,
      0
    ),
    Beskatningsrate_elv_vekt_37 = if_else(
      Innsig_elv_vekt_37 > 0,
      Elvefangst_vekt_37 / Innsig_elv_vekt_37,
      0
    ),
    Beskatningsrate_elv_vekt_o7 = if_else(
      Innsig_elv_vekt_o7 > 0,
      Elvefangst_vekt_o7 / Innsig_elv_vekt_o7,
      0
    ),
    Beskatningsrate_elv_ant_u3 = if_else(
      Innsig_elv_ant_u3 > 0,
      Elvefangst_ant_u3 / Innsig_elv_ant_u3,
      0
    ),
    Beskatningsrate_elv_ant_37 = if_else(
      Innsig_elv_ant_37 > 0,
      Elvefangst_ant_37 / Innsig_elv_ant_37,
      0
    ),
    Beskatningsrate_elv_ant_o7 = if_else(
      Innsig_elv_ant_o7 > 0,
      Elvefangst_ant_o7 / Innsig_elv_ant_o7,
      0
    ),
    Beskatningsrate_elv_vekt_hunn = if_else(
      (Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7) > 0,
      (Elvefangst_vekt_u3 * Andel_hunn_u3 + Elvefangst_vekt_37 * Andel_hunn_37 +
         Elvefangst_vekt_o7 * Andel_hunn_o7) / (Innsig_elv_vekt_u3 * Andel_hunn_u3 +
         Innsig_elv_vekt_37 * Andel_hunn_37 + Innsig_elv_vekt_o7 * Andel_hunn_o7),
      0
    ),
    Beskatningsrate_elv_ant_hunn = if_else(
      (Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7) > 0,
      (Elvefangst_ant_u3 * Andel_hunn_u3 + Elvefangst_ant_37 * Andel_hunn_37 +
         Elvefangst_ant_o7 * Andel_hunn_o7) / (Innsig_elv_ant_u3 * Andel_hunn_u3 +
         Innsig_elv_ant_37 * Andel_hunn_37 + Innsig_elv_ant_o7 * Andel_hunn_o7),
      0
    )
  )

export(
  rapport_andre_elv,
  sprintf("results/rapport_andre_elv_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

#-----------------------------------------------------------------------------------------------------------------
# Beregne elveinnsig i de minste vassdragene som bare har sporadisk fangst og data fra SSB-fil
#-----------------------------------------------------------------------------------------------------------------
# MARK:Elveinnsig SSB

# antagelser om data fra SSB-fangstfila
fangstrate_ssb <- 0.5
andel_hunn_ssb <- 0.5

# hent data fra SSB-fangstfila for elvene listet i elver_bare_ssb
ssb_elv_data <- elvefangst_ssb %>%
  # Filtrer de elvene som er listet i elver_bare_ssb
  filter(VdrNr %in% elver_bare_ssb) %>%
  # organiser og velg ut relevante kolonner
  select(
    VdrNr,
    Vassdrag,
    Aar,
    Region,
    total_ant,
    total_vekt,
    Laks_vekt_u3kg,
    Laks_vekt_o3u7kg,
    Laks_vekt_o7kg,
    Laks_ant_u3kg,
    Laks_ant_o3u7kg,
    Laks_ant_o7kg
  ) %>%
  # ta bort rader med år utenfor start- og sluttår som ble definert i starten av skriptet
  filter(Aar >= start_aar & Aar <= slutt_aar) %>%
  # sorter etter vassdragsnr og år
  arrange(
    VdrNr,
    Aar
  )

ssb_elv_data <- ssb_elv_data %>%
  mutate(
    Elvefangst_hunn_u3 = Laks_vekt_u3kg * andel_hunn_ssb,
    Elvefangst_hunn_37 = Laks_vekt_o3u7kg * andel_hunn_ssb,
    Elvefangst_hunn_o7 = Laks_vekt_o7kg * andel_hunn_ssb,
    Elvefangst_hunn_total = Elvefangst_hunn_u3 + Elvefangst_hunn_37 + Elvefangst_hunn_o7,
    Gyting_vekt_u3 = if_else(
      total_ant > 0,
      Laks_vekt_u3kg / fangstrate_ssb,
      0
    ),
    Gyting_vekt_37 = if_else(
      total_ant > 0,
      Laks_vekt_o3u7kg / fangstrate_ssb,
      0
    ),
    Gyting_vekt_o7 = if_else(
      total_ant > 0,
      Laks_vekt_o7kg / fangstrate_ssb,
      0
    ),
    Gyting_ant_u3 = if_else(
      total_ant > 0,
      Laks_ant_u3kg / fangstrate_ssb,
      0
    ),
    Gyting_ant_37 = if_else(
      total_ant > 0,
      Laks_ant_o3u7kg / fangstrate_ssb,
      0
    ),
    Gyting_ant_o7 = if_else(
      total_ant > 0,
      Laks_ant_o7kg / fangstrate_ssb,
      0
    ),
    Gyting_hunn_u3 = Gyting_vekt_u3 * andel_hunn_ssb,
    Gyting_hunn_37 = Gyting_vekt_37 * andel_hunn_ssb,
    Gyting_hunn_o7 = Gyting_vekt_o7 * andel_hunn_ssb,
    Gyting_hunn_total = Gyting_hunn_u3 + Gyting_hunn_37 + Gyting_hunn_o7
  )

# etabler en matrise med alle mulige kombinasjoner av elver og år,
# denne brukes som utgangspunkt for å legge inn data i resultat_fordeling
ssb_elver_aar <- expand_grid(
  VdrNr = elver_bare_ssb,
  Aar = aar_liste
)

# sett sammen en resultatmatrise for de minste vassdragene med sporadisk fangst
rapport_ssb_elv <- ssb_elver_aar %>%
  # koble til elveliste for å få med vassdragsnavn, region og gytebestandsmål
  left_join(
    select(
      elveliste,
      VdrNr,
      Vassdrag,
      RegionNr,
      Region = RegionNavn,
      GBM,
      Delbestand
    ),
    by = "VdrNr"
  ) %>%
  mutate(
    GytingSimulert = 0,
    Inkluder_til_Norm = 0,
    VisSimulert = 0
  ) %>%
  # koble til ssb_elv_data for å få alle relevante data
  left_join(
    select(
      ssb_elv_data,
      VdrNr,
      Aar,
      Elvefangst_vekt_u3 = Laks_vekt_u3kg,
      Elvefangst_vekt_37 = Laks_vekt_o3u7kg,
      Elvefangst_vekt_o7 = Laks_vekt_o7kg,
      Elvefangst_vekt_total = total_vekt,
      Elvefangst_ant_u3 = Laks_ant_u3kg,
      Elvefangst_ant_37 = Laks_ant_o3u7kg,
      Elvefangst_ant_o7 = Laks_ant_o7kg,
      Elvefangst_ant_total = total_ant,
      Elvefangst_hunn_u3,
      Elvefangst_hunn_37,
      Elvefangst_hunn_o7,
      Elvefangst_hunn_total
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  mutate(
    Snittvekt_u3 = NA_real_,
    Snittvekt_37 = NA_real_,
    Snittvekt_o7 = NA_real_,
    Andel_vekt_u3 = NA_real_,
    Andel_vekt_37 = NA_real_,
    Andel_vekt_o7 = NA_real_,
    Andel_ant_u3 = NA_real_,
    Andel_ant_37 = NA_real_,
    Andel_ant_o7 = NA_real_,
    Andel_over_1.5kg = 0.5,
    Andel_hunn_u3 = andel_hunn_ssb,
    Andel_hunn_37 = andel_hunn_ssb,
    Andel_hunn_o7 = andel_hunn_ssb
  )

# koble til ssb_elv_data for å få gytebestand og fangst
rapport_ssb_elv <- rapport_ssb_elv %>%
  left_join(
    select(
      ssb_elv_data,
      VdrNr,
      Aar,
      Gyting_hunn_u3,
      Gyting_hunn_37,
      Gyting_hunn_o7,
      Gyting_hunn_total,
      Gyting_vekt_u3,
      Gyting_vekt_37,
      Gyting_vekt_o7,
      Gyting_ant_u3,
      Gyting_ant_37,
      Gyting_ant_o7
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  # sett opp variabler for innsig elv
  mutate(
    # innsig elv
    Innsig_elv_vekt_u3 = Gyting_vekt_u3 + Elvefangst_vekt_u3,
    Innsig_elv_vekt_37 = Gyting_vekt_37 + Elvefangst_vekt_37,
    Innsig_elv_vekt_o7 = Gyting_vekt_o7 + Elvefangst_vekt_o7,
    Innsig_elv_vekt_total = Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7,
    Innsig_elv_ant_u3 = Gyting_ant_u3 + Elvefangst_ant_u3,
    Innsig_elv_ant_37 = Gyting_ant_37 + Elvefangst_ant_37,
    Innsig_elv_ant_o7 = Gyting_ant_o7 + Elvefangst_ant_o7,
    Innsig_elv_ant_total = Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7,
    Innsig_elv_hunn_u3 = Gyting_hunn_u3 + Elvefangst_hunn_u3,
    Innsig_elv_hunn_37 = Gyting_hunn_37 + Elvefangst_hunn_37,
    Innsig_elv_hunn_o7 = Gyting_hunn_o7 + Elvefangst_hunn_o7,
    Innsig_elv_hunn_total = Innsig_elv_hunn_u3 + Innsig_elv_hunn_37 + Innsig_elv_hunn_o7,

    # legg inn variable også for innsig u3 over 1.5 kg
    Innsig_elv_vekt_u3_over15 = Innsig_elv_vekt_u3 * Andel_over_1.5kg,
    Innsig_elv_ant_u3_over15 = Innsig_elv_ant_u3 * Andel_over_1.5kg,

    # lag variable for beskatningsrate av de tre størrelsesgruppene
    Beskatningsrate_elv_vekt_u3 = fangstrate_ssb,
    Beskatningsrate_elv_vekt_37 = fangstrate_ssb,
    Beskatningsrate_elv_vekt_o7 = fangstrate_ssb,
    Beskatningsrate_elv_ant_u3 = fangstrate_ssb,
    Beskatningsrate_elv_ant_37 = fangstrate_ssb,
    Beskatningsrate_elv_ant_o7 = fangstrate_ssb,
    Beskatningsrate_elv_vekt_hunn = fangstrate_ssb,
    Beskatningsrate_elv_ant_hunn = fangstrate_ssb
  )

export(
  rapport_ssb_elv,
  sprintf("results/rapport_ssb_elv_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

#-----------------------------------------------------------------------------------------------------------------
# kombiner alle elvene til en dataframe for å gjøre klar til sjøfordeling
#-----------------------------------------------------------------------------------------------------------------

rapport_alle_elver <- bind_rows(
  # legg inn en kolonne for å markere hvor dataene kommer fra
  rapport_sim_elv %>% mutate(type_elv = "Simulerte vassdrag"),
  rapport_andre_elv %>% mutate(type_elv = "Andre vassdrag"),
  rapport_ssb_elv %>% mutate(type_elv = "SSB-vassdrag")
)

# beregn samlet elveinnsig pr region for alle elvene som har Delbestand = 0
# (altså hovedvassdragene og ikke eventuelle sidestrenger)
elveinnsig_region <- rapport_alle_elver %>%
  # filtrer bort elver med Delbestand = 1 (sidestrenger) slik at kun samlet hovedelv brukes
  filter(
    Delbestand == 0
  ) %>%
  # grupper etter region og år
  group_by(
    Region,
    Aar
  ) %>%
  # summer alle elveinnsigsvariablene
  summarise(
    # elveinnsig vekt (kg)
    Region_elveinnsig_vekt_u3 = sum(Innsig_elv_vekt_u3, na.rm = TRUE),
    Region_elveinnsig_vekt_37 = sum(Innsig_elv_vekt_37, na.rm = TRUE),
    Region_elveinnsig_vekt_o7 = sum(Innsig_elv_vekt_o7, na.rm = TRUE),
    Region_elveinnsig_vekt_total = sum(
      Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7,
      na.rm = TRUE
    ),

    # elveinnsig antall
    Region_elveinnsig_ant_u3 = sum(Innsig_elv_ant_u3, na.rm = TRUE),
    Region_elveinnsig_ant_37 = sum(Innsig_elv_ant_37, na.rm = TRUE),
    Region_elveinnsig_ant_o7 = sum(Innsig_elv_ant_o7, na.rm = TRUE),
    Region_elveinnsig_ant_total = sum(
      Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7,
      na.rm = TRUE
    ),

    # innsig smålaks over 1.5 kg
    Region_elveinnsig_vekt_u3_over15 = sum(
      Innsig_elv_vekt_u3_over15,
      na.rm = TRUE
    ),
    Region_elveinnsig_ant_u3_over15 = sum(
      Innsig_elv_ant_u3_over15,
      na.rm = TRUE
    ),

    # antall elver i regionen
    # (for å eventuelt kunne beregne gjennomsnittlig elveinnsig pr elv)
    antall_elver_region = n()
  ) %>%
  # sorter etter region og år
  arrange(
    Region,
    Aar
  )

# eksporter resultatene til en CSV-fil
export(
  elveinnsig_region,
  sprintf("results/elveinnsig_region_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# bruk de regionale verdiene til å beregne hva slags andel innsiget til en elv utgjør av regionens innsig
rapport_alle_elver <- rapport_alle_elver %>%
  # koble til verdier fra elveinnsig_region
  left_join(
    elveinnsig_region,
    by = c(
      "Region",
      "Aar"
    )
  ) %>%
  # regn ut prosentandeler
  mutate(
    # andel av vekt, +1 for å unngå at størrelsesgrupper tilstede i sjø faller bort fordi de ikke
    # har blitt fanget i elv
    Andel_region_vekt_u3 = (Innsig_elv_vekt_u3 + 1) / (Region_elveinnsig_vekt_u3 + 1),
    Andel_region_vekt_37 = (Innsig_elv_vekt_37 + 1) / (Region_elveinnsig_vekt_37 + 1),
    Andel_region_vekt_o7 = (Innsig_elv_vekt_o7 + 1) / (Region_elveinnsig_vekt_o7 + 1),
    Andel_region_vekt_total = (Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7) /
      Region_elveinnsig_vekt_total,

    # andel av antall
    Andel_region_ant_u3 = (Innsig_elv_ant_u3 + 1) / (Region_elveinnsig_ant_u3 + 1),
    Andel_region_ant_37 = (Innsig_elv_ant_37 + 1) / (Region_elveinnsig_ant_37 + 1),
    Andel_region_ant_o7 = (Innsig_elv_ant_o7 + 1) / (Region_elveinnsig_ant_o7 + 1),
    Andel_region_ant_total = (Innsig_elv_ant_u3 + Innsig_elv_ant_37 + Innsig_elv_ant_o7) /
      Region_elveinnsig_ant_total,

    # andel smålaks over 1.5 kg
    Andel_region_vekt_u3_over15 = Innsig_elv_vekt_u3_over15 / Region_elveinnsig_vekt_u3_over15,
    Andel_region_ant_u3_over15 = Innsig_elv_ant_u3_over15 / Region_elveinnsig_ant_u3_over15,

    # erstatt eventuelle NaN-verdier (fra å dele med 0) med 0 (skal ikke skje pga +1 ovenfor)
    across(
      starts_with("Andel_region"),
      ~replace_na(., 0)
    ),
    across(
      starts_with("Andel_region"),
      ~if_else(is.infinite(.), 0, .)
    )
  )

# verifiser at prosentberegningene summerer til 1 for hver region og hvert år
valider_innsigsandel <- rapport_alle_elver %>%
  filter(
    Delbestand == 0
  ) %>%
  group_by(
    Region,
    Aar
  ) %>%
  summarise(
    sum_andel_vekt = sum(Andel_region_vekt_total, na.rm = TRUE),
    sum_andel_ant = sum(Andel_region_ant_total, na.rm = TRUE),
    antall_elver = n()
  ) %>%
  mutate(
    vekt_ok = abs(sum_andel_vekt - 1) < 1e-6,
    ant_ok = abs(sum_andel_ant - 1) < 1e-6
  )

#-----------------------------------------------------------------------------------------------------------------
# lag regional fangstmatrise fra sjølaksefisket for valgte år
#-----------------------------------------------------------------------------------------------------------------
# MARK: Sjøfangstfordeling

# start med å importere de årlige fordelingsnøklene som brukes til å fordele kommunefangst til regioner
region_fordeling <- aar_liste %>%
  map_df(
    function(aar) {
      filbane <- file.path(
        "data",
        "fordeling_kommune",
        sprintf("fordelingsnokkel_kommune_fjord-%d.csv", aar)
      )
      tryCatch(
        {
          import(filbane, encoding = "UTF-8") %>%
            mutate(Aar = aar)
        },
        error = function(e) {
          warning(sprintf("Fant ikke kommunefordelingsfil for år %d: %s", aar, e$message))
        }
      )
    }
  ) %>%
  mutate(
    Fylke = as.numeric(Fylke)
  ) %>%
  replace(is.na(.), 0)

# importer årlige fordelingsnøklene som fordeler fangsten mellom regionene
kyst_fordeling <- aar_liste %>%
  map_df(
    function(aar) {
      filbane <- file.path(
        "data",
        "fordelingsnokkel",
        sprintf("fordelingsnokkel_sjofangst_%d.csv", aar)
      )
      tryCatch(
        {
          import(filbane, encoding = "UTF-8") %>%
            mutate(Aar = aar)
        },
        error = function(e) {
          warning(sprintf("Fant ikke fordelingsnøkkelfil for år %d: %s", aar, e$message))
        }
      )
    }
  )  %>%
  rename(Region = Kystregion)

# les inn fangststatistikk fra sjølaksefisket
sjofangst <- import(
  "data/ssb_sjo_1993-.csv",
  encoding = "UTF-8",
  na.strings = c(":", "..")
)

sjofangst <- sjofangst %>%
  rename(kommune = region) %>%
  rename(vekt_u3 = "Laks under 3 kg (kg)") %>%
  rename(vekt_37 = "Laks 3 - 6,9 kg (kg)") %>%
  rename(vekt_o7 = "Laks 7 kg og over (kg)") %>%
  rename(ant_u3 = "Laks under 3 kg (stk)") %>%
  rename(ant_37 = "Laks 3 - 6,9 kg (stk)") %>%
  rename(ant_o7 = "Laks 7 kg og over (stk)")

# les inn fylkesfordelt restfangst, må fordeles på regioner
restfangst_sjo <- import(
  "data/restfangst_fylker_sjo.csv",
  encoding = "UTF-8"
)

restfangst_sjo  <-  restfangst_sjo %>%
  rename(vekt_u3 = "Laks under 3 kg (kg)") %>%
  rename(vekt_37 = "Laks 3 - 6,9 kg (kg)") %>%
  rename(vekt_o7 = "Laks 7 kg og over (kg)") %>%
  rename(ant_u3 = "Laks under 3 kg (stk)") %>%
  rename(ant_37 = "Laks 3 - 6,9 kg (stk)") %>%
  rename(ant_o7 = "Laks 7 kg og over (stk)")

# les inn fil med villaks og oppdrettslaks fra simuleringen
ant_vill_og_oppdrett <- import(
  "results/AntVillogOppdrettElvEstimater2024.csv",
  encoding = "UTF-8"
)

ant_vill_og_oppdrett <- ant_vill_og_oppdrett %>%
  mutate(
    oppdrett_prosent = oppdrett_antall_q50 / (oppdrett_antall_q50 + villaks_antall_totalt_q50)
  ) %>%
  # endre NA til 0
  mutate(
    oppdrett_prosent = replace_na(oppdrett_prosent, 0)
  ) %>%
  rename(
    VdrNr = Vdrnr
  )

ant_vill_og_oppdrett_region <- ant_vill_og_oppdrett %>%
  # hent inn region fra elveliste
  left_join(
    select(
      elveliste,
      VdrNr,
      Region = RegionNavn
    ),
    by = "VdrNr"
  ) %>%
  # calculate average percentage of wild salmon in the region
  group_by(
    Region,
    Aar
  ) %>%
  summarise(
    # gjennomsnittlig prosentandel oppdrettslaks i regionene
    oppdrett_prosent = mean(oppdrett_prosent, na.rm = TRUE),
    .groups = "drop"
  )

# historisk er det en del fangst som ikke er fordelt på kommuner (pga få fiskere i kommunen), denne fangsten er oppsummert på fylkesnivå
# i restfangst_sjo. For å få denne med i regionfordelingen, fordeler vi restfangsten først til kommuner basert på antallet kommuner i fylket
# som ikke har fangst.

# lag først en fordeling av kommuner med manglende fangst (NA verdier)
rest_fordeling <- sjofangst %>%
  # filtrer rader med NA i fangstkolonnene
  filter(
    if_any(starts_with(c("vekt_", "ant_")), is.na)
  ) %>%
  # koble sammen med regionfordelingen basert på år og kommune
  left_join(
    region_fordeling,
    by = c(
      "aar" = "Aar",
      "komnr" = "Kommunenr"
    )
  ) %>%
  # grupper på fylke og år
  group_by(
    fylke,
    aar
  ) %>%
  # beregn andel av fangst på hver kommune i fylket
  mutate(
    prop = 1 / n()  # fordel likt på hver kommune
  )

# multipliser kommuneandelene med restfangsten
fordelt_rest <- restfangst_sjo %>%
  left_join(
    rest_fordeling,
    by = c(
      "Fylke" = "fylke",
      "Aar" = "aar"
    )
  ) %>%
  group_by(
    Aar,
    komnr
  ) %>%
  summarise(
    # multipliser hver restverdi med andel
    vekt_u3_rest = sum(vekt_u3.x * prop, na.rm = TRUE),
    vekt_37_rest = sum(vekt_37.x * prop, na.rm = TRUE),
    vekt_o7_rest = sum(vekt_o7.x * prop, na.rm = TRUE),
    ant_u3_rest  = sum(ant_u3.x * prop,  na.rm = TRUE),
    ant_37_rest  = sum(ant_37.x * prop,  na.rm = TRUE),
    ant_o7_rest  = sum(ant_o7.x * prop,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    aar = Aar
  )

# koble restfordelingen med den rapporterte sjøfangsten og legg til eksisterende fangstkolonner
sjofangst_oppdatert <- sjofangst %>%
  left_join(
    fordelt_rest,
    by = c(
      "aar",
      "komnr"
    )
  ) %>%
  mutate(
    vekt_u3 = coalesce(vekt_u3, 0) + coalesce(vekt_u3_rest, 0),
    vekt_37 = coalesce(vekt_37, 0) + coalesce(vekt_37_rest, 0),
    vekt_o7 = coalesce(vekt_o7, 0) + coalesce(vekt_o7_rest, 0),
    ant_u3  = coalesce(ant_u3,  0) + coalesce(ant_u3_rest,  0),
    ant_37  = coalesce(ant_37,  0) + coalesce(ant_37_rest,  0),
    ant_o7  = coalesce(ant_o7,  0) + coalesce(ant_o7_rest,  0)
  )

# lag en ny dataframe med fangstdata fra kommunene fordelt på regioner
sjofangst_region <- sjofangst_oppdatert %>%
  # koble sjøfangsten opp mot region_fordeling for å få andelskolonner for samme år og kommune
  left_join(
    region_fordeling,
    by = c(
      "aar" = "Aar",
      "komnr" = "Kommunenr"
    )
  ) %>%
  # endre fra separate fangstkolonner til en kolonne med fangst og en kolonne som beskriver størrelseskategori
  pivot_longer(
    cols = c(
      vekt_u3,
      vekt_37,
      vekt_o7,
      ant_u3,
      ant_37,
      ant_o7
    ),
    names_to = "fangstkategori",
    values_to = "fangstverdi"
  ) %>%
  # endre form bort fra separate regionkolonner til en kolonne med regionnavn og en kolonne med andelsverdier
  pivot_longer(
    cols = c(
      "Østlandet", "Agderkysten", "Jæren", "Kysten_fra_Stadt_til_Stavanger",
      "Kysten_av_Møre_og_Romsdal", "Kysten_av_Trøndelag",
      "Nordlandskysten_sør_for_Vestfjorden", "Lofoten_og_Vesterålen",
      "Kysten_av_Troms", "Finnmark_vest", "Finnmark_midt", "Finnmark_øst",
      "Varangerfjord", "Lyngen_Reisa", "Kvænangen", "Fjordene_i_Vest-Finnmark",
      "Porsangerfjorden", "Tanafjorden", "Køfjord_Bøkfjord_Jarfjord",
      "Indre_Rogaland", "Hardangerfjord", "Osterøy", "Austfjorden", "Sognefjorden",
      "Dalsfjorden", "Førdefjorden", "Nordfjord", "Sunnmørsfjordene",
      "Romsdalsfjord", "Sunndalsfjord", "Nordmørsfjordene", "Hemnfjorden",
      "Trondheimsfjorden", "Åfjord", "Namsfjorden", "Follafjorden", "Sørsalten",
      "Bindalsfjorden", "Vellfjorden", "Vefsnfjorden", "Sjona", "Ranafjorden",
      "Beiarfjorden", "Skjerstadfjorden", "Sørfolda", "Ofot_Efjord_Tysfjord",
      "Astafjorden_Salangen", "Malangen", "Balsfjord"
    ),
    names_to = "region",
    values_to = "andel"
  ) %>%
  # multipliser fangstverdi med andel for å få vektet fangst
  mutate(
    vektet_fangst = fangstverdi * andel
  ) %>%
  # summer total fangst på år, region og størrelseskategori
  group_by(
    aar,
    region,
    fangstkategori
  ) %>%
  summarise(
    total_fangst = sum(vektet_fangst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # forandre data tilbake til bredt format med separate kolonner for størrelseskategori i fangsten
  pivot_wider(
    names_from = fangstkategori,
    values_from = total_fangst,
    values_fill = 0
  ) %>%
  # endre navn for å være litt konsistent
  rename(
    Aar = aar,
    Region = region
  ) %>%
  # lag kolonner for total fangst i vekt og antall
  mutate(
    vekt_total = vekt_u3 + vekt_37 + vekt_o7,
    ant_total = ant_u3 + ant_37 + ant_o7
  ) %>%
  select(
    Region,
    Aar,
    vekt_u3,
    vekt_37,
    vekt_o7,
    vekt_total,
    ant_u3,
    ant_37,
    ant_o7,
    ant_total
  )

# hent inn prosentandel oppdrettslaks og juster regionfangsten slik at oppdrettslaks tas bort
sjofangst_region <- sjofangst_region %>%
  # koble til oppdrettslaksandelen
  left_join(
    select(
      ant_vill_og_oppdrett_region,
      Region,
      Aar,
      oppdrett_prosent
    ),
    by = c(
      "Aar",
      "Region"
    )
  ) %>%
  # fjern NA-verdier i oppdrett_prosent
  mutate(
    oppdrett_prosent = replace_na(oppdrett_prosent, 0)
  ) %>%
  # juster fangstene for oppdrettslaks
  mutate(
    vekt_u3 = vekt_u3 * (1 - oppdrett_prosent),
    vekt_37 = vekt_37 * (1 - oppdrett_prosent),
    vekt_o7 = vekt_o7 * (1 - oppdrett_prosent),
    ant_u3 = ant_u3 * (1 - oppdrett_prosent),
    ant_37 = ant_37 * (1 - oppdrett_prosent),
    ant_o7 = ant_o7 * (1 - oppdrett_prosent)
  )

# lagre resultatet til en rapportfil
export(
  sjofangst_region,
  sprintf("results/sjofangst_region_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

sjofordeling <- sjofangst_region %>%
  # koble rapportert regionfordelt sjøfangst opp mot fordelingsnøkkelen for å få andelskolonner
  # for samme år og region
  left_join(
    kyst_fordeling,
    by = c(
      "Aar",
      "Region"
    )
  ) %>%
  # endre fra separate fangstkolonner til en kolonne med fangst og en kolonne som beskriver størrelseskategori
  pivot_longer(
    cols = matches("^(Vekt|Ant)_"),
    names_to = "fangstkategori",
    values_to = "fangstverdi"
  ) %>%
  #  endre form bort fra separate regionkolonner til en kolonne med regionnavn og en kolonne med andelsverdier
  pivot_longer(
    cols = c(
      "Østlandet", "Agderkysten", "Jæren", "Kysten_fra_Stadt_til_Stavanger",
      "Kysten_av_Møre_og_Romsdal", "Kysten_av_Trøndelag",
      "Nordlandskysten_sør_for_Vestfjorden", "Lofoten_og_Vesterålen",
      "Kysten_av_Troms", "Finnmark_vest", "Finnmark_midt", "Finnmark_øst",
      "Varangerfjord", "Lyngen_Reisa", "Kvænangen", "Fjordene_i_Vest-Finnmark",
      "Porsangerfjorden", "Tanafjorden", "Køfjord_Bøkfjord_Jarfjord",
      "Indre_Rogaland", "Hardangerfjord", "Osterøy", "Austfjorden", "Sognefjorden",
      "Dalsfjorden", "Førdefjorden", "Nordfjord", "Sunnmørsfjordene",
      "Romsdalsfjord", "Sunndalsfjord", "Nordmørsfjordene", "Hemnfjorden",
      "Trondheimsfjorden", "Åfjord", "Namsfjorden", "Follafjorden", "Sørsalten",
      "Bindalsfjorden", "Vellfjorden", "Vefsnfjorden", "Sjona", "Ranafjorden",
      "Beiarfjorden", "Skjerstadfjorden", "Sørfolda", "Ofot_Efjord_Tysfjord",
      "Astafjorden_Salangen", "Malangen", "Balsfjord", "Utlandet"
    ),
    names_to = "region",
    values_to = "andel"
  ) %>%
  # multipliser fangstverdi med andeler for å få fordelt fangst
  mutate(
    fordelt_fangst = fangstverdi * andel
  ) %>%
  # summer total fangst på år, region og størrelseskategori
  group_by(
    Aar,
    region,
    fangstkategori
  ) %>%
  summarise(
    total_fangst = sum(fordelt_fangst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # forandre data tilbake til bredt format med separate kolonner for størrelseskategori i fangsten
  pivot_wider(
    names_from = fangstkategori,
    values_from = total_fangst,
    values_fill = 0
  ) %>%
  select(
    Region = region,
    Aar,
    vekt_u3,
    vekt_37,
    vekt_o7,
    vekt_total,
    ant_u3,
    ant_37,
    ant_o7,
    ant_total
  )

# lagre sjøfangstfordeling til fil
export(
  sjofordeling,
  sprintf("results/sjofangstfordeling_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

#-----------------------------------------------------------------------------------------------------------------
# regn ut samlet sjøfangst til hver elv basert på andel elvene utgjør i regionen fra år til år
#-----------------------------------------------------------------------------------------------------------------
# MARK: Samlet innsig til elv

rapport_alle_elver <- rapport_alle_elver %>%
  # koble til sjøfangsten for hver region hvert år
  left_join(
    select(
      sjofordeling,
      Region,
      Aar,
      region_vektu3 = vekt_u3,
      region_vekt37 = vekt_37,
      region_vekto7 = vekt_o7,
      region_antu3 = ant_u3,
      region_ant37 = ant_37,
      region_anto7 = ant_o7
    ),
    by = c(
      "Region",
      "Aar"
    )
  ) %>%
  # beregn sjøfangst fordelt på hver elv basert på andelen av regionalt innsig
  mutate(
    Sjofangst_vekt_u3 = Andel_region_vekt_u3 * region_vektu3,
    Sjofangst_vekt_37 = Andel_region_vekt_37 * region_vekt37,
    Sjofangst_vekt_o7 = Andel_region_vekt_o7 * region_vekto7,
    Sjofangst_vekt_total = Sjofangst_vekt_u3 +
      Sjofangst_vekt_37 + Sjofangst_vekt_o7,
    Sjofangst_ant_u3 = Andel_region_ant_u3 * region_antu3,
    Sjofangst_ant_37 = Andel_region_ant_37 * region_ant37,
    Sjofangst_ant_o7 = Andel_region_ant_o7 * region_anto7,
    Sjofangst_ant_total = Sjofangst_ant_u3 +
      Sjofangst_ant_37 + Sjofangst_ant_o7,
    Sjofangst_hunn_u3 = Sjofangst_vekt_u3 * Andel_hunn_u3,
    Sjofangst_hunn_37 = Sjofangst_vekt_37 * Andel_hunn_37,
    Sjofangst_hunn_o7 = Sjofangst_vekt_o7 * Andel_hunn_o7,
    Sjofangst_hunn_total = Sjofangst_hunn_u3 +
      Sjofangst_hunn_37 + Sjofangst_hunn_o7
  ) %>%
  # fjern unødvendige kolonner
  select(
    -region_vektu3,
    -region_vekt37,
    -region_vekto7,
    -region_antu3,
    -region_ant37,
    -region_anto7
  ) %>%
  # beregn innsiget i hver størrelsesgruppe for hver elv
  mutate(
    Innsig_sjo_vekt_u3 = Innsig_elv_vekt_u3 + Sjofangst_vekt_u3,
    Innsig_sjo_vekt_37 = Innsig_elv_vekt_37 + Sjofangst_vekt_37,
    Innsig_sjo_vekt_o7 = Innsig_elv_vekt_o7 + Sjofangst_vekt_o7,
    Innsig_sjo_vekt_total = Innsig_sjo_vekt_u3 +
      Innsig_sjo_vekt_37 + Innsig_sjo_vekt_o7,
    Innsig_sjo_ant_u3 = Innsig_elv_ant_u3 + Sjofangst_ant_u3,
    Innsig_sjo_ant_37 = Innsig_elv_ant_37 + Sjofangst_ant_37,
    Innsig_sjo_ant_o7 = Innsig_elv_ant_o7 + Sjofangst_ant_o7,
    Innsig_sjo_ant_total = Innsig_sjo_ant_u3 +
      Innsig_sjo_ant_37 + Innsig_sjo_ant_o7,
    Innsig_sjo_hunn_u3 = Innsig_sjo_vekt_u3 * Andel_hunn_u3,
    Innsig_sjo_hunn_37 = Innsig_sjo_vekt_37 * Andel_hunn_37,
    Innsig_sjo_hunn_o7 = Innsig_sjo_vekt_o7 * Andel_hunn_o7,
    Innsig_sjo_hunn_total = Innsig_sjo_hunn_u3 +
      Innsig_sjo_hunn_37 + Innsig_sjo_hunn_o7,
  ) %>%
  # beregn total beskatningsrate (sjø+elv) for hver elv
  mutate(
    Total_beskatning_vekt_u3 = (Elvefangst_vekt_u3 + Sjofangst_vekt_u3) /
      Innsig_sjo_vekt_u3,
    Total_beskatning_vekt_37 = (Elvefangst_vekt_37 + Sjofangst_vekt_37) /
      Innsig_sjo_vekt_37,
    Total_beskatning_vekt_o7 = (Elvefangst_vekt_o7 + Sjofangst_vekt_o7) /
      Innsig_sjo_vekt_o7,
    Total_beskatning_vekt = (Elvefangst_vekt_u3 + Elvefangst_vekt_37 +
      Elvefangst_vekt_o7 + Sjofangst_vekt_u3 + Sjofangst_vekt_37 +
      Sjofangst_vekt_o7) / Innsig_sjo_vekt_total,
    Total_beskatning_ant_u3 = (Elvefangst_ant_u3 + Sjofangst_ant_u3) /
      Innsig_sjo_ant_u3,
    Total_beskatning_ant_37 = (Elvefangst_ant_37 + Sjofangst_ant_37) /
      Innsig_sjo_ant_37,
    Total_beskatning_ant_o7 = (Elvefangst_ant_o7 + Sjofangst_ant_o7) /
      Innsig_sjo_ant_o7,
    Total_beskatning_ant = (Elvefangst_ant_u3 + Elvefangst_ant_37 +
      Elvefangst_ant_o7 + Sjofangst_ant_u3 + Sjofangst_ant_37 +
      Sjofangst_ant_o7) / Innsig_sjo_ant_total,
    Total_beskatning_hunn_u3 = (Elvefangst_hunn_u3 + Sjofangst_hunn_u3) /
      Innsig_sjo_hunn_u3,
    Total_beskatning_hunn_37 = (Elvefangst_hunn_37 + Sjofangst_hunn_37) /
      Innsig_sjo_hunn_37,
    Total_beskatning_hunn_o7 = (Elvefangst_hunn_o7 + Sjofangst_hunn_o7) /
      Innsig_sjo_hunn_o7,
    Total_beskatning_hunn = (Elvefangst_hunn_u3 + Elvefangst_hunn_37 +
      Elvefangst_hunn_o7 + Sjofangst_hunn_u3 + Sjofangst_hunn_37 +
      Sjofangst_hunn_o7) / Innsig_sjo_hunn_total
  ) %>%
  # erstatt eventuelle NaN eller Inf med 0
  mutate(
    across(
      starts_with("Total_beskatning"),
      ~ifelse(
        is.nan(.) | is.infinite(.),
        NA,
        .
      )
    )
  )

#-----------------------------------------------------------------------------------------------------------------
# koble til og beregn forskjellige forvaltningsrelaterte parametere til de ulike elvene
#-----------------------------------------------------------------------------------------------------------------
# MARK: Forvaltningsvariable

rapport_alle_elver <- rapport_alle_elver %>%
  # Koble til prosent_sannsynlighet_gbm fra simul_kghunnlaks
  left_join(
    select(
      simul_kghunnlaks,
      VdrNr,  # Note: In simul_kghunnlaks it's VdrNr (not Vdrnr)
      Aar,
      prosent_sannsynlighet_gbm,
      prosent_sanns_siste_fire,
      prosent_maaloppnaaelse,
      prosent_maalopp_siste_fire,
      prosent_maaloppnaaelse_utrunk,
      pros_maalopp_siste_fire_utrunk,
    ),
    by = c(
      "VdrNr",
      "Aar"
    )
  ) %>%
  left_join(
    select(
      elveliste,
      VdrNr,
      Normalbeskatningsregion
    ),
    by = "VdrNr"
  ) %>%
  # Konverter prosent til desimaltall for konsistens med andre prosentvariabler
  mutate(
    prosent_sanns_gbm = prosent_sannsynlighet_gbm / 100,
    prosent_sanns_siste4 = prosent_sanns_siste_fire / 100,
    prosent_maalopp = prosent_maaloppnaaelse / 100,
    prosent_maalopp_siste4 = prosent_maalopp_siste_fire / 100,
    prosent_maalopp_utrunk = prosent_maaloppnaaelse_utrunk / 100,
    prosent_maalopp_siste4_utrunk = pros_maalopp_siste_fire_utrunk / 100,

    # Fjern originalene for å unngå forvirring
    prosent_sannsynlighet_gbm = NULL,
    prosent_sanns_siste_fire = NULL,
    prosent_maaloppnaaelse = NULL,
    prosent_maalopp_siste_fire = NULL,
    prosent_maaloppnaaelse_utrunk = NULL,
    pros_maalopp_siste_fire_utrunk = NULL
  ) %>%
  # beregn maksimal bærekraftig beskatning (%), fiskbart overskudd (kg) og overbeskatning
  mutate(
    hostbart_overskudd_kg = ifelse(
      GBM > 0,
      ifelse(
        Innsig_sjo_hunn_total > GBM,
        Innsig_sjo_hunn_total - GBM,
        0
      ),
      NA
    ),
    hostbart_prosent_gbm = ifelse(
      GBM > 0,
      ifelse(
        Innsig_sjo_hunn_total > GBM,
        Innsig_sjo_hunn_total / GBM,
        0
      ),
      NA
    ),
    # beregn høstbart overskudd i prosent av innsig, dette tilsvarer
    # maksimal bærekraftig beskatning i prosent
    hostbart_prosent_innsig = ifelse(
      GBM > 0,
      ifelse(
        Innsig_sjo_hunn_total > GBM,
        hostbart_overskudd_kg / Innsig_sjo_hunn_total,
        0
      ),
      NA
    ),
    maks_baerekraft_total = hostbart_prosent_innsig,
    maks_baerekraft_elv = ifelse(
      GBM > 0,
      ifelse(
        Innsig_elv_hunn_total > GBM,
        (Innsig_elv_hunn_total - GBM) / Innsig_elv_hunn_total,
        0
      ),
      NA
    ),
    overbeskatning = ifelse(
      GBM > 0,
      ifelse(
        Gyting_hunn_total > GBM,
        0, # GBM nådd, ingen overbeskatning
        ifelse(
          Innsig_sjo_hunn_total > GBM,
          (GBM - Gyting_hunn_total) / GBM, # bare fangst under GBM er overbeskatning
          (Elvefangst_hunn_total + Sjofangst_hunn_total) / GBM # all fangst er overbeskatning
        )
      ),
      NA # ingen verdi dersom vassdrag mangler GBM
    )
  )

#-----------------------------------------------------------------------------------------------------------------
# fordel innsiget på sjøaldersgrupper
#-----------------------------------------------------------------------------------------------------------------
rapport_alle_elver <- rapport_alle_elver %>%
  left_join(
    select(
      region_andel_1sw,
      Aar = InnsigsAar,
      Normalbeskatningsregion,
      andel_1sw = Andel1SW
    ),
    by = c(
      "Aar",
      "Normalbeskatningsregion"
    )
  ) %>%
  mutate(
    Innsig_sjo_1sw_ant = Innsig_sjo_ant_u3 * andel_1sw,
    Innsig_sjo_2sw_ant = Innsig_sjo_ant_u3 * (1 - andel_1sw) + Innsig_sjo_ant_37,
    Innsig_sjo_msw_ant = Innsig_sjo_ant_o7
  )

#-----------------------------------------------------------------------------------------------------------------
# prosedyre for å beregne normalbeskatning og normalt høstbart overskudd
#-----------------------------------------------------------------------------------------------------------------
# MARK: Normalbeskatning

rapport_alle_elver <- rapport_alle_elver %>%
  left_join(
    select(
      elveliste,
      VdrNr,
      hunnlaks_alder = FemSeaAge,
      smolt_alder = SmoltAge,
      # inkluder nedenfor er nå erstattet med inkluder_til_norm i inputfilene for hvert vassdrag
      inkluder_fra_elveliste = Inkluder
    ),
    by = "VdrNr"
  ) %>%
  mutate(
    # regn ut årstall for foreldrene til innsiget i en elv
    foreldre_aar = Aar - hunnlaks_alder - smolt_alder - 1,
    # sjekk om foreldreåret er tidligere enn startår for fordelingen
    foreldre_aar = ifelse(
      foreldre_aar < start_aar,
      0,
      foreldre_aar
    ),
    # initier en variabel for bestandsstørrelse (måloppnåelsen) til foreldrene
    foreldre_bestand = NA_real_
  )

# gå gjennom hver rad i datasettet og sett inn bestandsstørrelsen til foreldrene
for (i in seq_len(nrow(rapport_alle_elver))) {
  if (rapport_alle_elver$foreldre_aar[i] > 0 && rapport_alle_elver$Inkluder_til_Norm[i] == 1) {
    if (rapport_alle_elver$hunnlaks_alder[i] == 1.5) { # spesialtilfelle dersom hunnlaksen er en blanding av 1SW og 2SW
      antall_aar_tilbake <- rapport_alle_elver$Aar[i] - rapport_alle_elver$foreldre_aar[i] - 0.5
      if (rapport_alle_elver$GytingSimulert[i - antall_aar_tilbake] && rapport_alle_elver$GytingSimulert[i - antall_aar_tilbake + 1]) {
        # dersom det er gyting i begge år, brukes gjennomsnittet av måloppnåelsen i de to årene
        rapport_alle_elver$foreldre_bestand[i] <- (rapport_alle_elver$prosent_maalopp_utrunk[i - antall_aar_tilbake] +
                                                     rapport_alle_elver$prosent_maalopp_utrunk[i - antall_aar_tilbake + 1]) / 2
      } else {
        rapport_alle_elver$foreldre_bestand[i]  <- NA_real_
      }
    } else {
      antall_aar_tilbake <- rapport_alle_elver$Aar[i] - rapport_alle_elver$foreldre_aar[i]
      if (rapport_alle_elver$GytingSimulert[i - antall_aar_tilbake]) {
        rapport_alle_elver$foreldre_bestand[i]  <- rapport_alle_elver$prosent_maalopp_utrunk[i - antall_aar_tilbake]
      } else {
        rapport_alle_elver$foreldre_bestand[i]  <- NA_real_
      }
    }
  } else {
    rapport_alle_elver$foreldre_bestand[i] <- NA_real_
  }
}

# lag en liste over elvene med høy måloppnåelse i foreldregenerasjonen og med
# flagget inkluder_til_norm satt til 1
vassdrag_normal <- rapport_alle_elver %>%
  filter(
    foreldre_bestand > 0.9 & Inkluder_til_Norm == 1
  ) %>%
  select(
    Normalbeskatningsregion,
    VdrNr,
    Vassdrag,
    Aar,
    foreldre_bestand,
    prosent_maalopp_utrunk,
    hostbart_prosent_gbm,
    hostbart_prosent_innsig
  ) %>%
  # sorter etter normalbeskatningsregion og år
  arrange(
    Normalbeskatningsregion,
    Aar
  )

# eksporter listen over vassdrag til en csv-fil
export(
  vassdrag_normal,
  sprintf("results/normalbeskatningsvassdrag_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# beregn årlig median av hostbart_prosent_gbm for hver normalbeskatningsregion
regionliste <- unique(vassdrag_normal$Normalbeskatningsregion)
antall_regioner <- length(regionliste)

# lag en dataframe for å lagre de årlige medianene
regional_median_hostbart <- vassdrag_normal %>%
  group_by(Normalbeskatningsregion, Aar) %>%
  summarise(
    median_hostbart_prosent_gbm = median(hostbart_prosent_gbm, na.rm = TRUE),
    median_hostbart_prosent_innsig = median(hostbart_prosent_innsig, na.rm = TRUE),
    antall_vassdrag = n(),
    .groups = "drop"
  )

# eksporter regional median hostbart overskudd til en csv-fil
export(
  regional_median_hostbart,
  sprintf("results/regional_median_hostbart_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)

# koble normal beskatning tilbake til rapporteringen og beregn høstingspotensiale (høstbart overskudd
# i prosent delt på normalbeskatningen i regionen)
rapport_alle_elver <- rapport_alle_elver %>%
  left_join(
    select(
      regional_median_hostbart,
      Aar,
      Normalbeskatningsregion,
      normalbeskatning = median_hostbart_prosent_innsig
    ),
    by = c(
      "Aar",
      "Normalbeskatningsregion"
    )
  ) %>%
  mutate(
    hostingspotensiale = hostbart_prosent_innsig / normalbeskatning
  ) %>%
  mutate(
    Sanns_kat = case_when(
      prosent_sanns_gbm > 0.75 ~ "D",
      prosent_sanns_gbm >= 0.4 ~ "C",
      (prosent_sanns_gbm < 0.4 & hostbart_overskudd_kg > 0)  ~ "B",
      hostbart_overskudd_kg == 0 ~ "A"
    )
  ) %>%
  mutate(
    Norm_kat = case_when(
      (prosent_sanns_gbm >= 0.9 & hostingspotensiale >= 0.9) ~ "E",
      (prosent_sanns_gbm >= 0.9 & hostingspotensiale >= 0.8) ~ "D",
      (prosent_sanns_gbm >= 0.9 & hostingspotensiale >= 0.6) ~ "C",
      (prosent_sanns_gbm >= 0.9 & hostingspotensiale < 0.6) ~ "B",
      (prosent_sanns_gbm >= 0.8 & hostingspotensiale >= 0.9) ~ "D",
      (prosent_sanns_gbm >= 0.8 & hostingspotensiale >= 0.8) ~ "C",
      (prosent_sanns_gbm >= 0.8 & hostingspotensiale >= 0.6) ~ "B",
      (prosent_sanns_gbm >= 0.8 & hostingspotensiale < 0.6) ~ "A",
      (prosent_sanns_gbm >= 0.7 & hostingspotensiale >= 0.9) ~ "C",
      (prosent_sanns_gbm >= 0.7 & hostingspotensiale >= 0.8) ~ "B",
      (prosent_sanns_gbm >= 0.7 & hostingspotensiale < 0.8) ~ "A",
      (prosent_sanns_gbm >= 0.5 & hostingspotensiale >= 0.9) ~ "B",
      (prosent_sanns_gbm >= 0.5 & hostingspotensiale < 0.8) ~ "A",
      (prosent_sanns_gbm < 0.5 & hostingspotensiale >= 0) ~ "A"
    )
  )

# eksporter det kombinerte ferdige datasettet for elvene til en CSV-fil
export(
  rapport_alle_elver,
  sprintf("results/rapport_alle_elver_ferdig_%d-%d.csv", start_aar, slutt_aar),
  sep = ";",
  dec = ".",
  bom = TRUE
)
