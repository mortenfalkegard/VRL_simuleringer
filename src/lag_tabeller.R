# Script for summarizing the dataset on salmon distribution

library(tidyverse)
library(rio)
#library(gt)

options(scipen = 999)

resultat_fordeling  <- import("results/resultat_fordeling_1993-2024.csv")
sjofangst <- import("results/sjofangst_region_1993-2024.csv")
sjofangst_ford <- import("results/sjofangstfordeling_1993-2024.csv")
elveliste <- import("data/elveliste.csv")

start_aar <- 1993
slutt_aar <- 2024

elver_region <- resultat_fordeling %>%
  filter(Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    unique_rivers = n_distinct(Vassdrag)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = unique_rivers,
    values_fill = list(unique_rivers = 0)
)

simulerte_elver_region <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    unique_rivers = n_distinct(Vassdrag)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = unique_rivers,
    values_fill = list(unique_rivers = 0)
  )

sum_gbm_region <- resultat_fordeling %>%
  filter(Delbestand == 0)  %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_spawning_target = sum(GBM, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_spawning_target,
    values_fill = list(sum_spawning_target = 0)
  )

sum_sim_gbm_region <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_GBM_sim = sum(GBM, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_GBM_sim,
    values_fill = list(sum_GBM_sim = 0)
  )

sanns_naad_gbm_region <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    avg_prob_GBM = weighted.mean(Sannsyn_GBM, GBM, na.rm = TRUE) * 100 # convert to percentage
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = avg_prob_GBM,
    values_fill = list(avg_prob_GBM = 0)
  )

vektet_oppnaad_snitt_region <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    weighted_avg_perc_attain = weighted.mean(ProsOppnaadTrunk, GBM, na.rm = TRUE) * 100 # convert to percentage
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = weighted_avg_perc_attain,
    values_fill = list(weighted_avg_perc_attain = 0)
  )

resultat_fordeling <- resultat_fordeling %>%
  mutate(
    Innsig_total = Innsig_sjo_vekt_u3 + Innsig_sjo_vekt_37 + Innsig_sjo_vekt_o7,
  )

samlet_innsig_region <- resultat_fordeling %>%
  filter(Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    PFA = sum(Innsig_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = PFA,
    values_fill = list(PFA = 0)
  )

resultat_fordeling <- resultat_fordeling %>%
  mutate(
    samlet_andel_hunn = if_else(
      GytingSimulert == 1,
      ((Innsig_sjo_vekt_u3 * Andel_hunn_u3) +
       (Innsig_sjo_vekt_37 * Andel_hunn_37) +
       (Innsig_sjo_vekt_o7 * Andel_hunn_o7)) / Innsig_total,
      NA_real_
    ),
    Hostbart_overskudd_tot = if_else(
      GytingSimulert == 1,
      pmax(Innsig_total - (GBM / samlet_andel_hunn), 0),
      NA_real_
    )
  )
#export(resultat_fordeling, "results/resultat_fordeling_2002-2024-deb.csv", sep = ";", dec = ".", bom = TRUE)

fiskbart_overskudd_region <- resultat_fordeling %>%
  filter(Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    expl_surplus = sum(Hostbart_overskudd_tot, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = expl_surplus,
    values_fill = list(expl_surplus = 0)
  )

sjofangst_region <- sjofangst %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_sjofangst = sum(Vekt_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_sjofangst, values_fill = list(sum_sjofangst = 0)
  )

sjofangst_fordelt <- sjofangst_ford %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_sjofangst_ford = sum(Vekt_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_sjofangst_ford, values_fill = list(sum_sjofangst_ford = 0)
  )

sjofangst_prosent_innsig <- sjofangst_ford %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_sjofangst_ford = sum(Vekt_total, na.rm = TRUE)
  ) %>%
  mutate(Aar = as.character(Aar)) %>%
  left_join(samlet_innsig_region %>% 
              pivot_longer(cols = -Region,
                           names_to = "Aar",
                           values_to = "PFA")) %>%
  mutate(percent = (sum_sjofangst_ford / PFA) * 100) %>%
  select(Region, Aar, percent) %>%
  pivot_wider(
    names_from = Aar,
    values_from = percent,
    values_fill = list(percent = 0)
  )

resultat_fordeling <- resultat_fordeling %>%
  mutate(
    Elvefangst_total = Fangst_elv_vekt_u3 + Fangst_elv_vekt_37 + Fangst_elv_vekt_o7,
  )

elvefangst_region <- resultat_fordeling %>%
  filter(Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_elvefangst = sum(Elvefangst_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_elvefangst,
    values_fill = list(sum_elvefangst = 0)
  )

elvefangst_prosent_innsig <- resultat_fordeling %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_elvefangst = sum(Elvefangst_total, na.rm = TRUE)
  ) %>%
  mutate(Aar = as.character(Aar)) %>%
  left_join(samlet_innsig_region %>% 
              pivot_longer(cols = -Region,
                           names_to = "Aar",
                           values_to = "PFA")) %>%
  mutate(percent = (sum_elvefangst / PFA) * 100) %>%
  select(Region, Aar, percent) %>%
  pivot_wider(
    names_from = Aar,
    values_from = percent,
    values_fill = list(percent = 0)
  )

resultat_fordeling <- resultat_fordeling %>%
  mutate(
    Elveinnsig_total = Innsig_elv_vekt_u3 + Innsig_elv_vekt_37 + Innsig_elv_vekt_o7,
  )

elveinnsig_region <- resultat_fordeling %>%
  filter(Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_elveinnsig = sum(Elveinnsig_total, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_elveinnsig,
    values_fill = list(sum_elveinnsig = 0)
  )

elvefangst_prosent_elveinnsig <- resultat_fordeling %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_elvefangst = sum(Elvefangst_total, na.rm = TRUE)
  ) %>%
  mutate(Aar = as.character(Aar)) %>%
  left_join(elveinnsig_region %>% 
              pivot_longer(cols = -Region,
                           names_to = "Aar",
                           values_to = "PFA")) %>%
  mutate(percent = (sum_elvefangst / PFA) * 100) %>%
  select(Region, Aar, percent) %>%
  pivot_wider(
    names_from = Aar,
    values_from = percent,
    values_fill = list(percent = 0)
  )

sim_gytebestand_region <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    sum_gytebestand = sum(Gyting_hunn, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = sum_gytebestand,
    values_fill = list(sum_gytebestand = 0)
  )

resultat_fordeling <- resultat_fordeling %>%
  mutate(
    manglende_gytere = if_else(GytingSimulert == 1, pmax(GBM - Gyting_hunn, 0), NA_real_)
  )

gytefisk_mangler <- resultat_fordeling %>%
  filter(GytingSimulert == 1, Delbestand == 0) %>%
  group_by(Region, Aar) %>%
  summarise(
    diff = sum(manglende_gytere, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = Aar,
    values_from = diff,
    values_fill = list(diff = 0)
  )

#gytefisk_mangler <- sim_gytebestand_region %>%
#  pivot_longer(
#    cols = -Region,
#    names_to = "Aar",
#    values_to = "sim_gytebestand"
#  ) %>%
# left_join(
#    sum_sim_gbm_region %>%
#      pivot_longer(
#        cols = -Region,
#        names_to = "Aar",
#        values_to = "sum_sim_gbm"
#      ),
#    by = c("Region", "Aar")
#  ) %>%
#  mutate(
#    diff = pmax(sum_sim_gbm - sim_gytebestand, 0)
#  ) %>%
#  select(Region, Aar, diff) %>%
#  pivot_wider(
#    names_from = Aar,
#    values_from = diff,
#    values_fill = list(diff = 0)
#  )

regionliste <- unique(resultat_fordeling$Region)
antall_regioner  <- length(regionliste)

# lag tallrekke fra start_aar til slutt_aar
utvalgte_aar <- seq(start_aar, slutt_aar)
aar_kolonner <- as.character(utvalgte_aar)

for (i in 1:antall_regioner) {

  # sett opp de ulike radene i rapporteringen
  rad1 <- elver_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Antall laksevassdrag") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad1) == 0) {
    rad1 <- tibble(
      Type = "Antall laksevassdrag",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad2 <- simulerte_elver_region %>%
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Antall simulerte laksevassdrag") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad2) == 0) {
    rad2 <- tibble(
      Type = "Antall simulerte laksevassdrag",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad3 <- sum_gbm_region %>%
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Samlet gytebestandsmål alle vassdrag (kg hunner)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad3) == 0) {
    rad3 <- tibble(
      Type = "Samlet gytebestandsmål alle vassdrag (kg hunner)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad4 <- sum_sim_gbm_region %>%
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Samlet gytebestandsmål i simulerte vassdrag (kg hunner)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad4) == 0) {
    rad4 <- tibble(
      Type = "Samlet gytebestandsmål i simulerte vassdrag (kg hunner)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad5 <- sanns_naad_gbm_region %>%
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Gjennomsnittlig veid sannsynlighet for oppnåelse") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad5) == 0) {
    rad5 <- tibble(
      Type = "Gjennomsnittlig veid sannsynlighet for oppnåelse",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad6 <- vektet_oppnaad_snitt_region %>%
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Gjennomsnittlig veid prosentvis oppnåelse") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad6) == 0) {
    rad6 <- tibble(
      Type = "Gjennomsnittlig veid prosentvis oppnåelse",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad7 <- samlet_innsig_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Samlet innsig (kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad7) == 0) {
    rad7 <- tibble(
      Type = "Samlet innsig (kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad8 <- fiskbart_overskudd_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Høstbart overskudd (kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad8) == 0) {
    rad8 <- tibble(
      Type = "Høstbart overskudd (kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad9 <- sjofangst_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Sjøfangst i regionen (kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad9) == 0) {
    rad9 <- tibble(
      Type = "Sjøfangst i regionen (kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad10 <- sjofangst_fordelt %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Totalfangst i sjø på laks fra elvene i regionen (kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad10) == 0) {
    rad10 <- tibble(
      Type = "Totalfangst i sjø på laks fra elvene i regionen (kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad11 <- sjofangst_prosent_innsig %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Sjøfangst i prosent av innsig") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad11) == 0) {
    rad11 <- tibble(
      Type = "Sjøfangst i prosent av innsig",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad12 <- elvefangst_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Elvefangst i regionen (avlivet, kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad12) == 0) {
    rad12 <- tibble(
      Type = "Elvefangst i regionen (avlivet, kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad13 <- elvefangst_prosent_innsig %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Elvefangst i % av innsig") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad13) == 0) {
    rad13 <- tibble(
      Type = "Elvefangst i % av innsig",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad14 <- elveinnsig_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Innsig til elvene i regionen (kg)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad14) == 0) {
    rad14 <- tibble(
      Type = "Innsig til elvene i regionen (kg)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad15 <- elvefangst_prosent_elveinnsig %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Elvefangst i % av innsig til elvene") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad15) == 0) {
    rad15 <- tibble(
      Type = "Elvefangst i % av innsig til elvene",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad16 <- sim_gytebestand_region %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Gytebestand i simulerte elver (kg hunner)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad16) == 0) {
    rad16 <- tibble(
      Type = "Gytebestand i simulerte elver (kg hunner)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  rad17 <- gytefisk_mangler %>% 
    filter(Region == regionliste[i]) %>%
    select(all_of(aar_kolonner)) %>%
    mutate(Type = "Mangler gytefisk i simulerte elver (kg hunner)") %>%
    ungroup() %>%
    select(Type, all_of(aar_kolonner))
  if (nrow(rad17) == 0) {
    rad17 <- tibble(
      Type = "Mangler gytefisk i simulerte elver (kg hunner)",
      !!!setNames(rep(0, length(aar_kolonner)), aar_kolonner)
    )
  }

  # sett sammen radene til en tibble
  rapportering <- bind_rows(
    rad1, rad2, rad3, rad4, rad5, rad6, rad7, rad8, rad9, rad10, rad11, rad12, rad13, rad14, rad15, rad16, rad17
  )

  rapport_filnavn <- paste("results/tables/rapportering_", regionliste[i], "_", start_aar, "-", slutt_aar, ".csv", sep = "")
  export(rapportering, rapport_filnavn, sep = ";", dec = ".", bom = TRUE)
}
