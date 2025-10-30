#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Dette skriptet tar utgangspunkt i simuleringene av gytebestand og innsigsberegningen, og lager figurene
# som brukes på innsynsløsningen på nettsiden til vitenskapsrådet.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)
library(slider)

options(scipen = 999)

resultat_fordeling <- import("results/rapport_alle_elver_ferdig_1993-2024.csv")
elveliste <- import("data/elveliste.csv")

antall_elver <- nrow(elveliste)

statusfarger <- c(A = "#D00000", B = "orange", C = "yellow", D = "green3")
norm_farger <- c(A = "#D00000", B = "orange", C = "yellow", D = "green3", E = "green4")

for (m in 1:antall_elver) {
  if (elveliste$GytingSim[m]) {
    simulering_filnavn <- paste("results/vassdrag/", elveliste[m, "Filnavn"], "-gyting_hunn_kg_totalt.csv", sep = "")
    simulering_kghuntot <- import(simulering_filnavn)

    df <- filter(resultat_fordeling, VdrNr == elveliste$VdrNr[m]) %>%
      mutate(GBM_lav = elveliste$GBM_lav[m]) %>%
      mutate(GBM_hoy = elveliste$GBM_hoy[m])

    df$Sanns_kat <- factor(df$Sanns_kat, levels = c("A", "B", "C", "D"))

    df$Norm_kat <- factor(df$Norm_kat, levels = c("A", "B", "C", "D", "E"))

    antall_aar <- length(df$Aar)

    df <- df %>%
      mutate(Simulert = 0) %>%
      mutate(Gyting_ant_total = Gyting_ant_u3 + Gyting_ant_37 + Gyting_ant_o7)

    df$Simulert <- ifelse(df$Gyting_hunn_total == 0, 0, 1)

    kategorisering <- as.vector(df$Sanns_kat)
    Aar <- as.vector(df$Aar)
    kategorigrunnlag <- cbind(Aar, kategorisering)
    simulering_kghuntot <- merge(simulering_kghuntot, kategorigrunnlag, by = "Aar")
    simulering_kghuntot$kategorisering <- factor(simulering_kghuntot$kategorisering, levels = c("A", "B", "C", "D"))

    symbolstorrelse <- 8

    gbm_data <- data.frame(y = c(df$GBM_lav[1], df$GBM[1], df$GBM_hoy[1]), type = factor(c(2, 1, 2)),
                           stringsAsFactors = FALSE)

    #================================================
    # figur 1, boksplott av gytebestand
    #================================================

    # finn maksverdi for gyting_hunn_kg_totalt i hver årgang
    maks_gyting_aar <- simulering_kghuntot %>%
      group_by(Aar) %>%
      summarize(maks_gyting = if (all(is.na(gyting_hunn_kg_totalt))) {
        NA_real_
      } else {
        max(gyting_hunn_kg_totalt, na.rm = TRUE)
      }) %>%
      mutate(maks_gyting = ifelse(is.na(maks_gyting), 0, maks_gyting))

    # finn året med høyest gyting_hunn_kg_totalt
    maks_aar <- maks_gyting_aar %>%
      filter(maks_gyting == max(maks_gyting)) %>%
      pull(Aar)

    data_for_maks_aar <- simulering_kghuntot %>%
      filter(Aar == maks_aar) %>%
      pull(gyting_hunn_kg_totalt)

    # sett en passe maksverdi for x-aksen i figurene basert på høyeste gyting_hunn_kg_totalt
    maks_x_akse_p1 <- ceiling((quantile(data_for_maks_aar, 0.75, na.rm = TRUE) +
      1.5 * IQR(data_for_maks_aar, na.rm = TRUE)) / 100) * 100

    p1_alpha <- ggplot(simulering_kghuntot, aes(x = factor(Aar), y = gyting_hunn_kg_totalt)) +
      theme_light() +
      # Join the VisSimulert information from df to simulering_kghuntot
      geom_boxplot(data = simulering_kghuntot %>%
                     left_join(select(df, Aar, VisSimulert), by = "Aar"),
                   aes(fill = kategorisering, alpha = VisSimulert),
                   outlier.shape = NA, na.rm = TRUE) +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      scale_fill_manual(values = statusfarger) +
      # Set up alpha scale - 0.3 for years with VisSimulert=0, 1.0 for VisSimulert=1
      scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      ylab("Gytebestand hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      theme(legend.position = "none") +
      scale_y_continuous(
        labels = scales::label_number(big.mark = " ", accuracy = 1),
        # Calculate upper limit based on IQR
        limits = c(0, max(
          maks_x_akse_p1,
          df$GBM_hoy[1] * 1.2  # Ensure at least 20% above upper GBM bound
        ))
      ) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    # ta bort data med VisSimulert == 0 fra simulering_kghuntot for å unngå at de påvirker boksplottet
    simulering_kghuntot <- simulering_kghuntot %>%
      left_join(select(df, Aar, VisSimulert), by = "Aar") %>%
      mutate(gyting_hunn_kg_totalt = ifelse(VisSimulert == 0, NA, gyting_hunn_kg_totalt))

    # finn ny maksverdi for gyting_hunn_kg_totalt i hver årgang etter at data med VisSimulert == 0 er fjernet
    maks_gyting_aar <- simulering_kghuntot %>%
      group_by(Aar) %>%
      summarize(maks_gyting = if (all(is.na(gyting_hunn_kg_totalt))) {
        NA_real_
      } else {
        max(gyting_hunn_kg_totalt, na.rm = TRUE)
      }) %>%
      mutate(maks_gyting = ifelse(is.na(maks_gyting), 0, maks_gyting))

    # finn året med høyest gyting_hunn_kg_totalt
    maks_aar <- maks_gyting_aar %>%
      filter(maks_gyting == max(maks_gyting)) %>%
      pull(Aar)

    data_for_maks_aar <- simulering_kghuntot %>%
      filter(Aar == maks_aar) %>%
      pull(gyting_hunn_kg_totalt)

    # sett en passe maksverdi for x-aksen i figurene basert på høyeste gyting_hunn_kg_totalt
    maks_x_akse_p1 <- ceiling((quantile(data_for_maks_aar, 0.75, na.rm = TRUE) +
      1.5 * IQR(data_for_maks_aar, na.rm = TRUE)) / 100) * 100

    p1 <- ggplot(simulering_kghuntot, aes(x = factor(Aar), y = gyting_hunn_kg_totalt)) +
      theme_light() +
      geom_boxplot(aes(fill = kategorisering), outlier.shape = NA, na.rm = TRUE) +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      scale_fill_manual(values = statusfarger) +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      ylab("Gytebestand hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      theme(legend.position = "none") +
      scale_y_continuous(
        labels = scales::label_number(big.mark = " ", accuracy = 1),
        # Calculate upper limit based on IQR
        limits = c(0, max(
          maks_x_akse_p1,
          df$GBM_hoy[1] * 1.2  # Ensure at least 20% above upper GBM bound
        ))
      ) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    #================================================
    # figur 2, barplot av måloppnåelse
    #================================================
    df_2 <- df %>%
      select(Aar, prosent_maalopp, Sanns_kat, VisSimulert) %>%
      add_row(Aar = 2002, prosent_maalopp = 0, Sanns_kat = "A") %>%
      add_row(Aar = 2002, prosent_maalopp = 0, Sanns_kat = "B") %>%
      add_row(Aar = 2002, prosent_maalopp = 0, Sanns_kat = "C") %>%
      add_row(Aar = 2002, prosent_maalopp = 0, Sanns_kat = "D")

    df_2$prosent_maalopp[df_2$VisSimulert == 0] <- 0

    p2 <- ggplot(df_2, aes(x = factor(Aar), y = prosent_maalopp)) +
      theme_light() +
      geom_col(aes(fill = Sanns_kat)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
      scale_fill_manual(values = c("#D00000", "orange", "yellow", "green3"),
                        labels = c("ikke høstbart overskudd", "<40 %, høstbart overskudd", "40-75 %", ">75 %"),
                        drop = FALSE,
                        name = "% sannsynlighet for nådd GBM") +
      ylab("Prosent oppnåelse av GBM") +
      xlab("År") +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

    #================================================
    # figur 3, innsig fordelt
    #================================================
    df_p3 <- df %>%
      select(Aar, Sjofangst_ant_total, Elvefangst_ant_total, Gyting_ant_total, VisSimulert) %>%
      pivot_longer(cols = c("Sjofangst_ant_total", "Elvefangst_ant_total", "Gyting_ant_total"),
                   names_to = "Omraade", values_to = "Antall_fisk")
    df_p3$Omraade[df_p3$Omraade == "Sjofangst_ant_total"] <- "Sjøfiske"
    df_p3$Omraade[df_p3$Omraade == "Elvefangst_ant_total"] <- "Elvefiske"
    df_p3$Omraade[df_p3$Omraade == "Gyting_ant_total"] <- "Gytebestand"
    df_p3$Omraade <- factor(df_p3$Omraade, levels = c("Sjøfiske", "Elvefiske", "Gytebestand"))

    p3_alpha <- ggplot(df_p3, aes(x = factor(Aar), y = Antall_fisk, alpha = VisSimulert)) +
      geom_col(aes(fill = Omraade)) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      # Add custom alpha scale for transparent years without simulation
      scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
      ylab("Innsig laks (antall)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", accuracy = 1)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    df_p3$Antall_fisk[df_p3$VisSimulert == 0] <- 0
    p3 <- ggplot(df_p3, aes(x = factor(Aar), y = Antall_fisk)) +
      geom_col(aes(fill = Omraade)) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      ylab("Innsig laks (antall)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", accuracy = 1)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    #================================================
    # figur 4, innsig hunn vs GBM
    #================================================
    df_p4 <- df %>%
      select(Aar, Sjofangst_hunn_total, Elvefangst_hunn_total, Gyting_hunn_total, VisSimulert) %>%
      pivot_longer(cols = c("Sjofangst_hunn_total", "Elvefangst_hunn_total", "Gyting_hunn_total"),
                   names_to = "Omraade", values_to = "Vekt_hunnlaks")
    df_p4$Omraade[df_p4$Omraade == "Sjofangst_hunn_total"] <- "Sjøfiske"
    df_p4$Omraade[df_p4$Omraade == "Elvefangst_hunn_total"] <- "Elvefiske"
    df_p4$Omraade[df_p4$Omraade == "Gyting_hunn_total"] <- "Gytebestand"
    df_p4$Omraade <- factor(df_p4$Omraade, levels = c("Sjøfiske", "Elvefiske", "Gytebestand"))

    p4_alpha <- ggplot(df_p4, aes(x = factor(Aar), y = Vekt_hunnlaks, alpha = VisSimulert)) +
      geom_col(aes(fill = Omraade)) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      # Add custom alpha scale for transparent years without simulation
      scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
      ylab("Innsig hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      scale_linetype_manual(values = 1:2, labels = c("Gytebestandsmål", "Øvre/nedre grense"), name = "") +
      guides(fill = guide_legend(override.aes = list(linetype = "blank"))) +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", accuracy = 1)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    df_p4$Vekt_hunnlaks[df_p4$VisSimulert == 0] <- 0
    p4 <- ggplot(df_p4, aes(x = factor(Aar), y = Vekt_hunnlaks)) +
      geom_col(aes(fill = Omraade)) +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      ylab("Innsig hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      scale_linetype_manual(values = 1:2, labels = c("Gytebestandsmål", "Øvre/nedre grense"), name = "") +
      guides(fill = guide_legend(override.aes = list(linetype = "blank"))) +
      scale_y_continuous(labels = scales::label_number(big.mark = " ", accuracy = 1)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)])

    #================================================
    # figur 5, høstingspotensiale
    #================================================
    df_5 <- df %>%
      select(
        Aar,
        hostingspotensiale,
        prosent_sanns_gbm,
        Norm_kat,
        VisSimulert
      ) %>%
      mutate(hostingspotensiale = ifelse(VisSimulert == 0, 0, hostingspotensiale)) %>%
      arrange(Aar) %>%
      mutate(
        hostingspotensiale_snitt4 = slide_dbl(
          hostingspotensiale,
          ~mean(.x, na.rm = TRUE),
          .before = 3,
          .complete = TRUE
        )
      )

    p5 <- ggplot(df_5, aes(x = factor(Aar), y = hostingspotensiale)) +
      theme_light() +
      geom_col(aes(fill = Norm_kat)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
      scale_fill_manual(values = c("#D00000", "orange", "yellow", "green3", "green4"),
                        labels = c("<40 %, ikke overskudd", "<40 %, høstbart overskudd", "40-75 %", ">75 %", "100 %"),
                        drop = FALSE,
                        name = "% sannsynlighet for nådd GBM") +
      ylab("Høstingsnivå i prosent av normalt") +
      xlab("År") +
      theme(legend.position = "none") +
      geom_text(data = subset(df, VisSimulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

    p6 <- ggplot(df_5, aes(x = prosent_sanns_gbm, y = hostingspotensiale)) +
      theme_light() +
      geom_point(aes(color = Norm_kat), size = 3) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_color_manual(values = c("#D00000", "orange", "yellow", "green3", "green4"),
                         labels = c("<40 %, ikke overskudd", "<40 %, høstbart overskudd", "40-75 %", ">75 %", "100 %"),
                         drop = FALSE,
                         name = "% sannsynlighet for nådd GBM") +
      ylab("Høstingsnivå i prosent av normalt") +
      xlab("Sannsynlighet for oppnådd gytebestandsmål") +
      theme(legend.position = "none") +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

    df_6 <- df_5 %>%
      filter(!is.na(hostingspotensiale_snitt4)) %>%
      arrange(Aar) %>%
      mutate(
        xend = lead(factor(Aar)),
        yend = lead(hostingspotensiale_snitt4)
      )

    p7 <- ggplot(df_6, aes(x = factor(Aar), y = hostingspotensiale_snitt4, group = 1)) +
      theme_light() +
      # Add line segments with color varying by Norm_kat
      geom_segment(aes(xend = xend, yend = yend, color = Norm_kat),
                   size = 1.5, na.rm = TRUE) +
      # Add points to mark each year's value
      geom_point(aes(color = Norm_kat), size = 3, na.rm = TRUE) +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
      scale_color_manual(values = c("#D00000", "orange", "yellow", "green3", "green4"),
                         labels = c("<40 %, ikke overskudd", "<40 %, høstbart overskudd", "40-75 %", ">75 %", "100 %"),
                         drop = FALSE,
                         name = "% sannsynlighet for nådd GBM") +
      ylab("4-års gjennomsnitt høstingsnivå") +
      xlab("År") +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

    #================================================
    # patchwork
    #================================================
    filnavn <- paste("results/figures/", df$VdrNr[1], "_", df$Vassdrag[1], "_",
                     df$Aar[max(antall_aar)], ".png", sep = "")
    filnavn_ext <- paste("results/figures-ext/", df$VdrNr[1], "_", df$Vassdrag[1], "_",
                     df$Aar[max(antall_aar)], ".png", sep = "")
    figur_tittel <- paste(df$VdrNr[1], df$Vassdrag[1])

    figur_ext <- (p1_alpha + p2) / (p3_alpha + p4_alpha) / (p5 + p7) +
      plot_layout(guides = "collect") +
      plot_annotation(title = figur_tittel, theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_annotation(caption = "* År som mangler data og derfor ikke er inkludert i vurdering av måloppnåelse",
                      theme = theme(plot.caption = element_text(size = 12)))
    ggsave(filnavn_ext, figur_ext, device = "png", width = 12, height = 12)

    figur <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect") +
      plot_annotation(title = figur_tittel, theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_annotation(caption = "* År som mangler data og derfor ikke er inkludert i vurdering av måloppnåelse",
                      theme = theme(plot.caption = element_text(size = 12)))
    ggsave(filnavn, figur, device = "png", width = 12, height = 10)
  }
}
