library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)

resultat_fordeling <- import("results/resultat_fordeling_2023.csv")
elveliste <- import("data/elveliste.csv")

antall_elver <- nrow(elveliste)

statusfarger <- c(A = "#D00000", B = "orange", C = "yellow", D = "green3")

for (m in 1:antall_elver) {
  if (elveliste$GytingSim[m]) {
    simulering_filnavn <- paste("results/vassdrag/", elveliste[m, "Filnavn"], "-gyting_hunn_kg_totalt.csv", sep = "")
    simulering_kghuntot <- import(simulering_filnavn)

    # Til rapport 2024: legg inn midlertidig begrensning til 2002 pga at dette er startår i fordelingen av innsig
    simulering_kghuntot <- filter(simulering_kghuntot, Aar >= 2002)

    df <- filter(resultat_fordeling, VdrNr == elveliste$VdrNr[m]) %>%
      mutate(GBM_lav = elveliste$GBM_lav[m]) %>%
      mutate(GBM_hoy = elveliste$GBM_hoy[m])

    df <- df %>% mutate(Sanns_kat = case_when(
      Sannsyn_GBM > 0.75 ~ "D",
      Sannsyn_GBM >= 0.4 ~ "C",
      (Sannsyn_GBM < 0.4 & Hostbart_overskudd > 0)  ~ "B",
      Hostbart_overskudd == 0 ~ "A"
    ))

    df$Sanns_kat <- factor(df$Sanns_kat, levels = c("A", "B", "C", "D"))

    antall_aar <- length(df$Aar)

    df <- df %>% mutate(Innsig_samlet_ant = Innsig_sjo_ant_u3 + Innsig_sjo_ant_37 + Innsig_sjo_ant_o7)
    df <- df %>% mutate(Innsig_samlet_vekt = Innsig_sjo_vekt_u3 + Innsig_sjo_vekt_37 + Innsig_sjo_vekt_o7)
    df <- df %>%
      mutate(Fangst_elv_samlet_ant = Fangst_elv_ant_u3 + Fangst_elv_ant_37 + Fangst_elv_ant_o7) %>%
      mutate(Fangst_sjo_samlet_ant = Sjofangst_ant_u3 + Sjofangst_ant_37 + Sjofangst_ant_o7) %>%
      mutate(Gyt_samlet_ant = Gyt_ant_u3 + Gyt_ant_37 + Gyt_ant_o7)
    df <- df %>%
      mutate(Sjofangst_hunn = Sjofangst_vekt_u3 * Andel_hunn_u3 + Sjofangst_vekt_37 * Andel_hunn_37 + Sjofangst_vekt_o7 * Andel_hunn_o7) %>%
      mutate(Elvefangst_hunn = Fangst_elv_vekt_u3 * Andel_hunn_u3 + Fangst_elv_vekt_37 * Andel_hunn_37 + Fangst_elv_vekt_o7 * Andel_hunn_o7)
    df <- df %>% mutate(Simulert = 0)

    for (i in 1:antall_aar) {
      if (df$Gyting_hunn[i] == 0)
        df$Simulert[i] <- 0
      else
        df$Simulert[i] <- 1
    }

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
    p1 <- ggplot(simulering_kghuntot, aes(x = factor(Aar), y = gyting_hunn_kg_totalt)) +
      theme_light() +
      geom_boxplot(aes(fill = kategorisering), outlier.shape = NA, na.rm = TRUE) +
      geom_text(data = subset(df, Simulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      scale_fill_manual(values = statusfarger) +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      ylab("Gytebestand hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      ylim(0, NA) +
      theme(legend.position = "none")

    #================================================
    # figur 2, barplot av måloppnåelse
    #================================================
    df_2 <- df %>%
      select(Aar, ProsOppnaadTrunk, Sanns_kat) %>%
      add_row(Aar = 2002, ProsOppnaadTrunk = 0, Sanns_kat = "A") %>%
      add_row(Aar = 2002, ProsOppnaadTrunk = 0, Sanns_kat = "B") %>%
      add_row(Aar = 2002, ProsOppnaadTrunk = 0, Sanns_kat = "C") %>%
      add_row(Aar = 2002, ProsOppnaadTrunk = 0, Sanns_kat = "D")

    p2 <- ggplot(df_2, aes(x = factor(Aar), y = ProsOppnaadTrunk)) +
      theme_light() +
      geom_col(aes(fill = Sanns_kat)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_fill_manual(values = c("#D00000", "orange", "yellow", "green3"),
                        labels = c("<40 %, ikke overskudd", "<40 %, høstbart overskudd", "40-75 %", ">75 %"),
                        drop = FALSE,
                        name = "%sannsynlighet for nådd GBM") +
      ylab("Prosent oppnåelse av GBM") +
      xlab("År") +
      geom_text(data = subset(df, Simulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

    #================================================
    # figur 3, innsig fordelt
    #================================================
    df_p3 <- df %>%
      select(Aar, Fangst_sjo_samlet_ant, Fangst_elv_samlet_ant, Gyt_samlet_ant, Simulert) %>%
      pivot_longer(cols = c("Fangst_sjo_samlet_ant", "Fangst_elv_samlet_ant", "Gyt_samlet_ant"),
                   names_to = "Omraade", values_to = "Antall_fisk")
    df_p3$Omraade[df_p3$Omraade == "Fangst_sjo_samlet_ant"] <- "Sjøfiske"
    df_p3$Omraade[df_p3$Omraade == "Fangst_elv_samlet_ant"] <- "Elvefiske"
    df_p3$Omraade[df_p3$Omraade == "Gyt_samlet_ant"] <- "Gytebestand"
    df_p3$Antall_fisk[df_p3$Simulert == 0] <- 0
    df_p3$Omraade <- factor(df_p3$Omraade, levels = c("Sjøfiske", "Elvefiske", "Gytebestand"))

    p3 <- ggplot(df_p3, aes(x = factor(Aar), y = Antall_fisk)) +
      geom_col(aes(fill = Omraade)) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      ylab("Innsig laks (antall)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      geom_text(data = subset(df, Simulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme(legend.position = "none")

    #================================================
    # figur 4, innsig hunn vs GBM
    #================================================
    df_p4 <- df %>%
      select(Aar, Sjofangst_hunn, Elvefangst_hunn, Gyting_hunn, Simulert) %>%
      pivot_longer(cols = c("Sjofangst_hunn", "Elvefangst_hunn", "Gyting_hunn"),
                   names_to = "Omraade", values_to = "Vekt_hunnlaks")
    df_p4$Omraade[df_p4$Omraade == "Sjofangst_hunn"] <- "Sjøfiske"
    df_p4$Omraade[df_p4$Omraade == "Elvefangst_hunn"] <- "Elvefiske"
    df_p4$Omraade[df_p4$Omraade == "Gyting_hunn"] <- "Gytebestand"
    df_p4$Vekt_hunnlaks[df_p4$Simulert == 0] <- 0
    df_p4$Omraade <- factor(df_p4$Omraade, levels = c("Sjøfiske", "Elvefiske", "Gytebestand"))

    p4 <- ggplot(df_p4, aes(x = factor(Aar), y = Vekt_hunnlaks)) +
      geom_col(aes(fill = Omraade)) +
      geom_text(data = subset(df, Simulert == 0), aes(y = 0, label = "*"), size = symbolstorrelse) +
      theme_light() +
      scale_fill_manual(values = c("blue", "#D00000", "green3"), name = "Fordeling innsig") +
      ylab("Innsig hunnlaks (kg)") +
      xlab("År") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
      geom_hline(data = gbm_data, aes(yintercept = y, linetype = type)) +
      scale_linetype_manual(values = 1:2, labels = c("Gytebestandsmål", "Øvre/nedre grense"), name = "") +
      guides(fill = guide_legend(override.aes = list(linetype = "blank")))

    filnavn <- paste("results/figures/", df$VdrNr[1], "_", df$Vassdrag[1], "_",
                     df$Aar[max(antall_aar)], ".png", sep = "")
    figur_tittel <- paste(df$VdrNr[1], df$Vassdrag[1])

    #================================================
    # patchwork
    #================================================
    figur <- (p1 + p2) / (p3 + p4) +
      plot_layout(guides = "collect") +
      plot_annotation(title = figur_tittel, theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_annotation(caption = "* År som mangler data og derfor ikke er inkludert i vurdering av måloppnåelse",
                      theme = theme(plot.caption = element_text(size = 12)))
    ggsave(filnavn, figur, device = "png", width = 12, height = 10)
  }
}
