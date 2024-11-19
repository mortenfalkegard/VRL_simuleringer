#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Skriptet er laget for å se litt nærmere på hvordan ulike sider ved innsigsberegningene og simuleringene
# kan brukes til å lage prediksjoner for kommende år.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(zoo)

# Hent inn datafiler
resultat_fordeling <- import("results/resultat_fordeling_2023.csv")
elveliste <- import("data/elveliste.csv")

antall_elver <- nrow(elveliste)

# sett opp en tom liste som brukes til å lagre plott
plots_list <- list()

for (m in 1:antall_elver) {
  if (elveliste$GytingSim[m]) {
    df <- filter(resultat_fordeling, VdrNr == elveliste$VdrNr[m]) %>%
      mutate(GBM_lav = elveliste$GBM_lav[m]) %>%
      mutate(GBM_hoy = elveliste$GBM_hoy[m])

    # lag en variabel som gir gjennomsnittlig måloppnåelse fra fire foregående år
    df <- df  %>%
      mutate(ProsOppnaad_snitt = rollmeanr(ProsOppnaad, k = 4, fill = NA))

    # sett opp en ferdig dataframe med verdiene som skal brukes i de ulike plottene
    df_plot <- df %>%
      arrange(Aar) %>%
      mutate(Innsig_sjo_ant_2SW_neste = lead(Innsig_sjo_ant_2SW)) %>%
      mutate(Innsig_total_hunn_neste = lead(Innsig_total_hunn)) %>%
      na.omit() %>%
      filter(Aar >= 2002) %>%
      filter(Aar <= 2022)

    # Færste plott: Innsig av mellomlaks opp mot innsig smålaks året før
    p1 <- ggplot(df_plot, aes(x = Innsig_sjo_ant_1SW, y = Innsig_sjo_ant_2SW_neste)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Innsig 1SW") +
      ylab("Innsig 2SW neste år") +
      ggtitle(df_plot$Vassdrag[1])

    # Andre plott: Innsig av hunnlaks opp mot innsig av hunnlaks året før
    p2 <- ggplot(df_plot, aes(x = Innsig_total_hunn, y = Innsig_total_hunn_neste)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Innsig totalt hunn") +
      ylab("Innsig totalt hunn neste år") +
      ggtitle(df_plot$Vassdrag[1])

    # Tredje plott: Innsig av hunnlaks opp mot innsig av smålaks året før
    p3  <- ggplot(df_plot, aes(x = Innsig_sjo_ant_1SW, y = Innsig_total_hunn_neste)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Innsig 1SW") +
      ylab("Innsig totalt hunn neste år") +
      ggtitle(df_plot$Vassdrag[1])

    # Fjerde plott: Måloppnåelse (ikke trunkert) opp mot gjennomsnittlig måloppnåelse siste fire år
    p4 <- ggplot(df_plot, aes(x = ProsOppnaad_snitt, y = ProsOppnaad)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Prosent oppnådd snitt") +
      ylab("Prosent oppnådd") +
      ggtitle(df_plot$Vassdrag[1])

    # kombiner plottene i et patchwork og lagre i en liste
    plots_list[[length(plots_list) + 1]] <- (p1 + p2) / (p3 + p4)
  }
}

# Lagre alle vassdragsplottene i en pdf-fil
pdf("results/figures/all_plots.pdf")
for (plot in plots_list) {
  print(plot)
}
dev.off()
