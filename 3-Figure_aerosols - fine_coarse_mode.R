library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpattern)
library(ggh4x)
library(ggtext)

#Fine-mode_AOD

df <- read.table(header = TRUE, sep = ",", dec = ".", text = "
Year,Month,FineMode_500nm
2011,MAY,0.062
2011,JUN,0.107
2011,AUG,0.057
2011,SEP,0.089
2012,MAY,0.111
2012,JUN,0.097
2012,JUL,0.130
2012,AUG,0.105
2012,SEP,0.115
2013,MAY,0.076
2013,JUN,0.097
2013,JUL,0.126
2013,AUG,0.063
2013,SEP,0.033
2014,JUN,0.052
2014,JUL,0.225
2014,AUG,0.147
2014,SEP,0.057
2015,MAY,0.073
2015,JUN,0.084
2015,JUL,0.239
2015,AUG,0.174
2015,SEP,0.066
2016,MAY,0.204
2016,JUN,0.075
2016,JUL,0.066
2016,AUG,0.074
2016,SEP,0.069
2017,MAY,0.038
2018,MAY,0.060
2018,SEP,0.018
2019,MAY,0.049
2019,JUN,0.057
2019,JUL,0.161
2019,AUG,0.138
2019,SEP,0.090
2021,JUN,0.089
2021,JUL,0.218
2021,AUG,0.306
2021,SEP,0.067
2022,MAY,0.038
2022,JUN,0.042
2022,JUL,0.115
2022,AUG,0.058
2022,SEP,0.082
2023,MAY,0.101
2023,JUN,0.824
2023,JUL,0.279
2023,AUG,0.155
2023,SEP,0.249
2024,MAY,0.062
2024,JUL,0.196
2024,AUG,0.401
2024,SEP,0.136
2025,AUG,0.233

")

df$Month <- factor(df$Month,
                   levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
df_fine <- df

df_long <- df %>%
  pivot_longer(cols = c(FineMode_500nm),
               names_to = "Variable",
               values_to = "Value")

df2 <- df %>%
  filter(!Month %in% c("NOV", "DEC")) %>%
  pivot_longer(cols = c(FineMode_500nm),
               names_to = "Variable",
               values_to = "Value")

# 1. Retirer Avril, Oct, Nov, Dec
df_filtered <- df[!df$Month %in% c("APR", "OCT", "NOV", "DEC"), ]
df_filtered$Month <- droplevels(df_filtered$Month)

# 2. Identifier, PAR ANNÉE, si au moins un mois (Mai-Sep) est manquant (NA)
#    -> ceci servira à colorer l'étiquette en rouge dans le composite
all_years <- 2011:2025
season_months <- c("MAY", "JUN", "JUL", "AUG", "SEP")

missing_by_year <- sapply(all_years, function(y) {
  sub <- df_filtered %>% filter(Year == y, Month %in% season_months)
  # manquant si: pas les 5 mois présents, OU une valeur NA parmi eux
  length(sub$Month) < length(season_months) || any(is.na(sub$FineMode_500nm))
})
names(missing_by_year) <- as.character(all_years)

# 3. Calculer la moyenne (Mai à Septembre) par année -> Composite (Mean)
composites <- df_filtered %>%
  filter(Month %in% season_months) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(FineMode_500nm, na.rm = TRUE))

composite_moyenne <- data.frame(Year = composites$Year, Month = "Composite (Mean)", FineMode_500nm = composites$Moyenne)

df_final <- bind_rows(
  df_filtered[, c("Year", "Month", "FineMode_500nm")],
  composite_moyenne
)

# 4. Remplacer les NA par 0 pour garder la barre (hauteur 0)
df_final$FineMode_500nm[is.na(df_final$FineMode_500nm)] <- 0

df_final$Month <- factor(df_final$Month,
                         levels = c("MAY", "JUN", "JUL", "AUG", "SEP", "Composite (Mean)"))
df_final$Year <- factor(df_final$Year, levels = all_years)

month_levels <- levels(df_final$Month)
year_labels_plain <- as.character(all_years)

# 5. Construire les échelles X par facette
#    - Pour MAY..SEP : label vide si valeur = 0 (comme avant)
#    - Pour Composite (Mean) : toutes les années affichées, en ROUGE si missing_by_year == TRUE
x_scales <- Map(function(m) {
  if (m == "Composite (Mean)") {
    labs_local <- ifelse(missing_by_year[year_labels_plain],
                         paste0("<span style='color:red'>", year_labels_plain, "</span>"),
                         year_labels_plain)
  } else {
    sub <- df_final %>% filter(Month == m)
    vals <- sapply(year_labels_plain, function(y) {
      v <- sub$FineMode_500nm[sub$Year == y]
      if (length(v) == 0) 0 else v[1]
    })
    labs_local <- ifelse(vals == 0, "", year_labels_plain)
  }
  local({
    labs_capture <- labs_local
    as.formula(sprintf("Month == '%s' ~ scale_x_discrete(labels = labs_capture, drop = FALSE)", m))
  })
}, month_levels)

# 6. Labeller pour renommer les titres de facette
month_labeller <- as_labeller(c(
  "MAY" = "MAY",
  "JUN" = "JUNE",
  "JUL" = "JULY",
  "AUG" = "AUGUST",
  "SEP" = "SEPTEMBER",
  "Composite (Mean)" = "Composite (Mean)"
))

# 7. Graphique
ggplot(df_final, aes(x = Year, y = FineMode_500nm)) +
  geom_col(fill = "black", width = 0.6) +
  facet_wrap(~ Month, ncol = 3, scales = "free", axes = "all_x",
             labeller = month_labeller) +
  facetted_pos_scales(
    x = x_scales,
    y = list(
      Month %in% c("MAY","JUN","JUL","AUG","SEP") ~ scale_y_continuous(limits = c(0, 1.0)),
      Month == "Composite (Mean)" ~ scale_y_continuous()
    )
  ) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Fine Mode AOD 500nm", fill = NULL)

# Calcul des moyennes Mai–Septembre par année
composites <- df_filtered %>%
  filter(Month %in% c("MAY","JUN","JUL","AUG","SEP")) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(FineMode_500nm, na.rm = TRUE))

# Affichage des valeurs
print(composites)





#Coarse-mode_AOD

df <- read.table(header = TRUE, sep = ",", dec = ".", text = "
Year,Month,CoarseMode_500nm
2011,MAY,0.020
2011,JUN,0.012
2011,AUG,0.085
2011,SEP,0.030
2012,MAY,0.042
2012,JUN,0.026
2012,JUL,0.020
2012,AUG,0.044
2012,SEP,0.041
2013,MAY,0.061
2013,JUN,0.050
2013,JUL,0.044
2013,AUG,0.005
2013,SEP,0.010
2014,JUN,0.017
2014,JUL,0.016
2014,AUG,0.019
2014,SEP,0.013
2015,MAY,0.024
2015,JUN,0.019
2015,JUL,0.014
2015,AUG,0.011
2015,SEP,0.018
2016,MAY,0.015
2016,JUN,0.024
2016,JUL,0.024
2016,AUG,0.031
2016,SEP,0.021
2017,MAY,0.029
2018,MAY,0.012
2018,SEP,0.010
2019,MAY,0.020
2019,JUN,0.019
2019,JUL,0.022
2019,AUG,0.018
2019,SEP,0.018
2021,JUN,0.018
2021,JUL,0.009
2021,AUG,0.011
2021,SEP,0.014
2022,MAY,0.017
2022,JUN,0.019
2022,JUL,0.019
2022,AUG,0.020
2022,SEP,0.018
2023,MAY,0.026
2023,JUN,0.041
2023,JUL,0.027
2023,AUG,0.016
2023,SEP,0.015
2024,MAY,0.020
2024,JUL,0.011
2024,AUG,0.009
2024,SEP,0.012
2025,AUG,0.016

")

df$Month <- factor(df$Month,
                   levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
df_coarse <- df
df_long <- df %>%
  pivot_longer(cols = c(CoarseMode_500nm),
               names_to = "Variable",
               values_to = "Value")

df2 <- df %>%
  filter(!Month %in% c("NOV", "DEC")) %>%
  pivot_longer(cols = c(CoarseMode_500nm),
               names_to = "Variable",
               values_to = "Value")

# 1. Retirer Avril, Oct, Nov, Dec
df_filtered <- df[!df$Month %in% c("APR", "OCT", "NOV", "DEC"), ]
df_filtered$Month <- droplevels(df_filtered$Month)

# 2. Identifier, PAR ANNÉE, si au moins un mois (Mai-Sep) est manquant (NA)
#    -> ceci servira à colorer l'étiquette en rouge dans le composite
all_years <- 2011:2025
season_months <- c("MAY", "JUN", "JUL", "AUG", "SEP")

missing_by_year <- sapply(all_years, function(y) {
  sub <- df_filtered %>% filter(Year == y, Month %in% season_months)
  # manquant si: pas les 5 mois présents, OU une valeur NA parmi eux
  length(sub$Month) < length(season_months) || any(is.na(sub$CoarseMode_500nm))
})
names(missing_by_year) <- as.character(all_years)

# 3. Calculer la moyenne (Mai à Septembre) par année -> Composite (Mean)
composites <- df_filtered %>%
  filter(Month %in% season_months) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(CoarseMode_500nm, na.rm = TRUE))

composite_moyenne <- data.frame(Year = composites$Year, Month = "Composite (Mean)", CoarseMode_500nm = composites$Moyenne)

df_final <- bind_rows(
  df_filtered[, c("Year", "Month", "CoarseMode_500nm")],
  composite_moyenne
)

# 4. Remplacer les NA par 0 pour garder la barre (hauteur 0)
df_final$CoarseMode_500nm[is.na(df_final$CoarseMode_500nm)] <- 0

df_final$Month <- factor(df_final$Month,
                         levels = c("MAY", "JUN", "JUL", "AUG", "SEP", "Composite (Mean)"))
df_final$Year <- factor(df_final$Year, levels = all_years)

month_levels <- levels(df_final$Month)
year_labels_plain <- as.character(all_years)

# 5. Construire les échelles X par facette
#    - Pour MAY..SEP : label vide si valeur = 0 (comme avant)
#    - Pour Composite (Mean) : toutes les années affichées, en ROUGE si missing_by_year == TRUE
x_scales <- Map(function(m) {
  if (m == "Composite (Mean)") {
    labs_local <- ifelse(missing_by_year[year_labels_plain],
                         paste0("<span style='color:red'>", year_labels_plain, "</span>"),
                         year_labels_plain)
  } else {
    sub <- df_final %>% filter(Month == m)
    vals <- sapply(year_labels_plain, function(y) {
      v <- sub$CoarseMode_500nm[sub$Year == y]
      if (length(v) == 0) 0 else v[1]
    })
    labs_local <- ifelse(vals == 0, "", year_labels_plain)
  }
  local({
    labs_capture <- labs_local
    as.formula(sprintf("Month == '%s' ~ scale_x_discrete(labels = labs_capture, drop = FALSE)", m))
  })
}, month_levels)

# 6. Labeller pour renommer les titres de facette
month_labeller <- as_labeller(c(
  "MAY" = "MAY",
  "JUN" = "JUNE",
  "JUL" = "JULY",
  "AUG" = "AUGUST",
  "SEP" = "SEPTEMBER",
  "Composite (Mean)" = "Composite (Mean)"
))

# 7. Graphique
ggplot(df_final, aes(x = Year, y = CoarseMode_500nm)) +
  geom_col(fill = "black", width = 0.6) +
  facet_wrap(~ Month, ncol = 3, scales = "free", axes = "all_x",
             labeller = month_labeller) +
  facetted_pos_scales(
    x = x_scales,
    y = list(
      Month %in% c("MAY","JUN","JUL","AUG","SEP") ~ scale_y_continuous(limits = c(0, 0.1)),
      Month == "Composite (Mean)" ~ scale_y_continuous()
    )
  ) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "Coarse Mode AOD 500nm", fill = NULL)

# Calcul des moyennes Mai–Septembre par année
composites <- df_filtered %>%
  filter(Month %in% c("MAY","JUN","JUL","AUG","SEP")) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(CoarseMode_500nm, na.rm = TRUE))

# Affichage des valeurs
print(composites)





# ============================================================
# Fine + Coarse mode AOD combinés (barres empilées = AOD totale)
# ============================================================
season_months <- c("MAY", "JUN", "JUL", "AUG", "SEP")
all_years <- 2011:2025

# 1. Fusion des deux jeux (mois de la saison seulement)
df_both <- full_join(
  df_fine   %>% mutate(Month = as.character(Month)),
  df_coarse %>% mutate(Month = as.character(Month)),
  by = c("Year", "Month")
) %>%
  filter(Month %in% season_months)

# 2. Composite (moyenne Mai–Sep) pour chaque mode
comp <- df_both %>%
  group_by(Year) %>%
  summarise(FineMode_500nm   = mean(FineMode_500nm,   na.rm = TRUE),
            CoarseMode_500nm = mean(CoarseMode_500nm, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Month = "Composite (Mean)")

# 3. Années incomplètes (moins de 5 mois) -> étiquette rouge dans le composite
missing_by_year <- sapply(all_years, function(y)
  sum(df_both$Year == y & !is.na(df_both$FineMode_500nm)) < length(season_months))
names(missing_by_year) <- as.character(all_years)

# 4. Format long pour l'empilement
df_plot <- bind_rows(df_both, comp) %>%
  pivot_longer(c(FineMode_500nm, CoarseMode_500nm),
               names_to = "Mode", values_to = "AOD") %>%
  mutate(Mode  = factor(Mode,
                        levels = c("CoarseMode_500nm", "FineMode_500nm"),
                        labels = c("Coarse mode", "Fine mode")),
         Month = factor(Month, levels = c(season_months, "Composite (Mean)")),
         Year  = factor(Year, levels = all_years))

# 5. Échelles X par facette (même logique que tes graphiques)
year_labels_plain <- as.character(all_years)
totals <- df_plot %>%
  group_by(Month, Year) %>%
  summarise(Total = sum(AOD, na.rm = TRUE), .groups = "drop")

x_scales <- Map(function(m) {
  if (m == "Composite (Mean)") {
    labs_local <- ifelse(missing_by_year[year_labels_plain],
                         paste0("<span style='color:red'>", year_labels_plain, "</span>"),
                         year_labels_plain)
  } else {
    sub <- totals %>% filter(Month == m)
    vals <- sapply(year_labels_plain, function(y) {
      v <- sub$Total[as.character(sub$Year) == y]
      if (length(v) == 0) 0 else v[1]
    })
    labs_local <- ifelse(vals == 0, "", year_labels_plain)
  }
  local({
    labs_capture <- labs_local
    as.formula(sprintf("Month == '%s' ~ scale_x_discrete(labels = labs_capture, drop = FALSE)", m))
  })
}, levels(df_plot$Month))

month_labeller <- as_labeller(c(
  "MAY" = "MAY", "JUN" = "JUNE", "JUL" = "JULY", "AUG" = "AUGUST",
  "SEP" = "SEPTEMBER", "Composite (Mean)" = "Composite (Mean)"
))

# 6. Graphique combiné
ggplot(df_plot, aes(x = Year, y = AOD, fill = Mode)) +
  geom_col(width = 0.6, position = "stack") +
  scale_fill_manual(values = c("Fine mode" = "black", "Coarse mode" = "grey65")) +
  facet_wrap(~ Month, ncol = 3, scales = "free", axes = "all_x",
             labeller = month_labeller) +
  facetted_pos_scales(
    x = x_scales,
    y = list(
      Month %in% season_months ~ scale_y_continuous(limits = c(0, 1.0)),
      Month == "Composite (Mean)" ~ scale_y_continuous()
    )
  ) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1),
        legend.position = "top") +
  labs(x = NULL, y = "AOD 500 nm (fine + coarse)", fill = NULL)
