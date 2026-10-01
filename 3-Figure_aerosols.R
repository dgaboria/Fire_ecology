library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(ggpattern)
library(ggh4x)
library(ggtext)

df <- read.table(header = TRUE, sep = ",", dec = ".", text = "
Year,Month,AOD_675nm,Precip_cm
2011,APR,0.052011,0.460785
2011,MAY,0.047535,0.807557
2011,JUN,0.067415,1.039135
2011,AUG,0.115925,1.452835
2011,SEP,0.07825,1.463229
2011,OCT,0.077636,1.061504
2012,APR,0.073029,0.498635
2012,MAY,0.10006,1.104637
2012,JUN,0.072577,1.744062
2012,JUL,0.092428,1.919027
2012,AUG,0.101511,2.104325
2012,SEP,0.100518,1.796765
2012,OCT,0.089814,1.394112
2013,APR,0.111677,0.57447
2013,MAY,0.099621,1.275179
2013,JUN,0.173612,1.378572
2013,JUL,0.110637,2.101662
2013,AUG,0.038138,1.665744
2013,SEP,0.021434,1.23046
2013,OCT,0.028732,1.379408
2014,APR,0.049983,0.591684
2014,JUN,0.050141,1.441012
2014,JUL,0.154026,1.803639
2014,AUG,0.105401,1.895999
2014,SEP,0.04398,1.559706
2014,OCT,0.025375,0.904221
2015,APR,0.053476,0.487798
2015,MAY,0.066896,0.862826
2015,JUN,0.065988,1.277901
2015,JUL,0.156608,1.813068
2015,AUG,0.111393,1.968106
2015,SEP,0.053069,1.569381
2015,OCT,0.019232,0.702912
2016,APR,0.056798,0.465897
2016,MAY,0.139921,0.98194
2016,JUN,0.064264,1.524677
2016,JUL,0.05856,1.861726
2016,AUG,0.069646,1.971
2016,SEP,0.059383,1.439149
2016,OCT,0.041076,1.296884
2017,APR,0.046829,0.63789
2017,MAY,0.043599,1.333575
2018,APR,0.054427,0.406639
2018,MAY,0.049031,0.554335
2018,SEP,0.019182,0.694764
2018,OCT,0.022241,0.675024
2019,MAY,0.047183,0.721969
2019,JUN,0.042862,1.437177
2019,JUL,0.125583,1.810344
2019,AUG,0.105665,1.646727
2019,SEP,0.078138,1.149615
2019,OCT,0.070133,0.630323
2020,JUN,0.051004,1.558799
2020,JUL,0.057096,2.064647
2020,AUG,0.053155,1.642937
2020,SEP,0.087391,1.117014
2020,OCT,0.0581,0.576943
2020,NOV,0.065147,0.635185
2021,APR,0.038481,0.72992
2021,MAY,0.043373,0.91887
2021,JUN,0.03841,1.29664
2021,JUL,0.137014,1.590199
2021,AUG,0.205915,2.103187
2021,SEP,0.051446,1.476114
2021,OCT,0.045656,1.114034
2021,NOV,0.043617,0.637204
2021,DEC,0.024836,0.201736
2022,MAY,0.030911,0.924305
2022,JUN,0.033871,1.636745
2022,JUL,0.079014,1.860354
2022,AUG,0.037196,1.824082
2022,SEP,0.053861,1.38627
2022,OCT,0.043974,0.996139
2023,APR,0.050163,0.802078
2023,MAY,0.081105,0.851089
2023,JUN,0.648917,1.493327
2023,JUL,0.192251,2.125892
2023,AUG,0.104608,1.551266
2023,SEP,0.178692,1.575133
2023,OCT,0.038893,1.44662
2024,APR,0.038726,0.541443
2024,MAY,0.054668,1.146704
2024,JUL,0.124525,1.857533
2024,AUG,0.259783,1.798677
2024,SEP,0.089889,1.853602
2024,OCT,0.039377,0.939673
2025,AUG,0.153368,2.593149

")

df$Month <- factor(df$Month,
                   levels = c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))

df_long <- df %>%
  pivot_longer(cols = c(AOD_675nm, Precip_cm),
               names_to = "Variable",
               values_to = "Value")

df2 <- df %>%
  filter(!Month %in% c("NOV", "DEC")) %>%
  pivot_longer(cols = c(AOD_675nm, Precip_cm),
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
  length(sub$Month) < length(season_months) || any(is.na(sub$AOD_675nm))
})
names(missing_by_year) <- as.character(all_years)

# 3. Calculer la moyenne (Mai à Septembre) par année -> Composite (Mean)
composites <- df_filtered %>%
  filter(Month %in% season_months) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(AOD_675nm, na.rm = TRUE))

composite_moyenne <- data.frame(Year = composites$Year, Month = "Composite (Mean)", AOD_675nm = composites$Moyenne)

df_final <- bind_rows(
  df_filtered[, c("Year", "Month", "AOD_675nm")],
  composite_moyenne
)

# 4. Remplacer les NA par 0 pour garder la barre (hauteur 0)
df_final$AOD_675nm[is.na(df_final$AOD_675nm)] <- 0

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
      v <- sub$AOD_675nm[sub$Year == y]
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
ggplot(df_final, aes(x = Year, y = AOD_675nm)) +
  geom_col(fill = "black", width = 0.6) +
  facet_wrap(~ Month, ncol = 3, scales = "free", axes = "all_x",
             labeller = month_labeller) +
  facetted_pos_scales(
    x = x_scales,
    y = list(
      Month %in% c("MAY","JUN","JUL","AUG","SEP") ~ scale_y_continuous(limits = c(0, 0.7)),
      Month == "Composite (Mean)" ~ scale_y_continuous()
    )
  ) +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1)) +
  labs(x = NULL, y = "AOD (675 nm)", fill = NULL)

# Calcul des moyennes Mai–Septembre par année
composites <- df_filtered %>%
  filter(Month %in% c("MAY","JUN","JUL","AUG","SEP")) %>%
  group_by(Year) %>%
  summarise(Moyenne = mean(AOD_675nm, na.rm = TRUE))

# Affichage des valeurs
print(composites)
