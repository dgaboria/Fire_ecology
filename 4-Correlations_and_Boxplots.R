rm(list = ls())  # Deleting variables from the environment R

# Importation of libraries
library(devtools)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(patchwork)
library(readr)
library(tidyverse)
library(corrplot)

df<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Data_select.csv",h=T,sep=";", dec = ',')

#1 tester la normalité

#2 transformer les variables non normales (min–max scaling)

#3 calculer la corrélation de Spearman

#4 produire un boxplot vertical pour chaque variable

# Conversion des colonnes en numérique (remplacement virgule → point déjà géré par dec=",")
df <- df %>% mutate(across(where(is.character), ~as.numeric(str_replace(., ",", "."))))


# Test de normalité Shapiro-Wilk pour chaque colonne numérique
normality <- df[,2:6] %>% 
  summarise(across(where(is.numeric), ~shapiro.test(.)$p.value)) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value")

print(normality)

#p‑value < 0.05 → la distribution n’est pas normale for Area_100km, Area_30km, Nbr_100km,Traps_sum_area,
#Traps_sum_nbr, Traps_area_mean, Traps_nbr_mean, AOD_Fire_season, AOD_June

# Transformation min-max pour les variables non normales (p < 0.05)

# Colonnes à transformer (p < 0.05)
vars_to_scale <- normality$variable[normality$p_value < 0.05]

log_minmax <- function(x){
  if(all(is.na(x))) return(rep(NA, length(x)))
  
  lx <- log1p(x)                 # log(x + 1)
  mn <- min(lx, na.rm = TRUE)
  mx <- max(lx, na.rm = TRUE)
  
  if(mn == mx) return(rep(0.5, length(x)))   # variance nulle
  
  (lx - mn) / (mx - mn)          # scaling 0–1
}

df_scaled <- df %>% 
  mutate(across(
    c(Area_100km, Area_30km, Traps_area_sum,
      AOD_Mean_June_to_August, AOD_Mean_June),
    ~log_minmax(.),
    .names = "{.col}_scaled"
  ))


# ============================================================
# 1. Extraire les noms des colonnes scalées
# ============================================================
vars_corr <- c(
    "Area_100km_scaled",
    'Area_30km_scaled',
    "Traps_area_sum_scaled",
    "AOD_Mean_June_scaled",
    "AOD_Mean_June_to_August_scaled"
    
  )
  

df_scaled <- df_scaled %>% 
  select(Year, all_of(vars_corr))

# ============================================================
# 2. Correlation (Spearman) matrix without 'Year'
# ============================================================

df_corr <- df_scaled %>%
  select(-Year) %>%
  cor(method = "kendall", use = "pairwise.complete.obs")

# ============================================================
# 3. Measure Spearman p-values
# ============================================================

cor_pmat <- function(df){
  n <- ncol(df)
  p.mat <- matrix(NA, n, n)
  colnames(p.mat) <- colnames(df)
  rownames(p.mat) <- colnames(df)
  
  for(i in 1:n){
    for(j in 1:n){
      test <- suppressWarnings(cor.test(df[[i]], df[[j]], method = "spearman"))
      p.mat[i,j] <- test$p.value
    }
  }
  return(p.mat)
}

p_mat <- cor_pmat(df_scaled %>% select(-Year))

# Variables avec au moins une corrélation significative
sig_vars <- rownames(p_mat)[apply(p_mat < 0.05, 1, any)]
sig_vars
df_corr_sig <- df_corr[sig_vars, sig_vars]
p_mat_sig   <- p_mat[sig_vars, sig_vars]

# ============================================================
# 4. Corrplot filtré par p-value
# ============================================================
corrplot(
  df_corr_sig,
  method = "color",
  type = "lower",
  tl.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7,
  addCoef.col = "black",
  col = colorRampPalette(c("#4575b4", "#ffffbf", "#d73027"))(200),
  diag = FALSE,
  p.mat = p_mat_sig,
  sig.level = 0.05,
  insig = "blank"
)

# ============================================================
# 5. Boxplots améliorés avec légende + années colorées
# ============================================================

# Variables à garder
vars_keep <- c(
  "Area_100km_scaled",
  'Area_30km_scaled',
  "Traps_area_sum_scaled",
  "AOD_Mean_June_scaled",
  "AOD_Mean_June_to_August_scaled"
)


# Vérification : garder uniquement celles qui existent
vars_keep <- vars_keep[vars_keep %in% names(df_scaled)]

# Labels correspondants
label_map <- c(
  "Area_100km_scaled"      = "Burned areas 100",
  "Area_30km_scaled"       = "Burned areas 30",
  "Traps_area_sum_scaled"  = "CHAR area traps",
  "AOD_Mean_June_to_August_scaled" = "AOD JUN-AUG",
  "AOD_Mean_June_scaled"        = "AOD JUNE"
)

labels_keep <- label_map[vars_keep]

# ============================================================
# 2. Pivot + classification des années
# ============================================================

df_long <- df_scaled %>% 
  pivot_longer(
    cols = all_of(vars_keep),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Variable = factor(Variable, levels = vars_keep, labels = labels_keep),
    
    YearClass = case_when(
      Year == 2023 ~ "2023",
      Year == 2013 ~ "2013",
      Year == 2022 ~ "2022",
      Year == 2025 ~ "2025",
      TRUE ~ NA_character_
    ),
    
    YearOther = ifelse(is.na(YearClass), "Other", NA)
  )

# Couleurs, formes et tailles (lisible pour les daltoniens)
colz <- c(
  "2023" = "#000000",   # noir : année extrême
  "2013" = "#E69F00",   # orange
  "2022" = "#0072B2",   # bleu foncé
  "2025" = "#56B4E9"    # bleu clair
)

shapez <- c(
  "2023" = 16,  # rond
  "2013" = 17,  # triangle
  "2022" = 15,  # carré
  "2025" = 18   # losange
)

sizez <- c(
  "2023" = 3.0,
  "2013" = 3.0,
  "2022" = 3.0,
  "2025" = 3.5   # le losange (18) paraît plus petit, on compense
)

# ============================================================
# 4. Figure finale — légende en bas
# ============================================================
fig_box <- ggplot(df_long, aes(x = Variable, y = Value)) +
  
  geom_boxplot(
    outlier.shape = NA,
    fill = "white",
    color = "black",
    width = 0.55
  ) +
  
  # Autres années : gris clair pour ne pas concurrencer 2023 (noir)
  geom_point(
    data = df_long %>% filter(!is.na(YearOther)),
    color = "grey60",
    size = 1.3,
    alpha = 0.5,
    position = position_dodge2(width = 0.6, padding = 0.3)
  ) +
  
  # Années mises en évidence : couleur + forme + taille
  geom_point(
    data = df_long %>% filter(!is.na(YearClass)),
    aes(color = YearClass, shape = YearClass, size = YearClass),
    alpha = 0.9,
    position = position_dodge2(width = 0.6, padding = 0.6)
  ) +
  
  # Même name dans les 3 échelles, pour une seule légende fusionnée
  scale_color_manual(values = colz,   name = NULL) +
  scale_shape_manual(values = shapez, name = NULL) +
  scale_size_manual(values = sizez,   name = NULL) +
  
  labs(
    x = NULL,
    y = "Min–Max Log(x+1)"
  ) +
  
  theme_bw(base_size = 15) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                               face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 15)
  )

fig_box

# ============================================================
# 6. Médiane, Q1, Q3 et IQR pour chaque variable
# ============================================================

stats_summary <- df_scaled %>%
  select(all_of(vars_keep)) %>%
  summarise(
    across(
      everything(),
      list(
        median = ~median(., na.rm = TRUE),
        Q1     = ~quantile(., 0.25, na.rm = TRUE),
        Q3     = ~quantile(., 0.75, na.rm = TRUE),
        IQR    = ~IQR(., na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Stat"),
    names_pattern = "(.*)_(median|Q1|Q3|IQR)"
  ) %>%
  pivot_wider(
    names_from = Stat,
    values_from = value
  )

print(stats_summary)
