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

#Data importation

df<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Data_select_fine_coarse.csv",h=T,sep=";", dec = ',')
df = df[,1:7]

#1 test normality
#2 transform non-normal variables (min–max scaling)
#3 calculate Spearman correlations la corrélation de Spearman
#4 Boxplot for each variable

# Converting columns to numeric (comma → decimal point replacement already handled by dec=",")
df <- df %>% mutate(across(where(is.character), ~as.numeric(str_replace(., ",", "."))))

# Shapiro-Wilk normality test for each numerical variable
normality <- df[,2:7] %>% 
  summarise(across(where(is.numeric), ~shapiro.test(.)$p.value)) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "p_value")

print(normality) #p‑value < 0.05 → non-normal distribution

# Min-Max scaling for non-normal variables (p < 0.05)

vars_to_scale <- normality$variable[normality$p_value < 0.05]

log_minmax <- function(x){
  if(all(is.na(x))) return(rep(NA, length(x)))
  
  lx <- log1p(x)                 # log(x + 1)
  mn <- min(lx, na.rm = TRUE)
  mx <- max(lx, na.rm = TRUE)
  
  if(mn == mx) return(rep(0.5, length(x)))   # variance nulle
  
  (lx - mn) / (mx - mn)          # scaling 0–1
}

#New dataset with scaled variables

df_scaled <- df %>% 
  mutate(across(
    c(Area_100km, Area_30km, Traps_area_sum,Traps_nbr_sum,
      Total_AOD_Mean_June_to_August, Total_AOD_Mean_June),
    ~log_minmax(.),
    .names = "{.col}_scaled"
  ))

# =============================
# 1. Extract column names
# =============================
vars_corr <- c(
    "Area_100km_scaled",
    'Area_30km_scaled',
    "Traps_area_sum_scaled",
    "Traps_nbr_sum_scaled",
    "Total_AOD_Mean_June_scaled",
    "Total_AOD_Mean_June_to_August_scaled"
    
  )
  

df_scaled <- df_scaled %>% 
  select(Year, all_of(vars_corr))

# ============================================================
# 2. Spearman correlation (Spearman) matrix without 'Year'
# ============================================================

df_corr <- df_scaled %>%
  select(-Year) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

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

# Variables with at least one significant correlation
sig_vars <- rownames(p_mat)[apply(p_mat < 0.05, 1, any)]
sig_vars
df_corr_sig <- df_corr[sig_vars, sig_vars]
p_mat_sig   <- p_mat[sig_vars, sig_vars]

# =================
# 4. Corrplot
# =================
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

# ============
# 5. Boxplots
# ============

vars_keep <- c(
  "Area_100km_scaled",
  'Area_30km_scaled',
  "Traps_area_sum_scaled",
  "Traps_nbr_sum_scaled",
  "Total_AOD_Mean_June_scaled",
  "Total_AOD_Mean_June_to_August_scaled"
)

vars_keep <- vars_keep[vars_keep %in% names(df_scaled)]

# Corresponding labels
label_map <- c(
  "Area_100km_scaled"      = "Area burned 100",
  "Area_30km_scaled"       = "Area burned 30",
  "Traps_area_sum_scaled"  = "CHAR area traps",
  'Traps_nbr_sum_scaled'   = 'CHAR nbr traps',
  "Total_AOD_Mean_June_to_August_scaled" = "Total AOD JUN-AUG",
  "Total_AOD_Mean_June_scaled"        = "Total AOD JUNE"
)

labels_keep <- label_map[vars_keep]

# =======================================
# Years classification
# =======================================

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

# Colors, shape, size
colz <- c(
  "2023" = "red",   # red : année extrême
  "2013" = "#E69F00",   # orange
  "2022" = "#0072B2",   # dark blue
  "2025" = "#56B4E9"    # light blue
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
# Final Boxplot figure
# ============================================================
fig_box <- ggplot(df_long, aes(x = Variable, y = Value)) +
  
  geom_boxplot(
    outlier.shape = NA,
    fill = "white",
    color = "black",
    width = 0.55
  ) +
  
  geom_point(
    data = df_long %>% filter(!is.na(YearOther)),
    color = "grey60",
    size = 1.3,
    alpha = 0.5,
    position = position_dodge2(width = 0.6, padding = 0.3)
  ) +
  
  geom_point(
    data = df_long %>% filter(!is.na(YearClass)),
    aes(color = YearClass, shape = YearClass, size = YearClass),
    alpha = 0.9,
    position = position_dodge2(width = 0.6, padding = 0.6)
  ) +
  
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
# Médiane, Q1, Q3 and IQR for each variable
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

# ============================================================
# Spearman correlations : Traps vs AOD
# ============================================================

traps_vars <- c("Traps_area_sum", "Traps_nbr_sum")
aod_vars   <- c("Total_AOD_Mean_June", "Total_AOD_Mean_June_to_August")

spearman_traps_aod <- expand_grid(Traps = traps_vars, AOD = aod_vars) %>%
  mutate(
    res = map2(Traps, AOD, ~{
      x  <- df[[.x]]
      y  <- df[[.y]]
      ok <- complete.cases(x, y)
      
      # exact = FALSE : évite l'avertissement en cas d'ex aequo
      test <- suppressWarnings(
        cor.test(x[ok], y[ok], method = "spearman", exact = FALSE)
      )
      
      tibble(
        n       = sum(ok),
        rho     = unname(test$estimate),
        S       = unname(test$statistic),
        p_value = test$p.value
      )
    })
  ) %>%
  unnest(res) %>%
  mutate(
    p_adj_holm = p.adjust(p_value, method = "holm"),  # correction pour 4 tests
    signif = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

print(spearman_traps_aod, width = Inf)

###############################################################################
# SPEARMAN: TRAPS (AREA, COUNT) vs AOD (JUNE, JUNE-AUGUST)
#    -> per lake + composite, matched by YEAR
###############################################################################

dir_trap <- "D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/"
lakes      <- c("DAVE", "GAROT", "LOUP", "NANO", "PESSIERE", "SCHON", "WALT")
vars       <- c(lakes, "COMPOSITE")
plot_order <- c("LOUP", "NANO", "DAVE", "WALT", "GAROT", "SCHON", "PESSIERE", "COMPOSITE")
lake_names <- c(DAVE = "Dave", GAROT = "Garot", LOUP = "Loup", NANO = "Nano",
                PESSIERE = "Pessière", SCHON = "Schön", WALT = "Walt",
                COMPOSITE = "Composite (sum)")

read_csv_fr <- function(path) read.csv(path, header = TRUE, sep = ";", dec = ",")

spearman_by_year <- function(x_df, y_df, sites = vars, x_label = NA, y_label = NA) {
  do.call(rbind, lapply(sites, function(v) {
    d <- merge(x_df[, c("YEAR", v)], y_df[, c("YEAR", v)],
               by = "YEAR", suffixes = c("_x", "_y"))
    x  <- d[[paste0(v, "_x")]]
    y  <- d[[paste0(v, "_y")]]
    ok <- complete.cases(x, y)
    tst <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
    data.frame(X = x_label, Y = y_label, Site = v, n_years = sum(ok),
               rho = unname(tst$estimate), p_value = tst$p.value)
  }))
}

add_signif <- function(tab) {
  tab$signif <- as.character(cut(tab$p_value,
                                 breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                                 labels = c("***", "**", "*", ".", "ns")))
  tab
}

traps_area <- read_csv_fr(paste0(dir_trap, "Area_mm2_Trappes_2011_2025.csv"))
traps_nbr  <- read_csv_fr(paste0(dir_trap, "Nbr_Trappes_2011_2025.csv"))

# ---- AOD ----
make_aod_table <- function(aod_values, years) {
  out <- data.frame(YEAR = years)
  for (v in vars) out[[v]] <- aod_values
  out
}
aod_june   <- make_aod_table(df$Total_AOD_Mean_June,           df$Year)
aod_summer <- make_aod_table(df$Total_AOD_Mean_June_to_August, df$Year)

# ---- Excluded years(2013, 2023)
excl_years <- c()
drop_years_simple <- function(x) x[!(x$YEAR %in% excl_years), , drop = FALSE]

traps_area_f <- drop_years_simple(traps_area)
traps_nbr_f  <- drop_years_simple(traps_nbr)
aod_june_f   <- drop_years_simple(aod_june)
aod_summer_f <- drop_years_simple(aod_summer)

a1 <- spearman_by_year(traps_area_f, aod_june_f,   vars, "Traps area",  "AOD June")
a2 <- spearman_by_year(traps_area_f, aod_summer_f, vars, "Traps area",  "AOD Jun-Aug")
a3 <- spearman_by_year(traps_nbr_f,  aod_june_f,   vars, "Traps count", "AOD June")
a4 <- spearman_by_year(traps_nbr_f,  aod_summer_f, vars, "Traps count", "AOD Jun-Aug")

corr_aod <- add_signif(rbind(a1, a2, a3, a4))
corr_aod$Combination <- paste(corr_aod$X, "vs", corr_aod$Y)

corr_aod$p_adj_holm <- ave(corr_aod$p_value, corr_aod$Site,
                           FUN = function(p) p.adjust(p, method = "holm"))

print(a1); print(a2); print(a3); print(a4)

corr_aod_wide <- reshape(corr_aod[, c("Site", "Combination", "rho")],
                         idvar = "Site", timevar = "Combination", direction = "wide")
print(corr_aod_wide)

# Matrices rho
combo_levels_aod <- c("Traps area vs AOD June", "Traps area vs AOD Jun-Aug",
                      "Traps count vs AOD June", "Traps count vs AOD Jun-Aug")

rho_mat <- with(corr_aod, tapply(rho,     list(Site, Combination), c))
p_mat   <- with(corr_aod, tapply(p_value, list(Site, Combination), c))
rho_mat <- rho_mat[plot_order, combo_levels_aod]
p_mat   <- p_mat[plot_order,   combo_levels_aod]
rownames(rho_mat) <- rownames(p_mat) <- lake_names[plot_order]

# ---- Corrplot -----------
stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                                                     ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", ""))))

corrplot(rho_mat,
         method  = "color",
         is.corr = TRUE,
         col     = colorRampPalette(c("#4575b4", "#ffffbf", "#d73027"))(200),
         tl.col  = "black", tl.cex = 0.8, tl.srt = 45,
         cl.pos  = "r",
         mar     = c(0, 0, 1, 0))

nr <- nrow(rho_mat)
for (i in seq_len(nr)) {
  for (j in seq_len(ncol(rho_mat))) {
    text(j, nr - i + 1,
         labels = paste0(sprintf("%.2f", rho_mat[i, j]), stars(p_mat[i, j])),
         cex = 0.8, font = ifelse(p_mat[i, j] < 0.05, 2, 1))
  }
}

# ---- Heatmap ggplot ----------------------
heat_aod <- corr_aod
heat_aod$Site        <- factor(heat_aod$Site, levels = plot_order, labels = lake_names[plot_order])
heat_aod$Combination <- factor(heat_aod$Combination, levels = rev(combo_levels_aod))
heat_aod$label       <- paste0(sprintf("%.2f", heat_aod$rho), "\n",
                               ifelse(heat_aod$signif == "ns", "", heat_aod$signif))

fig_heat_aod <- ggplot(heat_aod, aes(x = Site, y = Combination, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 3.8, lineheight = 0.9) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1), name = "Spearman\nrho") +
  coord_fixed(ratio = 0.8) +
  labs(x = NULL, y = NULL,
       caption = paste0("*** p<0.001  ** p<0.01  * p<0.05  . p<0.1  (no symbol: p>=0.1)",
                        if (length(excl_years) > 0)
                          paste0("\nYears excluded: ", paste(excl_years, collapse = ", ")) else "")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_blank(), axis.ticks = element_blank())
print(fig_heat_aod)
