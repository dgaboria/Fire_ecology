###############################################################################
# Charcoal deposition in sediment traps (area and count) vs. historical fire
# activity (30 and 100 km buffers) and regional aerosols
# Boreal forests of northwestern Quebec, 2011-2025
###############################################################################

if (!is.null(dev.list())) dev.off()
rm(list = ls())

# ---- Libraries --------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(cowplot)
library(corrplot)
library(Kendall)
library(trend)
library(FactoMineR)
library(factoextra)

# ---- Paths (modify here only) -----------------------------------------------
dir_trap <- "D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/"
dir_proj <- "D:/Projects/Projet_trappes_pollen/"
dir_scr  <- "D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/"
dir_heat <- paste0(dir_proj, "Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/")
dir_aero <- paste0(dir_proj, "Aerosols/")


# ---- Constants / helpers ----------------------------------------------------
lakes      <- c("DAVE", "GAROT", "LOUP", "NANO", "PESSIERE", "SCHON", "WALT")
vars       <- c(lakes, "COMPOSITE")
plot_order <- c("LOUP", "NANO", "DAVE", "WALT", "GAROT", "SCHON", "PESSIERE", "COMPOSITE")

lake_names <- c(DAVE = "Dave", GAROT = "Garot", LOUP = "Loup", NANO = "Nano",
                PESSIERE = "Pessière", SCHON = "Schön", WALT = "Walt",
                COMPOSITE = "Composite (sum)")
lake_lat   <- c(DAVE = "Dave 52.1°N", GAROT = "Garot 51.1°N", LOUP = "Loup 53.1°N",
                NANO = "Nano 53.0°N", PESSIERE = "Pessière 49.5°N",
                SCHON = "Schön 50.6°N", WALT = "Walt 51.9°N")

read_csv_fr <- function(path) read.csv(path, header = TRUE, sep = ";", dec = ",")

# Spearman between two tables (same column names, matched BY YEAR)
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

###############################################################################
# 1) HISTORICAL FIRE ACTIVITY 2011-2025 (AREA BURNED + NUMBER OF FIRES)
###############################################################################

# Files: historical area burned (km2) and number of fires in 100 km and 30 km
# buffers around each lake, per year
read_archives <- function(suffix) {
  setNames(lapply(vars, function(v) read_csv_fr(paste0(dir_trap, v, suffix))), vars)
}
arch_area_raw <- read_archives("_archivesbuffer.csv")
arch_nbr_raw  <- read_archives("_archivesbuffernbr.csv")

# Finds the buffer column even when its name is irregular
# (X100km_area, X100km_area_km2, X30km_area_km2...) and stops with a clear error
get_col <- function(df, buffer, site) {
  col <- grep(paste0("^X", buffer, "km_area(_km2)?$"), names(df), value = TRUE)
  if (length(col) != 1)
    stop("Column not found/ambiguous for ", site, " (", buffer, " km). Columns: ",
         paste(names(df), collapse = ", "))
  df[[col]]
}

build_archive <- function(raw_list, buffer) {
  out <- data.frame(YEAR = raw_list[["DAVE"]]$YEAR)
  for (v in vars) {
    stopifnot(identical(raw_list[[v]]$YEAR, out$YEAR))   # same years everywhere
    out[[v]] <- get_col(raw_list[[v]], buffer, v)
  }
  out
}

arch_area_100 <- build_archive(arch_area_raw, 100)
arch_nbr_100  <- build_archive(arch_nbr_raw,  100)
arch_area_30  <- build_archive(arch_area_raw, 30)
arch_nbr_30   <- build_archive(arch_nbr_raw,  30)

# ---- Figure: area burned (bars) + number of fires (line) --------------------
fire_axes <- function(var, buffer) {
  if (buffer == 100) {
    if (var == "COMPOSITE") {
      list(prim = c(0, 16000), brk = seq(0, 16000, 4000), sec = c(0, 60), sbrk = pretty(c(0, 60), n = 6))
    } else {
      list(prim = c(0, 8000), brk = seq(0, 8000, 2000), sec = c(0, 20), sbrk = pretty(c(0, 20), n = 6))
    }
  } else {
    if (var == "COMPOSITE") {
      list(prim = c(0, 2000), brk = pretty(c(0, 2000), n = 6), sec = c(0, 10), sbrk = pretty(c(0, 10), n = 6))
    } else if (var %in% c("SCHON", "PESSIERE")) {
      list(prim = c(0, 5), brk = 0:5, sec = c(0, 4), sbrk = 0:4)
    } else if (var %in% c("DAVE", "WALT", "GAROT")) {
      list(prim = c(0, 150), brk = pretty(c(0, 150), n = 6), sec = c(0, 4), sbrk = 0:4)
    } else {
      list(prim = c(0, 1500), brk = pretty(c(0, 1500), n = 6), sec = c(0, 4), sbrk = 0:4)
    }
  }
}

make_fire_plot <- function(var, title, area_df, nbr_df, buffer) {
  ax  <- fire_axes(var, buffer)
  div <- ax$sec[2] / ax$prim[2]
  
  # minimum visible height (0.1 % of the axis) for years with fire
  min_h <- 0.001 * ax$prim[2]
  area_plot <- data.frame(YEAR = area_df$YEAR,
                          val  = ifelse(area_df[[var]] > 0, pmax(area_df[[var]], min_h), NA))
  
  ggplot() +
    geom_col(data = area_plot, aes(x = YEAR, y = val),
             width = 0.5, fill = "#3A5FCD", na.rm = TRUE) +
    geom_line(data = nbr_df, aes(x = YEAR, y = .data[[var]] / div),
              color = "black", linewidth = 1) +
    scale_x_continuous(breaks = seq(2011, 2025, 1)) +
    scale_y_continuous(name = "Area burned (km²)", limits = ax$prim, breaks = ax$brk,
                       sec.axis = sec_axis(~ . * div, name = "Fire occurrences",
                                           breaks = ax$sbrk)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y.left  = element_text(color = "#3A5FCD", size = 13),
          axis.text.y.left   = element_text(color = "#3A5FCD", size = 12),
          axis.title.y.right = element_text(color = "black", size = 13),
          axis.text.y.right  = element_text(color = "black", size = 12),
          axis.line.y.left   = element_line(color = "#3A5FCD"),
          axis.ticks.y.left  = element_line(color = "#3A5FCD")) +
    annotate("text", x = -Inf, y = Inf, label = title,
             hjust = -0.1, vjust = 1.5, fontface = "bold", size = 7)
}

make_fire_figure <- function(area_df, nbr_df, buffer) {
  plots <- lapply(plot_order, function(v)
    make_fire_plot(v, paste0(" ", lake_names[v]), area_df, nbr_df, buffer))
  ggarrange(plotlist = plots, ncol = 3, nrow = 3)
}

fig_fire_100 <- make_fire_figure(arch_area_100, arch_nbr_100, 100); print(fig_fire_100)
# ggsave("figure_fires_100.pdf", fig_fire_100, width = 13, height = 11, units = "in", device = cairo_pdf)

fig_fire_30 <- make_fire_figure(arch_area_30, arch_nbr_30, 30); print(fig_fire_30)
# ggsave("figure_fires_30.pdf", fig_fire_30, width = 13, height = 11, units = "in", device = cairo_pdf)

# Composite values
composite_100 <- data.frame(YEAR = arch_area_100$YEAR,
                            AREA_100km = arch_area_100$COMPOSITE,
                            NBR_100km  = arch_nbr_100$COMPOSITE)
composite_30  <- data.frame(YEAR = arch_area_30$YEAR,
                            AREA_30km = arch_area_30$COMPOSITE,
                            NBR_30km  = arch_nbr_30$COMPOSITE)
print(composite_100)
print(composite_30)

# Spearman, archives: area burned vs number of fires (per lake + composite)
corr_arch_100 <- add_signif(spearman_by_year(arch_area_100, arch_nbr_100, vars,
                                             "Area burned 100 km", "Nbr fires 100 km"))
corr_arch_30  <- add_signif(spearman_by_year(arch_area_30, arch_nbr_30, vars,
                                             "Area burned 30 km", "Nbr fires 30 km"))
print(corr_arch_100)
print(corr_arch_30)

###############################################################################
# 2) CHARCOAL PARTICLES FROM TRAPS 2011-2025 (AREA mm2 + COUNT)
###############################################################################

traps_area <- read_csv_fr(paste0(dir_trap, "Area_mm2_Trappes_2011_2025.csv"))
traps_nbr  <- read_csv_fr(paste0(dir_trap, "Nbr_Trappes_2011_2025.csv"))

make_traps_plot <- function(var, title, area_df, nbr_df) {
  is_comp <- var == "COMPOSITE"
  prim <- if (is_comp) c(0, 100) else c(0, 40)
  sec  <- if (is_comp) c(0, 600) else if (var %in% c("LOUP", "NANO")) c(0, 250) else c(0, 80)
  div  <- sec[2] / prim[2]
  
  ggplot() +
    geom_bar(data = area_df, aes(x = YEAR, y = .data[[var]]),
             stat = "identity", width = 0.5, fill = "red") +
    geom_line(data = nbr_df, aes(x = YEAR, y = .data[[var]] / div),
              color = "black", linewidth = 1) +
    scale_x_continuous(breaks = seq(2011, 2025, 1)) +
    scale_y_continuous(name = expression("Char area (" * mm^2 * ")"),
                       limits = prim, breaks = pretty(prim, n = 6),
                       sec.axis = sec_axis(~ . * div, name = "Char count",
                                           breaks = pretty(sec, n = 6))) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13, color = "black"),
          axis.title.x = element_blank(),
          axis.title.y.left  = element_text(color = "red", size = 13),
          axis.text.y.left   = element_text(color = "red", size = 12),
          axis.title.y.right = element_text(color = "black", size = 13),
          axis.text.y.right  = element_text(color = "black", size = 12),
          axis.line.y.left   = element_line(color = "red"),
          axis.ticks.y.left  = element_line(color = "red")) +
    annotate("text", x = -Inf, y = Inf, label = title,
             hjust = -0.1, vjust = 1.5, fontface = "bold", size = 7)
}

p_traps <- setNames(lapply(plot_order, function(v)
  make_traps_plot(v, paste0(" ", lake_names[v]), traps_area, traps_nbr)), plot_order)

fig_charcoal <- ggarrange(plotlist = p_traps, ncol = 3, nrow = 3); print(fig_charcoal)
# ggsave("figure_charbon.pdf", fig_charcoal, width = 15, height = 11, units = "in", device = cairo_pdf)

# Spearman: char AREA vs char COUNT in traps
traps_cols <- setdiff(colnames(traps_area), "YEAR")
corr_traps <- add_signif(spearman_by_year(traps_area, traps_nbr, traps_cols,
                                          "Traps area", "Traps count"))
print(corr_traps)

###############################################################################
# 3) SPEARMAN: TRAPS (AREA, COUNT) vs FIRE ARCHIVES (AREA BURNED 100 / 30 km)
#    -> per lake + composite, matched by YEAR
###############################################################################

r1 <- spearman_by_year(traps_area, arch_area_100, vars, "Traps area",  "Area burned 100 km")
r2 <- spearman_by_year(traps_area, arch_area_30,  vars, "Traps area",  "Area burned 30 km")
r3 <- spearman_by_year(traps_nbr,  arch_area_100, vars, "Traps count", "Area burned 100 km")
r4 <- spearman_by_year(traps_nbr,  arch_area_30,  vars, "Traps count", "Area burned 30 km")

corr_all <- add_signif(rbind(r1, r2, r3, r4))
corr_all$Combination <- paste(corr_all$X, "vs", corr_all$Y)

print(r1); print(r2); print(r3); print(r4)

# Optional: multiple-testing correction (32 tests)
# corr_all$p_adj <- p.adjust(corr_all$p_value, method = "BH")

# Wide table (rho per site x combination)
corr_wide <- reshape(corr_all[, c("Site", "Combination", "rho")],
                     idvar = "Site", timevar = "Combination", direction = "wide")
print(corr_wide)

#write.csv2(transform(corr_all, rho = round(rho, 2), p_value = round(p_value, 3)),
#paste0(dir_trap, "Spearman_traps_vs_archives.csv"), row.names = FALSE)

# ---- Heatmap of rho (stars = significance) ----------------------------------
combo_levels <- c("Traps area vs Area burned 100 km", "Traps area vs Area burned 30 km",
                  "Traps count vs Area burned 100 km", "Traps count vs Area burned 30 km")
heat_df <- corr_all
heat_df$Site        <- factor(heat_df$Site, levels = plot_order, labels = lake_names[plot_order])
heat_df$Combination <- factor(heat_df$Combination, levels = rev(combo_levels))
heat_df$label       <- paste0(sprintf("%.2f", heat_df$rho), "\n",
                              ifelse(heat_df$signif == "ns", "", heat_df$signif))

fig_heat_rho <- ggplot(heat_df, aes(x = Site, y = Combination, fill = rho)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), size = 3.8, lineheight = 0.9) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1), name = "Spearman\nrho") +
  coord_fixed(ratio = 0.8) +
  labs(x = NULL, y = NULL,
       caption = "*** p<0.001  ** p<0.01  * p<0.05  . p<0.1  (no symbol: p>=0.1)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.line = element_blank(), axis.ticks = element_blank())
print(fig_heat_rho)
# ggsave("heatmap_spearman_traps_archives.pdf", fig_heat_rho, width = 11, height = 4.5)

###############################################################################
# 4) NORMALITY TESTS (appendix)
###############################################################################

make_hist <- function(df, var) {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(fill = "#3A5FCD", color = "white", bins = 10) +
    theme_classic() + ggtitle(var)
}
hist_figure <- function(df) ggarrange(plotlist = lapply(plot_order, function(v) make_hist(df, v)),
                                      ncol = 3, nrow = 3)
print(hist_figure(traps_area))
print(hist_figure(traps_nbr))
print(hist_figure(arch_area_100))

# Shapiro-Wilk (n < 50): p > 0.05 = compatible with normality
shapiro_tab <- function(df, label) {
  do.call(rbind, lapply(vars, function(v) {
    x <- df[[v]][!is.na(df[[v]])]
    s <- tryCatch(shapiro.test(x), error = function(e) NULL)
    data.frame(Dataset = label, Site = v,
               W = if (is.null(s)) NA else unname(s$statistic),
               p_value = if (is.null(s)) NA else s$p.value,
               normal = if (is.null(s)) NA else s$p.value > 0.05)
  }))
}
shapiro_all <- rbind(shapiro_tab(traps_area,    "Traps area"),
                     shapiro_tab(traps_nbr,     "Traps count"),
                     shapiro_tab(arch_area_100, "Archives area 100 km"))
print(shapiro_all)

# QQ plots (count and area, per site and composite)
op <- par(mfrow = c(4, 4))
for (v in vars) {
  qqnorm(traps_nbr[[v]],  main = paste(v, "- count"));  qqline(traps_nbr[[v]])
  qqnorm(traps_area[[v]], main = paste(v, "- area"));   qqline(traps_area[[v]])
}
par(op)

# Data are not normally distributed -> Spearman correlations.
# We keep COUNT for the following analyses (more sensitive to extreme pulses
# and significant for the regional composite).

###############################################################################
# 5) AEROSOLS
# TODO (collaborator's note): Wilcoxon tests comparing annual char count,
# annual area burned and aerosols as boxplots (compare annual medians).
###############################################################################
getwd()
median_char <- read_csv_fr(paste0(dir_scr, "Median_CHAR_area_2011_2025.csv"))  # loaded BEFORE use
aerosol <- read.csv('D:/PROJETS_POSTDOC/Trappes_charbon/Datasets/Aerosols/Aerosols.csv', header = TRUE, sep = ";", dec = ".")
aerosol$Mean_aerosol <- rowMeans(aerosol[, 2:6], na.rm = TRUE)

# Rows removed (same as the old aerosol[-7,]; [-7,]; [-12,] done in sequence
# = original rows 7, 8 and 14). Check the years match before trusting results:
drop_rows <- c(7, 8, 14,15)

aerosol_c <- aerosol[-drop_rows, ]
area_aero <- traps_area[-drop_rows, ]
nbr_aero  <- traps_nbr[-drop_rows, ]
med_aero  <- median_char[-drop_rows, ]

aerosol_corr <- function(df, label) {
  series <- c(lapply(lakes, function(v) df[[v]]), list(rowMeans(df[, lakes])))
  names(series) <- c(lakes, "Mean of lakes")
  out <- do.call(rbind, lapply(names(series), function(nm) {
    tst <- suppressWarnings(cor.test(aerosol_c$Mean_aerosol, series[[nm]],
                                     method = "spearman", exact = FALSE))
    data.frame(Variable = label, Site = nm, rho = unname(tst$estimate), p_value = tst$p.value)
  }))
  rownames(out) <- NULL
  out
}
corr_aero <- rbind(aerosol_corr(area_aero, "Traps area vs aerosols"),
                   aerosol_corr(nbr_aero,  "Traps count vs aerosols"),
                   aerosol_corr(med_aero,  "Median char size vs aerosols"))
print(corr_aero)


