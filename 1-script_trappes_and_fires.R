dev.off()
rm(list = ls())  # Deleting variables from the environment R

#### Relationships between charcoal deposition (area and count) in sediment traps
#### historical fire activity (30–100 km buffer)
#### regional atmospheric fine-particle and aerosol concentrations
#### in the boreal forests of northwestern Quebec from 2011 to 2025

# Importation of libraries
library(devtools)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(patchwork)
library(readr)

################# ################# ################# ################# ################# ################# 
################# ################# ################# ################# ################# ################# 
################# Historical fire activity 2011-2025 (AREA + NUMBER)    ################# ################# 
################# ################# ################# ################# ################# ################# 
################# ################# ################# ################# ################# ################# 

# These files contain historical area burned (km2) in 100km and 30km buffers around lakes during each year
archive_Dave<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/DAVE_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Garot<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/GAROT_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Loup<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/LOUP_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Nano<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/NANO_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Pessiere<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/PESSIERE_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Schon<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/SCHON_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Walt<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/WALT_archivesbuffer.csv",h=T,sep=";", dec = ',')
archive_Composite<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/COMPOSITE_archivesbuffer.csv",h=T,sep=";", dec = ',')

# These files contain historical number of fires in 100km and 30km buffers around lakes during each year
archive_Davenbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/DAVE_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Garotnbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/GAROT_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Loupnbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/LOUP_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Nanonbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/NANO_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Pessierenbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/PESSIERE_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Schonnbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/SCHON_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Waltnbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/WALT_archivesbuffernbr.csv",h=T,sep=";", dec = ',')
archive_Compositenbr<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/COMPOSITE_archivesbuffernbr.csv",h=T,sep=";", dec = ',')

#100 km

#On combine les donnees d'archives dans une meme table
# ---------------------------------------------------------
# 0) Construction des tableaux area et nbr
# ---------------------------------------------------------

archive = as.data.frame(cbind(
  archive_Dave$YEAR, archive_Dave$X100km_area,
  archive_Garot$X100km_area_km2, archive_Loup$X100km_area,
  archive_Nano$X100km_area, archive_Pessiere$X100km_area,
  archive_Schon$X100km_area, archive_Walt$X100km_area,
  archive_Composite$X100km_area
))

archivenbr = as.data.frame(cbind(
  archive_Davenbr$YEAR, archive_Davenbr$X100km_area,
  archive_Garotnbr$X100km_area_km2, archive_Loupnbr$X100km_area,
  archive_Nanonbr$X100km_area, archive_Pessierenbr$X100km_area,
  archive_Schonnbr$X100km_area, archive_Waltnbr$X100km_area,
  archive_Compositenbr$X100km_area
))

colnames(archive)    = c('YEAR','DAVE','GAROT','LOUP','NANO','PESSIERE','SCHON','WALT','COMPOSITE')
colnames(archivenbr) = c('YEAR','DAVE','GAROT','LOUP','NANO','PESSIERE','SCHON','WALT','COMPOSITE')

library(ggplot2)
library(ggpubr)
library(ggtext)

# ---------------------------------------------------------
# 1) Préparation des données
# ---------------------------------------------------------

area <- archive
nbr  <- archivenbr

# ---------------------------------------------------------
# 2) Fonction générique avec axes fixes
# ---------------------------------------------------------

make_plot <- function(var, title) {
  
  # Détection composite vs lac
  is_composite <- (var == "COMPOSITE")
  
  # Axe primaire fixe
  ylim_primary <- if (is_composite) c(0, 16000) else c(0, 8000)
  breaks_primary <- seq(0, ylim_primary[2], by = if (is_composite) 4000 else 2000)
  
  # Axe secondaire fixe
  ylim_secondary <- if (is_composite) c(0, 60) else c(0, 20)
  breaks_secondary <- pretty(ylim_secondary, n = 6)
  
  # Division optimale pour aligner la courbe dans l'axe primaire fixe
  div <- ylim_secondary[2] / ylim_primary[2]
  
  # Hauteur minimale visible (1,5 % de l'axe) pour les années avec feu
  min_h <- 0.001 * ylim_primary[2]
  area_plot <- data.frame(
    YEAR = area$YEAR,
    val  = ifelse(area[[var]] > 0, pmax(area[[var]], min_h), NA)
  )
  
  ggplot() +
    geom_col(data = area_plot,
             aes(x = YEAR, y = val),
             width = 0.5, fill = "#3A5FCD", na.rm = TRUE) +
    
    geom_line(data = nbr,
              aes(x = YEAR, y = .data[[var]] / div),
              color = "black", size = 1) +
    
    scale_x_continuous(breaks = seq(2011, 2025, 1)) +
    
    scale_y_continuous(
      name = "Area burned (km²)",
      limits = ylim_primary,
      breaks = breaks_primary,
      sec.axis = sec_axis(
        trans = ~ . * div,
        name = "Fire occurrences",
        breaks = breaks_secondary
      )
    ) +
    
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13, color = "black"),
      axis.title.x = element_blank(),
      axis.title.y.left = element_text(color = "#3A5FCD", size = 13),
      axis.text.y.left  = element_text(color = "#3A5FCD", size = 12),
      axis.title.y.right = element_text(color = "black", size = 13),
      axis.text.y.right  = element_text(color = "black", size = 12),
      axis.line.y.left  = element_line(color = "#3A5FCD"),
      axis.ticks.y.left = element_line(color = "#3A5FCD")
    ) +
    
    annotate("text", x = -Inf, y = Inf, label = title,
             hjust = -0.1, vjust = 1.5, fontface = "bold", size = 7)
}


# ---------------------------------------------------------
# 3) Figures pour chaque lac
# ---------------------------------------------------------

dave      <- make_plot("DAVE",      " Dave")
garot     <- make_plot("GAROT",     " Garot")
loup      <- make_plot("LOUP",      " Loup")
nano      <- make_plot("NANO",      " Nano")
pessiere  <- make_plot("PESSIERE",  " Pessière")
schon     <- make_plot("SCHON",     " Schön")
walt      <- make_plot("WALT",      " Walt")
composite <- make_plot("COMPOSITE", "Composite (sum)")

# ---------------------------------------------------------
# 4) Assemblage final
# ---------------------------------------------------------

fig = ggarrange(loup, nano, dave, walt, garot, schon, pessiere, composite,
                ncol = 3, nrow = 3)

getwd()
ggsave("figure_fires_100.pdf", fig,
       width = 13, height = 11, units = "in", device = cairo_pdf)

# Valeurs du composite (area)
composite_area <- data.frame(
  YEAR = archive$YEAR,
  AREA_100km = archive$COMPOSITE
)
composite_area

# Valeurs du composite (nbr)
composite_nbr <- data.frame(
  YEAR = archivenbr$YEAR,
  NBR_100km = archivenbr$COMPOSITE
)
composite_nbr

cor(area$COMPOSITE, nbr$COMPOSITE, method = "spearman")
cor.test(area$COMPOSITE, nbr$COMPOSITE, method = "spearman")

###############################################
# SPEARMAN correlations for each lake
###############################################

lakes <- c("DAVE","GAROT","LOUP","NANO","PESSIERE","SCHON","WALT")

compute_composite_corr <- function(area_df, nbr_df, lakes){
  out <- data.frame(
    Lake = lakes,
    Spearman = NA,
    p_value = NA
  )
  
  for(i in seq_along(lakes)){
    lake <- lakes[i]
    rho <- cor(area_df[[lake]], nbr_df[[lake]], method = "spearman")
    test <- cor.test(area_df[[lake]], nbr_df[[lake]], method = "spearman")
    
    out$Spearman[i] <- rho
    out$p_value[i]  <- test$p.value
  }
  
  return(out)
}

# Résultat
corr_composite <- compute_composite_corr(area, nbr, lakes)
print(corr_composite)



#########################################################################################################
#########################################################################################################
#########################################################################################################

#30 km

#On combine les donnees d'archives dans une meme table
# ---------------------------------------------------------
# Construction des tableaux area et nbr
# ---------------------------------------------------------
archive = as.data.frame(cbind(archive_Dave$YEAR, archive_Dave$X30km_area, archive_Garot$X30km_area, archive_Loup$X30km_area, archive_Nano$X30km_area, archive_Pessiere$X30km_area, archive_Schon$X30km_area, archive_Walt$X30km_area, archive_Composite$X30km_area_km2))
archivenbr = as.data.frame(cbind(archive_Davenbr$YEAR, archive_Davenbr$X30km_area, archive_Garotnbr$X30km_area, archive_Loupnbr$X30km_area, archive_Nanonbr$X30km_area, archive_Pessierenbr$X30km_area, archive_Schonnbr$X30km_area, archive_Waltnbr$X30km_area, archive_Compositenbr$X30km_area))
colnames(archive) = c('YEAR', 'DAVE', 'GAROT', 'LOUP', 'NANO', 'PESSIERE', 'SCHON', 'WALT', 'COMPOSITE')
colnames(archivenbr) = c('YEAR', 'DAVE', 'GAROT', 'LOUP', 'NANO', 'PESSIERE', 'SCHON', 'WALT', 'COMPOSITE')

library(ggplot2)
library(ggpubr)
library(ggtext)

# ---------------------------------------------------------
# 1) Préparation des données
# ---------------------------------------------------------
area <- archive
nbr  <- archivenbr

# ---------------------------------------------------------
# 2) Fonction générique avec axes fixes
# ---------------------------------------------------------

make_plot <- function(var, title) {
  
  # Groupes de lacs
  small_lakes <- c("DAVE", "WALT", "GAROT")
  very_small_lakes <- c("SCHON", "PESSIERE")
  
  # Détection composite vs lac
  is_composite <- (var == "COMPOSITE")
  is_small     <- (var %in% small_lakes)
  is_very_small <- (var %in% very_small_lakes)
  
  # Axe primaire
  ylim_primary <- if (is_composite) {
    c(0, 2000)
  } else if (is_very_small) {
    c(0, 5)
  } else if (is_small) {
    c(0, 150)
  } else {
    c(0, 1500)
  }
  
  # Breaks primaires
  breaks_primary <- if (is_very_small) {
    0:5
  } else {
    pretty(ylim_primary, n = 6)
  }
  
  # Axe secondaire : fixe pour les lacs, dynamique pour composite
  if (is_composite) {
    ylim_secondary  <- c(0, 10)
    breaks_secondary <- pretty(ylim_secondary, n = 6)
  } else {
    ylim_secondary  <- c(0, 4)
    breaks_secondary <- 0:4
  }
  
  # Division pour aligner la courbe
  div <- ylim_secondary[2] / ylim_primary[2]
  
    
# Hauteur minimale visible (1,5 % de l'axe) pour les années avec feu
min_h <- 0.001 * ylim_primary[2]
area_plot <- data.frame(
YEAR = area$YEAR,
val  = ifelse(area[[var]] > 0, pmax(area[[var]], min_h), NA))
  
  ggplot() +
    geom_col(data = area_plot,
             aes(x = YEAR, y = val),
             width = 0.5, fill = "#3A5FCD", na.rm = TRUE) +
    
    geom_line(data = nbr,
              aes(x = YEAR, y = .data[[var]] / div),
              color = "black", size = 1) +
    
    scale_x_continuous(breaks = seq(2011, 2025, 1)) +
    
    scale_y_continuous(
      name = "Area burned (km²)",
      limits = ylim_primary,
      breaks = breaks_primary,
      sec.axis = sec_axis(
        trans = ~ . * div,
        name = "Fire occurrences",
        breaks = breaks_secondary
      )
    ) +
    
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13, color = "black"),
      axis.title.x = element_blank(),
      axis.title.y.left = element_text(color = "#3A5FCD", size = 13),
      axis.text.y.left  = element_text(color = "#3A5FCD", size = 12),
      axis.title.y.right = element_text(color = "black", size = 13),
      axis.text.y.right  = element_text(color = "black", size = 12),
      axis.line.y.left  = element_line(color = "#3A5FCD"),
      axis.ticks.y.left = element_line(color = "#3A5FCD")
    ) +
    
    annotate("text", x = -Inf, y = Inf, label = title,
             hjust = -0.1, vjust = 1.5, fontface = "bold", size = 7)
}


# ---------------------------------------------------------
# 3) Figures pour chaque lac
# ---------------------------------------------------------
dave      <- make_plot("DAVE",      " Dave")
garot     <- make_plot("GAROT",     " Garot")
loup      <- make_plot("LOUP",      " Loup")
nano      <- make_plot("NANO",      " Nano")
pessiere  <- make_plot("PESSIERE",  " Pessière")
schon     <- make_plot("SCHON",     " Schön")
walt      <- make_plot("WALT",      " Walt")
composite <- make_plot("COMPOSITE", "Composite (sum)")

# ---------------------------------------------------------
# 4) Assemblage final
# ---------------------------------------------------------

fig = ggarrange(loup, nano, dave, walt, garot, schon, pessiere, composite,
                ncol = 3, nrow = 3)

getwd()
ggsave("figure_fires_30.pdf", fig,
       width = 13, height = 11, units = "in", device = cairo_pdf)

# Valeurs du composite (area)
composite_area <- data.frame(
  YEAR = archive$YEAR,
  AREA_100km = archive$COMPOSITE
)
composite_area

# Valeurs du composite (nbr)
composite_nbr <- data.frame(
  YEAR = archivenbr$YEAR,
  NBR_100km = archivenbr$COMPOSITE
)
composite_nbr

cor(area$COMPOSITE, nbr$COMPOSITE, method = "spearman")
cor.test(area$COMPOSITE, nbr$COMPOSITE, method = "spearman")

###############################################
# SPEARMAN correlations for each lake
###############################################

lakes <- c("DAVE","GAROT","LOUP","NANO","PESSIERE","SCHON","WALT")

compute_composite_corr <- function(area_df, nbr_df, lakes){
  out <- data.frame(
    Lake = lakes,
    Spearman = NA,
    p_value = NA
  )
  
  for(i in seq_along(lakes)){
    lake <- lakes[i]
    rho <- cor(area_df[[lake]], nbr_df[[lake]], method = "spearman")
    test <- cor.test(area_df[[lake]], nbr_df[[lake]], method = "spearman")
    
    out$Spearman[i] <- rho
    out$p_value[i]  <- test$p.value
  }
  
  return(out)
}

# Result
corr_composite <- compute_composite_corr(area, nbr, lakes)
print(corr_composite)

################# ################# ################# ################# ################# ################# 
################# ################# ################# ################# ################# ################# 
################# CHARCOAL PARTICLE FROM TRAPS   ################# ################# 
################# ################# ################# ################# ################# ################# 
################# ################# ################# ################# ################# ################# 

# charcoal from traps 2011_2025 (AREA + NUMBER)
area<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/Area_mm2_Trappes_2011_2025.csv",h=T,sep=";", dec = ',')
nbr<-read.csv('D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/Nbr_Trappes_2011_2025.csv',h=T,sep=";", dec = ',')

#########################################################################################################
#FIGURE CHARCOALS (Area mm2 + Count)
#########################################################################################################
library(ggplot2)
library(ggtext)

# ---- Labels HTML pour sec.axis ----
sec_labels <- function(x){
  ifelse(
    x %in% c(2013, 2023, 2024),
    sprintf("<span style='font-weight:bold; font-size:14pt'>%s</span>", x),
    sprintf("<span style='font-size:10pt'>%s</span>", x)
  )
}

ylim.prim = c(0,40)
# ---- Fonction générique pour produire une figure ----


make_plot <- function(var, title) {
  
  # Groupes de lacs pour l’axe secondaire
  high_occ_lakes <- c("LOUP", "NANO")          # 0–250
  low_occ_lakes  <- c("DAVE", "WALT", "GAROT",
                      "SCHON", "PESSIERE")     # 0–80
  
  # Détection composite vs lac
  is_composite <- (var == "COMPOSITE")
  is_high_occ  <- (var %in% high_occ_lakes)
  is_low_occ   <- (var %in% low_occ_lakes)
  
  # Axe primaire
  ylim_primary <- if (is_composite) c(0, 100) else c(0, 40)
  breaks_primary <- pretty(ylim_primary, n = 6)
  
  # Axe secondaire
  if (is_composite) {
    ylim_secondary  <- c(0, 600)
    breaks_secondary <- pretty(ylim_secondary, n = 6)
  } else if (is_high_occ) {
    ylim_secondary  <- c(0, 250)
    breaks_secondary <- pretty(ylim_secondary, n = 6)
  } else {
    ylim_secondary  <- c(0, 80)
    breaks_secondary <- pretty(ylim_secondary, n = 6)
  }
  
  # Division pour aligner la courbe
  div <- ylim_secondary[2] / ylim_primary[2]
  
  ggplot() +
    geom_bar(data = area,
             aes(x = YEAR, y = .data[[var]]),
             stat = "identity", width = 0.5, fill = "red") +
    
    geom_line(data = nbr,
              aes(x = YEAR, y = .data[[var]] / div),
              color = "black", size = 1) +
    
    scale_x_continuous(
      breaks = seq(2011, 2025, 1)
    ) +
    
    scale_y_continuous(
      name = expression("Char area (" * mm^2 * ")"),
      limits = ylim_primary,
      breaks = breaks_primary,
      sec.axis = sec_axis(
        trans = ~ . * div,
        name = "Char count",
        breaks = breaks_secondary
      )
    ) +
    
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13, color = "black"),
      axis.title.x = element_blank(),
      axis.title.y.left = element_text(color = "red", size = 13),
      axis.text.y.left  = element_text(color = "red", size = 12),
      axis.title.y.right = element_text(color = "black", size = 13),
      axis.text.y.right  = element_text(color = "black", size = 12),
      axis.line.y.left  = element_line(color = "red"),
      axis.ticks.y.left = element_line(color = "red")
    ) +
    
    annotate("text", x = -Inf, y = Inf, label = title,
             hjust = -0.1, vjust = 1.5, fontface = "bold", size = 7)
}


# ---------------------------------------------------------
# 3) Figures pour chaque lac
# ---------------------------------------------------------

dave      <- make_plot("DAVE",      " Dave")
garot     <- make_plot("GAROT",     " Garot")
loup      <- make_plot("LOUP",      " Loup")
nano      <- make_plot("NANO",      " Nano")
pessiere  <- make_plot("PESSIERE",  " Pessière")
schon     <- make_plot("SCHON",     " Schön")
walt      <- make_plot("WALT",      " Walt")
composite <- make_plot("COMPOSITE", "Composite (sum)")


# ---------------------------------------------------------
# 4) Assemblage final
# ---------------------------------------------------------

fig = ggarrange(loup, nano, dave, walt, garot, schon, pessiere, composite,
          ncol = 3, nrow = 3)

getwd()
ggsave("figure_charbon.pdf", fig,
       width = 15, height = 11, units = "in", device = cairo_pdf)

############################################################
# SPEARMAN : Char AREA vs Char NBR (Trappes 2011–2025)
############################################################

# Liste des pièges (colonnes identiques dans area et nbr)
traps <- colnames(area)[colnames(area) != "YEAR"]

compute_trap_corr <- function(area_df, nbr_df, traps){
  out <- data.frame(
    Trap = traps,
    Spearman = NA,
    p_value = NA,
    n_years = NA
  )
  
  for(i in seq_along(traps)){
    trap <- traps[i]
    
    rho  <- cor(area_df[[trap]], nbr_df[[trap]], method = "spearman")
    test <- cor.test(area_df[[trap]], nbr_df[[trap]], method = "spearman")
    
    out$Spearman[i] <- rho
    out$p_value[i]  <- test$p.value
    out$n_years[i]  <- sum(!is.na(area_df[[trap]]) & !is.na(nbr_df[[trap]]))
  }
  
  return(out)
}

# Résultat final
corr_traps <- compute_trap_corr(area, nbr, traps)
print(corr_traps)


##############################################################################################################
# Normality test for charcoal distributions and fire archive distributions + transformations of the data
##############################################################################################################

#EN ANNEXES

library(ggplot2)
library(ggpubr)

make_hist <- function(x, title){
  ggplot(area, aes(x = .data[[x]])) +
    geom_histogram(fill = "#3A5FCD", color = "white") +
    theme_classic() +
    ggtitle(title)
}

a <- make_hist("DAVE",      "DAVE")
b <- make_hist("GAROT",     "GAROT")
c <- make_hist("LOUP",      "LOUP")
d <- make_hist("NANO",      "NANO")
e <- make_hist("PESSIERE",  "PESSIERE")
f <- make_hist("SCHON",     "SCHON")
g <- make_hist("WALT",      "WALT")
h <- make_hist("COMPOSITE", "COMPOSITE")

ggarrange(c, d, a, g, b, f, e, h, ncol = 3, nrow = 3)


make_hist_nbr <- function(var){
  ggplot(nbr, aes(x = .data[[var]])) +
    geom_histogram(fill = "#3A5FCD", color = "white") +
    theme_classic() +
    ggtitle(var)
}

h1 <- make_hist_nbr("DAVE")
h2 <- make_hist_nbr("GAROT")
h3 <- make_hist_nbr("LOUP")
h4 <- make_hist_nbr("NANO")
h5 <- make_hist_nbr("PESSIERE")
h6 <- make_hist_nbr("SCHON")
h7 <- make_hist_nbr("WALT")
h8 <- make_hist_nbr("COMPOSITE")

ggarrange(h3, h4, h1, h7, h2, h6, h5, h8,
          ncol = 3, nrow = 3)

# Fonction robuste : détecte automatiquement la bonne colonne
make_hist_archive <- function(df, title){
  
  col <- NULL
  
  # Cas général
  if ("X100km_area" %in% names(df)) {
    col <- "X100km_area"
  }
  
  # Cas particulier : GAROT
  if ("X100km_area_km2" %in% names(df)) {
    col <- "X100km_area_km2"
  }
  
  # Si aucune colonne trouvée → erreur claire
  if (is.null(col)) {
    stop(paste("Aucune colonne X100km_area ou X100km_area_mm2 trouvée dans", title))
  }
  
  ggplot(df, aes(x = .data[[col]])) +
    geom_histogram(fill = "#3A5FCD", color = "white") +
    theme_classic() +
    ggtitle(title)
}

# Création des histogrammes
z1 <- make_hist_archive(archive_Dave,      "Dave")
z2 <- make_hist_archive(archive_Garot,     "Garot")      # ← colonne spéciale gérée automatiquement
z3 <- make_hist_archive(archive_Loup,      "Loup")
z4 <- make_hist_archive(archive_Nano,      "Nano")
z5 <- make_hist_archive(archive_Pessiere,  "Pessière")
z6 <- make_hist_archive(archive_Schon,     "Schön")
z7 <- make_hist_archive(archive_Walt,      "Walt")
z8 <- make_hist_archive(archive_Composite, "Composite")

# Assemblage
ggarrange(z1, z2, z3, z4, z5, z6, z7, z8,
          ncol = 3, nrow = 3)

######################################################
#TEST DE NORMALITÉ
######################################################

# Shapiro Wilk test (for less than 50 values)
#p > 0.05 → données compatibles avec une distribution normale
#p < 0.05 → données non normales

shapiro.test(area$DAVE)
shapiro.test(area$GAROT)
shapiro.test(area$LOUP) # distribution normale
shapiro.test(area$NANO)
shapiro.test(area$PESSIERE)
shapiro.test(area$SCHON)
shapiro.test(area$WALT)
shapiro.test(area$COMPOSITE)

shapiro.test(nbr$DAVE)
shapiro.test(nbr$GAROT)
shapiro.test(nbr$LOUP)
shapiro.test(nbr$NANO)
shapiro.test(nbr$PESSIERE)
shapiro.test(nbr$SCHON)
shapiro.test(nbr$WALT)
shapiro.test(nbr$COMPOSITE)

shapiro.test(archive_Dave$X100km_area)
shapiro.test(archive_Garot$X100km_area)
shapiro.test(archive_Loup$X100km_area)
shapiro.test(archive_Nano$X100km_area)
shapiro.test(archive_Pessiere$X100km_area)
shapiro.test(archive_Schon$X100km_area)
shapiro.test(archive_Walt$X100km_area)
shapiro.test(archive_Composite$X100km_area)

# Representation graphique for number and area of charcoals

qqnorm(nbr$DAVE)
qqline(nbr$DAVE)
qqnorm(area$DAVE)
qqline(area$DAVE)
qqnorm(nbr$GAROT)
qqline(nbr$GAROT)
qqnorm(area$GAROT)
qqline(area$GAROT)
qqnorm(nbr$LOUP)
qqline(nbr$LOUP)
qqnorm(area$LOUP)
qqline(area$LOUP)
qqnorm(nbr$NANO)
qqline(nbr$NANO)
qqnorm(area$NANO)
qqline(area$NANO)
qqnorm(nbr$PESSIERE)
qqline(nbr$PESSIERE)
qqnorm(area$PESSIERE)
qqline(area$PESSIERE)
qqnorm(nbr$SCHON)
qqline(nbr$SCHON)
qqnorm(area$SCHON)
qqline(area$SCHON)
qqnorm(nbr$WALT)
qqline(nbr$WALT)
qqnorm(area$WALT)
qqline(area$WALT)
qqnorm(nbr$COMPOSITE)
qqline(nbr$COMPOSITE)
qqnorm(area$COMPOSITE)
qqline(area$COMPOSITE)

#Les donnees ne sont pas distribuees normalement, on appliquera donc une correlation de spearman pour les comparaisons entre jeux de donnees

#Maintenant on veut savoir si AREA ou NOMBRE de charbons corrèle le mieux avec les archives historiques (surface de superficies brulées dans un buffer de 100 km)

###############################################
# CORRÉLATIONS SPEARMAN LAC ↔ LAC
# ARCHIVE vs AREA  (14 premières années)
# ARCHIVE vs NBR   (14 premières années)
###############################################

vars <- c("DAVE","GAROT","LOUP","NANO","PESSIERE","SCHON","WALT","COMPOSITE")

# ---- Fonction générique ----
compute_corr <- function(df1, df2, vars){
  out <- data.frame(
    Variable = vars,
    Spearman = NA,
    p_value  = NA
  )
  
  for(i in seq_along(vars)){
    v <- vars[i]
    test <- cor.test(df1[[v]], df2[[v]], method = "spearman")
    out$Spearman[i] <- test$estimate
    out$p_value[i]  <- test$p.value
  }
  return(out)
}

###############################################
# TABLEAU 1 : ARCHIVE vs AREA
###############################################

corr_archive_area <- compute_corr(
  archive[, vars],
  area[1:14, vars],
  vars
)

print(corr_archive_area)

#SIGNIFICATIVE CORRELATION FOR : Garot 0.76; Nano 0.74 ; Schon 0.56

###############################################
# TABLEAU 2 : ARCHIVE vs NBR
###############################################

corr_archive_nbr <- compute_corr(
  archive[, vars],
  nbr[1:14, vars],
  vars
)

print(corr_archive_nbr)

#SIGNIFICATIVE CORRELATION FOR : Garot 0.60; Nano 0.73 ; Composite 0.54

#Pour la suite des analyses on garde le COUNT (Nbr of charcoals) car plus sensible aux pulse extremes et significatif pour le composite régional. 

#Au vue des résultats on choisira donc de travailler plutot avec le nombre plutot qu'avec les aires de charbon à présent


#######################################################################
#A toi de jour pour la suite. Je stoppe pour le week end. 
#Aerosols
# Tu peux appliquer le meme pricnipe avec les corrélations 
#et aussi rajouter l'analyse ou tu fais des wilcoxon tests pour comparer valeurs de CHAR Nbr, Area Burnd annuel et Aerosols sous forme de boxplot. Tu compares les medianes annuelles pour voir si c'est significatif ou non.
#######################################################################


aerosol<-read.csv("D:/Projects/Projet_trappes_pollen/Aerosols/Aerosols.csv",h=T,sep=";", dec = '.')
aerosol$Mean_aerosol <- rowMeans(aerosol[, 2:6], na.rm = TRUE)

aerosol = aerosol[-7,]
aerosol = aerosol[-7,]
aerosol = aerosol[-12,]

area = area[-7,]
area = area[-7,]
area = area[-12,]

#spearman correlation Area
corr_spearman = cor(aerosol[,7], area[,2:8], method = "spearman")
corr_spearman 
corrplot(corr_spearman, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))
cor.test(aerosol$Mean_aerosol, area$DAVE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$GAROT, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$LOUP, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$NANO, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$PESSIERE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$SCHON, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, area$WALT, method=c("spearman"))

cor.test(aerosol$Mean_aerosol, apply(area[,2:8],1,mean), method=c("spearman"))


nbr = nbr[-7,]
nbr = nbr[-7,]
nbr = nbr[-12,]

#spearman correlation Nbr
corr_spearman = cor(aerosol$Mean_aerosol, nbr[2:8], method = "spearman")
corr_spearman 
corrplot(corr_spearman, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))
cor.test(aerosol$Mean_aerosol, nbr$DAVE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$GAROT, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$LOUP, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$NANO, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$PESSIERE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$SCHON, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, nbr$WALT, method=c("spearman"))

cor.test(aerosol$Mean_aerosol, apply(nbr[,2:8],1,mean), method=c("spearman"))

#spearman correlation Median size and aerosols

median = median[-7,]
median = median[-7,]
median = median[-12,]

corr_spearman = cor(aerosol$Mean_aerosol, median[2:8], method = "spearman")
corr_spearman 
corrplot(corr_spearman, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))
cor.test(aerosol$Mean_aerosol, median$DAVE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$GAROT, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$LOUP, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$NANO, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$PESSIERE, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$SCHON, method=c("spearman"))
cor.test(aerosol$Mean_aerosol, median$WALT, method=c("spearman"))

cor.test(aerosol$Mean_aerosol, apply(median[,2:8],1,mean), method=c("spearman"))

#######################################################################
###############Median charcoal area versus buffer 30 km##################
#######################################################################

median<-read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Median_CHAR_area_2011_2025.csv",h=T,sep=";", dec = ',')
#Area_3 = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Area_record_3km.csv",h=T,sep=";", dec = ',')
Area_30 = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Area_record_30km.csv",h=T,sep=";", dec = ',')
Area_100 = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Area_record_100km.csv",h=T,sep=";", dec = ',')

library(ggplot2)

ylim.prim <- c(0,2.2)
dave = ggplot() +
  geom_bar(data=median [1:15,], aes(x=YEAR, y = DAVE), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = DAVE/30000), color = 'black', size = 1.4, lty = 1)+
  #geom_line(data= Area_30, aes(x = YEAR, y = DAVE/30000), color = 'red', size = 1.4, lty = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2017, 2024), labels = c("*", "*", "*")))+
  # scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 30000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
  
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Dave 52.1°N')
dave

cor.test(median$DAVE, Area_100$DAVE, method=c("spearman"))

garot = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = GAROT), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = GAROT/30000), color = 'black', size = 1.4, lty = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2018, 2023), labels = c("*", "*", "*")))+
  #scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 30000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')

garot

cor.test(median$GAROT, Area_100$GAROT, method=c("spearman"))

loup = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = LOUP), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = LOUP/50000), color = 'black', size = 1.4, lty = 1)+
    scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1))+
  # scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . *50000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Loup 53.1°N')

loup
cor.test(median$LOUP, Area_30$LOUP, method=c("spearman"))

nano = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = NANO), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = NANO/50000), color = 'black', size = 1.4, lty = 1)+
    scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    #scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 50000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Nano 53.0°N')

nano
cor.test(median$NANO, Area_100$NANO, method=c("spearman"))

pessiere = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = PESSIERE), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = PESSIERE/3000), color = 'black', size = 1.4, lty = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1))+
  #scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Pessière 49.5°N')
pessiere

cor.test(median$PESSIERE, Area_100$PESSIERE, method=c("spearman"))

schon = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = SCHON), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  # geom_line(data= Area_100, aes(x = YEAR, y = SCHON/30000), color = 'black', size = 1.4, lty = 1)+
    scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    # scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 30000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Schön 50.6°N')

schon

cor.test(median$SCHON, Area_100$SCHON, method=c("spearman"))

walt = ggplot() +
  geom_bar(data=median[1:15,], aes(x=YEAR, y = WALT), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  #geom_line(data= Area_100, aes(x = YEAR, y = WALT/30000), color = 'black', size = 1.4, lty = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2024), labels = c("*", "*")))+
  # scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim, sec.axis = sec_axis(~ . * 30000, name = expression(paste("Area burned 100 km buffer  ",  km^{2}))))+
  scale_y_continuous(expression(paste('Median CHAR particle size  ', mm^{2})), limits=ylim.prim)+
    theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')

walt

cor.test(median$WALT, Area_100$WALT, method=c("spearman"))

library(ggpubr)

ggarrange(loup, nano, dave, walt, garot, schon, pessiere, ncol = 3, nrow = 3)

median<-read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Median_CHAR_area_2011_2025.csv",h=T,sep=";", dec = ',')

##median[,2:8] = log(median[,2:8]+1)

corr_spearman = cor(Area_100[,2:8], median[,2:8], method = "spearman")
corr_spearman 
corrplot(corr_spearman, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))
cor.test(Area_30$DAVE, median$DAVE, method=c("spearman"))
cor.test(Area_30$GAROT, median$GAROT, method=c("spearman"))
cor.test(Area_30$LOUP, median$LOUP, method=c("spearman"))
cor.test(Area_30$NANO, median$NANO, method=c("spearman"))
cor.test(Area_30$PESSIERE, median$PESSIERE, method=c("spearman"))
cor.test(Area_30$SCHON, median$SCHON, method=c("spearman"))
cor.test(Area_30$WALT, median$WALT, method=c("spearman"))

cor.test(apply(Area_30[,2:8],1,median), apply(median[,2:8],1,median), method=c("spearman"))

corr_spearman = cor(Area_100[,2:8], median[,2:8], method = "spearman")
corr_spearman 
corrplot(corr_spearman, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))
cor.test(Area_100$DAVE, median$DAVE, method=c("spearman"))
cor.test(Area_100$GAROT, median$GAROT, method=c("spearman"))
cor.test(Area_100$LOUP, median$LOUP, method=c("spearman"))
cor.test(Area_100$NANO, median$NANO, method=c("spearman"))
cor.test(Area_100$PESSIERE, median$PESSIERE, method=c("spearman"))
cor.test(Area_100$SCHON, median$SCHON, method=c("spearman"))
cor.test(Area_100$WALT, median$WALT, method=c("spearman"))

cor.test(apply(Area_100[,2:8],1,median), apply(median[,2:8],1,median), method=c("spearman"))
################################################################################################################
################################################################################################################
################################################################################################################
#PCA

data_archives = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Archives_100km_15km_Traps.csv",h=T,sep=";", dec = ',')
#pessiere<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Pessiere.csv",h=T,sep=";", dec = ',')

library("FactoMineR")
library("factoextra")

# In principal component analysis, variables are often scaled (i.e. standardized)
# This is particularly recommended when variables are measured in different scale
# The goal is to make the variables comparable

acp_data = as.data.frame(scale(data_archives[,2:20], center = TRUE, scale = TRUE))
res.pca = PCA(acp_data[,1:19], graph = T)
res.pca
res.pca$var

#####
schon<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Schon.csv",h=T,sep=";", dec = ',')

library("FactoMineR")
library("factoextra")

# In principal component analysis, variables are often scaled (i.e. standardized)
# This is particularly recommended when variables are measured in different scale
# The goal is to make the variables comparable

acp_data = as.data.frame(scale(schon[,2:11], center = TRUE, scale = TRUE))
res.pca = PCA(acp_data[,1:10], graph = T)
res.pca

#####
loup<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Loup.csv",h=T,sep=";", dec = ',')

library("FactoMineR")
library("factoextra")

# In principal component analysis, variables are often scaled (i.e. standardized)
# This is particularly recommended when variables are measured in different scale
# The goal is to make the variables comparable

acp_data = as.data.frame(scale(loup[,2:13], center = TRUE, scale = TRUE))
res.pca = PCA(acp_data[,1:12], graph = T)
res.pca

#####
nano<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Nano.csv",h=T,sep=";", dec = ',')

library("FactoMineR")
library("factoextra")

# In principal component analysis, variables are often scaled (i.e. standardized)
# This is particularly recommended when variables are measured in different scale
# The goal is to make the variables comparable

acp_data = as.data.frame(scale(nano[,2:13], center = TRUE, scale = TRUE))
res.pca = PCA(acp_data[,1:12], graph = T)
res.pca

#####
garot<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Garot.csv",h=T,sep=";", dec = ',')

library("FactoMineR")
library("factoextra")

# In principal component analysis, variables are often scaled (i.e. standardized)
# This is particularly recommended when variables are measured in different scale
# The goal is to make the variables comparable

acp_data = as.data.frame(scale(garot[,2:13], center = TRUE, scale = TRUE))
res.pca = PCA(acp_data[,1:12], graph = T)
res.pca

#####################################################################################
#Heatmaps historical fire records
# Library
library(ggplot2)

#fire_archives = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Lakes_area_number_2011_2023.csv",h=T,sep=";", dec = ',')
#schon2<-read.csv("D:/POSTDOC_22_23_24/Projets_en_cours/Trappes/Schon2.csv",h=T,sep=";", dec = ',')

loupnbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Loup_Archives.csv",h=T,sep=";", dec = ',')
louparea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Loup_Archives_Area.csv",h=T,sep=";", dec = ',')

nanonbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Nano_Archives.csv",h=T,sep=";", dec = ',')
nanoarea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Nano_Archives_Area.csv",h=T,sep=";", dec = ',')

davenbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Dave_Archives.csv",h=T,sep=";", dec = ',')
davearea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Dave_Archives_Area.csv",h=T,sep=";", dec = ',')

waltnbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Walt_Archives.csv",h=T,sep=";", dec = ',')
waltarea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Walt_Archives_Area.csv",h=T,sep=";", dec = ',')

garotnbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Garot_Archives.csv",h=T,sep=";", dec = ',')
garotarea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Garot_Archives_Area.csv",h=T,sep=";", dec = ',')

schonnbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Schon_Archives.csv",h=T,sep=";", dec = ',')
schonarea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Schon_Archives_Area.csv",h=T,sep=";", dec = ',')

pessierenbr =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Pessiere_Archives.csv",h=T,sep=";", dec = ',')
pessierearea =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Pessiere_Archives_Area.csv",h=T,sep=";", dec = ',')


schonplot = ggplot(schonarea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000,7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Schön") 
schonplot


waltplot = ggplot(waltarea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000, 7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Walt") 
waltplot

daveplot = ggplot(davearea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000,7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Dave") 
daveplot

nanoplot = ggplot(nanoarea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000, 7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Nano") 
nanoplot

loupplot = ggplot(louparea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000, 7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Loup") 
loupplot

pessiereplot = ggplot(pessierearea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000, 7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Pessière") 
pessiereplot

garotplot = ggplot(garotarea, aes(YEAR, Var, fill= Area)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="red", limits = c(0,7000), breaks = c(0, 1000,2000,3000,4000,5000,6000, 7000))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 10))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Garot") 
garotplot

###########################################################################
###########################################################################
###########################################################################

library(cowplot)
library(ggpubr)
ggarrange(loupplot, nanoplot, daveplot, waltplot, garotplot, schonplot, pessiereplot, ncol = 1, nrow = 7)


#For number

schonplot = ggplot(schonnbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Schön") 
schonplot

waltplot = ggplot(waltnbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Walt") 
waltplot

daveplot = ggplot(davenbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Dave") 
daveplot

nanoplot = ggplot(nanonbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Nano") 
nanoplot

loupplot = ggplot(loupnbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Loup") 
loupplot

pessiereplot = ggplot(pessierenbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Pessiere") 
pessiereplot

garotplot = ggplot(garotnbr, aes(YEAR, Var, fill= Nbr)) + 
  geom_tile(color = "grey",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,10,20), limits = c(0,20))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Garot") 
garotplot

library(cowplot)
library(ggpubr)
ggarrange(loupplot, nanoplot, daveplot, waltplot, garotplot, schonplot, pessiereplot, ncol = 1, nrow = 7)

###########################################################################
###########################################################################
###########################################################################

#Presence / Absence

library(ggplot2)
library(cowplot)
library(ggpubr)

louppres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Loup_Archives_presence.csv",h=T,sep=";", dec = ',')
nanopres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Nano_Archives_presence.csv",h=T,sep=";", dec = ',')
davepres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Dave_Archives_presence.csv",h=T,sep=";", dec = ',')
waltpres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Walt_Archives_presence.csv",h=T,sep=";", dec = ',')
garotpres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Garot_Archives_presence.csv",h=T,sep=";", dec = ',')
schonpres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Schon_Archives_presence.csv",h=T,sep=";", dec = ',')
pessierepres =  read.csv("D:/Projects/Projet_trappes_pollen/Trappes_analyses_charbon_Campagne_2024/Archives_feux_nbac/Pessiere_Archives_presence.csv",h=T,sep=";", dec = ',')

schonplot = ggplot(schonpres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.text.y = element_text(face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Schön 1000-10000 ha") 
schonplot

schonplot2 = ggplot(schonpres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.text.y = element_text(face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Schön +10000 ha") 
schonplot2

waltplot = ggplot() + 
  geom_tile(data = waltpres, aes(YEAR, Var, fill= Nbr_1000),
            color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Walt 1000-10000 ha") 
waltplot

waltplot2 = ggplot() + 
  geom_tile(data = waltpres, aes(YEAR, Var, fill= Nbr_10000),
            color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Walt +10000 ha") 
waltplot2

daveplot = ggplot(davepres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Dave 1000-10000 ha") 
daveplot

daveplot2 = ggplot(davepres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Dave +10000 ha") 
daveplot2

nanoplot = ggplot(nanopres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km','30km'))+
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Nano 1000-10000 ha") 
nanoplot

nanoplot2 = ggplot(nanopres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km','30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Nano +10000 ha") 
nanoplot2

loupplot = ggplot(louppres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Loup 1000-10000 ha") 
loupplot

loupplot2 = ggplot(louppres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Loup +10000 ha") 
loupplot2

pessiereplot = ggplot(pessierepres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Pessière 1000-10000 ha") 
pessiereplot

pessiereplot2 = ggplot(pessierepres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Pessière +10000 ha") 
pessiereplot2

garotplot = ggplot(garotpres, aes(YEAR, Var, fill= Nbr_1000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Garot 1000-10000 ha") 
garotplot


garotplot2 = ggplot(garotpres, aes(YEAR, Var, fill= Nbr_10000)) + 
  geom_tile(color = "black",
            lwd = 0.1,
            linetype = 1)+
  scale_fill_gradient(low="white", high="black", breaks=c(0,1), limits = c(0,1))+
  coord_fixed()+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_discrete(labels = c('100km', '30km'))+
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 2))+
  theme_classic()+
  theme(legend.position='none', axis.text.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "bold"), legend.title = element_blank(), axis.line.y.left = element_line(color = "black"), axis.title.y = element_blank(), axis.title.x = element_blank())+
  ggtitle("Garot +10000 ha") 
garotplot2

library(cowplot)
library(ggpubr)
ggarrange(loupplot, loupplot2, nanoplot, nanoplot2, daveplot, daveplot2, waltplot, waltplot2, garotplot, garotplot2, schonplot, schonplot2, pessiereplot, pessiereplot2, ncol = 2, nrow = 7)


########################################################
########################################################
# AUTRES
########################################################
########################################################
########################################################
#Median charcoal particle size
########################################################
data <-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Mean_Median_CHAR_size_Traps.csv',h=T,sep=";", dec = ',')

library(ggplot2)
ylim.prim <- c(0,3)

dave =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=DAVE, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = DAVE/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Charcoal size", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Dave 52.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
dave

garot =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=GAROT, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = GAROT/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Charcoal size", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Garot 51.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
garot

loup =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=LOUP, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = LOUP/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Charcoal size", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Loup 53.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
loup

nano =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=NANO, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = NANO/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Nano 53.0°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
nano

pessiere =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=PESSIERE, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = PESSIERE/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Pessiere 49.5°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank())
pessiere

schon =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=SCHON, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = SCHON/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Schon 50.6°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
schon


walt =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=WALT, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = WALT/3000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 3000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Walt 51.9°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
walt

library(ggpubr)

ggarrange(loup, nano, dave, walt, garot, schon, pessiere, ncol = 3, nrow = 3)

################################################################################################################
library(ggplot2)
dave = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = DAVE), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = DAVE/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Dave 52.1°N')
dave

garot = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = GAROT), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = GAROT/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')
garot

loup = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = LOUP), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = LOUP/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Loup 53.1°N')
loup

nano = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = NANO), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = NANO/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Nano 53.0°N')
nano

pessiere = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = PESSIERE), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = PESSIERE/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Pessiere 49.5°N')
pessiere

schon = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = SCHON), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = SCHON/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Schon 50.6°N')
schon

walt = ggplot() +
  geom_bar(data= multiple[1:13,], aes(x = YEAR, y = WALT), stat = 'identity', position="dodge", width = 0.5, fill = 'green')+
  geom_line(data= Area_100, aes(x = YEAR, y = WALT/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 1))+
  scale_y_continuous("Area*number traps", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "green"), 
        axis.title.y.left = element_text(color = "green"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')
walt

library(ggpubr)

ggarrange(loup, nano, dave, walt, garot, schon, pessiere, ncol = 3, nrow = 3)

################################################################################################################
################################################################################################################

ratio<-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Ratio_Area_Nbr_Trappes_2011_2024.csv',h=T,sep=";", dec = ',')
Area_100 = read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Area_record_100km.csv",h=T,sep=";", dec = ',')
multiple<-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Multiple_Area_Nbr_Trappes_2011_2024.csv',h=T,sep=";", dec = ',')

data <-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Ratio_and_Multiple_Traps.csv',h=T,sep=";", dec = ',')

data[1:14,3:9] = scale(data[1:14,3:9])
data[15:28,3:9] = scale(data[15:28,3:9])

library(ggplot2)

ylim.prim <- c(-2,5)

dave =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=DAVE, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = DAVE/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Dave 52.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
dave

garot =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=GAROT, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = GAROT/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Garot 51.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
garot

loup =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=LOUP, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = LOUP/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Loup 53.1°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
loup

nano =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=NANO, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = NANO/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Nano 53.0°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
nano


pessiere =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=PESSIERE, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = PESSIERE/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Pessiere 49.5°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank())
pessiere

schon =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=SCHON, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = SCHON/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Schon 50.6°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
schon

walt =ggplot() +
  geom_bar(data=data, aes(fill=condition, y=WALT, x=specie), position="dodge", stat="identity")+
  geom_line(data= Area_100, aes(x = YEAR, y = WALT/2000), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1))+
  scale_y_continuous("Log'", limits=ylim.prim, sec.axis = sec_axis(~ . * 2000, name = "Char area archives 100km buffer (km2)"))+
  theme_classic()+ ggtitle('Walt 51.9°N')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x = element_blank(), legend.position="none")
walt

library(ggpubr)
ggarrange(loup, nano, dave, walt, garot, schon, pessiere, ncol = 3, nrow = 3)

########################################################

#FOR MEAN

# charcoal from traps 2011_2024
area<-read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Trappes/Area_mm2_Trappes_2011_2025_mean.csv",h=T,sep=";", dec = ',')
nbr<-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Trappes/Nbr_Trappes_2011_2025.csv',h=T,sep=";", dec = ',')

ylim.prim <- c(0,3)

dave1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = DAVE), stat = 'identity', width = 0.5, fill = 'blue')+
  #geom_bar(data= area100, aes(x = YEAR, y = DAVE), stat = 'identity', fill = 'orange')+
  geom_line(data= nbr, aes(x = YEAR, y = DAVE/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = DAVE/2), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  #  geom_line(data= archive_Dave, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Dave 52.1°N')
dave1

garot1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = GAROT), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = GAROT/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = GAROT/2), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  #  geom_line(data= archive_Garot, aes(x = YEAR, y = X50km_area/100), color = 'yellow3', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')
garot1

loup1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = LOUP), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = LOUP/45), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = LOUP/4), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  # geom_line(data= archive_Loup, aes(x = YEAR, y = X50km_area/10000), color = 'yellow3', size = 1)+
  
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 45, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Loup 53.1°N')
loup1

nano1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = NANO), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = NANO/100), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = NANO/7), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Nano, aes(x = YEAR, y = X50km_area/10000), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 100, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Nano 53.0°N')
nano1

pessiere1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = PESSIERE), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = PESSIERE/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = PESSIERE/2), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Pessiere, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})), limits=ylim.prim,sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Pessière 49.5°N')
pessiere1

schon1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = SCHON), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = SCHON/35), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = SCHON/2), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Schon, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023, 2024)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Schön 50.6°N')
schon1

walt1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = WALT), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = WALT/35), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = WALT/2), color = 'black', size = 1, lty = 4)+
  #geom_line(data= archive_Walt, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')
walt1

composite1 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = COMPOSITE), stat = 'identity', position="dodge", width = 0.5, fill = 'blue')+
  geom_line(data= nbr, aes(x = YEAR, y = COMPOSITE/300), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = WALT/2), color = 'black', size = 1, lty = 4)+
  #geom_line(data= archive_Walt, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023, 2024)))+
  scale_y_continuous(expression(paste('MEAN Char area ', mm^{2})),limits=c(0,2), sec.axis = sec_axis(~ . * 300, name = "Sum of Char counts"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "blue"), axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue"), axis.title.x = element_blank())+
  ggtitle('Composite record')
composite1

ggarrange(loup1, nano1, dave1, walt1, garot1, schon1, pessiere1,composite1, ncol = 3, nrow = 3)

###################################################################################################

#FOR MEDIAN

# charcoal from traps 2011_2024
area<-read.csv("D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Trappes/Area_mm2_Trappes_2011_2025_median.csv",h=T,sep=";", dec = ',')
nbr<-read.csv('D:/Projects/Projet_trappes_pollen/Fichiers_scripts/Trappes/Nbr_Trappes_2011_2025.csv',h=T,sep=";", dec = ',')

ylim.prim <- c(0,3)

dave2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = DAVE), stat = 'identity', width = 0.5, fill = 'darkgreen')+
  #geom_bar(data= area100, aes(x = YEAR, y = DAVE), stat = 'identity', fill = 'orange')+
  geom_line(data= nbr, aes(x = YEAR, y = DAVE/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = DAVE/2), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  #  geom_line(data= archive_Dave, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Dave 52.1°N')
dave2

garot2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = GAROT), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = GAROT/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = GAROT/2), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  #  geom_line(data= archive_Garot, aes(x = YEAR, y = X50km_area/100), color = 'yellow3', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')
garot2

loup2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = LOUP), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = LOUP/45), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = LOUP/4), color = 'black', size = 1, lty = 4)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  # geom_line(data= archive_Loup, aes(x = YEAR, y = X50km_area/10000), color = 'yellow3', size = 1)+
  
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 45, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Loup 53.1°N')
loup2

nano2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = NANO), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = NANO/100), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = NANO/7), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Nano, aes(x = YEAR, y = X50km_area/10000), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 100, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Nano 53.0°N')
nano2

pessiere2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = PESSIERE), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = PESSIERE/35), color = 'black', size = 1)+
  #geom_line(data= nbr100, aes(x = YEAR, y = PESSIERE/2), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Pessiere, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})), limits=ylim.prim,sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Pessière 49.5°N')
pessiere2

schon2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = SCHON), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = SCHON/35), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = SCHON/2), color = 'black', size = 1, lty = 4)+
  # geom_line(data= archive_Schon, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2023, 2024)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Schön 50.6°N')
schon2

walt2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = WALT), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = WALT/35), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = WALT/2), color = 'black', size = 1, lty = 4)+
  #geom_line(data= archive_Walt, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . * 35, name = "Char count"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')
walt2

composite2 = ggplot() +
  geom_bar(data=area, aes(x=YEAR, y = COMPOSITE), stat = 'identity', position="dodge", width = 0.5, fill = 'darkgreen')+
  geom_line(data= nbr, aes(x = YEAR, y = COMPOSITE/300), color = 'black', size = 1)+
  # geom_line(data= nbr100, aes(x = YEAR, y = WALT/2), color = 'black', size = 1, lty = 4)+
  #geom_line(data= archive_Walt, aes(x = YEAR, y = X50km_area/10), color = 'yellow3', size = 1)+
  
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2025, by = 1), sec.axis = sec_axis(~ ., breaks = c(2013, 2014, 2023, 2024)))+
  scale_y_continuous(expression(paste('MEDIAN Char area ', mm^{2})),limits=c(0,2), sec.axis = sec_axis(~ . * 300, name = "Sum of Char counts"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "darkgreen"), axis.ticks.y.left = element_line(color = "darkgreen"),
        axis.text.y.left = element_text(color = "darkgreen"), 
        axis.title.y.left = element_text(color = "darkgreen"), axis.title.x = element_blank())+
  ggtitle('Composite record')
composite2

ggarrange(loup2, nano2, dave2, walt2, garot2, schon2, pessiere2,composite2, ncol = 3, nrow = 3)

ggarrange(loup, loup1,loup2,nano, nano1, nano2, dave, dave1, dave2, walt, walt1,walt2, garot, garot1,garot2, schon, schon1,schon2, pessiere, pessiere1,pessiere2,composite, composite1,composite2, ncol =3, nrow = 8)




##############################################
# Trend analysis with MANN KENDALL test
##############################################
library(ggplot2)
library(cowplot)
library(Kendall)
library(trend)

#Tau and p-values (for trends of each variable for the study period) are calculated after

graphareaDAVE <- ggplot(area, aes(YEAR, DAVE))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("DAVE CHAR area")+
  labs(title="MK-test : tau = 0.22, p = 0.28")+
  theme(plot.title = element_text(size=15))
graphareaDAVE 

res2 <-  MannKendall(area$DAVE)
summary(res2)
res2 = mk.test(area$DAVE, continuity = TRUE)
res2
sens.slope(area$DAVE, conf.level = 0.95)

graphnbrDAVE <- ggplot(nbr, aes(YEAR, DAVE))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("DAVE CHAR count")+
  labs(title="MK-test : tau = 0.30, p = 0.13")+
  theme(plot.title = element_text(size=15))
graphnbrDAVE

res2 <-  MannKendall(nbr$DAVE)
summary(res2)
res2 = mk.test(nbr$DAVE, continuity = TRUE)
res2
sens.slope(nbr$DAVE, conf.level = 0.95)

graphareaGAROT <- ggplot(area, aes(YEAR, GAROT))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("GAROT CHAR area")+
  labs(title="MK-test : tau = -0.27, p = 0.18")+
  theme(plot.title = element_text(size=15))
graphareaGAROT 

res2 <-  MannKendall(area$GAROT)
summary(res2)
res2 = mk.test(area$GAROT, continuity = TRUE)
res2
sens.slope(area$GAROT, conf.level = 0.95)

graphnbrGAROT <- ggplot(nbr, aes(YEAR, GAROT))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("GAROT CHAR count")+
  labs(title="MK-test : tau = -0.24, p = 0.24")+
  theme(plot.title = element_text(size=15))
graphnbrGAROT

res2 <-  MannKendall(nbr$GAROT)
summary(res2)
res2 = mk.test(nbr$GAROT, continuity = TRUE)
res2
sens.slope(nbr$GAROT, conf.level = 0.95)

graphareaLOUP <- ggplot(area, aes(YEAR, LOUP))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("LOUP CHAR area")+
  labs(title="MK-test : tau = 0.45, p = 0.02")+
  theme(plot.title = element_text(size=15))
graphareaLOUP 

res2 <-  MannKendall(area$LOUP)
summary(res2)
res2 = mk.test(area$LOUP, continuity = TRUE)
res2
sens.slope(area$LOUP, conf.level = 0.95)

graphnbrLOUP <- ggplot(nbr, aes(YEAR, LOUP))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("LOUP CHAR count")+
  labs(title="MK-test : tau = 0.57, p = 0.004")+
  theme(plot.title = element_text(size=15))
graphnbrLOUP

res2 <-  MannKendall(nbr$LOUP)
summary(res2)
res2 = mk.test(nbr$LOUP, continuity = TRUE)
res2
sens.slope(nbr$LOUP, conf.level = 0.95)

graphareaNANO <- ggplot(area, aes(YEAR, NANO))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("NANO CHAR area")+
  labs(title="MK-test : tau = 0.18, p = 0.37")+
  theme(plot.title = element_text(size=15))
graphareaNANO 

res2 <-  MannKendall(area$NANO)
summary(res2)
res2 = mk.test(area$NANO, continuity = TRUE)
res2
sens.slope(area$NANO, conf.level = 0.95)

graphnbrNANO <- ggplot(nbr, aes(YEAR, NANO))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("NANO CHAR count")+
  labs(title="MK-test : tau = 0.33, p = 0.10")+
  theme(plot.title = element_text(size=15))
graphnbrNANO

res2 <-  MannKendall(nbr$NANO)
summary(res2)
res2 = mk.test(nbr$NANO, continuity = TRUE)
res2
sens.slope(nbr$NANO, conf.level = 0.95)

graphareaPESSIERE <- ggplot(area, aes(YEAR, PESSIERE))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("PESSIÈRE CHAR area")+
  labs(title="MK-test : tau = 0.19, p = 0.34")+
  theme(plot.title = element_text(size=15))
graphareaPESSIERE 

res2 <-  MannKendall(area$PESSIERE)
summary(res2)
res2 = mk.test(area$PESSIERE, continuity = TRUE)
res2
sens.slope(area$PESSIERE, conf.level = 0.95)

graphnbrPESSIERE <- ggplot(nbr, aes(YEAR, PESSIERE))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("PESSIÈRE CHAR count")+
  labs(title="MK-test : tau = 0.26, p = 0.21")+
  theme(plot.title = element_text(size=15))
graphnbrPESSIERE

res2 <-  MannKendall(nbr$PESSIERE)
summary(res2)
res2 = mk.test(nbr$PESSIERE, continuity = TRUE)
res2
sens.slope(nbr$PESSIERE, conf.level = 0.95)

graphareaSCHON <- ggplot(area, aes(YEAR, SCHON))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("SCHÖN CHAR area")+
  labs(title="MK-test : tau = -0.09, p = 0.71")+
  theme(plot.title = element_text(size=15))
graphareaSCHON 

res2 <-  MannKendall(area$SCHON)
summary(res2)
res2 = mk.test(area$SCHON, continuity = TRUE)
res2
sens.slope(area$SCHON, conf.level = 0.95)

graphnbrSCHON <- ggplot(nbr, aes(YEAR, SCHON))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("SCHÖN CHAR count")+
  labs(title="MK-test : tau = -0.03, p = 0.92")+
  theme(plot.title = element_text(size=15))
graphnbrSCHON

res2 <-  MannKendall(nbr$SCHON)
summary(res2)
res2 = mk.test(nbr$SCHON, continuity = TRUE)
res2
sens.slope(nbr$SCHON, conf.level = 0.95)

graphareaWALT <- ggplot(area, aes(YEAR, WALT))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("WALT CHAR area")+
  labs(title="MK-test : tau = -0.25, p = 0.21")+
  theme(plot.title = element_text(size=15))
graphareaWALT 

res2 <-  MannKendall(area$WALT)
summary(res2)
res2 = mk.test(area$WALT, continuity = TRUE)
res2
sens.slope(area$WALT, conf.level = 0.95)

graphnbrWALT <- ggplot(nbr, aes(YEAR, WALT))+ 
  geom_line()+
  geom_smooth(aes(group = 1), method = 'lm', position= "identity")+
  theme_classic()+
  theme(axis.title.y = element_text(color="black", size=12, face="bold"), axis.text.y = element_text(face="bold", color="black", size=15, angle=20))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(face="bold", color="black", size=8.5))+
  ylab("WALT CHAR count")+
  labs(title="MK-test : tau = -0.06, p = 0.80")+
  theme(plot.title = element_text(size=15))
graphnbrWALT

res2 <-  MannKendall(nbr$WALT)
summary(res2)
res2 = mk.test(nbr$WALT, continuity = TRUE)
res2
sens.slope(nbr$WALT, conf.level = 0.95)

##############################################
#FIGURE 3
##############################################

plot_grid(graphareaLOUP, graphnbrLOUP, graphareaNANO, graphnbrNANO, graphareaDAVE, graphnbrDAVE, graphareaWALT, graphnbrWALT, graphareaGAROT, graphnbrGAROT, graphareaSCHON, graphnbrSCHON, graphareaPESSIERE, graphnbrPESSIERE, ncol = 2, nrow = 7)

