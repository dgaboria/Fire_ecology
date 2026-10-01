dev.off()
rm(list = ls())  # Deleting variables from the environment R

##########################################################################################
#FIGURE - Sediments from upper 10 cm cores collected in 2024 in each lake
##########################################################################################

# Importation of libraries
library(devtools)
library(ggplot2)
library(ggpubr)

area<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Sediments/Area_mm2_10cm.csv",h=T,sep=";", dec = ',')
nbr<-read.csv('D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Sediments/Nbr_10cm.csv',h=T,sep=";", dec = ',')

ylim.prim <- c(0,6)   # in this example, area
ylim.sec <- c(0,65)    # in this example, nbr

pessiere = ggplot() +
  geom_bar(data = area,
           aes(x = rev(YEAR), y = PESSIERE),
           stat = "identity", position = "dodge",
           width = 0.5, fill = "red") +
  
  geom_line(data = nbr,
            aes(x = rev(YEAR), y = PESSIERE / 12),
            color = "black", size = 1) +
  
  scale_x_continuous(
    breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5),
    labels = c("9.5","9","8.5","8","7.5","7","6.5","6","5.5","5","4.5","4",
               "3.5","3","2.5","2","1.5","1","0.5","0")
  ) +
  
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line.y.left = element_line(color = "red"),
    axis.ticks.y.left = element_line(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    axis.title.y.left = element_text(color = "red"),
    axis.title.x = element_blank()
  ) +
  
  ggtitle("Pessière 49.5°N")

pessiere


schon = ggplot() +
  geom_bar(data = area,
           aes(x = rev(YEAR), y = SCHON),
           stat = "identity", position = "dodge",
           width = 0.5, fill = "red") +
  
  geom_line(data = nbr,
            aes(x = rev(YEAR), y = SCHON / 12),
            color = "black", size = 1) +
  
  scale_x_continuous(
    breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5),
    labels = c("9.5","9","8.5","8","7.5","7","6.5","6","5.5","5","4.5","4",
               "3.5","3","2.5","2","1.5","1","0.5","0")
  ) +
  
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line.y.left = element_line(color = "red"),
    axis.ticks.y.left = element_line(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    axis.title.y.left = element_text(color = "red"),
    axis.title.x = element_blank()
  ) +
  
  ggtitle("Schön 50.6°N")

schon


garot = ggplot() +
  geom_bar(data=area, aes(x=rev(YEAR), y = GAROT), stat = 'identity', position="dodge", width = 0.5, fill = 'red')+
  geom_line(data= nbr, aes(x = rev(YEAR), y = GAROT/12), color = 'black', size = 1)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5), labels = c("9.5", "9", "8.5", "8", "7.5", "7", "6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0'))+
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')
garot

walt = ggplot() +
  geom_bar(data=area, aes(x=rev(YEAR), y = WALT), stat = 'identity', position="dodge", width = 0.5, fill = 'red')+
  geom_line(data= nbr, aes(x = rev(YEAR), y = WALT/12), color = 'black', size = 1)+
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5), labels = c("9.5", "9", "8.5", "8", "7.5", "7", "6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0'))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')
walt

dave = ggplot() +
  geom_bar(data=area, aes(x=rev(YEAR), y = DAVE), stat = 'identity', position="dodge", width = 0.5, fill = 'red')+
  geom_line(data= nbr, aes(x = rev(YEAR), y = DAVE/12), color = 'black', size = 1)+
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5), labels = c("9.5", "9", "8.5", "8", "7.5", "7", "6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0'), name = c('Depth in cm'))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"))+
  ggtitle('Dave 52.1°N')
dave

nano = ggplot() +
  geom_bar(data=area, aes(x=rev(YEAR), y = NANO), stat = 'identity', position="dodge", width = 0.5, fill = 'red')+
  geom_line(data= nbr, aes(x = rev(YEAR), y = NANO/12), color = 'black', size = 1)+
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5), labels = c("9.5", "9", "8.5", "8", "7.5", "7", "6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0'), name = c('Depth in cm'))+
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"))+
  ggtitle('Nano 53.0°N')
nano

loup = ggplot() +
  geom_bar(data=area, aes(x=rev(YEAR), y = LOUP), stat = 'identity', position="dodge", width = 0.5, fill = 'red')+
  geom_line(data= nbr, aes(x = rev(YEAR), y = LOUP/12), color = 'black', size = 1)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5), labels = c("9.5", "9", "8.5", "8", "7.5", "7", "6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0'), name = c('Depth in cm'))+
  scale_y_continuous(
    name = expression("Char area (" * mm^2 * ")"),
    limits = ylim.prim,
    sec.axis = sec_axis(~ . * 12, name = "Char number")
  ) +  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"))+
  ggtitle('Loup 53.1°N')
loup

##########################################################################################

ggarrange(loup, nano, dave, walt, garot, schon, pessiere,  ncol = 3, nrow = 3)

##########################################################################################
#FIGURE - Sediments 6.5 cm and traps (TEST 1)
##########################################################################################

area_6_5 = read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Sediments/Area_mm2_6_5_cm_2011_2024.csv",h=T,sep=";", dec = ',')
area_traps<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/Area_mm2_Trappes_2011_2025.csv",h=T,sep=";", dec = ',')

ylim.prim <- c(0,6)   # in this example, area
ylim.sec <- c(0,0.8)    # in this example, nbr

area_traps = area_traps[-15,]
loup = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = LOUP), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = LOUP*0.15), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.15, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Loup 53.1°N')
loup

nano = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = NANO), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = NANO*0.15), color = 'black', size = 1)+
  #geom_smooth(method = "loess", level = .90,colour="black")+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.15, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Nano 53.0°N')

nano

dave = ggplot() +
  geom_line(data = area_6_5, aes(x = YEAR, y = DAVE), color = 'red', size = 1) +
  geom_line(data = area_traps, aes(x = YEAR, y = DAVE * 0.15), color = 'black', size = 1) +
  
  scale_x_continuous(
    breaks = 2011:2024,
    sec.axis = sec_axis(
      ~ .,
      breaks = 2011:2024,
      labels = c("6.5","6","5.5","5","4.5","4","3.5","3","2.5","2","1.5","1","0.5","0")
    )
  ) +
  
  scale_y_continuous(
    expression(paste('Char area sediments ', mm^{2})),
    limits = ylim.prim,
    sec.axis = sec_axis(
      ~ . / 0.15,
      name = "Charcoal surface area (mm²)"   # ← label noir ajouté
    )
  ) +
  
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    
    # Axe Y gauche (rouge)
    axis.line.y.left = element_line(color = "red"),
    axis.ticks.y.left = element_line(color = "red"),
    axis.text.y.left = element_text(color = "red"),
    axis.title.y.left = element_text(color = "red"),
    
    # Axe X supérieur (label cm)
    axis.title.x.top = element_text(size = 10),  # ← petit label "(cm)"
    
    axis.title.x = element_blank()
  ) +
  ggtitle("Dave 52.1°N")

dave

walt = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = WALT), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = WALT*0.75), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.75, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Walt 51.9°N')
walt

garot = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = GAROT), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = GAROT*0.25), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.25, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Garot 51.1°N')
garot

schon = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = SCHON), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = SCHON*0.25), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.25, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Schön 50.6°N')
schon

pessiere = ggplot() +
  geom_line(data=area_6_5, aes(x=YEAR, y = PESSIERE), color = 'red', size =1)+
  geom_line(data= area_traps, aes(x = YEAR, y = PESSIERE*0.1), color = 'black', size = 1)+
  scale_x_continuous(breaks = seq(from = 2011, to = 2024, by = 1), sec.axis = sec_axis(~ ., breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021, 2022, 2023, 2024), labels = c("6.5", "6", "5.5", "5", "4.5", '4', '3.5', '3', '2.5', '2', '1.5', '1', '0.5', '0')))+
  scale_y_continuous(expression(paste('Char area sediments ', mm^{2})),limits=ylim.prim, sec.axis = sec_axis(~ . /0.1, name = "Char area from traps"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.line.y.left = element_line(color = "red"), axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"), axis.title.x = element_blank())+
  ggtitle('Pessière 49.5°N')
pessiere

ggarrange(loup, nano, dave, walt, garot, schon, pessiere,  ncol = 3, nrow = 3)

##############################################################
# Spearman correlation
##############################################################
library(corrplot)

#Corr. Pearson
corr_pearson = cor(area_traps[,2:8], area_6_5[,2:8], method = "pearson")
corr_pearson
corrplot(corr_pearson, type = "lower", method = "number", order = "hclust", addrect = 2, col = c("black", "red"))

cor.test(area_traps$DAVE, area_6_5$DAVE, method=c("pearson"))
cor.test(area_traps$GAROT, area_6_5$GAROT, method=c("pearson"))
cor.test(area_traps$LOUP, area_6_5$LOUP, method=c("pearson"))
cor.test(area_traps$NANO, area_6_5$NANO, method=c("pearson"))
cor.test(area_traps$PESSIERE, area_6_5$PESSIERE, method=c("pearson"))
cor.test(area_traps$SCHON, area_6_5$SCHON, method=c("pearson"))
cor.test(area_traps$WALT, area_6_5$WALT, method=c("pearson"))

cor.test(apply(area_traps[,2:8],1,mean), apply(area_6_5[,2:8],1,mean), method=c("pearson"))


##############################################################

# ============================================================
# TEST 2 - Trap/core correlation by lake — age–depth model
# ============================================================

area_traps<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Trappes/Area_mm2_Trappes_2011_2025.csv",h=T,sep=";", dec = ',')
area<-read.csv("D:/PROJETS_POSTDOC/Trappes_charbon/Fichiers_scripts/Sediments/Area_mm2_10cm.csv",h=T,sep=";", dec = ',')

# ---- 1. Chargement des données ----
traps <- area_traps[-15,]
core  <- area

colnames(traps) = c('Year','Dave', 'Garot', 'Loup', 'Nano', 'Pessière', 'Schön', 'Walt', 'Composite')
colnames(core) = c('Depth','Pessière', 'Schön', 'Garot', 'Walt', 'Dave', 'Nano', 'Loup')

names(core)[names(core) == "Year"] <- "Depth"

trap_years  <- traps$Year
depths      <- core$Depth
ANCHOR_YEAR <- 2024

if (!is.numeric(trap_years)) stop("trap_years doit être numérique")
if (!is.numeric(depths))     stop("depths doit être numérique")

# ---- 2. Fonction objectif ----
#But général : la fonction reçoit des valeurs candidates pour d0 (profondeur d'ancrage) 
#et s (années par cm), et retourne une valeur à minimiser. Comme on veut maximiser la corrélation 
#de Pearson, la fonction retourne -r (l'opposé de r) — c'est une astuce classique pour transformer 
#un problème de maximisation en problème de minimisation, compatible avec des fonctions d'optimisation 
#comme optim() (L-BFGS-B, mentionné dans votre méthode).

neg_corr <- function(par, trap_val, core_val, trap_years, depths, anchor_year) {
  d0 <- par[1]; s <- par[2]
  if (!is.finite(d0) || !is.finite(s)) return(1)
  if (s <= 0.05 || d0 < 0 || d0 > 1) return(1)
  
  depth_pred <- d0 - (trap_years - anchor_year) / s
  valid <- depth_pred >= min(depths) & depth_pred <= max(depths)
  if (sum(valid) < 6) return(1)
  
  core_at_year <- approx(depths, core_val, xout = depth_pred[valid])$y
  if (any(is.na(core_at_year))) return(1)
  
  r <- suppressWarnings(cor(trap_val[valid], core_at_year))
  if (is.na(r)) return(1)
  
  -r
}

# ---- 3. Ajustement par lac ----
#Cette fonction applique, lac par lac, la stratégie d'optimisation en deux temps 
#d'abord une exploration grossière par grille pour éviter les minima locaux, suivie d'un 
#raffinement précis par L-BFGS-B — 
#puis calcule les statistiques finales (r, p, n) à reporter dans le Tableau 2.

fit_lake <- function(lake) {
  
  trap_val <- as.numeric(traps[[lake]])
  core_val <- as.numeric(core[[lake]])
  
  best <- list(r = -2, d0 = 0, s = 1, n = 0)
  
  for (d0 in seq(0, 1, by = 0.05)) {
    for (s in seq(0.2, 5, by = 0.02)) {
      
      depth_pred <- d0 - (trap_years - ANCHOR_YEAR) / s
      valid <- depth_pred >= min(depths) & depth_pred <= max(depths)
      if (sum(valid) < 6) next
      
      core_at_year <- approx(depths, core_val, xout = depth_pred[valid])$y
      if (any(is.na(core_at_year))) next
      
      r <- suppressWarnings(cor(trap_val[valid], core_at_year))
      
      if (!is.na(r) && r > best$r) {
        best <- list(r = r, d0 = d0, s = s, n = sum(valid))
      }
    }
  }
  
  if (best$r <= -1) {
    return(list(lake = lake, r = NA, p = NA, d0 = NA, s = NA, n = 0))
  }
  
  opt <- optim(par = c(best$d0, best$s), fn = neg_corr,
               trap_val = trap_val, core_val = core_val,
               trap_years = trap_years, depths = depths,
               anchor_year = ANCHOR_YEAR,
               method = "L-BFGS-B", lower = c(0, 0.15), upper = c(1, 6))
  
  d0 <- opt$par[1]; s <- opt$par[2]
  
  depth_pred <- d0 - (trap_years - ANCHOR_YEAR) / s
  valid <- depth_pred >= min(depths) & depth_pred <= max(depths)
  core_at_year <- approx(depths, core_val, xout = depth_pred[valid])$y
  
  r <- suppressWarnings(cor(trap_val[valid], core_at_year))
  p <- suppressWarnings(cor.test(trap_val[valid], core_at_year)$p.value)
  
  list(lake = lake, r = r, p = p, d0 = d0, s = s, n = sum(valid))
}

# ---- 4. Lacs dans l’ordre demandé ----

# noms internes = colonnes des dataframes
lakes_internal <- c("Loup", "Nano", "Dave", "Walt", "Garot", "Schön", "Pessière")

# noms affichés = titres des graphiques
lakes_display  <- c("Loup", "Nano", "Dave", "Walt", "Garot", "Schön", "Pessière")

fits <- setNames(lapply(lakes_internal, fit_lake), lakes_display)


# ---- 6. Figure ----
X11()
par(mfrow = c(4, 2), mar = c(4, 4, 5, 4))

for (lake in lakes_display) {
  
  f <- fits[[lake]]
  trap_val <- as.numeric(traps[[lake]])
  core_val <- as.numeric(core[[lake]])
  
  if (is.na(f$d0)) {
    plot.new()
    mtext(lake, side = 3, line = 3, cex = 0.9)
    next
  }
  
  core_year <- ANCHOR_YEAR - (depths - f$d0) * f$s
  mask <- core_year >= (min(trap_years) - 1) & core_year <= (max(trap_years) + 1)
  depth_pred <- f$d0 - (trap_years - ANCHOR_YEAR) / f$s
  
  plot(trap_years, trap_val, type = "n",
       xlab = "", ylab = "", main = "", xaxt = "n")
  
  axis(1, at = pretty(trap_years), labels = pretty(trap_years))
  axis(3, at = trap_years, labels = round(depth_pred, 1))
  
  mtext(lake, side = 3, line = 3, cex = 0.9)
  
  rect(trap_years - 0.25, 0, trap_years + 0.25, trap_val,
       col = adjustcolor("red", alpha.f = 0.55), border = NA)
  
  mtext("CHAR traps (mm²)", side = 2, line = 2.3, col = "red", cex = 0.7)
  axis(2, col.axis = "red")
  
  par(new = TRUE)
  plot(core_year[mask], core_val[mask], type = "o", pch = 16, cex = 1, lwd = 2,
       col = "black", axes = FALSE, xlab = "", ylab = "",
       xlim = range(trap_years))
  
  axis(4, col.axis = "black")
  mtext("CHAR cores (mm²)", side = 4, line = 2.3, col = "black", cex = 0.7)
}
# ---- 5. Résumé ----

cat("Lake        r        p-value        n    d0(cm)   sedim(cm/yr)\n")
for (lake in lakes_display) {
  f <- fits[[lake]]
  cat(sprintf("%-10s %.3f    %.3g      %2d    %.2f      %.3f\n",
              lake, f$r, f$p, f$n, f$d0, 1 / f$s))
}

