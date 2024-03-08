# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# |                                                                                    |
# |          SCRIPT WRITTEN BY THOMAS DELATTRE thomas.delattre@inrae.fr| 
# | "99.9% based on Joshua Flickinger's amazing work available here :   "              |
# | https://rug.mnhn.fr/seewave/spec.html                                             |
# |                              ----------------                                      | 
# |                              LICENCE CC-BY-SA                                      | 
# |                              ----------------                                      |
# | This license lets others remix, adapt, and build upon your work even for           |
# | commercial purposes, as long as they credit you and license their new creations    |
# | under the identical terms.                                                         |
# |                                                                                    |
# | The proposed code has a purely academic purpose, is valid under the conditions     |
# | of use of the scientific project for which it was funded and at the date of        |
# | acceptance of the article presenting the code. As with any research work, the      |
# | code is not free of possible errors, approximations, sub-optimisations or          |
# | defects in monitoring dependencies between libraries of the program.               |
# |                                                                                    |
# ______________________________________________________________________________________ 
# |                                                                                    |
# | Cette licence permet à d'autres personnes de remixer, d'adapter et de              |
# | développer ce travail, même à des fins commerciales, à condition qu'elles          |
# | créditent l'auteur et accordent une licence pour leurs nouvelles créations aux     |
# | mêmes conditions.                                                                  |
# |                                                                                    |
# | Le code proposé a une visée purement académique, est valable dans les conditions   |
# | d'utilisation du projet scientifique pour lequel il a été financé et à la date de  |
# | d'acceptation de l'article de présentation du code.                                |
# | Comme tout travail de recherche, le code n'est pas exempt d'éventuelles erreurs,   |
# | approximations, sous-optimisations ou défauts de suivi des dépendances entre       |
# | sous-éléments du programme.                                                        |
# ______________________________________________________________________________________ 
# Objectif du script : 
# The script takes a Wave R object (created by readWave function) and gives 
# a nice and contrasted spectrogram of your recording. 
# Please be aware that long and high-frequency recordings can take a lot of 
# time to show up.
# --------------------------------------------
# Changelog : 
# V1 = initial commit
# --------------------------------------------


spectroCustom = function (sourceWav) {


#-------------------------------------------
## LOADING REQUIRED PACKAGES
#-------------------------------------------

library(seewave)
library(tuneR)
library(ggplot2)
library(viridis)
library(grid)
library(gridExtra)

#-------------------------------------------
## SETUP FOR PLOTS
#-------------------------------------------

## PLOT LABELLERS

# x label formatter
s_formatter <- function(x){
  lab <- paste0(x, " s")
}

# y label formatter
khz_formatter <- function(y){
  lab <- paste0(y, " kHz")
}

## THEMES

oscillo_theme_dark <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                            panel.grid.major.x = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_rect(fill="transparent"),
                            panel.border = element_rect(linetype = "solid", fill = NA, color = "grey"),
                            axis.line = element_blank(),
                            legend.position = "none",
                            plot.background = element_rect(fill="black"),
                            plot.margin = unit(c(0,1,1,1), "lines"),
                            axis.title = element_blank(),
                            axis.text = element_text(size=14, color = "grey"),
                            axis.ticks = element_line(color="grey"))

hot_theme <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_rect(fill="transparent"),
                   panel.border = element_rect(linetype = "solid", fill = NA, color = "grey"),
                   axis.line = element_blank(),
                   legend.position = "top",
                   legend.justification = "right",
                   legend.background = element_rect(fill="black"),
                   legend.key.width = unit(50, "native"),
                   legend.title = element_text(size=16, color="grey"),
                   legend.text = element_text(size=16, color="grey"),
                   plot.background = element_rect(fill="black"),
                   axis.title = element_blank(),
                   axis.text = element_text(size=16, color = "grey"),
                   axis.ticks = element_line(color="grey"))

hot_theme_grid <- theme(panel.grid.major.y = element_line(color="black", linetype = "dotted"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_rect(fill="transparent"),
                        panel.border = element_rect(linetype = "solid", fill = NA, color = "grey"),
                        axis.line = element_blank(),
                        legend.position = "top",
                        legend.justification = "right",
                        legend.background = element_rect(fill="black"),
                        legend.key.width = unit(50, "native"),
                        legend.title = element_text(size=16, color="grey"),
                        legend.text = element_text(size=16, color="grey"),
                        plot.background = element_rect(fill="black"),
                        plot.margin = margin(1,1,0,1, "lines"),
                        axis.title = element_blank(),
                        axis.text = element_text(size=16, color = "grey"),
                        axis.text.x = element_blank(),
                        axis.ticks = element_line(color="grey"))

## COLORS

hot_colors <- inferno(n=9)

#-------------------------------------------
## LOADING IN A WAV
#-------------------------------------------

# the path to .wav file
# note: the file is a clip of a canyon wren song, originally uploaded to xeno-canto.org by Bobby Wilcox
# note: the sound clip is in the creative commons (CC BY-NC-SA 4.0), downloaded from this link:
# https://www.xeno-canto.org/381415

# wavefile_path <- ".\\XC381415 - Canyon Wren - Catherpes mexicanus.wav"
# 
# # loads a wave object from the .wav file path
# wav <- readWave(wavefile_path)
wav=sourceWav

# builds a dataframe of the wave object values
sample <- seq(1:length(wav@left))
time <- sample/wav@samp.rate
sample.left <- as.vector(cbind(wav@left))
df <- data.frame(sample, time, sample.left)

# subsets the dataframe to a more manageable size for plotting
last.index <- tail(df$sample,1)
index <- seq(from = 1, to = last.index, by = 20)
df2 <- df[index,]


#-------------------------------------------
## GGSPECTRO PLOTS
#-------------------------------------------

# builds a spectrogram using ggspectro()
# note: no x-axis labels because the plot is designed to be aligned with the oscillogram in a grid
# for x-axis labels, use hot_theme instead of hot_theme_grid
hotplot <- ggspectro(wave = wav, f = wav@samp.rate, ovlp=90)+ 
  scale_x_continuous(labels=s_formatter, expand = c(0,0))+
  scale_y_continuous(breaks = seq(from = 5, to = 20, by=5), expand = c(0,0), labels = khz_formatter, position = "right")+
  geom_raster(aes(fill=amplitude), hjust = 0, vjust = 0, interpolate = F)+
  scale_fill_gradientn(colours = hot_colors, name = "Amplitude \n (dB)", na.value = "transparent", limits = c(-60,0))+
  hot_theme_grid

# builds an oscillogram
oscplot <- ggplot(df2)+
  geom_line(mapping = aes(x=time, y=sample.left), color="grey")+ 
  scale_x_continuous(labels=s_formatter, expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), position = "right")+
  geom_hline(yintercept = 0, color="white", linetype = "dotted")+
  oscillo_theme_dark

#-------------------------------------------
## PLOT GRID
#-------------------------------------------

gA=ggplot_gtable(ggplot_build(hotplot))
gB=ggplot_gtable(ggplot_build(oscplot))
maxWidth = grid::unit.pmax(gA$widths, gB$widths)
gA$widths <- as.list(maxWidth)
gB$widths <- as.list(maxWidth)
layo <- rbind(c(1,1,1),
              c(1,1,1),
              c(1,1,1),
              c(2,2,2))

grid.newpage()
gg=grid.arrange(gA, gB, layout_matrix = layo)
return(gg)
}
             