###############################################################################
#     Analysis of fire and Autumn Persistent Green on Cape York Peninsula
#                     Gabriel Crowley 6 April 2023
#  This file uses generalized additive modelling to explain changes in fire
#  regime and Autumn Persistent Green (a measure of projective foliage cover) 
#  on Cape York Peninsula
###############################################################################

###############################################################################
#                 LOAD PACKAGES and SET UP DIRECTORY                          #
###############################################################################
# setwd("~/R")
# download the file "CYP.RData" from 
# https://github.com/GM-Crowley/CYP to your working directory 

# create a folder called "output" within your working directory to store figures
# and tables

# clear list
rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(mgcv)
  library(tidyr)
  library(visreg)
})

###############################################################################
#                     Load and format data table
###############################################################################

load("CYP.RData")

# organise factors

cols <- c("cells", "Vname", "Grazing" , "Burnt.LY", "EBurnt.TY", "LBurnt.TY")
CYP[cols] <- lapply(CYP[cols], factor)
CYP$Vname <- factor(CYP$Vname, levels= c("Rainforest",  "Eucalypt woodland", 
    "Floodplain forest",  "Teatree woodland",  "Grassland", "Other vegetation"))
CYP$Burnt.LY <- factor(CYP$Burnt.LY, levels= c("Unburnt", "EDS", "LDS"))
summary(CYP)
rm(cols)

###############################################################################
#               Produce summary statistics for historical file
###############################################################################

temp <- subset(CYP, select = c("cells", "year", "Vname", "PG"))
CYP_wide <- reshape(temp, v.names="PG", timevar="year", 
                    idvar=c("cells", "Vname"),direction="wide")
CYP_wide <- subset(CYP_wide, select = c("cells", "Vname", "PG.2001", "PG.2018"))
CYP_wide$PG.change <- CYP_wide$PG.2018-CYP_wide$PG.2001

CYP.PG.change.summary <- CYP_wide %>%
  group_by(Vname) %>%
  summarize(n = n(),
            mean = round(mean(PG.change), 2),
            sem = round(sd(PG.change)/ sqrt(n), 2))

temp <- CYP_wide %>%
  summarize(n = n(),
            mean = round(mean(PG.change), 2),
            sem = round(sd(PG.change)/ sqrt(n), 2))

temp$Vname <- "Cape York Peninsula"

CYP.PG.change.summary <- rbind(CYP.PG.change.summary, temp)
rm(temp, CYP_wide)

CYP.PG.change.summary
write.csv(CYP.PG.change.summary, "./output/CYP.PG.change.summary.csv")
rm(CYP.PG.change.summary)

CYP.burn.freq <- as.data.frame(with(CYP, table(year, Burnt.LY)))
CYP.burn.freq.wide <- tidyr::spread(CYP.burn.freq, Burnt.LY, Freq)
CYP.burn.freq.wide$total <- CYP.burn.freq.wide$Unburnt + 
  CYP.burn.freq.wide$EDS + CYP.burn.freq.wide$LDS
a <- mean(CYP.burn.freq.wide$total)/100

CYP.burn.freq.wide$Unburnt <- round(CYP.burn.freq.wide$Unburnt/a,2)
CYP.burn.freq.wide$EDS <- round(CYP.burn.freq.wide$EDS/a,2)
CYP.burn.freq.wide$LDS <- round(CYP.burn.freq.wide$LDS/a,2)
CYP.burn.freq.wide$total <- CYP.burn.freq.wide$Unburnt + 
  CYP.burn.freq.wide$EDS + CYP.burn.freq.wide$LDS
CYP.burn.freq.wide <- subset(CYP.burn.freq.wide, select = -c(total))

CYP.burn.freq$Freq <- CYP.burn.freq$Freq/a
CYP.burn.freq.summary <- CYP.burn.freq %>%
  group_by(Burnt.LY) %>%
  summarize(mean = round(mean(Freq), 2),
            s.e.m = round(sd(Freq)/ sqrt(n()), 2))
CYP.burn.freq.summary <- t(CYP.burn.freq.summary)
colnames(CYP.burn.freq.summary) <- as.character(CYP.burn.freq.summary[1,])
CYP.burn.freq.summary <- CYP.burn.freq.summary[-1,]
CYP.burn.freq.summary <- cbind(rownames(CYP.burn.freq.summary), 
  data.frame(CYP.burn.freq.summary, row.names=NULL))
names(CYP.burn.freq.summary$`rownames(CYP.burn.freq.summary)`) <- "year"
colnames(CYP.burn.freq.summary)[1] <- "year"

CYP.burn.freq <- rbind(CYP.burn.freq.wide, CYP.burn.freq.summary)

CYP.burn.freq
write.csv(CYP.burn.freq, "./output/CYP.burn.freq.csv")
rm(CYP.burn.freq.summary, CYP.burn.freq, CYP.burn.freq.wide, a)

###############################################################################
#             Characterise Early Dry Season Fire occurrence
###############################################################################

# run Generalized Additive Models to examine year effect on EDS fire occurrence 
# in each vegetation type, accounting for grazing, rainfall, and location as 
# random effects, and removing non-significant independent variables from the 
# models 

# identify whether EDS fire is most responsive to one-, two-, or
# three-yearly rainfall anomalies, using one knot for each year (k = 18)

# model with one-year rainfall anomaly
EBurn.bam.RA1 <- mgcv::bam(EBurnt.TY ~
                     s(year, Vname, k = 18, bs = "fs") + 
                     s(Burnt.LY, bs = "re") + 
                     s(Grazing, bs = "re") + 
                     s(Rain.Avg, bs = 're') + 
                     s(RA1, bs = 're') + 
                     s(x, y, bs = 're'),
                   data = CYP, family = binomial, method = 'fREML', 
                   discrete = TRUE, nthreads = 2)
summary(EBurn.bam.RA1)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with two-year rainfall anomaly
EBurn.bam.RA2 <- mgcv::bam(EBurnt.TY ~
                     s(year, Vname, k = 18, bs = "fs") + 
                     s(Grazing, bs = "re") + 
                     s(Burnt.LY, bs = "re") + 
                     s(RA2, bs = 're') + 
                     s(Rain.Avg, bs = 're') + 
                     s(x, y, bs = 're'),
                   data = CYP, family = binomial, method = 'fREML', 
                   discrete = TRUE, nthreads = 2)
summary(EBurn.bam.RA2)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with three-year rainfall anomaly
EBurn.bam.RA3 <- mgcv::bam(EBurnt.TY ~
                     s(year, Vname, k = 18, bs = "fs") + 
                     s(Grazing, bs = "re") + 
                     s(Burnt.LY, bs = "re") + 
                     s(RA3, bs = 're') + 
                     s(Rain.Avg, bs = 're') + 
                     s(x, y, bs = 're'),
                   data = CYP, family = binomial, method = 'fREML', 
                   discrete = TRUE, nthreads = 2)
summary(EBurn.bam.RA3)
# all independent variables except location make significant contributions, so 
# retain in the model

# select the model with the lowest residual deviance
anova(EBurn.bam.RA1, EBurn.bam.RA2, EBurn.bam.RA3, test = "Chisq")
# EBurn.bam.RA1 is the best-fit model because it has the lowest residual deviance

# assess the effect of spatial variation on best-fit model
EBurn.bam.RA1.noloc <- mgcv::bam(EBurnt.TY ~
                      s(year, Vname, k = 18, bs = "fs") + 
                      s(Grazing, bs = "re") + 
                      s(Burnt.LY, bs = "re") + 
                      s(RA3, bs = 're') + 
                      s(Rain.Avg, bs = 're'),
                   data = CYP, family = binomial, method = 'fREML', 
                   discrete = TRUE, nthreads = 2)
summary(EBurn.bam.RA1.noloc)

# select the model with the lowest residual deviance
anova(EBurn.bam.RA1, EBurn.bam.RA1.noloc, test = "Chisq")
# EBurn.bam.RA1 is the best-fit model because it has the lowest residual deviance

# save summary statistics of best-fit model
sink("./output/EDS_bestfit_model.txt")
summary(EBurn.bam.RA1)
sink()

# rerun the best-fit with three knots for maximum smoothing
EBurn3k.bam.RA1 <- mgcv::bam(EBurnt.TY ~
                               s(year, Vname, k = 3, bs = "fs") + 
                               s(Burnt.LY, bs = "re") + 
                               s(Grazing, bs = "re") + 
                               s(Rain.Avg, bs = 're') + 
                               s(RA1, bs = 're') + 
                               s(x, y, bs = 're'),
                             data = CYP, family = binomial, method = 'fREML',
                             discrete = TRUE, nthreads = 2)
summary(EBurn3k.bam.RA1)

# compare the 18 knot and three knot models
anova(EBurn.bam.RA1, EBurn3k.bam.RA1, test = "Chisq")
# EBurn.bam.RA1 is the best-fit model because it has the lowest residual deviance

# save summary statistics of 3-knot model
sink("./output/EDS_3kmodel.txt")
summary(EBurn3k.bam.RA1)
sink()

# extract and plot year effect on EDS fire occurrence from best-fit model

# plot the best-fit model with 18 knots using visreg
vEDS18k <- visreg::visreg(EBurn.bam.RA1, "year", "Vname", trans = plogis, 
       partial=FALSE, rug=FALSE, 
       ylab=list(label ="Probability of Early Dry Season fire", cex = 0.8), 
       xlab = "", ylim = c(0, 0.6), line=list(col="black", lwd = 5), 
       fill=list(col="grey"), scales=list(cex=0.6))

# plot the best-fit model with three knots using visreg
vEDS3k <- visreg::visreg(EBurn3k.bam.RA1, "year", "Vname", trans = plogis, 
       partial=FALSE, rug=FALSE, 
       ylab=list(label ="Probability of Early Dry Season fire", 
       cex = 0.8), xlab = "", ylim = c(0, 0.6), 
       line=list(col="black", lwd = 5, lty = "dashed"), 
       fill=list(col="white"), scales=list(cex=0.6))

dev.off()

# extract the data used for these plots to combine in a single plot of yearly 
# fire occurrence

fitsEDS <- dplyr::bind_rows(
  dplyr::mutate(vEDS3k$fit, plt = "EBurn3k.bam.RA1"),
  dplyr::mutate(vEDS18k$fit, plt = "EBurn.bam.RA1"))

# format y-axis text
yaxislab1 <- paste0("Estimated year effect")
yaxislab2 <- paste0("(","\u00B1","95% confidence limits) on")
yaxislab3 <- paste0("Early Dry Season fire occurrence (%)")
yaxislab <-paste(yaxislab1, yaxislab2, yaxislab3, sep = "\n")

PlotEDS <- ggplot2::ggplot(data = fitsEDS) +
  theme_bw()+
  geom_ribbon(aes(year, ymin=visregLwr*100, ymax=visregUpr*100, group=plt), fill="grey") +
  geom_line(aes(year, visregFit*100, group=plt, linetype = plt), 
            lwd = 1) +
  facet_grid(~Vname) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(yaxislab)+xlab("")+
  theme (legend.position = "none") +
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  theme(strip.background = element_rect(color="black", fill="white"))+
  theme(strip.text.x = element_text(size = 14)) +
  theme(text = element_text(size = 14, color = "black"), 
        axis.title.y = element_text(size = 14, color = "black"), 
        axis.text = element_text(size = 14, color = "black"))+
  scale_y_continuous(limits = c(0, 60))

# clean up
rm(EBurn.bam.RA1, EBurn.bam.RA2, EBurn.bam.RA3, EBurn.bam.RA1.noloc, 
   EBurn3k.bam.RA1, yaxislab, yaxislab1, yaxislab2, yaxislab3)
invisible(gc())

###############################################################################
# Characterise Late Dry Season Fire occurrence
###############################################################################

# run Generalized Additive Models to examine year effect on EDS fire occurrence 
# in each vegetation type, accounting for grazing, rainfall, and location as 
# random effects, and removing non-significant independent variables from the 
# models 

# identify whether LDS fire is most responsive to one-, two-, or 
# three-yearly rainfall anomalies, using one knot for each year (k = 18)

# model with one-year rainfall anomaly
LBurn.bam.RA1 <- mgcv::bam(LBurnt.TY ~
                   s(year, Vname, k = 18, bs = "fs") + 
                   s(EBurnt.TY, bs = "re") + 
                   s(Burnt.LY, bs = "re") +
                   s(Grazing, bs = "re") + 
                   s(Rain.Avg, bs = 're') + 
                   s(RA1, bs = 're') + 
                   s(x, y, bs = 're'),
                   data = CYP, family = binomial, method = 'fREML',
                   discrete = TRUE, nthreads = 2)
summary(LBurn.bam.RA1)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with two-year rainfall anomaly
LBurn.bam.RA2 <- mgcv::bam(LBurnt.TY ~
                             s(year, Vname, k = 18, bs = "fs") + 
                             s(EBurnt.TY, bs = "re") + 
                             s(Burnt.LY, bs = "re") +
                             s(Grazing, bs = "re") + 
                             s(Rain.Avg, bs = 're') + 
                             s(RA2, bs = 're') + 
                             s(x, y, bs = 're'),
                           data = CYP, family = binomial, method = 'fREML',
                           discrete = TRUE, nthreads = 2)
summary(LBurn.bam.RA2)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with three-year rainfall anomaly
LBurn.bam.RA3 <- mgcv::bam(LBurnt.TY ~
                             s(year, Vname, k = 18, bs = "fs") + 
                             s(EBurnt.TY, bs = "re") + 
                             s(Burnt.LY, bs = "re") +
                             s(Grazing, bs = "re") + 
                             s(Rain.Avg, bs = 're') + 
                             s(RA3, bs = 're') + 
                             s(x, y, bs = 're'),
                           data = CYP, family = binomial, method = 'fREML',
                           discrete = TRUE, nthreads = 2)
summary(LBurn.bam.RA3)
# all independent variables except location make significant contributions, so 
# retain in the model

# select the model with the lowest residual deviance
anova(LBurn.bam.RA1, LBurn.bam.RA2, LBurn.bam.RA3, test = "Chisq")
# LBurn.bam.RA3 is the best-fit model because it has the lowest residual deviance

# assess the effect of spatial variation on best-fit model
LBurn.bam.RA3.noloc <- mgcv::bam(LBurnt.TY ~
                     s(year, Vname, k = 18, bs = "fs") + 
                     s(EBurnt.TY, bs = "re") + 
                     s(Burnt.LY, bs = "re") +
                     s(Grazing, bs = "re") + 
                     s(Rain.Avg, bs = 're') +
                     s(RA3, bs = 're'), 
                    data = CYP, family = binomial, method = 'fREML',
                    discrete = TRUE, nthreads = 2)
summary(LBurn.bam.RA3.noloc)

# select the model with the lowest deviance score as the best-fit model
anova(LBurn.bam.RA3, LBurn.bam.RA3.noloc, test = "Chisq")
# LBurn.bam.RA3 is the best-fit model because it has the lowest residual deviance

# save summary statistics of best-fit model
sink("./output/LDS_bestfit_model.txt")
summary(LBurn.bam.RA3)
sink()

# rerun the best-fit with three knots for maximum smoothing
LBurn3k.bam.RA3 <- mgcv::bam(LBurnt.TY ~
                               s(year, Vname, k = 3, bs = "fs") + 
                       s(EBurnt.TY, bs = "re") + 
                       s(Burnt.LY, bs = "re") +
                       s(Grazing, bs = "re") + 
                       s(Rain.Avg, bs = 're') + 
                       s(RA3, bs = 're') + 
                       s(x, y, bs = 're'),
                     data = CYP, family = binomial, method = 'fREML',
                     discrete = TRUE, nthreads = 2)
summary(LBurn3k.bam.RA3)

# compare the 18 knot and three knot models
anova(LBurn.bam.RA3, LBurn3k.bam.RA3, test = "Chisq")
# LBurn.bam.RA3 is the best-fit model because it has the lowest residual deviance

# save summary statistics of 3-knot model
sink("./output/LDS_3kmodel.txt")
summary(LBurn3k.bam.RA3)
sink()

# extract and plot year effect on LDS fire occurrence from best-fit model

# plot the best-fit model with 18 knots using visreg
vLDS18k <- visreg::visreg(LBurn.bam.RA3, "year", "Vname", trans = plogis,
       partial=FALSE, rug=FALSE, 
       ylab=list(label ="Probability of Late Dry Season fire", cex = 0.8),
       xlab = "", ylim = c(0, 0.6), line=list(col="black", lwd = 5),
       fill=list(col="grey"), scales=list(cex=0.6), plot = FALSE)

# plot the best-fit model with three knots using visreg
vLDS3k <- visreg::visreg(LBurn3k.bam.RA3, "year", "Vname", trans = plogis,
       partial=FALSE, rug=FALSE, 
       ylab=list(label ="Probability of Late Dry Season fire", cex = 0.8),
       xlab = "", ylim = c(0, 0.6),
       line=list(col="black", lwd = 5, lty = "dashed"), 
       fill=list(col="white"), scales=list(cex=0.6), plot = FALSE)

# extract the data used for these plots to combine in a single plot of yearly 
# fire occurrence
fitsLDS <- dplyr::bind_rows(dplyr::mutate(vLDS3k$fit, plt = "LBurn3k.bam.RA3"),
  dplyr::mutate(vLDS18k$fit, plt = "LBurn.bam.RA3"))

# format y-axis text
# format y-axis text
yaxislab1 <- paste0("Estimated year effect")
yaxislab2 <- paste0("(","\u00B1","95% confidence limits) on")
yaxislab3 <- paste0("Late Dry Season fire occurrence (%)")
yaxislab <-paste(yaxislab1, yaxislab2, yaxislab3, sep = "\n")

PlotLDS <- ggplot2::ggplot() +
  theme_bw()+
  geom_ribbon(
    data = fitsLDS, 
     aes(year, ymin=visregLwr*100, ymax=visregUpr*100, group=plt), fill="grey") +
  geom_line(data = fitsLDS, 
           aes(year, visregFit*100, group=plt, linetype = plt), lwd = 1) +
  facet_grid(~Vname) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(yaxislab)+xlab("")+
  theme (legend.position = "none") +
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  theme(strip.background = element_rect(color="black", fill="white"))+
  theme(text = element_text(size = 14), axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14))+
  theme(strip.text.x = element_text(size = 14)) +
  scale_y_continuous(limits = c(0, 60))

Fire <- ggpubr::ggarrange(PlotEDS, PlotLDS, ncol = 1,
          nrow = 2,  labels=c("(a)", "(b)"), 
          font.label = list(size = 14, color = "black", 
                            face = "italic", family = NULL))

ggsave("./output/Plot_Fire_trends.tiff",
       Fire, width = 36.4, height = 24.3, units = "cm", dpi = 499)

# clean up
rm(LBurn.bam.RA1, LBurn.bam.RA2, LBurn.bam.RA3, LBurn.bam.RA3.noloc,
   LBurn3k.bam.RA3)
rm(vLDS18k, vLDS3k, PlotLDS, Fire, fitsLDS)
rm(vEDS18k, vEDS3k, PlotEDS, fitsEDS)
rm(yaxislab, yaxislab1, yaxislab2, yaxislab3)
invisible(gc())

################################################################################
#                      Analyse Autumn Persistent Green
################################################################################

# run Generalized Additive Model to identify changes in Persistent Green that 
# have occurred in each vegetation type over 18 years, using one knot for each
# year (k = 18)

PG.bam.yr <- mgcv::bam(PG ~
                 s(year, Vname, k = 18, bs = "fs"),
                 data = CYP, family = gaussian, 
                 method = 'fREML', discrete = TRUE, nthreads = 2)
summary(PG.bam.yr)

# plot  changes in Persistent Green that have occurred in 
# each vegetation type over 18 years using visreg

PG_trend <- visreg::visreg(PG.bam.yr, "year", "Vname", 
       partial=FALSE, overlay = TRUE, rug = FALSE, 
       ylim = c(0, 70), ylab="Autumn Persistent Green", xlab = "",
       line=list(col="black", lwd = 3, lty = c(2,5,1,3,4,6)), 
       fill=list(col="light grey"))

dev.off()

# extract the data used for this plot, for combining in a single plot
# of yearly PG change in each vegetation type
fitsPGtrend <- dplyr::bind_rows(dplyr::mutate(PG_trend$fit, 
                                              plt = "Uncorrected"),)

fitsPGtrend$Vname <- ordered(fitsPGtrend$Vname, levels =c("Rainforest",
    "Eucalypt woodland", "Other vegetation", "Teatree woodland", 
    "Floodplain forest", "Grassland"))

# format y-axis text
yaxislab1 <- paste0("Actual change (","\u00B1","95% confidence limits) in")
yaxislab2 <- "Autumn Persistent Green (%)"
yaxislab <-paste(yaxislab1, yaxislab2, sep = "\n")

PlotPGtrend <- 
  ggplot() +
  theme_bw()+
  geom_ribbon(
    data = fitsPGtrend, 
    aes(year, ymin=visregLwr, ymax=visregUpr, group=Vname), fill="gray90") +
  geom_line(data = fitsPGtrend, aes(year, visregFit, group=Vname, 
                                    linetype = Vname), lwd = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  ylab(yaxislab)+xlab("")+
  theme (legend.position=c(.75,.65), 
         legend.box.background = element_rect(colour = "black", size = 1.5), 
         legend.key.width = unit(4, "line"))+
  theme(strip.background = element_rect(color="black", fill="white"))+
  theme(text = element_text(size = 14), axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14, color = "black"), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14))+
  scale_y_continuous(limits = c(0, 70))+
  guides(linetype=guide_legend(title="Vegetation type"))

# run Generalized Additive Models to examine impact of year, recent fire 
# history and grazing land tenure on Persistent Green in each vegetation type, 
# with rainfall, rainfall variation and location as random effects, using one
# knot for each year (k = 18), and removing non-significant independent
# variables from the models

# identify whether one-, two- or three-yearly rainfall anomalies has the 
# greatest effect

# model with one-year rainfall anomaly
PG.bam.RA1 <- mgcv::bam(PG ~
                          s(year, Vname, k = 18, bs = "fs") +
                          Vname * Burnt.LY +
                          Vname * Grazing +
                          s(Rain.Avg, bs = "re") +
                          s(RA1, bs = "re") +
                          s(x, y, bs = "re"),
 data = CYP, family = gaussian, method = 'fREML', discrete = TRUE, nthreads = 2)
summary(PG.bam.RA1)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with two-year rainfall anomaly
PG.bam.RA2 <- mgcv::bam(PG ~  
                          s(year, Vname, k = 18, bs = "fs") +
                          Vname * Burnt.LY +
                          Vname * Grazing +
                          s(Rain.Avg, bs = "re") +
                          s(RA2, bs = "re") +
                          s(x, y, bs = "re"),
 data = CYP, family = gaussian, method = 'fREML', discrete = TRUE, nthreads = 2)
summary(PG.bam.RA2)
# all independent variables except location make significant contributions, so 
# retain in the model

# model with three-year rainfall anomaly
PG.bam.RA3 <- mgcv::bam(PG ~  
                          s(year, Vname, k = 18, bs = "fs") +
                          Vname * Burnt.LY +
                          Vname * Grazing +
                          s(Rain.Avg, bs = "re") +
                          s(RA3, bs = "re") +
                          s(x, y, bs = "re"),
 data = CYP, family = gaussian, method = 'fREML', discrete = TRUE, nthreads = 2)
summary(PG.bam.RA3)
# all independent variables except location make significant contributions, so 
# retain in the model

# select the model with the lowest deviance score
anova(PG.bam.RA1, PG.bam.RA2, PG.bam.RA3, test = "Chisq")
# PG.bam.RA3 is the best-fit model because it has the lowest residual deviance

# assess the effect of spatial variation
PG.bam.RA3.noloc <- mgcv::bam(PG ~  
                          s(year, Vname, k = 18, bs = "fs") +
                          Vname * Burnt.LY +
                          Vname * Grazing +
                          s(RA3, bs = "re") +
                          s(Rain.Avg, bs = "re"),
                  data = CYP, family = gaussian, method = 'fREML',
                  discrete = TRUE, nthreads = 2)
summary(PG.bam.RA3.noloc)

# select the model with the lowest deviance score as the best-fit model
anova(PG.bam.RA3, PG.bam.RA3.noloc, test = "Chisq")
# PG.bam.RA3 is the best-fit model because it has the lowest residual deviance

# save summary statistics of best-fit model
sink("./output/PG_bestfit_model.txt")
summary(PG.bam.RA3)
sink()

# extract and plot year effect on PG from best-fit model

# plot best fit model in visreg
PG_year <- visreg::visreg(PG.bam.RA3, "year", "Vname", partial=FALSE, 
       overlay = TRUE, rug = FALSE, 
       ylim = c(0, 70), ylab="Persistent Green", xlab = "",
       line=list(col="black", lwd = 1, lty = c(2,5,1,3,4,6)), 
       fill=list(col="light grey"))

dev.off()

# extract the data used for this plot, for combining in a single plot
# of yearly PG change in each vegetation type
fitsYear <- dplyr::bind_rows(dplyr::mutate(PG_year$fit, plt = "Corrected"),)

fitsYear$Vname <- ordered(fitsYear$Vname, levels =c("Rainforest", 
  "Eucalypt woodland", "Other vegetation", "Teatree woodland", 
  "Floodplain forest", "Grassland"))

# format y-axis text
yaxislab1 <- paste0("Estimated change (","\u00B1","95% confidence limits) in")
yaxislab2 <- "Autumn Persistent Green based on year effect alone (%)"
yaxislab <-paste(yaxislab1, yaxislab2, sep = "\n")

PlotPGyear <- 
ggplot() +
  theme_bw()+
  geom_ribbon(
    data = fitsYear, 
    aes(year, ymin=visregLwr, ymax=visregUpr, group=Vname), fill="gray90", 
    size = 1) +
  geom_line(data = fitsYear, aes(year, visregFit, group=Vname, 
                                 linetype = Vname), lwd = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  ylab(yaxislab)+xlab("")+
  theme (legend.position="none") +
  theme(strip.background = element_rect(color="black", fill="white"))+
  theme(text = element_text(size = 14), axis.title.y = element_text(size = 14), 
        axis.text = element_text(size = 14, color = "black"), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14))+
  scale_y_continuous(limits = c(0, 70))

PG <- ggpubr::ggarrange(PlotPGtrend, PlotPGyear, ncol = 1, nrow = 2, 
    labels=c("(a)", "(b)"), font.label = list(size = 14, color = "black",
                                              face = "italic", family = NULL))

ggsave("./output/Plot_PG_Trend.tiff", 
       PG, width = 17.6, height = 30, units = "cm", dpi = 499)

#clean up
rm(PG.bam.yr, PG.bam.RA1, PG.bam.RA2, PG.bam.RA3.noloc)
rm(PG_trend, PG_year)
rm(fitsPGtrend, fitsYear)
rm(PG, PlotPGtrend, PlotPGyear)
invisible(gc())

# extract and plot impact of fire season on PG from best-fit model

# plot best-fit model using visreg
PG_Fire <- visreg::visreg(PG.bam.RA3, "Burnt.LY", by = "Vname")

dev.off()

# extract the data used for this plot, for combining in a single plot
# of influence of management on PG in each vegetation type
fitsPGfire <- dplyr::bind_rows(dplyr::mutate(PG_Fire$fit, plt = "Uncorrected"),)
fitsPGfire$Burnt.LY <- ordered(fitsPGfire$Burnt.LY, 
                               levels =c("Unburnt", "LDS", "EDS"))

# format y-axis text
yaxislab1 <- paste0("Estimated effect (","\u00B1","95% confidence limits) on")
yaxislab2 <- "Autumn Persistent Green (%)"
yaxislab <-paste(yaxislab1, yaxislab2, sep = "\n")

Fire <- ggplot(fitsPGfire, 
               aes(x=Vname, y=visregFit, shape = Burnt.LY, fill=Burnt.LY)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(yaxislab)+xlab("")+
  geom_errorbar(aes(ymin=visregLwr, ymax=visregUpr), width=0.35, lwd = 1) +
  geom_point(size = 5, stroke = 1.5)+
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  theme (legend.position=c(.8,.75), 
         legend.box.background = element_rect(colour = "black", size = 1.5), 
         legend.key.width = unit(2, "line"))+
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        text = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14)) +
  scale_shape_manual(name = "Previous year\nfire season", 
                     values=c(21, 22, 23), 
                     labels = c("None", "Late Dry Season", 
                                "Early Dry Season")) +
  scale_fill_manual(name = "Previous year\nfire season", 
                    values=c("white", "black", "grey"), 
                    labels = c("None", "Late Dry Season", 
                               "Early Dry Season")) +
  scale_x_discrete(name ="", labels = c("Rainforest", "Eucalypt\nwoodland",
  "Floodplain\nforest", "Teatree\nwoodland", "Grassland", "Other\nvegetation"))

# extract and plot effects of grazing land tenure on PG from best-fit model

# plot best-fit model using visreg
PG_Graz <- visreg::visreg(PG.bam.RA3, "Grazing", by = "Vname")

dev.off()

# extract the data used for this plot, for combining in a single plot
# of influence of management on PG in each vegetation type
fitsPGGraz <- dplyr::bind_rows(dplyr::mutate(PG_Graz$fit, plt = "Uncorrected"),)

Graz <- ggplot(fitsPGGraz, 
               aes(x=Vname, y=visregFit, shape = Grazing, fill=Grazing)) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab(yaxislab)+xlab("")+
  geom_errorbar(aes(ymin=visregLwr, ymax=visregUpr), width=0.35, lwd = 1) +
  geom_point(size = 5, stroke = 1.5)+
  theme(axis.line = element_line(size = 0), 
        axis.ticks = element_line(size = 0.8), 
        panel.border = element_rect(size=1.5))+
  theme (legend.position=c(.8,.75), 
         legend.box.background = element_rect(colour = "black", size = 1.5), 
         legend.key.width = unit(2, "line"))+
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        text = element_text(size = 14), axis.title.y = element_text(size = 14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14)) +
  scale_shape_manual(name = "Grazing land", 
                     values=c(21, 22), 
                     labels = c("No", "Yes")) +
  scale_fill_manual(name = "Grazing land", 
                    values=c("white", "black"), 
                    labels = c("No", "Yes")) +
  scale_x_discrete(name ="", labels = c("Rainforest", "Eucalypt\nwoodland", 
  "Floodplain\nforest", "Teatree\nwoodland", "Grassland", "Other\nvegetation"))

Management <- ggpubr::ggarrange(Fire, Graz, ncol = 1, nrow = 2, 
      labels=c("(a)", "(b)"), font.label = list(size = 14, color = "black", 
                                                face = "italic", family = NULL))

ggsave("./output/Plot_PG_Management.tiff",
       Management, width = 17.6, height = 30, units = "cm", dpi = 499)

# clean up
rm(CYP, Fire, fitsPGfire, fitsPGGraz, Graz, Management, PG_Fire, PG_Graz, 
   PG.bam.RA3, yaxislab, yaxislab1, yaxislab2)
